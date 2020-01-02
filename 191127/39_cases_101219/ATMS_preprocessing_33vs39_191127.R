###################################################################
#     ATMS_preprocessing_33vs39_191127                                  #
#     R script                                                    #
#     10/12/19 - Lucas Michon                                     #
#  preprocessing ATMS data for satscan analysis                   #
#  joining cases file + births file + coordinate files            #
#  comparison between the different cases files                   #
#                                                                 #
#  #
###################################################################

setwd("F:/ATMS/191127/39_cases_101219/")
##library
{
  library(tidyverse)
  library(stringr)
}

##function
{
  factorToNumeric=function(x) as.numeric(as.character(x))
}

##data
{
  new_atms=read.csv2("F:/ATMS/191127/atms191127.csv",sep=",",header = TRUE)
  load("F:/ATMS/population.RData")
  naissance_vivante = read.csv2("F:/ATMS/population.csv",sep = ",") #born files
  coordonne = read.csv2("F:/ATMS/superficie.csv") #admin file with gps coor
  old_cases=readxl::read_xlsx("F:/ATMS/ATMS pour Lucas.xlsx")
  
}


#########################
##   MAIN              ##
#########################
{
  
  
  old_cases=old_cases%>%filter(`Année de naissance`>=2009,`Année de naissance`<=2014,`departement T1`%in%c("AIN","ISERE","RHONE","LOIRE"),!grepl("sai",`Commune T1`),`Etat vital`=="vivant  à la déclaration")%>%
    mutate(`code INSEE commune T1`=as.character(`code INSEE commune T1`))%>%
    mutate(`code INSEE commune T1` = if_else(grepl("^6938[1-9]$",`code INSEE commune T1`) ,as.integer(69123),as.integer(`code INSEE commune T1`) ))%>%
    mutate(`code INSEE commune T1`=factorToNumeric(`code INSEE commune T1`))
  
  cases39 = old_cases%>%select(`Numéro de dossier Remera`, `code INSEE commune T1`,`Année de naissance`)
  #keep the img
  #toRemove = "1410212" #IMG
  #warning !!!!!! -> seems that 1440219 is also an IMG
  
  #insee code of Lyon by arrondissement -> for the whole commune
  new_atms=new_atms%>%mutate(code_INSEE = if_else(grepl("^6938[1-9]$",code.INSEE.commune.T1) ,as.integer(69123),as.integer(code.INSEE.commune.T1) ))%>%
    mutate(Numéro.de.dossier.Remera=as.character(Numéro.de.dossier.Remera))
  
  
  atms_all=new_atms%>%
    select(Numéro.de.dossier.Remera,code_INSEE,Année.de.naissance)%>%
    full_join(cases39,by=c("Numéro.de.dossier.Remera" = "Numéro de dossier Remera"),keep=TRUE)%>%
    mutate(files = ifelse(is.na(code_INSEE),"second",ifelse(is.na(`code INSEE commune T1`),"first","both")))%>%
    mutate(first_file=as.integer(!is.na(Année.de.naissance)),second_file=as.integer(!is.na(`Année de naissance`)))%>%
    mutate(Année.de.naissance=ifelse(!is.na(Année.de.naissance),Année.de.naissance,`Année de naissance`))%>%
    mutate(code_INSEE=ifelse(!is.na(code_INSEE),code_INSEE,`code INSEE commune T1`))%>%
    select(code_INSEE,annee=Année.de.naissance,first_file,second_file)
    
  
  #preprocess cases file
  new_atms_summary=atms_all%>%#new_atms%>%select(annee=Année.de.naissance,code_INSEE)%>%#=`code INSEE commune T1`)%>%
    group_by(annee,code_INSEE)%>%
    summarise(cas_1 = sum(first_file),cas_2=sum(second_file))%>%
    mutate(code_INSEE = as.numeric(code_INSEE))
  
  #join birth file & coordinate file by city (insee code)
  naissance_coor=naissance_vivante%>%
    left_join(coordonne%>%select(Code.INSEE,geo_point_2d)%>%
                mutate(Code.INSEE=as.numeric(as.character(Code.INSEE)),geo_point_2d=as.character(geo_point_2d) ),
              by=c("CODGEO"="Code.INSEE"))
  
  #lyon is by 'quartier' in coordonne
  naissance_coor$geo_point_2d[which(naissance_coor$CODGEO==69123)]="45.767840, 4.835890"
  
  #transform geo_point2d = "0.0, 1.0" in x=1.0 & y =0.0
  naissance_coor = naissance_coor%>%mutate(y=geo_point_2d%>% str_extract("^.*(?=,)")%>%as.numeric(),
                                           x=geo_point_2d%>% str_extract("(?<=, ).*$")%>%as.numeric())
  
  
  naissance_long=naissance_coor%>%gather(key="annee",value="naissance",NAISD08:NAISD17)
  
  # transform year into the format dddd 
  naissance_long = naissance_long%>%
    mutate(annee_d = annee%>%str_extract("\\d+$")%>%
             as.integer()+2000)%>%filter(annee_d%in%2009:2014)
  
  
  # join birthf file & cases file by city & date
  new_data = naissance_long%>%left_join(new_atms_summary, by= c("CODGEO" = "code_INSEE",
                                                                "annee_d"="annee"))
  
  new_data = new_data %>% mutate(cas_1 = replace_na(cas_1 , 0), cas_2= replace_na(cas_2 , 0))
  

}

{
  atms = new_atms%>%
    select(Numéro.de.dossier.Remera,code_INSEE,Année.de.naissance)%>%
    full_join(cases39,by=c("Numéro.de.dossier.Remera" = "Numéro de dossier Remera"),keep=TRUE)%>%
    mutate(files = ifelse(is.na(code_INSEE),"second",ifelse(is.na(`code INSEE commune T1`),"first","both")))%>%
    mutate(first_file=as.integer(!is.na(Année.de.naissance)),second_file=as.integer(!is.na(`Année de naissance`)))%>%
    mutate(Année.de.naissance=ifelse(!is.na(Année.de.naissance),Année.de.naissance,`Année de naissance`))%>%
    mutate(code_INSEE=ifelse(!is.na(code_INSEE),code_INSEE,`code INSEE commune T1`))%>%
    select(code_INSEE,annee=Année.de.naissance,files)%>%
    group_by(annee,code_INSEE,files)%>%
    summarise(n=n())%>%
    ungroup()%>%
    mutate(code_INSEE = as.numeric(code_INSEE))
  
  file_comparison = naissance_long%>%left_join(atms, by= c("CODGEO" = "code_INSEE",
                                                     "annee_d"="annee"))%>%
    mutate(n=ifelse(is.na(n),naissance,n),files=replace_na(files,"birth"))
  
}

##save
{
  save(new_data,file = "ATMS_33vs39_191210.RData")
  save(file_comparison,file="ATMS_33vs39_191218_filecomparison.RData")
}
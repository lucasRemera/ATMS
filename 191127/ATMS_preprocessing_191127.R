###################################################################
#     ATMS_preprocessing_191127                                   #
#     R script                                                    #
#     27/11/19 - Lucas Michon                                     #
#  preprocessing ATMS data for satscan analysis                   #
#  joining cases file + births file + coordinate files            #
#  preprocessing into format readable by satscan                  #
#                                                                 #
# Edit 10/12/19 - LM : add a control/case file without Rhone data #
###################################################################

setwd("F:/ATMS/191127/")
##library
{
  library(tidyverse)
  library(stringr)
}

##function
{
  
}

##data
{
  new_atms=read.csv2("atms191127.csv",sep=",",header = TRUE)
  load("F:/ATMS/population.RData")
  naissance_vivante = read.csv2("F:/ATMS/population.csv",sep = ",") #born files
  coordonne = read.csv2("F:/ATMS/superficie.csv") #admin file with gps coor
  
}


#########################
##   MAIN              ##
#########################
{
  #keep the img
  #toRemove = "1410212" #IMG
  #warning !!!!!! -> seems that 1440219 is also an IMG
  
  #insee code of Lyon by arrondissement -> for the whole commune
  new_atms=new_atms%>%mutate(code_INSEE = if_else(grepl("^6938[1-9]$",code.INSEE.commune.T1) ,as.integer(69123),as.integer(code.INSEE.commune.T1) ))
  
  
  #preprocess cases file
  new_atms_summary=new_atms%>%select(annee=Année.de.naissance,code_INSEE)%>%#=`code INSEE commune T1`)%>%
    group_by(annee,code_INSEE)%>%
    summarise(cas = n())%>%
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
  
  new_data = new_data %>% mutate(cas = replace_na(cas , 0))%>%
    mutate(temoins = naissance - cas)
  
  ## edit LM - 10/12/19 : data without Rhone
  data_woRhone = new_data%>%filter(DEP!=69)
  ## end edit LM - 10/12/19
}

##saving
{
  save(new_data,file = "ATMS_2009-2014_191127.RData")
  write.csv(new_data,file = "ATMS_2009-2014_191127.csv")
  
  ## edit LM - 10/12/19 : without rhone
  write.csv(data_woRhone,file = "ATMS_2009-2014_woRhone_191210.csv")
  ## end edit LM - 10/12/19
}

# #data loading
# atms = readxl::read_xlsx("F:/ATMS pour Lucas.xlsx")
# #?filtering on 2009 to 2014, in the 4 dpt and selected cases
# atms_selected = atms%>%mutate(Annee=`Année de naissance`)%>%
#   filter(Annee%in%2009:2014,`departement T1`%in%c("AIN","LOIRE","ISERE","RHONE"))%>%
#   mutate_if(is.character , list(as.factor))%>%
#   filter(`à garder pour lucas`!="revoir")
# 
# summary(atms_selected)
# #attention ! 1 img
# 
# load("F:/ATMS/population.RData")
# naissance_vivante = read.csv2("F:/ATMS/population.csv",sep = ",") #born files
# coordonne = read.csv2("F:/ATMS/superficie.csv") #admin file with gps coor
# 
# naissance_coor=naissance_vivante%>%
#   left_join(coordonne%>%select(Code.INSEE,geo_point_2d)%>%
#               mutate(Code.INSEE=as.numeric(as.character(Code.INSEE)),geo_point_2d=as.character(geo_point_2d) ),
#             by=c("CODGEO"="Code.INSEE"))
# 
# #lyon is by 'quartier' in coordonne
# naissance_coor$geo_point_2d[which(naissance_coor$CODGEO==69123)]="45.767840, 4.835890"
# 
# #transform geo_point2d = "0.0, 1.0" in x=1.0 & y =0.0
# naissance_coor = naissance_coor%>%mutate(y=geo_point_2d%>% str_extract("^.*(?=,)")%>%as.numeric(),
#                                          x=geo_point_2d%>% str_extract("(?<=, ).*$")%>%as.numeric())
# 
# atms=atms%>%mutate(code_INSEE = if_else(grepl("^6938[1-9]$",`code INSEE commune T1`) ,as.integer(69123),as.integer(`code INSEE commune T1`) ))
# 
# 
# naissance_long=naissance_coor%>%gather(key="annee",value="naissance",NAISD08:NAISD17)
# 
# naissance_long = naissance_long%>%
#   mutate(annee_d = annee%>%str_extract("\\d+$")%>%
#            as.integer()+2000)%>%filter(annee_d%in%2009:2014)
# 
# atms_summary = atms%>%select(annee=`Année de naissance`,code_INSEE)%>%
#   group_by(annee,code_INSEE)%>%
#   summarise(cas = n())
# 
# 
# 
# ATMS_data = naissance_long%>%
#   left_join(atms_summary, by = c("CODGEO" = "code_INSEE" , "annee_d" = "annee"))%>%
#   mutate(cas = replace_na(cas , 0))%>%
#   mutate(temoins = naissance - cas)
# 
# save(ATMS_data,file = "ATMS_2009-2014_AILR_casOK_AllStatus_noINSEEremoved.RData")

#############################
#     nouveau fichier de cas
#############################
new_atms=readxl::read_xlsx("F:/ATMS/190827/Copie de Copie de ATMS pour CS 14 05 19 (spécifique).xlsx")
toRemove = "1410212" #IMG
#warning !!!!!! -> seems that 1440219 is also an IMG
new_atms=new_atms%>%filter(!`Numéro de dossier Remera`%in%toRemove)%>%mutate(code_INSEE = if_else(grepl("^6938[1-9]$",`code INSEE commune T1`) ,as.integer(69123),as.integer(`code INSEE commune T1`) ))



new_atms_summary=new_atms%>%select(annee=`Année de naissance`,code_INSEE)%>%#=`code INSEE commune T1`)%>%
  group_by(annee,code_INSEE)%>%
  summarise(cas = n())%>%
  mutate(code_INSEE = as.numeric(code_INSEE))

new_data = naissance_long%>%left_join(new_atms_summary, by= c("CODGEO" = "code_INSEE",
                                                              "annee_d"="annee"))

new_data = new_data %>% mutate(cas = replace_na(cas , 0))%>%
  mutate(temoins = naissance - cas)

save(new_data,file = "ATMS_2009-2014_AILR_casSelected_noINSEEremoved.RData")

write.csv(new_data,file = "atms_data_190828_0914.csv")


new_data%>%select(CODGEO,LIBGEO,y,x)%>%distinct()%>%
  write.csv("coordonnee.csv",dec = ".")


###########################
#   plot                  #
###########################

load("F:/ATMS/isere.RData")
load("F:/ATMS/rhone.RData")
load("F:/ATMS/ain.RData")
load("F:/ATMS/loire.RData")

ggrhone=fortify(rhone)
ggloire=fortify(loire)
ggisere=fortify(isere)
ggain=fortify(a2) #departement in polygon format

# AR=1/cos(mean(ggisere$lat)*pi/180)
# g2=ggplot()+ geom_polygon(data=ggrhone, aes(long, lat, group = group),colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   geom_polygon(data=ggisere, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   geom_polygon(data=ggain, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   geom_polygon(data=ggloire, aes(long, lat, group = group), colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = .3)+
#   coord_fixed(ratio=AR) #the card of Rhone-Alpes region

#get card of Rhone-Alpes region
plotRA=function(region=list(ggrhone,ggisere,ggloire,ggain),ar=TRUE,transparency=0.3){
  gg=ggplot()
  for(r in region){
    gg=gg+geom_polygon(data=r, aes(long, lat, group = group),colour = alpha("black", 1/2), size = 0.7, fill = 'grey', alpha = transparency)
  }
  if(ar){
    AR=1/cos(mean(region[[1]]$lat)*pi/180)
    gg=gg+coord_fixed(ratio=AR)
  }
  return(gg)
}

gRA=plotRA()
#atms1=melt(atms_agg[c(1:4,7:8)],id.vars =1:4 )
new_data%>%group_by(CODGEO)%>%summarise(ncas=sum(cas),pop=sum(cas)+sum(temoins))
gRA+geom_point(data=atms1[atms1$value>0,],aes(x=longitude,y=latitude,size=(value),col=variable),alpha=.5)+
  scale_color_manual(values = c("Temoins"="green","Cas_vivant"="red"))+
  labs(col=NULL,size="naissances\nvivantes")+
  ggtitle("ATMS 2010-2017")


### 19/09/04
data20092014=new_data%>%group_by(CODGEO,x,y)%>%summarise(tt=sum(temoins),nn=sum(naissance),cc=sum(cas))

df = data20092014%>%gather(key="type",value = "n",c("nn","cc"))%>%mutate(type=if_else(type=="nn","naissance","cas"))%>%filter(n>0)

gRA+geom_point(data=df,aes(x=x,y=y,size=(n),col=type),alpha=.5)+
  scale_color_manual(values = c("naissance"="green","cas"="red"))+
  labs(col=NULL,size="naissances\nvivantes")+
  ggtitle("ATMS 2009-2014")

ggsave("ATMS2009-2014.png")
ggsave("ATMS2009-2014.pdf")

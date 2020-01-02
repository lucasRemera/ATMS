###################################################################
#     ATMS_plot_33vs39_191127                                  #
#     R script                                                    #
#     18/12/19 - Lucas Michon                                     #
#  plot ATMS cases               #
#           #
#  comparison between the different cases files                   #
#                                                                 #
#  #
###################################################################

setwd("F:/ATMS/191127/39_cases_101219/")
##library
{
  library(tidyverse)
  library(stringr)
  library(ggforce)
}

##function
{
  factorToNumeric=function(x) as.numeric(as.character(x))
  
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
  
  cooccurence=function(x,other.constraint=TRUE,n=rep(1,length(x))){
    #length(which(word==x & other.constraint))
    sapply(x, function(xx) sum(n[which(x==xx & other.constraint)]))
  } 
  
  #CODGEO     x     y    tt type          n
  jitter_cases.comparison=function(df,noise=.01){
    i=which(df$files!="birth" & cooccurence(df$CODGEO,df$files!="birth",df$n)>1)
    df1=df[-i,]
    df2=df[i,]
    #df3=apply(df2,1,function(i) replicate(i[6],{i}))
    
    #df3=lapply(1:length(i), function(ii) replicate(as.integer(df2[ii,6]),{df2[ii,]}))
    #for(ii in length(i)) df1=cbind(df1,)
    #print(df2[,6])
    nc=as.integer(df2%>%pull(n))
    #print(nc)
    ii=rep(i,nc)
    df2=df[ii,]
    df2$n=1
    df2$x=df2$x+rnorm(length(ii),0,noise)
    df2$y=df2$y+rnorm(length(ii),0,noise)
    return(rbind(df1,df2))
  }
  
}

##data
{
  
  
  load("ATMS_33vs39_191218_filecomparison.RData",verbose=TRUE) #cases file
  
  load("F:/ATMS/isere.RData")
  load("F:/ATMS/rhone.RData")
  load("F:/ATMS/ain.RData")
  load("F:/ATMS/loire.RData")
  
  ggrhone=fortify(rhone)
  ggloire=fortify(loire)
  ggisere=fortify(isere)
  ggain=fortify(a2) #departement in polygon format
  
}


#########################
##   MAIN              ##
#########################
{
  to_plot=file_comparison%>%select(CODGEO,x,y,annee_d,files,n)%>%group_by(CODGEO,files,x,y)%>%
    summarise(n=sum(n))%>%ungroup()%>%jitter_cases.comparison()%>%filter(n>0)%>%
    mutate(files=str_replace(files,"first","33 cases"),files=str_replace(files,"second","39 cases"))
  
  plotRA()+geom_point(data=to_plot,aes(x=x,y=y,size=n,col=files),alpha=.5)+
    scale_color_manual(values=c("birth"="green","both"="blue","39 cases"="red","33 cases"="purple"))+
    ggtitle("Comparison between first 39 cases file (red) and\n final 33 cases file (purple)",subtitle = "common cases in blue - 2009-2014")
  
  ggsave("ATMS2009-2014_comparison.png")
  ggsave("ATMS2009-2014_comparison.pdf")
  
  plotRA()+geom_point(data=to_plot%>%filter(files!="birth"),aes(x=x,y=y,col=files),alpha=.5)+
    scale_color_manual(values=c("both"="blue","39 cases"="red","33 cases"="purple"))+
    ggtitle("Comparison between first 39 cases file (red) and\n final 33 cases file (purple)",subtitle = "common cases in blue - 2009-2014")
  
  ggsave("ATMS2009-2014_comparison_wobirth.png")
  ggsave("ATMS2009-2014_comparison_wobirth.pdf")
}
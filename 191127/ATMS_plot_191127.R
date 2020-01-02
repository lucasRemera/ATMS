setwd("F:/ATMS/191127/")

#library
{
  library(tidyverse)
  library(stringr)
  library(ggforce)
}

#function
{
  read_satscan=function(file){
    texte=read.delim(file,header=FALSE,colClasses = "character")
    bounds=which(texte[,1]=="_______________________________________________________________________________________________")
    b1=bounds[2]+2
    b2=bounds[3]-1
    beginning=grep("^\\d\\.",texte[,1])
    n.cluster=length(beginning)
    ids = paste(texte[b1:b2,1],collapse = "")%>%
      gsub(" +"," ",.)%>%
      str_extract_all("(?<=Location IDs included.: )[\\d \\.,]+(?= Overlap with clusters)")%>%
      unlist()%>%
      strsplit(", ")
    coor = texte[b1:b2,1]%>%grep("Coordinates / radius",.,value = TRUE)
    y = coor%>%str_extract_all("(?<=\\()[\\d\\.]+(?= N,)")
    x = coor%>%str_extract_all("(?<= N, )[\\d\\.]+(?= E)")
    r = coor%>%str_extract_all("(?<= / )[\\d\\.]+(?= km)")
    pop = texte[b1:b2,]%>%grep(" Population\\.",.,value = TRUE)%>%str_extract_all("\\d+")
    cases = texte[b1:b2,]%>%grep(" Number of case",.,value = TRUE)%>%str_extract_all("\\d+")
    E = texte[b1:b2,]%>%grep(" Expected case",.,value = TRUE)%>%str_extract_all("(?<= )[\\d\\.]+$")
    ratioOE=texte[b1:b2,]%>%grep(" Observed / expected",.,value = TRUE)%>%str_extract_all("(?<= )[\\d\\.]+$")
    rr=texte[b1:b2,]%>%grep(" Relative risk",.,value = TRUE)%>%str_extract_all("(?<= )[\\d\\.]+$")
    llr=texte[b1:b2,]%>%grep(" Log likelihood ratio",.,value = TRUE)%>%str_extract_all("(?<= )[\\d\\.]+$")
    p=texte[b1:b2,]%>%grep(" P-value",.,value = TRUE)%>%str_extract_all("(?<= )[\\d\\.]+$")
    
    satscan=list()
    for(i in 1:n.cluster){
      satscan[[i]]=list(ids=ids[[i]],x=as.numeric(x[[i]]),y=as.numeric(y[[i]]),r=as.numeric(r[[i]]),population=as.numeric(pop[[i]]),cases=as.numeric(cases[[i]]),expected=as.numeric(E[[i]]),ratio=as.numeric(ratioOE[[i]]),loglikelihoodratio=as.numeric(llr[[i]]),pvalue=as.numeric(p[[i]]))
    }
    return(satscan)
  }
  
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
  
  factorToNumeric=function(x) as.numeric(as.character(x))
  
  stat_circle_card=function(x0,y0,r,prec=.01){ #x,y long lat, radius km
    theta=seq(0,2*pi,by=prec)
    # x=x0+111.11*cos(y0*pi/180)*r*cos(theta)
    # y=y0+111.11*r*sin(theta)
    # return(data.frame(x=x,y=y))
    
    ar=cos(y0*pi/180)
    xx0=111.11*x0*ar
    yy0=111.11*y0
    x=xx0+r*cos(theta)
    y=yy0+r*sin(theta)
    df=data.frame(y=y/111.11,x=x/(111.11*ar))
  }
  
  #CODGEO     x     y    tt type          n
  jitter_cases=function(df,noise=.01){
    i=which(df$type=="cas" & df$n>1)
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

#data loading
{
  load("ATMS_2009-2014_191127.RData",verbose = TRUE)
  coordonnees=read.csv("coordonnee.csv")
  
  load("F:/ATMS/isere.RData")
  load("F:/ATMS/rhone.RData")
  load("F:/ATMS/ain.RData")
  load("F:/ATMS/loire.RData")
  
  ggrhone=fortify(rhone)
  ggloire=fortify(loire)
  ggisere=fortify(isere)
  ggain=fortify(a2) #departement in polygon format
}

#global var
{
  # RADIUS_CLUSTER_KM=16.24
  # X_CLUSTER=5.216652
  # Y_CLUSTER=46.066788
}

################################
#     main                     #
################################
{
  gRA=plotRA()
  
  coor_cluster=read_satscan("satscan_2009-2014_191127_one-sided.txt")
  RADIUS_CLUSTER_KM=coor_cluster[[1]]$r
  X_CLUSTER = coor_cluster[[1]]$x
  Y_CLUSTER = coor_cluster[[1]]$y
  
  data_aggregated=new_data%>%select(CODGEO,y,x,naissance,cas,temoins)%>%
    group_by(CODGEO,x,y)%>%summarise(tt=sum(temoins),nn=sum(naissance),cc=sum(cas))%>%
    gather(key="type",value = "n",c("nn","cc"))%>%mutate(type=if_else(type=="nn","naissance","cas"))%>%filter(n>0)%>%
    ungroup()%>% mutate(x=factorToNumeric(x),y=factorToNumeric(y))
  
  gg_atms=gRA+geom_point(data=data_aggregated,aes(x=x,y=y,size=(n),col=type),alpha=.5)+
    scale_color_manual(values = c("naissance"="green","cas"="red"),labels=c("cases","births"))+
    labs(col=NULL,size="naissances\nvivantes")+
    ggtitle("ATMS 2009-2014")+guides(size=FALSE)#+geom_circle(aes(x0=X_CLUSTER,y0=Y_CLUSTER,r=1))
  
  circle_cluster=stat_circle_card(X_CLUSTER,Y_CLUSTER,RADIUS_CLUSTER_KM)
  
  gg_atms+geom_path(data=circle_cluster,aes(x=x,y=y))
  ggsave("ATMS2009-2014_cluster.png")
  ggsave("ATMS2009-2014_cluster.pdf")
  
  
  #jittered
  data_jittered=data_aggregated%>%jitter_cases(noise = .01)
  gg_atms=gRA+geom_point(data=data_jittered,aes(x=x,y=y,size=(n),col=type),alpha=.5)+
    scale_color_manual(values = c("naissance"="green","cas"="red"),labels=c("cases","births"))+
    labs(col=NULL,size="naissances\nvivantes")+
    ggtitle("IUTULRD, 2009-2014")+guides(size=FALSE)#+geom_circle(aes(x0=X_CLUSTER,y0=Y_CLUSTER,r=1))
  
  
  gg_atms
  ggsave("ATMS2009-2014_cluster.png")
  ggsave("ATMS2009-2014_cluster.pdf")
  
  coordonnees$LIBGEO=as.character(coordonnees$LIBGEO)
  coordonnees$LIBGEO[1139]="Saint-Etienne"
  villes=coordonnees%>%filter(LIBGEO%in%c("Lyon","Grenoble","Saint-Etienne"))
  gg_atms+geom_path(data=circle_cluster,aes(x=x,y=y))+
    geom_text(data=villes,aes(x=x,y=y,label=LIBGEO))
  ggsave("ATMS2009-2014_cluster_annotation.png")
  ggsave("ATMS2009-2014_cluster_annotation.pdf")
}
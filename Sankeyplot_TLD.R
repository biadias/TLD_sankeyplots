#loading packages

library(plyr)
library(base)
library(tidyr)
library(dplyr)
library(devtools)
library(igraph)
#devtools::install_github('ramnathv/rCharts')
library(rCharts)
library(magrittr)
library(stringr)

TLdecompALL<-read.csv("C:\\Users\\Bia\\Dropbox\\CONFERENCES\\2016 ICES\\dataTLdecomp\\TLALL.csv")
colnames(TLdecompALL) <- c('Node','GroupAbrev','GroupName','ModelName','TL','Flow')

TLdecompGM<-TLdecompALL %>%
  unite(GroupModel, GroupAbrev, ModelName, remove=F )

TLALLfilter = TLdecompGM %>% filter(Flow > 0.0009)
glimpse(TLALLfilter)

head(TLALLfilter)

sankeyData=function(df,colsource='source',coltarget='target',colvalue='value')
{
  sankey.df=subset(df,select=c(colsource,coltarget,colvalue))
  colnames(sankey.df)=c('source','target','value')
  sankey.df
}

#For example:
TLALLfil<-sankeyData(TLALLfilter,'TL','GroupModel','Flow')
head(TLALLfil)

####################################
#Plot
####################################

sankeyPlot=function(df)
{
  sankeyPlot = rCharts$new()
  
  #--------
  #See note in PPS to this post about a simplification of this part....
  #We need to tell R where the Sankey library is.
  #I put it as a subdirectory to my current working directory (.)
  #setwd("C:\\Users\\Bia\\Documents\\R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages")
  #sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/')
  sankeyPlot$setLib("C:\\Users\\Bia\\Documents\\R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages")
  sankeyPlot$setTemplate(script = "C:\\Users\\Bia\\Documents\\R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages\\layouts\\chart_static_title.html")
  
  #sankeyPlot$setTemplate(script = "C:\\Users\\Bia\\Documents\\R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages\\layouts\\chart.html")
  
  #---------
  
  sankeyPlot$set(
    data = df,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = 1200,
    height = 500,
    #units = "t/km2/year",
    labelFormat = "20%",
    margin = list(right = 100, left = 20, bottom = 150, top = 20)
    
    
  )
  
  sankeyPlot
}

########################################################

########################################################
#Cod_S
########################################################
cod_S2<-dplyr::filter(TLALLfil, target%in%c("Phyto", "Phyto")) # how to make things more effective!

cod_S<-dplyr::slice(TLALLfil, 443:456)
cod_S<- cod_S[-c(6,7,13,14),]



r1<-sankeyPlot(cod_S2)

r1$save('Cod_Small_Final.html', cdn = TRUE)

setwd("C:\\Users\\Bia\\Dropbox\\EwE PhD BD\\EwE M\\ALOSINE PAPER\\AlosinePaperGraphs")

tiff("Cod_small.tiff", width = 7.2, height = 5, units = 'in', 
     compression = "lzw", res = 300)
r1
dev.off()

########################################################
# Cod_M
########################################################

#Cod_M<-dplyr::slice(TLALLfil, 457:472)
#Cod_M<- Cod_M[-c(6,7,8,14,15,16),]

Cod_M <- TLALLfil %>% 
  filter(str_detect(target, "Cod-M_")) # this is the most efficient way to filter, I was manually doing it before, but now I rather use this!


CM<-sankeyPlot(Cod_M)

CM$save('Cod_Medium_Final.html', cdn = TRUE)


########################################################
#Cod_L
########################################################

Cod_L<-dplyr::slice(TLALLfil, 473:488)
Cod_L<- Cod_L[-c(7,8,15,16),]



CL<-sankeyPlot(Cod_L)

CL$save('Cod_Large_Final.html', cdn = TRUE)





########################################################
#Odonto
########################################################

Odo<-dplyr::slice(TLALLfil, 751:766)
Odo<- Odo[-c(8,16),]



OD<-sankeyPlot(Odo)

OD$save('Odonto_Final.html', cdn = TRUE)





########################################################
#Bwhale
########################################################


Bwha<-dplyr::slice(TLALLfil, 735:750)
Bwha<- Bwha[-c(6,7,8,14,15,16),]



BW<-sankeyPlot(Bwha)

BW$save('BaleenWhale_Final.html', cdn = TRUE)


########################################################
#seaBirds
########################################################

Seab<-dplyr::slice(TLALLfil, 767:782)
Seab<- Seab[-c(5,6,7,8,13,14,15,16),]



SB<-sankeyPlot(Seab)

SB$save('Seabird_Final2.html', cdn = TRUE)



########################################################
#Shark_Coastal
########################################################

SharC<-dplyr::slice(TLALLfil, 671:686)
SharC<- SharC[-c(5,6,7,8,13,14,15,16),]



SC<-sankeyPlot(SharC)

SC$save('SharkCoastal_Final.html', cdn = TRUE)




########################################################
#Pinnipeds
########################################################

Pinn<-dplyr::slice(TLALLfil, 719:734)
Pinn<- Pinn[-c(6,7,8,14,15,16),]



PN<-sankeyPlot(Pinn)

PN$save('Pinnipeds_Final.html', cdn = TRUE)





########################################################
#Atl_Herring
########################################################


AtH<-dplyr::slice(TLALLfil, 145:156)
AtH<- AtH[-c(5,6,11,12),]



AH<-sankeyPlot(AtH)

AH$save('AtlHerring_Final.html', cdn = TRUE)



########################################################
#Striped Bass L
########################################################


SBassL<-dplyr::slice(TLALLfil, 353:368)
SBassL<- SBassL[-c(6,7,8,14,15,16),]



SBassL<-sankeyPlot(SBassL)

SBassL$save('StripedBassL_Final.html', cdn = TRUE)


########################################################
#Striped Bass M
########################################################


SBassM<-dplyr::slice(TLALLfil, 337:352)
SBassM<- SBassM[-c(6,7,8,14,15,16),]



SBassM<-sankeyPlot(SBassM)

SBassM$save('StripedBassM_Final.html', cdn = TRUE)



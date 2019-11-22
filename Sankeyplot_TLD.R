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

#Trophic level decomposition matrix have to be tranfromed to a tidy format
#As I was comparing both static models, I merge both files into one. 

#read the tidy .csv file
TLdecompALL<-read.csv("~/TLALL.csv")
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
  #setwd("~R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages")
  #sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/')
  sankeyPlot$setLib("~R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages")
  sankeyPlot$setTemplate(script = "~R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages\\layouts\\chart_static_title.html")
  
  #sankeyPlot$setTemplate(script = "~R\\win-library\\3.3\\rCharts\\rCharts_d3_sankey-gh-pages\\layouts\\chart.html")
  
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

#Example with one of the nodes
########################################################
# Cod_M
########################################################

Cod_M <- TLALLfil %>% 
  filter(str_detect(target, "Cod-M_")) 

CM<-sankeyPlot(Cod_M)

CM$save('Cod_Medium_Final.html', cdn = TRUE)

#The pkg rCharts will save the plots in .html as they are a D3 object. 
#After that I exported the plot and worked on that in Adobe Illustrator.




#this lets data be pushed to shinyapps.io
#library(rsconnect)
#rsconnect::deployApp('O:/Matt Working Folder/sacscoc_dash')


library(shiny) #this is shiny - a reactive data environment
library(shinydashboard) #this creates the dashboard view that looks smarmy 
library(XML) #i'm not entirely sure we need this but am keeping anyway
library(data.table) #one day we'll move to the tidyverse, but presently I'm old school and like data.table.
library(cowplot) #this just let the graphs look a little more modern and cleaner - its a ggplot2 clone+
library(shinyWidgets) #not required but it makes the buttons look 'neater'
library(RgoogleMaps) #our geodata map engine - there are options.
library(plotly) #one could do the whole graphs here - we just use this for hovertext which is addmitedly lame

denrol.dmg.crs<-readRDS("denrol.dmg.crs.rds") #read an R data object
denrol.dmg.crs<-as.data.table(denrol.dmg.crs) #convert this object to data table - the environment is global


denrol.dmg.crs<-denrol.dmg.crs[SFRSTCR_LEVL_CODE == "UG"] #only do UG level data not CE
denrol.dmg.crs<-denrol.dmg.crs[SSBSECT_SEQ_NUMB!="VCT"] #remove virtual college of texas
denrol.dmg.crs<-denrol.dmg.crs[SSBSECT_SEQ_NUMB!="LVT"] # remove labs from virtual college of texas
denrol.date<-file.mtime("denrol.dmg.crs.rds") #get the date the file was made so that folks can see age of data
denrol.trm.min <- min(denrol.dmg.crs$longTerm)
denrol.trm.max <- max(denrol.dmg.crs$longTerm)
denrol.yr.max<- max(denrol.dmg.crs$academicYeari)
denrol.trm.unique<-sort(unique(denrol.dmg.crs$longTerm))
denrol.div.unique<-sort(as.character(unique(denrol.dmg.crs$COLLEGE_DESC)))


#this is a modified colour blind friendly palette
cbbPalette <- c("#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442", "#006DDB", "#C2FFED", "#000000")


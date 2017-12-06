
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#



function(input, output) {
  
  #################################################################################################
  ###           Code for the OVERVIEW window is here                                          ###
  #################################################################################################  
  
  #over_ccd
  output$over_ccd_info<-renderText({
    
    d<-denrol.dmg.crs[SHRTCKG_GRDE_CODE_FINAL!="NA"]
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C", "SA", "SB", "SC", "S"), Grade:= "ABC"]
    d<-d[SSBSECT_CREDIT_HRS>0]
    d<-d[longTerm==input$term,.(SID,SSBSECT_CRN, Grade)]
    d<-unique(d)
    
    ##This is duplicated headcount
    d<-d[,.N, by = Grade]
    d<-d[,Per:= round(N/sum(N),2)*100]
    d<-d[Grade == "ABC"]
    
paste0("ABC Rate for ", input$term, " is ",d$Per, "%")
    
    
  })
  
  output$over_ccl_info<-renderText({
    
    d<-denrol.dmg.crs[SHRTCKG_GRDE_CODE_FINAL!="NA"]
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C", "SA", "SB", "SC", "S"), Grade:= "ABC"]
    d<-d[SSBSECT_CREDIT_HRS>0]
    d<-d[SSBSECT_SUBJ_CODE == "ENGL"]
    d<-d[SSBSECT_CRSE_NUMB=="1301"]
    d<-d[longTerm==input$term,.(SID,SSBSECT_CRN, Grade)]
    d<-unique(d)
    
    ##This is duplicated headcount
    d<-d[,.N, by = Grade]
    d<-d[,Per:= round(N/sum(N),2)*100]
    d<-d[Grade == "ABC"]
    
    paste0("ABC rate for ", input$term, " is ",d$Per, "%")
    
    
  })
  
  
  #over_cdt
  output$over_cdt_info<-renderText({
    
    ##this code makes it "fall to fall" I think
    d<-denrol.dmg.crs[longTerm == input$term]
    d<-d[,.(SID)]
    ##This is unduplicated headcount
    d <- unique(d)
    d<-nrow(d)
    paste0("Unduplicated Headcount: ", d, " for ", input$term, ".")
    
  })
  
  

#################################################################################################
###           Code for the ENROLLMENT window is here                                          ###
#################################################################################################
  ##enrl_res
  output$enrl_res_plt1<-renderPlot({
    d<-denrol.dmg.crs[longTerm == input$term]
    d<-d[, .(longitude,latitude,SID)]

    
    d<-setkey(d,NULL)
    d<-unique(d)

    center = c(42, 98)
    zoom <- 3
    
  Map <- GetMap(center=center, zoom=zoom, maptype = "hybrid",destfile = "MyTile1.png")
  
  tmp <- PlotOnStaticMap(Map, lat = d$latitude, 
                           lon = d$longitude, 
                           destfile = "MyTile1.png", cex=.5, pch=20,                       
                           col="green", add=FALSE)  
  }, res = 92)  
  
    
##enrl_eth
  
          ##this render is the heart of reactive shiny functions
  #it takes an object, and when input$term changes, it reruns this contained code
  #depending on what one does, one may choose to put code outside the reactive area to run only once
  #in fact, that is why we do quite a bit in global.R - it saves server time.
  output$enrl_eth_hst1<-renderPlotly({
    
    #data tables aren't as modern as tidyvers dplyr, but I know how to use them
    #in this instance, we select out only the elements of the longTerm column that match
    #our current input$term 
    d<-denrol.dmg.crs[longTerm == input$term]
    
    #this is a great example of data.table
    #Take subject code column subsetted to only INRW or ENGL,
    #then Calculate the unique count of student IDs
    #By ethnicity
    d<-d[SSBSECT_SUBJ_CODE %in% c("INRW", "ENGL"), uniqueN(SID), by = Ethnicity]
    
    
    #Take Ethnicity column and order alphabetically,
    #then Calculate a new value which is the unique count of student IDs by ethnicity divided by total
    #By each row individually since we left the last part empty
    d[order(Ethnicity),VictoriaCollege := V1/sum(V1)*100]
    
    ## I am not sure the above order sticks unless done this way, but I'm too lazy to experiment right now.
    d<- d[order(Ethnicity)]
    
    ##US census data entered manually for simplicity
    ##in real life, this feeds from the US Census API
    ##also in real life the census api is horribly complex and a massive headache
    #I keep beliving it ought to be easier, and maybe it is, but I'm not that clever yet
    uscen16<-data.table(Ethnicity=c("Black", "Hispanic", "Other", "White"),
                        ServiceArea=c(6.6, 46.3, 1.8, 45.3 ))
    setkey(d,Ethnicity)
    setkey(uscen16,Ethnicity)
    d<-d[uscen16,nomatch=0]
    d[,V1:=NULL]
    md<-melt(d[,.(Ethnicity,VictoriaCollege,ServiceArea)], id.vars = "Ethnicity")
        
    
    #this is a ggplot2 object, masked via the cowplot library to look 'nicer'
    
    p<-ggplot(md, aes(Ethnicity, value, fill=variable)) + #x axis data, y axis data, and fill/colour data
        geom_bar(stat="identity", position = position_dodge()) + #this controls how the data is displayed
        xlab(paste0("VC ",input$term, " vs Service Area 2016"))+ #x axis label
        ylab("Percent") + # Set axis labels 
        scale_fill_manual(values=cbbPalette)+ #this forces the colour blind palette from global.R over default
        theme(legend.position = "top")+ #this puts the legend at the top, although the plotly later messes with that
        theme(legend.title = element_blank()) #this blanks out the title because we already labeled the box
    
    ##this is a cop-out. Essentially a ggplot2 object is pushed through plotly
    #it would probably be smarter to build natively in plotly
    p<-ggplotly(p)
    p
  })
  

  #enrl_age
  output$enrl_age_hst1<-renderPlotly({
    d<-denrol.dmg.crs[longTerm == input$term]
    d <- unique(d[SSBSECT_SUBJ_CODE %in% c("INRW", "ENGL"),.(SID, Age, SPBPERS_SEX)])
    d[,Age1:="0-14"]
    d[Age>14,Age1:="15-19"]
    d[Age>19,Age1:="20-24"]
    d[Age>24,Age1:="25-34"]
    d[Age>34,Age1:="35-44"]
    d[Age>44,Age1:="45-54"]
    d[Age>54,Age1:="55-59"]
    d[Age>59,Age1:="60+"]
    d$Age1<-factor(d$Age1, levels = c("0-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-59", "60+"))
    d<-d[SPBPERS_SEX %in% c("F","M")]
    d<-d[SPBPERS_SEX=="F", SPBPERS_SEX:= "Female"]
    d<-d[SPBPERS_SEX=="M", SPBPERS_SEX:= "Male"]
    d<-d[,.N, by=.(Age1, SPBPERS_SEX)]
    d<- d[order(Age1, SPBPERS_SEX)]   
    
    
   p<-ggplot(d, aes(Age1, N, fill=SPBPERS_SEX)) +
        geom_bar(stat="identity") +
        geom_text(aes(label=N), position = position_stack(vjust=0.5))+
        xlab(paste0(input$term))+
        ylab("Head Count") + # Set axis labels
        scale_fill_manual(values=cbbPalette)+
        theme(legend.position = "top")+
        theme(legend.title = element_blank())
    p<-ggplotly(p)
    p
  })
  
  
  #enrl_cdt
  output$enrl_cdt_hst1<-renderPlot({
    
    ##there is apparently some debate about whether STYPE_CODE ==H is doing what I think it is doing. Explore! (30 May 2017)
    d<-denrol.dmg.crs[SFRSTCR_LEVL_CODE=="UG"]
    
    d<-d[longTerm %like% substr(input$term,1,4)]
    
    ##This is unduplicated headcount
    d <- unique(d[SSBSECT_SUBJ_CODE %in% c("INRW", "ENGL"),.(SID, SGBSTDN_STYP_CODE, longTerm)])
    d[,Type := "Continuing Students"]
    d[SGBSTDN_STYP_CODE == "F", Type:="First Time in College"]
    d[SGBSTDN_STYP_CODE == "H", Type:="Dual Enrollment"]
    d<-d[,.N, by = .(longTerm, Type)]
    d<- d[order(longTerm, Type)]
    
    ggplot(d, aes(longTerm, N, fill=Type)) +
      geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
      geom_text(aes(label=N), vjust=0, position = position_stack(reverse = TRUE, vjust=0.5))+
      xlab("")+
      ylab("Head Count") + # Set axis labels
      scale_fill_manual(values=c("#CC79A7", "#F0E442", "#006DDB"))+
      theme(legend.position = "top")+
      theme(legend.title = element_blank())
  })
  
#################################################################################################
###           Code for the LEARNING(SS) window is here                                        ###
#################################################################################################  
  
  ##lrng_qey
  output$lrng_qey_hst1<-renderPlot({
    
    
    d<-denrol.dmg.crs[SSBSECT_SUBJ_CODE %in% c("MATH", "PSYC", "BIOL", "HIST"),
                      .(SID, SSBSECT_SUBJ_CODE, SSBSECT_CRSE_NUMB, SHRTCKG_GRDE_CODE_FINAL, academicYearc, academicYeari) ]
    
    d[,Course:=paste0(SSBSECT_SUBJ_CODE,SSBSECT_CRSE_NUMB)]
    
    d<-d[Course %in% c("MATH1314", "PSYC2301", "BIOL2401", "BIOL2404", "HIST1301")]
    
    d<-d[SHRTCKG_GRDE_CODE_FINAL != "NA"]
    d<-d[academicYeari %between% c(2014,2019)]
    
    
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C"), Grade:= "ABC"]
    
    ##This is unduplicated headcount
    d<-d[order(academicYearc, Course, Grade),.N, by = .(academicYearc, Course, Grade)]
    d[,NP := N/sum(N)*100, by = .(academicYearc, Course)]
    d<-d[Grade == "ABC"]
    
    ggplot(d, aes(Course, NP, fill=academicYearc)) +
      geom_bar(stat="identity", position = "dodge") +
      geom_text(aes(label=sprintf("%1.0f%%", NP)), position = position_dodge(width=1), vjust = -0.5)+
      xlab("")+
      ylab("% ABC Rate") + # Set axis labels
      #ggtitle("ABC Success Rates by Academic Year")+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      #coord_cartesian(ylim = c(0,100))+
      theme(legend.position = "top")+
      theme(legend.title = element_blank())
    
  })
  
  
  ##lrng_qep
  output$lrng_qep_hst1<-renderPlot({
    
    
    d<-denrol.dmg.crs[SSBSECT_SUBJ_CODE %in% c("MATH", "PSYC", "BIOL", "HIST"),
                      .(SID, SFRSTCR_TERM_CODE, academicYearc, SSBSECT_SUBJ_CODE, SSBSECT_CRSE_NUMB,
                        shortTerm, longTerm, SHRTCKG_GRDE_CODE_FINAL, academicYeari) ]
    d[,Course:=paste0(SSBSECT_SUBJ_CODE,SSBSECT_CRSE_NUMB)]
    
    d<-d[Course %in% c("MATH1314", "PSYC2301", "BIOL2401", "BIOL2404", "HIST1301")]
    
    d<-d[SHRTCKG_GRDE_CODE_FINAL != "NA"]
    d<-d[academicYeari %between% c(2014,2019)]
    
    d<-d[longTerm %like% input$lrng_qep_trm]
    
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C"), Grade:= "ABC"]
    
    ##This is unduplicated headcount
    d<-d[order(longTerm, Course, Grade),.N, by = .(longTerm, Course, Grade)]
    d[,NP := N/sum(N)*100, by = .(longTerm, Course)]
    d<-d[Grade == "ABC"]
    
    ggplot(d, aes(Course, NP, fill=longTerm)) +
      geom_bar(stat="identity", position = "dodge") +
      geom_text(aes(label=sprintf("%1.0f%%", NP)), position = position_dodge(width=1), vjust = -0.5)+
      xlab("")+
      ylab("% ABC Rate") + # Set axis labels
      #ggtitle("ABC Success Rates")+
      scale_fill_manual(values=cbbPalette)+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      #coord_cartesian(ylim = c(0,100))+
      theme(legend.position = "top")+
      theme(legend.title = element_blank())
  
  })
  

#################################################################################################
###           Code for the Completion(SS) window is here                                      ###
#################################################################################################    
   #cmpl_ccd
  output$cmpl_ccd_hst1<-renderPlotly({
    
    d<-denrol.dmg.crs[SHRTCKG_GRDE_CODE_FINAL!="NA"]
    d<-d[SSBSECT_GRADABLE_IND != "N"]
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C", "SA", "SB", "SC", "S"), Grade:= "ABC"]
    d<-d[SSBSECT_SUBJ_CODE%in% c("INRW", "ENGL")]
    d<-d[,Course:=paste0(SSBSECT_SUBJ_CODE, SSBSECT_CRSE_NUMB)]
    d<-d[academicYeari %between% c(max(academicYeari)-3,max(academicYeari)-1)]
    d<-d[,.(SID,SSBSECT_CRN,academicYearc, longTerm,Grade, Course)]
  
    d<-unique(d)
    
    ##This is duplicated headcount
    d<-d[,.N, by = .(academicYearc, Course, Grade)]
    d<- d[order(academicYearc, Course, Grade)]
    d<-d[,Percent:= N/sum(N)*100, by = .(academicYearc, Course)]
    d<-d[,Total:= sum(N), by = .(academicYearc, Course)]
    d<-d[Grade == "ABC"]
    
    p<-ggplot(d, aes(Course, Percent, count = N, total = Total, fill=academicYearc)) +
        geom_bar(stat="identity", position=position_dodge()) +
        xlab("") +
        ylab("ABC Rate Percent") + # Set axis labels
        theme(axis.text.x=element_text(angle=40, hjust=1))+
        scale_fill_manual(values=cbbPalette)+
        coord_cartesian(ylim = c(0,100))+
        theme(legend.position = "top")+
        theme(legend.title = element_blank())
    
    ##this is a cop-out. Essentially a ggplot2 object is pushed through plotly
    #it would probably be smarter to build natively in plotly
    p<-ggplotly(p)
  })
  
   
  output$cmpl_cce_hst1<-renderPlot({
    
    d<-denrol.dmg.crs[SHRTCKG_GRDE_CODE_FINAL!="NA"]
    d<-d[SSBSECT_GRADABLE_IND != "N"]
    d<-d[ SFRSTCR_LEVL_CODE=="UG" ]
    d<-d[SSBSECT_SUBJ_CODE %in% c("INRW", "ENGL")]
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C", "SA", "SB", "SC", "S"), Grade:= "ABC"]
    d<-d[,.(academicYeari, SSBSECT_CRN, Grade, Ethnicity, SID)]
    d<-unique(d)
    ##This is duplicated headcount
    d<-d[,.N, by = .(academicYeari, Ethnicity, Grade)]
    d<- d[order(academicYeari, Ethnicity, Grade)]
    d<-d[,Per:= N/sum(N)*100, by = .(academicYeari, Ethnicity)]
    d<-d[Grade == "ABC"]
    d<-d[academicYeari %between% c(max(academicYeari)-3, max(academicYeari)-1)]
    d<-d[,academicYeari:=as.character(academicYeari)]
    
    ggplot(d, aes(Ethnicity, Per, fill=academicYeari)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_text(aes(label=sprintf("%1.0f%%", Per)), position = position_dodge(width=1), vjust = -0.5)+
      xlab("") + ylab("ABC Rate Percent") + # Set axis labels
      scale_fill_manual(values=cbbPalette)+
      coord_cartesian(ylim = c(0,100))+
      theme(legend.position = "top")+
      theme(legend.title = element_blank())
    
  })
  
  
  output$cmpl_ccr_hst1<-renderPlot({
    
    d<-denrol.dmg.crs[SHRTCKG_GRDE_CODE_FINAL!="NA"]
    d<-d[SSBSECT_GRADABLE_IND != "N"]
    d<-d[SFRSTCR_LEVL_CODE=="UG" ]
    d<-d[SSBSECT_SUBJ_CODE %in% c("INRW", "ENGL")]
    d[,Grade:="Not ABC"]
    d[,ECON_DISADV:="Not Econ_Disadv"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C", "SA", "SB", "SC", "S"), Grade:= "ABC"]
    d[SZRSSTD_ECON_DISADVANTAGED == "2", ECON_DISADV:="Econ_Disadv"]
    d<-d[,.(academicYeari, academicYearc, SSBSECT_CRN, Grade, ECON_DISADV, SID, SPBPERS_SEX)]
    d<-unique(d)
    d<-d[academicYeari %between% c(max(academicYeari)-3, max(academicYeari)-1)]

        ##This is duplicated headcount
    d<-d[,.N, by = .(academicYearc, SPBPERS_SEX, ECON_DISADV, Grade)]
    d<- d[order(academicYearc, SPBPERS_SEX, ECON_DISADV, Grade)]
    d<-d[,Per:= N/sum(N)*100, by = .(academicYearc, SPBPERS_SEX, ECON_DISADV)]
    d<-d[Grade == "ABC"]
    d<-d[,SECON:=paste(SPBPERS_SEX,ECON_DISADV)]
    
    ggplot(d, aes(SECON, Per, fill=academicYearc)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_text(aes(label=sprintf("%1.0f%%", Per)), position = position_dodge(width=1), vjust = -0.5)+
      xlab("Gender & Economic Status (Pell)") + ylab("ABC Rate Percent") + # Set axis labels
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      scale_fill_manual(values=cbbPalette)+
      coord_cartesian(ylim = c(0,100))+
      theme(legend.position = "top")+
      theme(legend.title = element_blank())
    
  })
  
  output$math_abcdm_hst1<-renderPlotly({
    
    d<-denrol.dmg.crs[SHRTCKG_GRDE_CODE_FINAL!="NA"]
    d<-d[SSBSECT_GRADABLE_IND != "N"]
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C", "SA", "SB", "SC", "S"), Grade:= "ABC"]
    d<-d[SSBSECT_SUBJ_CODE %in% c("MATH")]
    d<-d[,Course:=paste0(SSBSECT_SUBJ_CODE, SSBSECT_CRSE_NUMB)]
    d<-d[longTerm == input$term,
         .(SID,SSBSECT_CRN,Grade, Course, SSBSECT_INSM_CODE)]
    d<-unique(d)
    
    ##This is duplicated headcount
    d<-d[,.N, by = .(Course, Grade, SSBSECT_INSM_CODE)]
    d<- d[order(Course, Grade)]
    d<-d[,Percent:= N/sum(N)*100, by = .(Course, SSBSECT_INSM_CODE)]
    d<-d[,Total:= sum(N), by = .(Course, SSBSECT_INSM_CODE)]
    d<-d[Grade == "ABC"]
    
    p<-ggplot(d, aes(Course, Percent, count = N, total=Total, fill=SSBSECT_INSM_CODE)) +
        geom_bar(stat="identity", position=position_dodge()) +
        xlab(input$term) +
        ylab("ABC Rate Percent") + # Set axis labels
        theme(axis.text.x=element_text(angle=40, hjust=1))+
        scale_fill_manual(values=cbbPalette)+
        coord_cartesian(ylim = c(0,100))+
        theme(legend.position = "top")+
        theme(legend.title = element_blank())
    
    p<-ggplotly(p)
    p
  })
  
  output$math_abcnc_hst1<-renderPlotly({
    
    d<-denrol.dmg.crs[SHRTCKG_GRDE_CODE_FINAL!="NA"]
    d<-d[SSBSECT_GRADABLE_IND != "N"]
    d[,Grade:="Not ABC"]
    d[SHRTCKG_GRDE_CODE_FINAL %in% c("A", "B", "C", "SA", "SB", "SC", "S"), Grade:= "ABC"]
    d<-d[SSBSECT_SUBJ_CODE %in% c("INRW", "ENGL", "HIST", "PSYC")]
    d<-d[,Course:=paste0(SSBSECT_SUBJ_CODE, SSBSECT_CRSE_NUMB)]
    
    ####################################recode to do input$term
    d<-d[SFRSTCR_TERM_CODE <= unique(d[longTerm==input$term, SFRSTCR_TERM_CODE])]
    
    ########################recode to do input$math_abc_nc
    dCohort<-unique(d[Course==input$math_abc_nc, SID])
    
    d[, Cohort:= FALSE]
    d[SID %in% dCohort, Cohort:= TRUE]
    
    
    d<-d[,.(SID,SSBSECT_CRN,Grade, Course, Cohort, longTerm)]
    d<-unique(d)
    
    d<-d[longTerm==input$term,.N, by = .(Course, Grade, Cohort)]
    d<- d[order(Course, Grade)]
    d<-d[,Percent:= N/sum(N)*100, by = .(Course, Cohort)]
    d<-d[,Total:= sum(N), by = .(Course, Cohort)]
    d<-d[Grade == "ABC"]
    
    p<-ggplot(d, aes(x = Course, y = Percent, count=N, total = Total,fill=Cohort)) +
        geom_bar(stat="identity", position=position_dodge()) +
        xlab(input$term) +
        ylab("ABC Rate Percent") + # Set axis labels
        theme(axis.text.x=element_text(angle=40, hjust=1))+
        scale_fill_manual(values=c("#006DDB", "#F0E442"))+
        coord_cartesian(ylim = c(0,100))+
        theme(legend.position = "top")+
        theme(legend.title = element_blank())
    
    p<-ggplotly(p)
    p
  })

}

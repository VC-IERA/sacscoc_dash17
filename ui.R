
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#dashboard page is the main level page
dashboardPage(
  
  #headers have room for about 40 characters maybe?
  #you can expand the length, but I don't recall how off the top of my head.
  dashboardHeader(title = "CS-106 Dashboard"), 
    #this controls the side bar
    dashboardSidebar(
    #includeCSS("www/bootstrap.css"),  
    sidebarMenu(
      
      menuItem("Overview", tabName = "overview", icon = icon("tachometer")),
      menuItem("Enrollment (CN)",
               tabName = "enrollment",
               icon = icon("ship")
               ),
      menuItem("Learning (SS)", tabName = "qepgate", icon = icon("book")),
      
      #One can have submenus if that makes sense. However, submenus means the top one is only a drop-down
      #not a hot-link
      menuItem("Completion (SS)",
               tabName = "completion",
               icon = icon("anchor")
               ),
      menuItem("Departments", tabName = "engl", icon = icon("file-text-o"))
    ),
    selectInput("term", 
                label = "Please Select Term:",
                choices = denrol.trm.unique,
                selected = paste0("Fall ",denrol.yr.max-2) ),
    
    
    tags$div(title="Annual Meeting - December 2017",
             img(src = "vclogo.jpg", height = 81, width = 190)
    )
    
    
    
  ),
  dashboardBody(
    tabItems(
      # Overview This will have some sort of single Red/Yellow/Green Heads Up Display for the whole system
      ##we need to figure out how to computer that metric...
      tabItem(tabName = "overview",
              h1("Student Success Indicators", align = "center"),
              fluidRow(
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                         "Last Updated: ", denrol.date, "over_cdt_info"),
                                             img(src = "enrollment_ship.png", height = 50, width = 50),"Enrollment",
                                             icon("info-circle")) ),
                    solidHeader = TRUE,
                    status = "primary", 
                    textOutput("over_cdt_info")),
                
                box(title = tagList(img(src = "learning_helm.png", height = 50, width = 50),"Learning"),
                    solidHeader = TRUE,
                    status = "primary",
                    textOutput("over_ccl_info")
                    )
              ),
              
              fluidRow(
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                        "Last Updated: ", denrol.date, "over_ccd_hst1"),
                                            img(src = "completion_anchor.png", height = 50, width = 50),"Completion",
                                            icon("info-circle")) ),
                    solidHeader = TRUE,
                    status = "primary",
                    textOutput("over_ccd_info")),
                
                box(title = tagList(img(src = "affordability_map.png", height = 50, width = 61),"Affordability"),
                    solidHeader = TRUE,
                    status = "primary",
                    "A heads-up on cost goes here - redacted for talk."
                    )
              ),
              
              fluidRow(
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                         "Last Updated: ", denrol.date, "over_emp_info"),
                                             img(src = "employment_compass.png", height = 50, width = 50),"Employment",
                                             icon("info-circle")) ),
                    solidHeader = TRUE,
                    status = "primary",
                    "Student Employment Results live here - redacted for talk.",
                    a("Activity One Survey",
                      href="http://www.surveygizmo.com/s3/4034373/23ed5729f833",
                      target="_blank")
                    ),
                
                box(title = tagList(img(src = "http://mattwiley.org/images/mw.jpg", height = 50, width = 50),
                                    "Brief Biography"),
                    solidHeader = TRUE,
                    status = "primary", 
                    a("Matt Wiley",
                      href="http://mattwiley.org/",
                      target="_blank"),
                    " is a tenured, associate professor of mathematics who serves as quality enhancement director at Victoria College. He earned degrees in computer science, business, and pure mathematics from the University of California and Texas A&M systems."
                    )
              )
      ),
      
      
      # The enrollment tab is complex
      #we could either have 
      
      
      tabItem(tabName = "enrollment",
              fluidRow(
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere most recent Mailing Adress Zip Code",
                                                         "Last Updated: ", denrol.date, "enrl_res"),
                                             "Enrollment by Residence",icon("info-circle")) ),
                    solidHeader = TRUE, background = NULL, width = 6,
                    collapsible = TRUE, collapsed = FALSE,
                    status = "primary",
                    footer = "Geographic Location of Students in given semester redacted.",
                    plotOutput("enrl_res_plt1")
                    
                    
                    ),
                
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                         "Last Updated: ", denrol.date, "enrl_age"),
                                             "Age",icon("info-circle")) ),
                    solidHeader = TRUE, background = NULL, width = 6,
                    collapsible = TRUE, collapsed = FALSE,
                    status = "primary",
                    footer = "Unduplicated on age per term, except if a student has a birthday in a specific term
                    and is enrolled in a course after that birthdate. Then they would be counted twice.",
                    #plotOutput("enrl_age_hst1")
                    plotlyOutput("enrl_age_hst1")
                )
                
                
              ),
              
              
              fluidRow(
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                         "Last Updated: ", denrol.date, "enrl_cdt"),
                                             "Credit Enrollment Unduplicated by Student Type",icon("info-circle")) ),
                    solidHeader = TRUE, background = NULL, width = 6,
                    collapsible = TRUE, collapsed = FALSE,
                    status = "primary",
                    footer = "also unduplicated. I do not think there is any reason this would ever
                    for a single semester have more than one row per student.",                    
                    plotOutput("enrl_cdt_hst1")
                    ),
                
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                         "Last Updated: ", denrol.date,
                                                         " Service Area data from US Census Data and is different time-cycle than VC Data.",
                                                         "enrl_eth"),
                                             "Ethnicity: Victoria College versus Service Area",icon("info-circle")) ),
                    solidHeader = TRUE, background = NULL, width = 6,
                    collapsible = TRUE, collapsed = FALSE,
                    status = "primary",
                    footer = "Feeds from American Community Survey API (stopped for talk to decrease load time)",   
                    #plotOutput("enrl_eth_hst1")
                    plotlyOutput("enrl_eth_hst1")
                )
                
                
              )
      ),
      
      
      # Learning
      tabItem(tabName = "qepgate",
              h2("Learning (SS) - QEP Gateway"),
              
              fluidRow(
                
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                         "Last Updated: ", denrol.date, "lrng_qey"),
                                             "ABC Rates of QEP Gateway Courses",icon("info-circle")) ),
                    solidHeader = TRUE, background = NULL, width = 12,
                    status = "primary",
                    
                    plotOutput("lrng_qey_hst1"))
              ),
              
              fluidRow(
                box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                         "Last Updated: ", denrol.date, "lrng_qep"),
                                             "ABC Rates of QEP Gateway Courses by Term",icon("info-circle")) ),
                      solidHeader = TRUE, background = NULL, width = 12,
                    collapsible = TRUE, collapsed = FALSE,
                    status = "primary",
                    
                    radioGroupButtons(
                      inputId = "lrng_qep_trm", 
                      choices = c("Fall", "Spring", "Summer"),
                      selected = "Fall",
                      justified = TRUE, status = "primary",
                      checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                    ),     
                    plotOutput("lrng_qep_hst1"))
              )
              
      ),
      
      #Completion
      tabItem( tabName = "completion",
               fluidRow(
                 
                 box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                          "Last Updated: ", denrol.date, "cmpl_ccd"),
                                              "ENGL/INRW ABC Rates by Course",icon("info-circle")) ),
                     solidHeader = TRUE, background = NULL, width = 12,
                     status = "primary",
                     footer = "The percents are again 'ABC' rates by year and course.",
                     plotlyOutput("cmpl_ccd_hst1")
                     )
                 
                 
                 ),
               
               fluidRow(
                 
                 box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                          "Last Updated: ", denrol.date, "cmpl_cce"),
                                              "ABC Rates by Ethnicity",icon("info-circle")) ),
                     solidHeader = TRUE, background = NULL, width = 6,
                     status = "primary",
                     footer = "Includes NCBOs",
                     plotOutput("cmpl_cce_hst1")
                 ),
                 
                 box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                          "Last Updated: ", denrol.date, "cmpl_ccr"),
                                              "ABC Rates by Economic Status",icon("info-circle")) ),
                     solidHeader = TRUE, background = NULL, width = 6,
                     status = "primary",
                     footer = "Includes INRW NCBOs",
                     plotOutput("cmpl_ccr_hst1")
                     )
               )
      ),
      
      
      #Completion
      tabItem( tabName = "persistance",
               
               fluidRow(
                 
                 box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                          "Last Updated: ", denrol.date, "comp_ftic_per"),
                                              "First Time in College Persistence",icon("info-circle")) ),
                       solidHeader = TRUE, background = NULL, width = 6,
                     status = "primary",
                     footer = "This is NOT ENGL/INRW students only - that doesn't quite make sense.
                     this is simply college wide and something to think about.",
                     plotOutput("comp_ftic_per")
                     )
               
               )        
      ),
      
      tabItem( tabName = "engl",
               
               fluidRow(
                 
                 box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                          "Last Updated: ", denrol.date, "math_abcdm"),
                                              "ABC Rates by Delivery Method",icon("info-circle")) ),
                     solidHeader = TRUE, background = NULL, width = 12,
                     status = "primary",
                     footer = "FTV = broadcast, ITV = recipient, ONL = Online, 
                     SPD = Cafe, TRA = traditional",
                     plotlyOutput("math_abcdm_hst1")
                 )
                 
               ),
               fluidRow(
                 
                 box(title = tagList(tags$div(title=paste("VC Data: DataBaseTableReferenceHere",
                                                          "Last Updated: ", denrol.date, "math_abcnc"),
                                              "ABC Rates by next course",icon("info-circle")) ),
                     solidHeader = TRUE, background = NULL, width = 12,
                     status = "primary",
                     footer = "First select a course you wish to track. This creates a cohort is 
                     from students who took that selected course IN THE CURRENT TERM OR BEFORE.
                     From there, we see if that cohort is present in any current-term courses.
                     Note: in the hovertext, N is number of ABC students; divide by
                     Percent to determine class size...",
                     radioGroupButtons(
                       inputId = "math_abc_nc", 
                       choices = c("ENGL1301",
                                   "ENGL1302",
                                   "ENGL2311",
                                   "ENGL2322",
                                   "ENGL2327",
                                   "ENGL2328",
                                   "INRW0016",
                                   "INRW0301",
                                   "PSYC2301",
                                   "HIST1301",
                                   "HIST1302"),
                       selected = "INRW0016",
                       justified = TRUE, status = "primary",
                       checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                        no = icon("remove", lib = "glyphicon"))
                     ),
                     plotlyOutput("math_abcnc_hst1")
                     
                 )
                 
               )
      )

      
      
    )
    
  ),
  title = "Data to Dashboard",
  skin = "purple"
)
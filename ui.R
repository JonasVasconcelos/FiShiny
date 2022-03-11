##################################################
#####
##### Age and Growth app - User Interface (UI)
##### Jonas Vasconcelos-Filho
##### 09/06/2019
##### v 1.0
#####
##################################################

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c("shiny", "FSA", "fishmethods", "AICcmodavg", 
            "nlstools", "shinydashboard", "shinyWidgets", 
            "ggplot2", "waiter","shinyalert")
check.packages(packages)

title <- a(href="https://github.com/JonasVasconcelos",
           style = "font-family: monospace; color: white;",
           img(src="logo.png", width = '75'),
           strong('FiShiny'))
           

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs", 
              menuItem("Home", tabName = "home", 
                       icon = icon("home")),
              
              menuItem(text = "Length-Weight Relationship", tabName = "lwr", 
                       icon = icon("chart-line"),
                       menuSubItem(text = "Input", tabName = "lwrinput", 
                                   icon = icon("upload")),
                       menuSubItem("Graph and Fit", tabName = "graphfit", 
                                   icon = icon("bar-chart-o"))
              ),
              
              menuItem(text = "Age and Growth", 
                       tabName = "age", 
                       icon = icon("chart-line"),
                       menuSubItem(text = "Input", tabName = "ageinput", 
                                   icon = icon("upload")),
                       menuSubItem("Graph", tabName = "graph", 
                                   icon = icon("bar-chart-o")),
                       menuSubItem("Models", tabName = "models",
                                   icon = icon("fas fa-chart-line")),
                       menuSubItem("Kimura", tabName = "kimura",
                                   icon = icon("question"))
              ),
              
              menuItem(text = "Morphometrics", tabName = "morpho", 
                       icon = icon("ruler-horizontal"),
                       menuSubItem(text = "Input Images", tabName = "morphoinput", 
                                   icon = icon("upload")),
                       menuSubItem("Measurement", tabName = "graph", 
                                   icon = icon("ruler-horizontal"))
              )
  )
)

Home <- tabItem(tabName = "home",
                p("Do you have trouble with growth model fits? Your problems are over there that is an easy way to fit the von Bertalanffy, Gompertz, and Logistic functions. And as a tip, you can set how many bootstraps you want for confidence intervals, use the Akaike criterion for model evaluation, and run comparisons between groups.",
                style = "font-family: 'helvetica'; font-size: 12pt; text-align: justify"))

AgeInput <- tabItem(tabName = "ageinput",
                    fluidRow(
                      box(title = "Input sets",
                          status = "primary",
                          solidHeader = T,
                          collapsible = TRUE,
                        
                          fileInput("agefile",
                                   "Upload the file", 
                                    multiple = T),
                          helpText("Select the read.table parameters below"),
                                  
                          checkboxInput(inputId = "ageheader",
                                        label = "Header",
                                        value = T),
                        
                          checkboxInput(inputId = "agestringAsFactors",
                                        label = "stringAsFactors",
                                        value = T),
                                
                          radioButtons(inputId = "agesep",
                                       label = "Separator",
                                       choices = c(Semicolon = ";",
                                                   Comma = ",",
                                                   Tab = "\t",
                                                   Space = " "))
                      ),
                             
                      box(title = "Data table",
                          status = "primary",
                          solidHeader = T,
                          collapsible = TRUE,
                          uiOutput("agetb")
                      )
                    )
            )

LWRInput <- tabItem(tabName = "lwrinput",
                    fluidRow(
                      box(title = "Input sets",
                          status = "primary",
                          solidHeader = T,
                          collapsible = TRUE,
                          
                          fileInput("lwrfile",
                                    "Upload the file", 
                                    multiple = T),
                          helpText("Select the read.table parameters below"),
                          
                          checkboxInput(inputId = "header",
                                        label = "Header",
                                        value = T),
                          
                          checkboxInput(inputId = "stringAsFactors",
                                        label = "stringAsFactors",
                                        value = T),
                          
                          radioButtons(inputId = "lwrsep",
                                       label = "Separator",
                                       choices = c(Semicolon = ";",
                                                   Comma = ",",
                                                   Tab = "\t",
                                                   Space = " "))
                      ),
                      
                      box(title = "Data table",
                          status = "primary",
                          solidHeader = T,
                          collapsible = TRUE,
                          uiOutput("lwrtb")
                      )
                    )
)

GraphInputAge <- tabItem(tabName = "graph",
                        fluidRow(
                          box(title = "Variables and Graphical Parameters",
                              status = "primary",
                              solidHeader = T,
                              collapsible = TRUE,
                              selectInput("agex", "1. Select x-variable",
                                          choices = "", selected = "", multiple = F),
                            
                              selectInput("agey", "2. Select y-variable",
                                          choices = "", selected = "", multiple = F),
                                    
                              textInput("xlab", "3. Type the x-label."),
                              textInput("ylab", "4. Type the y-label."),
                                    
                              numericInput("agepch", "5. Choose the symbol points.",min = 1, max = 20, step = 1, value = 1),
                              selectInput("agecolpt", "6. Choose the color points.", 
                                          choices = c("Black" = 1,
                                                      "Red" = 2,
                                                      "Green" = 3,
                                                      "Blue" = 4,
                                                      "Cyan" = 5,
                                                      "Magenta" = 6,
                                                      "Yellow" = 7,
                                                      "Gray" = 8),
                                          multiple = F, selected = 1),
                                    
                              radioButtons("downloadfile", "Select the file type:",
                                          choices = c("tiff" = "tiff", 
                                                     "jpeg" = "jpeg")),
                              numericInput("width", "Width (px):", value = 2400, min = 800, max = 4000),
                              numericInput("height", "Height (px):", value = 1800, min = 600, max = 3000),
                              numericInput("res", "Resolution (dpi):", value = 300, min = 72, max = 300)
                          ),
                                  
                          box(title = "Plot",
                              status = "primary",
                              solidHeader = T,
                              collapsible = TRUE,
                              plotOutput("plot")
                          )
                        )
                )


ModelGraphsAge <- tabItem(tabName = "models",
                          fluidRow(
                            tabBox(
                              title = "Input",
                              tabPanel(
                                title = "Models",
                                status = "primary",
                                solidHeader = T,
                                collapsible = TRUE,
                                          
                                checkboxInput("modelsvbgf","von Bertalanffy", F),
                                checkboxInput("modelsgomp","Gompertz", F),
                                checkboxInput("modelslog","Logistic", F),
                                                
                                checkboxInput("boot","Bootstrap", F),
                                numericInput("nboot", "How many bootstrap do you want?",
                                              value = 999),
                                                  
                                plotOutput("plot2", brush = "plot_brush")
                              ),
      
                              tabPanel(
                                title = "von Bertalanffy",
                                status = "primary",
                                solidHeader = T,
                                collapsible = TRUE,
                                  
                                shinyjs::useShinyjs(),
                                sliderInput("vbgfL", "Linf/Winf", min = 0, max = 1000, value = 50, step = 1),
                                sliderInput("vbgfK", "K", min = 0, max = 5, value = 1, step = 0.01),
                                sliderInput("vbgft0", "t0", min = -2.5, max = 2.5, value = 0, step = 0.01),
                                  
                                plotOutput("plot3", brush = "plot_brush")
                              ),
          
                              tabPanel(
                                title = "Gompertz",
                                status = "primary",
                                
                                sliderInput("gompL", "Linf/Winf", min = 0, max = 1000, value = 50, step = 1),
                                sliderInput("gompgi", "gi", min = 0.001, max = 10, value = 5, step = 0.01),
                                sliderInput("gompti", "ti", min = -5, max = 5, value = 0, step = 0.01),
                                  
                                plotOutput("plot4", brush = "plot_brush")
                              ),
                                
                              tabPanel(
                                title = "Logistic",
                                status = "primary",
                                sliderInput("logL", "Linf/Winf", min = 0, max = 1000, value = 50, step = 1),
                                sliderInput("logginf", "gninf", min = 0.001, max = 5, value = 2.5, step = 0.01),
                                sliderInput("logti", "ti", min = -5, max = 5, value = 0, step = 0.01),
                                  
                                plotOutput("plot5", brush = "plot_brush")
                              )
                            ),
    
                          tabBox(
                            title = "Fit",
                            tabPanel(
                              title = "von Bertalanffy",
                              status = "primary",
                              solidHeader = T,
                              collapsible = TRUE,
                              plotOutput("vbgfBoot2"),
                              downloadButton("downVBGF", 
                                             "Download the plot"),
                              verbatimTextOutput("modVBGF2"),
                              downloadButton("downloadDataVBGF",
                                             "Download stats")
                            ),
                            
                            tabPanel(
                              title = "Gompertz",
                              status = "primary",
                              solidHeader = T,
                              collapsible = TRUE,
                              plotOutput("gomBoot"),
                              downloadButton("downGomp", 
                                             "Download the plot"),
                              verbatimTextOutput("modGom2"),
                              downloadButton("downloadDataGomp",
                                             "Download stats") 
                            ),
                            
                            tabPanel(
                              title = "Logistic",
                              status = "primary",
                              solidHeader = T,
                              collapsible = TRUE,
                              plotOutput("logBoot2"),
                              downloadButton("downLog", 
                                             "Download the plot"),
                              verbatimTextOutput("modLog2"),
                              downloadButton("downloadDataLog",
                                             "Download stats") 
                            ),
                            
                            tabPanel(
                              title = "All model",
                              status = "primary",
                              solidHeader = T,
                              collapsible = TRUE,
                              plotOutput("todos"),
                              downloadButton("downAll", 
                                             "Download the plot"),
                              tableOutput("AICprint"),
                              downloadButton("downloadAIC", 
                                             "Download the AIC table")
                            )
                          )
                        )
                  )

KimuraAge <- tabItem(tabName = "kimura",
                    fluidRow(
                      box(
                        title = "Kimura",
                        status = "primary",
                        solidHeader = T,
                        collapsible = TRUE,
                        
                        selectInput("kimurafactor", "Which group?", 
                                    choices = "", selected = "", multiple = F),
                        radioButtons(inputId = "kimuramodel",
                                     label = "Model",
                                     choices = c("von Bertalanffy" = 1,
                                                 "Gompertz" = 2,
                                                 "Logistic" = 3))),
                      
                      box(
                        title = "Stats",
                        status = "primary",
                        solidHeader = T,
                        collapsible = TRUE,
                        downloadButton("downloadKimura", 
                                       "Download stats"),
                        verbatimTextOutput("kimuraTab")
                      )
                    )
                  )


shinyUI(
  dashboardPage(
    
    dashboardHeader( title = title),
    dashboardSidebar(sidebar),
    dashboardBody( 
      shinyjs::useShinyjs(),
      autoWaiter(html =   spin_loaders(id = 15,
                                       color = rgb(0.25, 0.25, 0.25, 0.5)),
                 color = rgb(0.1, 0.1, 0.1, 0),
                 fadeout = T),
      
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tabItems(Home,
               LWRInput,
               AgeInput,
               GraphInputAge, 
               ModelGraphsAge,
               KimuraAge
      ),
    )
  )
)






# ,
# 
# tabItem(
#       tabName = "models",
#       plotOutput("plot"),
#       hr(),
#       
#       fluidRow(
#         column(3,
#           
# 
# column(4,
#   strong("Parameters"),
#   
#   shinyjs::useShinyjs(),
#   "von Bertalanffy",
#   sliderInput("vbgfL", "Linf/Winf", min = 0, max = 1000, value = 50, step = 1),
#   sliderInput("vbgfK", "K", min = 0, max = 10, value = 5, step = 0.01),
#   sliderInput("vbgft0", "t0", min = -5, max = 5, value = 0, step = 0.01),
#   
#   "Gompertz",
#   sliderInput("gompL", "Linf", min = 0, max = 1000, value = 50, step = 1),
#   sliderInput("gompgi", "gi", min = 0, max = 10, value = 5, step = 0.01),
#   sliderInput("gompti", "ti", min = -5, max = 5, value = 0, step = 0.01),
#   
#   "Logistic",
#   sliderInput("logL", "Linf", min = 0, max = 1000, value = 50, step = 1),
#   sliderInput("logginf", "gninf", min = 0, max = 10, value = 5, step = 0.01),
#   sliderInput("logti", "ti", min = -5, max = 5, value = 0, step = 0.01)
#   
#             
#                     # "Models:",
#                     #  c("von Bertalanffy" = "vbgf",
#                     #    "Gompertz" = "gomp",
#                     #    "Logistic" = "log"))
#   )
#)
#)

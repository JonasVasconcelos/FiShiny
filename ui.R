###################################################
#####
##### Age and Growth app - User Interface (UI)
##### Jonas Vasconcelos-Filho
##### 09/06/2019
##### v 1.0
#####
###################################################

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage example
packages<-c("shiny", "FSA", "fishmethods", "AICcmodavg", 
            "nlstools", "shinydashboard", "shinyWidgets", 
            "cluster","dendextend","agricolae")
check.packages(packages)

title <- a(href="https://github.com/JonasVasconcelos",
           style = "font-family: monospace; color: white;",
           img(src="logo.png", width = '75'),
           strong('FiShiny'))
           


  
shinyUI(
  dashboardPage(
    
    dashboardHeader( title = title),
    
    dashboardSidebar(
      
      sidebarMenu(

        menuItem("Home", tabName = "home", icon = icon("home")),
        
        menuItem("Input", tabName = "input", icon = icon("upload")),
            
        menuItem("Graph", tabName = "graph", icon = icon("bar-chart-o")),
        
        menuItem("Models", tabName = "models", icon = icon("fas fa-chart-line")),
        
        menuItem("Kimura", tabName = "kimura", icon = icon("question"))
        )
      ),
      
      dashboardBody(

        tabItems(
            tabItem(
              tabName = "home",
              p("Do you have trouble with growth model fits? Your problems are over! Here is an easy way to fit the von Bertalanffy, Gompertz, and Logistic functions. And, as a tip, you can set how many bootstraps you want to create confidence intervals, use the Akaike criterion for model evaluation, and run comparisons between groups.",
                style = "font-family: 'helvetica'; font-size: 12pt; text-align: justify")
            ),
        
          
            tabItem(
                tabName = "input",
                fluidRow(
                box(
                    title = "Input sets",
                    status = "primary",
                    solidHeader = T,
                    collapsible = TRUE,
                    
                  
                    fileInput("file",
                              "Upload the file", 
                              multiple = T),
                    helpText("Select th read.tabel parameters below"),
                    
                    checkboxInput(inputId = "header",
                                  label = "Header",
                                  value = T),
                    
                    checkboxInput(inputId = "stringAsFactors",
                                  label = "stringAsFactors",
                                  value = F),
                    
                    radioButtons(inputId = "sep",
                                 label = "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t",
                                             Space = " ")),
                    
                    uiOutput("selectfile")
                  )
                ,
        
                box(
                  title = "Data table",
                  status = "primary",
                  solidHeader = T,
                  collapsible = TRUE,
                  uiOutput("tb")
                )
              )
          )
          ,
        
          tabItem(
                tabName = "graph",
                fluidRow(
                box(
                    title = "Variables",
                    status = "primary",
                    solidHeader = T,
                    collapsible = TRUE,
                    selectInput("x", "1. Select x-variable", 
                                choices = c("Age" = 1,
                                            "Length" = 2,
                                            "Weight" = 3),
                                multiple = F, selected = 1),
                    selectInput("y", "2. Select y-variable", 
                                choices = c("Age" = 1,
                                            "Length" = 2,
                                            "Weight" = 3),
                                multiple = F, selected = 2),
                    
                    textInput("xlab", "Type the x-label."),
                    textInput("ylab", "Type the y-label."),
                    
                    textInput("pch", "Choose the symbol points.", value = 16),
                    textInput("colpt", "Choose the color points."),

                    radioButtons("downloadfile", "Select the file type:",
                                choices = c("tiff" = "tiff", 
                                            "jpeg" = "jpeg")),
                    textInput("width", "Width (px):", value = "2400"),
                    textInput("height", "Height (px):", value = "1800"),
                    textInput("res", "Resolution (dpi):", value = "300")
                )
                ,
                
                box(
                    title = "Plot",
                    status = "primary",
                    solidHeader = T,
                    collapsible = TRUE,
                    plotOutput("plot")
                )
              )
           )
          ,
          
          tabItem(
            tabName = "models",
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
                )
                ,
              
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
              
              # graficos
              tabBox(
                title = "Fit",
                tabPanel(
                  title = "von Bertalanffy",
                  status = "primary",
                  solidHeader = T,
                  collapsible = TRUE,
                  plotOutput("vbgfBoot2"),
                  downloadButton("downVBGF", "Download the plot"),
                  verbatimTextOutput("modVBGF2")  
                )
                ,
                tabPanel(
                  title = "Gompertz",
                  status = "primary",
                  solidHeader = T,
                  collapsible = TRUE,
                  plotOutput("gomBoot"),
                  verbatimTextOutput("modGom2") 
                )
                ,
                tabPanel(
                  title = "Logistic",
                  status = "primary",
                  solidHeader = T,
                  collapsible = TRUE,
                  plotOutput("logBoot2"),
                  verbatimTextOutput("modLog2")  
                )
                ,
                tabPanel(
                  title = "All model",
                  status = "primary",
                  solidHeader = T,
                  collapsible = TRUE,
                  plotOutput("todos"),
                  tableOutput("AIC")  
                )
              )
            )
          ),
          
          tabItem(
            tabName = "kimura",
            fluidRow(
              
              box(
                title = "Kimura",
                status = "primary",
                solidHeader = T,
                collapsible = TRUE,
                
                radioButtons("factor", "Which group?", 
                             choices = c("Local" = 4,"Sex" = 5,
                                         "Local + Sex" =6, "None" = 7), 
                             selected = 4)),
              
                box(
                  title = "Stats",
                  status = "primary",
                  solidHeader = T,
                  collapsible = TRUE,
                  
                  verbatimTextOutput("kimuraTab")
                )
            )
          )
        )
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

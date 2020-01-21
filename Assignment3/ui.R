shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 Peng Shen (57408055)"),
  checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput("Multiplier", "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput("BoxPlots"),
             plotOutput("Missing"),
             plotOutput("Corr"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("NULL Model",
             fluidRow(
               column(width = 4, selectizeInput(inputId = "NullPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c())),
               column(width = 1, actionButton(inputId = "NullGo", label = "Train", icon = icon("play")))
             ),
             verbatimTextOutput("NullModelSummary2")
    ),
    tabPanel("GLMnet Model",
             verbatimTextOutput("GlmnetModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy"))),
               column(width = 1, actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("GlmnetModelSummary1"),
             hr(),
             plotOutput("GlmnetModelPlots"),
             verbatimTextOutput("GlmnetModelSummary2")
    ),
    tabPanel("PLS Model",
             verbatimTextOutput("PlsModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy"))),
               column(width = 1, actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("Rpart Model",
             verbatimTextOutput("RpartModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c())),
               column(width = 1, actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("RpartModelSummary1"),
             hr(),
             plotOutput("RpartModelPlots"),
             plotOutput("RpartModelTree"),
             verbatimTextOutput("RpartModelSummary2")
    ),    
    tabPanel("Ridge Regression Model",
             verbatimTextOutput("RidgeModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "RidgePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy"))),
               column(width = 1, actionButton(inputId = "RidgeGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("RidgeModelSummary1"),
             hr(),
             plotOutput("RidgeModelPlots"),
             verbatimTextOutput("RidgeModelSummary2")
    ),
    tabPanel("Quantile Random Forest Model",
             verbatimTextOutput("QRFModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "QRFPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","pls"))),
               column(width = 1, actionButton(inputId = "QRFGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("QRFModelSummary1"),
             hr(),
             plotOutput("QRFModelPlots"),
             verbatimTextOutput("QRFModelSummary2")
    ),
    tabPanel("eXtreme Gradient Boosting Model",
             verbatimTextOutput("XgbModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "XgbPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute", "dummy"))),
               column(width = 1, actionButton(inputId = "XgbGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("XgbModelSummary1"),
             hr(),
             plotOutput("XgbModelPlots"),
             verbatimTextOutput("XgbModelSummary2")
    ),
    tabPanel("Support Vector Machine Model",
             verbatimTextOutput("SVMModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "SVMPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy","center","scale","pca"))),
               column(width = 1, actionButton(inputId = "SVMGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("SVMModelSummary1"),
             hr(),
             plotOutput("SVMModelPlots"),
             verbatimTextOutput("SVMModelSummary2")
    ),
    tabPanel("Gaussian Process with Ploynomial Kernel Model",
             verbatimTextOutput("GaussPolyModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "GaussPolyPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("dummy"))),
               column(width = 1, actionButton(inputId = "GaussPolyGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("GaussPolyModelSummary1"),
             hr(),
             plotOutput("GaussPolyModelPlots"),
             verbatimTextOutput("GaussPolyModelSummary2")
    ),
    tabPanel("Quantile Regression Neural Network Model",
             verbatimTextOutput("QrnnModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "QrnnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute", "dummy"))),
               column(width = 1, actionButton(inputId = "QrnnGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("QrnnModelSummary1"),
             hr(),
             plotOutput("QrnnModelPlots"),
             verbatimTextOutput("QrnnModelSummary2")
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             checkboxInput("NullNormalise", "Normalise", value = TRUE),
             plotOutput("SelectionBoxPlot"),
             radioButtons("Choice", "Model choice", choices = c(""), inline = TRUE)
    ),
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot")
    )
  )
))

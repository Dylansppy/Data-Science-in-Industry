#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

domChoices <- c("l","f","r","t","i","p")
dat <- read.csv("Ass1Data.csv", header = TRUE)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Assignment 1 - Peng Shen (Dylan)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            navbarPage(title=h4("Parameter Control"),
                       tabPanel(title="Overview",
                                checkboxInput("rownames", "Show row names", value=T),
                                checkboxInput("order", "Column ordering", value=T),
                                numericInput("threshold", "Input a threshold for missing value ratio", value=0.5, min=0, max=1, step=0.1),
                                selectInput("selection", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                                selectInput("filter", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices)
                       ),
                       tabPanel(title="Numerical Data Visualization",
                                selectInput("num_col", "Select the continuous variable(s) to show boxplot(s)", choices=c(colnames(dat[, c(1,15:44)])), multiple = TRUE, selected = "Y"),
                                numericInput("range", "Input a range value for boxplot", value=1.5, min=0, step=0.5),
                                selectInput("num_cols", "Select the numerical variables to show correlation / continuity / homogeneity / novelties", choices=c(colnames(dat[, c(1,15:44)])), multiple = TRUE, selected = c("Y", "sensor1")),
                                checkboxInput("centre", "Whether to centre the dataset", value=T),
                                checkboxInput("scale", "Whether to scale the dataset", value=T)
                       ),
                       tabPanel(title="Factor Data Visualization",
                                selectInput("fac_col", "Select the factor(s) to show bar chart(s) / homogeneity / novelties", choices=c(colnames(dat[, c(3,5:14)])), multiple = TRUE, selected = c("Author", "Priority"))
                       )
            )),
            
        # Show a plot of the generated distribution
        mainPanel(
            navbarPage(title=h4("Data Visualization"),
                       tabPanel(title="Data Table", icon=icon("table"),
                                 DT::dataTableOutput("tableX")
                       ),
                       tabPanel(title="Overview (datatype, novelty variables)", icon=icon("table"),
                                verbatimTextOutput("Structure"),
                                verbatimTextOutput("Datatype"),
                                textOutput("Novel_variable")
                       ),
                       tabPanel(title="Summary Statistics", icon=icon("table"),
                                 verbatimTextOutput("Summary")
                       ),
                       tabPanel(title="Data Roles", icon=icon("table"),
                                verbatimTextOutput("Role")
                       ),
                       tabPanel(title="Missing Value Distribution", icon=icon("chart-bar"),
                                plotOutput("missing_value"),
                                verbatimTextOutput("missing_var_obs"),
                                textOutput("Misstext")
                       ),
                       tabPanel(title="Boxplot", icon=icon("chart-bar"), 
                                plotOutput("boxplot"),
                                verbatimTextOutput("Range"),
                                textOutput("Boxtext")
                       ),
                       tabPanel(title="Timeseries", icon=icon("chart-line"),
                                plotOutput("time")
                       ),
                       tabPanel(title="Correlation", icon=icon("chart-line"),
                                plotOutput("corrs"),
                                textOutput("corrstext")
                       ),
                       tabPanel(title="Continuity", icon=icon("chart-line"),
                                plotOutput("continuity"),
                                textOutput("Continuity")
                       ),
                       tabPanel(title="Bar Chart", icon=icon("chart-bar"),
                                uiOutput("bar")
                       ),
                       tabPanel(title="Homogeneity", icon=icon("chart-line"),
                                plotOutput("homo_num"), 
                                textOutput("homotext"),
                                plotOutput("homo_all"),
                                textOutput("homotext2")
                       ),
                       tabPanel(title="Novelties", icon=icon("chart-line"),
                                plotOutput("pca"),
                                textOutput('pcatext'),
                                plotOutput("pca2"),
                                textOutput("pcatext2"),
                                plotOutput("mosaic")
                       )
            )
        )
    )
))

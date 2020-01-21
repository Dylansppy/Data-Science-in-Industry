#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(shinyjs)
#data(iris)
#data(Glass)

domChoices <- c("l","f","r","t","i","p")
    
    
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Assignment 2 Multi-level Classification - Peng Shen (57408055)"),
    
    # Print Option
    tags$head(tags$style(HTML("
                            #Data_Str {
                              font-size: 20px;
                              font-weight: bold;
                            }
                            #Des_K-means {
                              font-size: 20px;
                              font-weight: bold;
                            }
                            #DBscan_title {
                              font-size: 20px;
                              font-weight: bold;
                            }
                            "))),
    # Tabset
    tabsetPanel(
        # Dataset Panel
        tabPanel(
            title = "Dataset",
            sidebarLayout(
                sidebarPanel(
                    selectInput("data_select","Select a dataset", choices=c('iris','Glass','Null'), selected='iris'),
                    fileInput("file", label = "Or Import a CSV file"),
                    checkboxInput("rownames", "Show Row Names", value=T),
                    checkboxInput("order", "Column Ordering", value=T),
                    selectInput("selection", "Selection Type", choices=c("none","single","multiple"), selected = "none"),
                    selectInput("filter", "Filter Type", choices=c("none","top","bottom"), selected = "none"),
                    selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices)
                ),
                mainPanel(DT::dataTableOutput("Dataset"),
                          textOutput("Data_Str"),
                          verbatimTextOutput("Structure")
                          )
            )
        ),
        
        #Summary panel
        tabPanel(
            title = "Summary",
            verbatimTextOutput("Summary"),
            textOutput("Summary_result")
        ),
        
        # Data vasualization panel
        tabPanel(
            title = "EDA with Vasulization",
            tabsetPanel(
                # Target Panel
                tabPanel(title="Target",
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("Target_pie")
                             ),
                             mainPanel(
                                 plotOutput("Piechart"),
                                 textOutput("Des_iris_target"),
                                 textOutput("Des_glass_target")
                             )
                         )
                ),
                # Feature Panel
                tabPanel(title="Numeric Features",
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("Target_box"),
                                 uiOutput("Numeric_var"),
                                 numericInput("range", "Input a range value for boxplot", value=1.5, min=0, step=0.5),
                                 checkboxInput("yj_transform", "Perform a YeoJohnson transform?", value=F)
                             ),
                             mainPanel(
                                 plotOutput("Boxplot"),
                                 textOutput("Box_description"),
                                 textOutput("Box_description2")
                             )
                         )
                ),
                
                # Correlation Panel
                tabPanel(title="Correlation between Numeric Features",
                         sidebarLayout(
                             sidebarPanel(
                                 uiOutput("Numeric_var_corr")
                             ),
                             mainPanel(
                                 plotOutput("Corr"),
                                 textOutput("Des_Corr"),
                                 textOutput("Des_Corr2")
                             )
                         )
                ),
                # Clustering
                tabPanel(
                    title = "Clustering",
                    sidebarLayout(
                        sidebarPanel(
                            uiOutput("X_col"),
                            uiOutput("Y_col"),
                            numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
                            checkboxInput("yj_transform1", "Perform a YeoJohnson transform?", value=F)
                        ),
                        mainPanel(
                            textOutput('Des_K-means'),
                            plotOutput('Plot_cluster'),
                            textOutput('DBscan_title'),
                            verbatimTextOutput('DBscan'),
                            textOutput('Des_db'),
                            textOutput('Des_cluster_outlier')
                        )
                    )
                )
            )
        ),
        
        # Modelling Panel
        tabPanel(
            title = "Classification",
            sidebarLayout(
                sidebarPanel(
                    sliderInput("percent_train",
                                "Percentage of Training Set:",
                                min = 10,
                                max = 100,
                                value = 80),
                    uiOutput("Target"),
                    uiOutput("Features"),
                    fluidRow(
                        column(width=12,
                               radioButtons("algorithm", 
                                            label = h3("Choose Algorithm"), 
                                            choices = list("Logistic Regression" = 1, 
                                                           "Linear Discriminant Analysis" = 2, 
                                                           "K Nearest Neighbors (auto-tuning)" = 3,
                                                           "Support Vector Machine (auto-tuning)"= 4,
                                                           "NaiveBayes (auto-tuning)"= 5,
                                                           "Tree (auto-tuning)"= 6,
                                                           "Random Forests (auto-tuning)"= 7,
                                                           "XGBoost (auto-tuning)"= 8
                                            ),
                                            selected = 1
                               )
                        ),
                        column(width=12,
                               conditionalPanel("input.algorithm == 1",
                                                textOutput("Description_Logit")
                               ),
                               conditionalPanel("input.algorithm == 2",
                                                textOutput("Description_LDA")
                               ),
                               conditionalPanel("input.algorithm == 3",
                                                textOutput("Description_KNN"),
                                                selectInput("resampling1", "Resampling Method for Hyperparameter Tuning", choices=c('cv','repeatedcv','boot'), selected='cv'),
                                                sliderInput("folds1",
                                                            "Folds of Resampling:",
                                                            min = 1,
                                                            max = 100,
                                                            value = 5)
                               ),
                               conditionalPanel("input.algorithm == 4",
                                                textOutput("Description_SVM"),
                                                selectInput("resampling2", "Resampling Method for Hyperparameter Tuning", choices=c('cv','repeatedcv','boot'), selected='cv'),
                                                sliderInput("folds2",
                                                            "Folds of Resampling:",
                                                            min = 1,
                                                            max = 100,
                                                            value = 5)
                               ),       
                               conditionalPanel("input.algorithm == 5",
                                                textOutput("Description_Bayes"),
                                                selectInput("resampling3", "Resampling Method for Hyperparameter Tuning", choices=c('cv','repeatedcv','boot'), selected='cv'),
                                                sliderInput("folds3",
                                                            "Folds of Resampling:",
                                                            min = 1,
                                                            max = 100,
                                                            value = 5)
                               ),      
                               conditionalPanel("input.algorithm == 6",
                                                textOutput("Description_Tree"),
                                                selectInput("resampling4", "Resampling Method for Hyperparameter Tuning", choices=c('cv','repeatedcv','boot'), selected='cv'),
                                                sliderInput("folds4",
                                                            "Folds of Resampling:",
                                                            min = 1,
                                                            max = 100,
                                                            value = 5)
                               ),       
                               conditionalPanel("input.algorithm == 7",
                                                textOutput("Description_RF"),
                                                selectInput("resampling5", "Resampling Method for Hyperparameter Tuning", choices=c('cv','repeatedcv','boot'), selected='cv'),
                                                sliderInput("folds5",
                                                            "Folds of Resampling:",
                                                            min = 1,
                                                            max = 100,
                                                            value = 5)
                               ),        
                               conditionalPanel("input.algorithm == 8",
                                                textOutput("Description_XGB"),
                                                selectInput("resampling6", "Resampling Method for Hyperparameter Tuning", choices=c('cv','repeatedcv','boot'), selected='cv'),
                                                sliderInput("folds6",
                                                            "Folds of Resampling:",
                                                            min = 1,
                                                            max = 100,
                                                            value = 5)
                               )       
                        )
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(title="Model Summary",
                                 verbatimTextOutput("Classification")
                        ),
                        tabPanel(title="Model Performance",
                                 verbatimTextOutput("Matrix")
                        ),
                        tabPanel(title="Misclassification Visualization",
                                 plotOutput("Misclassification")
                        )
                    )
                )
            )
        )
    )
))

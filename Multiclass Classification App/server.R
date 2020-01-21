#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
#library(DT)
#library(MASS)
#library(mlbench)
#library(tidyverse)
#library(ggplot2)
#library(reshape2)
#library(caret)
#data(iris)
#data(Glass)

# Define server logic 
shinyServer(function(input, output) {
    # https://datatables.net/reference/option/dom
    
    # https://datatables.net/extensions/index
    ext <- list(Responsive = TRUE)
    
    # Get the data
    data <- reactive({
        if (input$data_select!='Null') {
            get(input$data_select)
        } else {
            read.csv(input$file)
        }
    })
    
    # DataTable
    output$Dataset <- DT::renderDataTable({
        DT::datatable(data = data(),
                      rownames = input$rownames,
                      selection = input$selection,
                      filter = list(position = input$filter),
                      options = list(searching = TRUE,
                                     pageLength = 10,
                                     lengthMenu = c(10, 100, 300),
                                     dom = paste(input$dom, collapse = ""),
                                     ordering = input$order
                      ),
                      extensions = ext
        )  %>%
        formatStyle(columns = 1, backgroundColor = "lightblue")  #%>%
        #formatCurrency(c(2), '$') %>%
        #formatPercentage(3, 2) %>%
        #formatRound(c("hp"), 3)
    })
    
    # Title of data structure
    output$Data_Str <- renderText({
        "Data Structure"
    })
    
    # Dataset structure
    output$Structure <- renderPrint({
        str(data())
    })
    
    # Summary statistics
    output$Summary <- renderPrint({
        summarytools::dfSummary(data(),
                  method = 'render',
                  omit.headings = TRUE,
                  bootstrap.css = FALSE)
    })
    
    # Description of summary
    output$Summary_result <- renderText({
        "Description - There is no missing data in the iris and Glass dataset and
        a total of 150 and 214 observations in iris and Glass dataset respectively.
        All the predictors are numeric in iris and Glass dataset.
        Outcome variable in iris dataset is a factor variable with 3 levels, 
        while in Glass dataset the outcome variable is a factor variable with 6 levels.
        "
    })
    
    # Target variable visualization using pie chart
    output$Target_pie <- renderUI({
        selectInput("target1","Select Target", choices=fac_name(), selected=fac_name()[1])
    })
    
    target1 <- reactive({
        data()[,input$target1]
    })
    
    output$Piechart <- renderPlot({
        col_num <- nlevels(as.factor(target1()))
        pie(table(target1()), 
            col = rainbow(col_num), 
            labels=paste(levels(as.factor(target1())),"\n",table(target1()),sep=""), 
            main=paste0("Proportion of Each Class in ", input$target1)
            )
        legend("right",legend=levels(as.factor(target1())), fill=rainbow(col_num), title=input$target1, box.lty=0)
    })
    
    output$Des_iris_target <- renderText({
        "Description - The iris species have balanced class."
    })
    
    output$Des_glass_target <- renderText({
        "Description - For Glass dataset, some class imbalance can be observed, which might bring some issues regarding model
        performance evaluation if using accuracy. Using models that use observation weighting and cope with 
        class imbalance well such as random forest, rpart or glm et al. would be a good idea. Moreover, 
        it's better to take Cohen's Kappa value into consideration as well as use precision and recall (sensitivity) 
        which are less prone to favour the most common outcomes."
        
    })
    
    # Boxplot for numerical variables
    output$Target_box <- renderUI({
        selectInput("target2","Select Target", choices=fac_name(), selected=fac_name()[1])
    }) 
    
    output$Numeric_var <- renderUI({
        selectInput("num_var","Select Numeric Features to Explore",choices=num_name(), multiple=TRUE, selected=num_name()[1])
    })
    
    num_name_selected <- reactive({
        input$num_var
    })
    
    target_name_selected <- reactive({
        input$target2
    })
    
    output$Boxplot <- renderPlot({
        label <- target_name_selected()
        if (input$yj_transform){
            rec <-recipe(as.formula(paste(target_name_selected(),"~", paste(num_name_selected(), collapse= "+"))), data = data()) %>%
                step_YeoJohnson(all_predictors()) %>%
                prep(data = data())
            processed <- bake(rec, data())
            df.m <- melt(processed, id.var = label)
            p <- ggplot(data = df.m, aes(x=variable, y=value, fill=get(label))) + 
                geom_boxplot(coef=input$range, notch = TRUE) +
                labs(title ="Boxplots for Selected Features in Each Target Class", x = "Standardised Features", y = "Std Value") +
                scale_fill_discrete(name=label) +
                theme(plot.title = element_text(hjust = 0.5))
        } else {
            df.m <- melt(data()[,c(num_name_selected(),target_name_selected())], id.var = label)
            p <- ggplot(data = df.m, aes(x=variable, y=value, fill=get(label))) + 
                geom_boxplot(coef=input$range, notch = TRUE) +
                labs(title ="Boxplots for Selected Features in Each Target Class", x = "Features", y = "Value") +
                scale_fill_discrete(name=label) +
                theme(plot.title = element_text(hjust = 0.5))
        }
        p + facet_wrap( ~ variable, scales="free")
    })
    
    output$Box_description <- renderText({
        "Description for iris dataset- There is no outlier outside 3.5*IQR range.It can be observed that
        the sepal length, petal length and width successively increase for setosa, versicolor, and virginica.
        Setosa has the largest sepal width among three, followed by virginica and then versicolor.           
        "
    })
    
    output$Box_description2 <- renderText({
        "Description for Glass dataset- It can be observed that numeric features in this dataset are not 
        normally distributed generally or in each target class. And some outliers of a certain element
        feature in a certain glass type can be observed, which might indicate that impure substance in
        the glass.The content of Ba is a strong indicator of type 7 glass, while element like Si doesn't 
        seem to be a good indicator of glass type.
        "
    })
        
    
    # Coorelation visualization
    num_name <- reactive({
        names(data() %>% select_if(is.numeric))
    })
    
    output$Numeric_var_corr <- renderUI({
        selectInput("num_var_corr","Select Numeric Variables to Explore Correlations",choices=num_name(), multiple=TRUE, selected=num_name()[1:2])
    })
    
    output$Corr <- renderPlot({
        chart.Correlation(data()[, input$num_var_corr])
    })
    
    output$Des_Corr <- renderText({
        "For iris dataset, some strong correlation between sepal length, petal length and petal width can be observed, 
        which might indicate some dimension reduction can be done to reduce the features used in classification models
        during feature selection."
    })
    
    output$Des_Corr2 <- renderText({
        "For Glass dataset, it can be obviously observed a strong correlation between RI and Ca, which 
        indicates some dimension reduction direction."
    })
    
    
    # Clustering
    output$X_col <- renderUI({
        selectInput("x_col","X variable", choices=num_name(), selected=num_name()[1])
    })
    
    output$Y_col <- renderUI({
        selectInput("y_col","Y variable", choices=num_name(), selected=num_name()[2])
    })
    
    selectedData <- reactive({
        data()[, c(input$x_col, input$y_col)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$`Des_K-means` <- renderText({
        "K-means Clustering"
    })
    
    output$Plot_cluster <- renderPlot({
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                  "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    output$DBscan_title <- renderText({
        "The following are the observations that were not allocated to a cluster in 
        the density based clustering model:"
    })
    
    output$DBscan <- renderPrint({
        if (input$yj_transform1){
            rec <- recipe(~.,data = data()) %>%
                step_naomit(everything()) %>% # or step_knnimpute(everything(), neighbors = 5)
                step_dummy(all_nominal(), one_hot = FALSE) %>%  # or step_rm(all_nominal())  
                step_nzv(all_predictors()) %>%
                step_lincomb(all_predictors()) %>%
                step_YeoJohnson(all_predictors()) %>%
                prep(data = data()) 
        }else{rec <- recipe(~.,data = data()) %>%
            step_naomit(everything()) %>% # or step_knnimpute(everything(), neighbors = 5)
            step_dummy(all_nominal(), one_hot = FALSE) %>%  # or step_rm(all_nominal())  
            step_nzv(all_predictors()) %>%
            step_lincomb(all_predictors()) %>%
            prep(data = data()) 
        }
        processed <- bake(rec, data())
        clustered <- dbscan::dbscan(processed, eps = input$clusters , minPts = 4)
        processed[clustered$cluster == 0,]
    })
    
    output$Des_db <- renderText({
        "Description- Points not allocated to clusters constitute “noise” and 
        can be investigated as outliers to the density based clustering model."
    })
    
    output$Des_cluster_outlier <- renderText({
        "It can be observed that when the cluster number was set to the number of levels in the outcome variable (Target)
        for the classification problem, there is no observation that is not allocated to a density based cluster."
    })
    #Classification
    # Training and testing data for classification
    fac_name <- reactive({
        names(data() %>% select_if(is.factor))
    })
    
    output$Target <- renderUI({
        selectInput("target","Select Target", choices=fac_name(), selected=fac_name()[1])
    })
    
    output$Features <- renderUI({
        selectInput("features","Select Predictors", choices=names(data())[names(data()) != input$target], multiple=TRUE, selected=names(data())[names(data()) != input$target][1])
    })
    
    target <- reactive({
        data()[,input$target]
    })
    
    trainInd <- reactive({
        caret::createDataPartition(y = target(), times = 1, p = input$percent_train * 0.01, list = FALSE)
    })
    
    train <- reactive({
        data()[trainInd(),]
    })
    
    test <- reactive({
        data()[-trainInd(),]
    })
    
    form <- reactive({
        as.formula(paste(input$target,"~", paste(input$features, collapse= "+")))
    })
    
    # Model
    mod <- reactive({
        if (input$algorithm == 1) {
            multinom(formula = form(), data = train())
        } else if (input$algorithm == 2) {
            lda(formula = form(), data = train(), na.action = na.omit)
        } else if (input$algorithm == 3) {
            caret::train(form(), data = train(), method = "knn",
                         trControl = trainControl(method = input$resampling1, number = input$folds1, repeats=3, verboseIter=FALSE), 
                         preProcess = c("center","scale"), 
                         tuneLength = 20
            )
        } else if (input$algorithm == 4) {
            caret::train(form(), data = train(), method = "svmLinear",
                         trControl = trainControl(method = input$resampling2, number = input$folds2, repeats=3, verboseIter=FALSE),
                         preProc = c("center", "scale"),
                         tuneLength = 20
            )
        } else if (input$algorithm == 5) {
            caret::train(form(), data = train(), method = "nb",
                         trControl = trainControl(method = input$resampling3, number = input$folds3, repeats=3, verboseIter = FALSE),
                         tuneGrid = expand.grid(usekernel = c(TRUE, FALSE),fL = 0:5,adjust = seq(0, 5, by = 1)),
                         preProc = c("BoxCox", "center", "scale", "pca")
            )
        } else if (input$algorithm == 6) {
            caret::train(form(), data = train(), method = "rpart",
                         trControl = trainControl(method = input$resampling4, number = input$folds4, repeats=3, verboseIter = FALSE)
            )
        } else if (input$algorithm == 7) {
            caret::train(form(), data = train(), method = "rf",
                         trControl = trainControl(method = input$resampling5, number = input$folds5, repeats=3, verboseIter = FALSE)
            )
        } else if (input$algorithm == 8) {
            caret::train(form(),data = train(), method = "xgbTree",
                         trControl = trainControl(method = input$resampling6, number = input$folds6, repeats=3,
                                                  verboseIter = FALSE, # no training log
                                                  allowParallel = TRUE), # FALSE for reproducible results
                         verbose = TRUE
            )
        } 
    })
    
    # Model summary
    output$Classification <- renderPrint({
        mod()
    })
    
    # Confusion Matrix
    yhat <- reactive({
        if (input$algorithm == 1) {
            predict(mod(), newdata = test(), type = "class")
        } else if (input$algorithm == 2) {
            predict(mod(), newdata = test())$class
        } else if (input$algorithm == 3) {
            predict(mod(), newdata = test())
        } else if (input$algorithm == 4) {
            predict(mod(), newdata = test())
        } else if (input$algorithm == 5) {
            predict(mod(), newdata = test())
        } else if (input$algorithm == 6) {
            predict(mod(), newdata = test())
        } else if (input$algorithm == 7) {
            predict(mod(), newdata = test())
        } else if (input$algorithm == 8) {
            predict(mod(), newdata = test())
        } 
        
    })
    
    cm <- reactive({
        caret::confusionMatrix(data = yhat(), reference = test()[, input$target])
    })
    
    # Model Performance
    output$Matrix <- renderPrint({
        cm()
    })
    
    # Misclassification
    output$Misclassification <- renderPlot({
        melted <- melt(cm()$table)
        par(pty = "s")
        alluvial::alluvial(
            melted[,1:2],
            freq = melted$value,
            col = ifelse(melted[, 1] == melted[, 2], "green", "red"),
            alpha = 0.5,
            hide  = melted$value == 0
        )
        mtext("Resampled confusion matrix", side = 3, line = 3, font = 2)
    })
    
    # Description of model
    output$Description_Logit <- renderText({
        "Logistic regression is used to describe data and to explain the relationship between one dependent binary variable and one or more nominal, ordinal,interval or ratio-level independent variables."
    })
    output$Description_LDA <- renderText({
        "LDA is closely related to analysis of variance (ANOVA) and regression analysis, which also attempt to express one dependent variable as a linear combination of other features or measurements."
    })
    output$Description_KNN <- renderText({
        "K nearest neighbors is a simple algorithm that stores all available cases and classifies new cases based on a similarity measure (e.g., distance functions)."
    })
    output$Description_Bayes <- renderText({
        "Naive Bayes classifiers are a family of simple probabilistic classifiers based on applying Bayes' theorem with strong (naive) independence assumptions between the features."
    })
    output$Description_SVM <- renderText({
        "The objective of the support vector machine algorithm is to find a hyperplane in an N-dimensional space (N the number of features) that distinctly classifies the data points."
    })
    output$Description_Tree <- renderText({
        "Decision Tree use recursive partitioning to make data structure analysis, classification and regression."
    })
    output$Description_RF <- renderText({
        "Random forests is an ensemble learning method for classification, regression and other tasks that operates by constructing a multitude of decision trees at training time."
    })
    output$Description_XGB <- renderText({
        "XGBoost is an optimized distributed gradient boosting library designed to be highly efficient, flexible and portable. It implements machine learning algorithms under the Gradient Boosting framework."
    })
})

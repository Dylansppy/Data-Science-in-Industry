shinyServer(function(input, output, session) {
  
  models <- reactiveValues()
  
  getData <- reactive({
    read.csv(file = "Ass3Data.csv", row.names = "ID")
  })
  
  ############################################################################## 
  getTrControl <- reactive({
    # shared bootstrap specification
    y <- getTrainData()[,"Y"]
    n <- 25
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, index = caret::createResample(y = y, times = n))
  })
  
  ############################################################################## 
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier)
  })
  
  ############################################################################## 
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  ############################################################################## 
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  ############################################################################## 
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  ############################################################################## 
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  ############################################################################## 
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  ############################################################################## 
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  ############################################################################## 
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  ##############################################################################  
  getNullRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$NullPreprocess)
  })
  
  ##############################################################################  
  getNullModel <- reactive({
    req(input$NullGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Null <- NULL
        method <- "null"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Null <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 0)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Null
    })
  })
  
  ############################################################################## 
  output$NullModelSummary2 <- renderPrint({
    print(getNullModel())
  })
  
  
  
  ##############################################################################  
  getGlmnetRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GlmnetPreprocess)
  })
  
  ##############################################################################  
  getGlmnetModel <- reactive({
    library(glmnet)
    req(input$GlmnetGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Glmnet <- NULL
        method <- "glmnet"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Glmnet <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Glmnet
    })
  })
  
  ############################################################################## 
  output$GlmnetModelSummary0 <- renderText({
    description("glmnet")
  })

  ############################################################################## 
  output$GlmnetModelSummary1 <- renderTable({
    mod <- getGlmnetModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$GlmnetModelPlots <- renderPlot({
    plot(getGlmnetModel())
  })     
  
  ############################################################################## 
  output$GlmnetModelSummary2 <- renderPrint({
    mod <- getGlmnetModel()
    print(mod)
  })

  
  
  ##############################################################################  
  getPlsRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$PlsPreprocess)
  })
  
  ##############################################################################
  getPlsModel <- reactive({
    library(pls)
    req(input$PlsGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Pls <- NULL
        method <- "pls"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Pls <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Pls
    })
  })
  
  ############################################################################## 
  output$PlsModelSummary0 <- renderText({
    description("pls")
  })

  ############################################################################## 
  output$PlsModelSummary1 <- renderTable({
    mod <- getPlsModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModel())
  })     
  
  ############################################################################## 
  output$PlsModelSummary2 <- renderPrint({
    mod <- getPlsModel()
    summary(mod$finalModel)
  })
  
  
  
  ##############################################################################  
  getRpartRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RpartPreprocess)
  })
  
  ##############################################################################
  getRpartModel <- reactive({
    library(rpart)
    req(input$RpartGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Rpart <- NULL
        method <- "rpart"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Rpart <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Rpart
    })
  })
  
  ############################################################################## 
  output$RpartModelSummary0 <- renderText({
    description("rpart")
  })
  
  ############################################################################## 
  output$RpartModelSummary1 <- renderTable({
    mod <- getRpartModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$RpartModelPlots <- renderPlot({
    plot(getRpartModel())
  })
  
  ############################################################################## 
  output$RpartModelTree <- renderPlot({
    library(rpart.plot)
    rpart.plot::rpart.plot(getRpartModel()$finalModel)
  })     
  
  ############################################################################## 
  output$RpartModelSummary2 <- renderPrint({
    mod <- getRpartModel()
    print(mod$finalModel)
  })
  
  
  ##############################################################################  
  getRidgeRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$RidgePreprocess)
  })
  
  ##############################################################################  
  getRidgeModel <- reactive({
    library(foba)
    req(input$RidgeGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Ridge <- NULL
        method <- "foba"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Ridge <- caret::train(getRidgeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Ridge
    })
  })
  
  ############################################################################## 
  output$RidgeModelSummary0 <- renderText({
    description("foba")
  })
  
  ############################################################################## 
  output$RidgeModelSummary1 <- renderTable({
    mod <- getRidgeModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$RidgeModelPlots <- renderPlot({
    plot(getRidgeModel())
  })     
  
  ############################################################################## 
  output$RidgeModelSummary2 <- renderPrint({
    mod <- getRidgeModel()
    print(mod)
  })
  
  
  ##############################################################################  
  getQRFRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$QRFPreprocess)
  })
  
  ##############################################################################
  getQRFModel <- reactive({
    library(quantregForest)
    req(input$QRFGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$QRF <- NULL
        method <- "qrf"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$QRF <- caret::train(getQRFRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$QRF
    })
  })
  
  ############################################################################## 
  output$QRFModelSummary0 <- renderText({
    description("qrf")
  })
  
  ############################################################################## 
  output$QRFModelSummary1 <- renderTable({
    mod <- getQRFModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$QRFModelPlots <- renderPlot({
    plot(getQRFModel())
  })
  
  ############################################################################## 
  output$QRFModelSummary2 <- renderPrint({
    mod <- getQRFModel()
    print(mod$finalModel)
  })
  
  
  ##############################################################################  
  getXgbRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$XgbPreprocess)
  })
  
  ##############################################################################
  getXgbModel <- reactive({
    library(xgboost)
    req(input$XgbGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Xgb <- NULL
        method <- "xgbLinear"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Xgb <- caret::train(getXgbRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Xgb
    })
  })
  
  ############################################################################## 
  output$XgbModelSummary0 <- renderText({
    description("xgbLinear")
  })
  
  ############################################################################## 
  output$XgbModelSummary1 <- renderTable({
    mod <- getXgbModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$XgbModelPlots <- renderPlot({
    plot(getXgbModel())
  })
  
  ############################################################################## 
  output$XgbModelSummary2 <- renderPrint({
    mod <- getXgbModel()
    print(mod$finalModel)
  })
  

  ##############################################################################  
  getSVMRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$SVMPreprocess)
  })
  
  ##############################################################################
  getSVMModel <- reactive({
    library(LiblineaR)
    req(input$SVMGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$SVM <- NULL
        method <- "svmLinear3"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$SVM <- caret::train(getSVMRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$SVM
    })
  })
  
  ############################################################################## 
  output$SVMModelSummary0 <- renderText({
    description("svmLinear3")
  })
  
  ############################################################################## 
  output$SVMModelSummary1 <- renderTable({
    mod <- getSVMModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$SVMModelPlots <- renderPlot({
    plot(getSVMModel())
  })
  
  ############################################################################## 
  output$SVMModelSummary2 <- renderPrint({
    mod <- getSVMModel()
    print(mod$finalModel)
  })
  
  
  ##############################################################################  
  getGaussPolyRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$GaussPolyPreprocess)
  })
  
  ##############################################################################
  getGaussPolyModel <- reactive({
    library(kernlab)
    req(input$GaussPolyGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$GaussPoly <- NULL
        method <- "gaussprPoly"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$GaussPoly <- caret::train(getGaussPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$GaussPoly
    })
  })
  
  ############################################################################## 
  output$GaussPolyModelSummary0 <- renderText({
    description("gaussprPoly")
  })
  
  ############################################################################## 
  output$GaussPolyModelSummary1 <- renderTable({
    mod <- getGaussPolyModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$GaussPolyModelPlots <- renderPlot({
    plot(getGaussPolyModel())
  })
  
  ############################################################################## 
  output$GaussPolyModelSummary2 <- renderPrint({
    mod <- getGaussPolyModel()
    print(mod$finalModel)
  })
  
  
  ##############################################################################  
  getQrnnRecipe <- reactive({
    recipe <- steps(recipes::recipe(Y ~ ., data = getTrainData()), input$QrnnPreprocess)
  })
  
  ##############################################################################
  getQrnnModel <- reactive({
    library(qrnn)
    req(input$QrnnGo)
    isolate({
      clus <- startMode(input$Parallel)
      tryCatch({
        models$Qrnn <- NULL
        method <- "qrnn"
        showNotification(id = method, paste("Assessing", method, "model using resampling"), session = session, duration = NULL)
        models$Qrnn <- caret::train(getQrnnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 6)
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
      models$Qrnn
    })
  })
  
  ############################################################################## 
  output$QrnnModelSummary0 <- renderText({
    description("qrnn")
  })
  
  ############################################################################## 
  output$QrnnModelSummary1 <- renderTable({
    mod <- getQrnnModel()
    req(mod)
    as.data.frame(mod$bestTune)
  })  
  
  ############################################################################## 
  output$QrnnModelPlots <- renderPlot({
    plot(getQrnnModel())
  })
  
  ############################################################################## 
  output$QrnnModelSummary2 <- renderPrint({
    mod <- getQrnnModel()
    print(mod$finalModel)
  })
  
  
  ############################################################################## 
  output$SelectionSummary <- renderPrint({
    results <- resamples(models)
    summary(results)
  })
  
  ############################################################################## 
  getResamples <- reactive({
    results <- caret::resamples(reactiveValuesToList(models))
    NullModel <- "Null"
    
    #scale metrics using null model. Tough code to follow -sorry
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models, selected = 'GaussPoly')
    results
  })
  
  
  ############################################################################## 
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  ############################################################################## 
  getTestResults <- reactive({
    test <- getTestData()
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
  })
  
  ############################################################################## 
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })
  
  ############################################################################## 
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  }, height = 600)
  
})

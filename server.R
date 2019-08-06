#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Import required libraries
library(shiny)
library(DT)
library(summarytools)
library(PerformanceAnalytics)
library(visdat)
library(ggplot2)
library(scales)
library(tabplot)
library(vcd)

# Load the data file
dat <- read.csv("Ass1Data.csv", header = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # https://datatables.net/reference/option/dom
    
    
    # https://datatables.net/extensions/index
    ext <- list(Responsive = TRUE)
    
    # DataTable
    output$tableX <- DT::renderDataTable({
        DT::datatable(data = dat,
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
            formatStyle(columns = c("Y"), backgroundColor = "lightblue")  #%>%
            #formatCurrency(c(2), '$') %>%
            #formatPercentage(3, 2) %>%
            #formatRound(c("hp"), 3)
    })
    
    # SelectRow 
    output$SelRows <- renderTable({
        req(input$tableX_rows_selected)
        print(dat[input$tableX_rows_selected,"Y"])
    })
    
    # Data set structure
    output$Structure <- renderPrint({
        str(dat)
    })
    
    # Datatypes
    output$Datatype <- renderPrint({
        threshhold <- 0.2
        lines <- "Variables in dataset:"
        for (col in colnames(dat)) {
            xline <- paste0(" ", col, " -", paste(collapse = ",", class(dat[,col])),"- ")
            ratio <- length(unique(dat[,col])) / nrow(dat)
            if ( ratio == 1) {
                xline <- paste0(xline, "is continuous, ratio = ", round(ratio,1))
            } else if (ratio > threshhold) {
                xline <- paste0(xline, "is as good as continuous, ratio = ", round(ratio,3))
            } else {
                xline <- paste0(xline, "is not continuous, ratio = ", round(ratio,3))
            }
            lines <- c(lines, xline)
        }
        cat(paste(lines, collapse = "\n"))
    })
    
    # Description of novelty variables
    output$Novel_variable <- renderText({
        "Description - No unexpected continuous variables. For non-continuous variables,\n
                      'ID' and 'Date' have the number of levels that equals to the number of observations,\n
                      'ID' should be treated as character string and has an ID role,\n
                      'Date' is timestamp so it's not unexpected."
    })
    
    # Summary statistics
    output$Summary <- renderPrint({
        dfSummary(dat,
                  method = 'render',
                  omit.headings = TRUE,
                  bootstrap.css = FALSE)
    })
    
    # Data roles 
    output$Role <- renderText({
        "Observation Identifier: ID (100% unique in current dataset)\nOutcome: Y (a speculation from the variable name)\nPredictors: Priority, Price, Speed, Duration, Scarcity, Location, Agreed, State, Class, Surface, sensor1-30\nObservation Groups: Author (Nominal variables with levels that are not fixed)"
    })
    
    # Boxplot for numerical variables
    output$boxplot <- renderPlot({
        numData <- scale(dat[,input$num_col], center = input$centre, scale = input$scale)
        boxplot(numData, 
                main="Boxplots of Continuous Variables",
                range=input$range, 
                notch=FALSE)
    })
    
    # Description of range value for boxplot
    output$Range <- renderPrint({
        line <- c("criterion value to identify uni-variable outliers set as:", input$range)
        cat(line)
    })
    
    # Description of boxplot
    output$Boxtext <- renderText({
        "Description - From the boxplot of all the continuous variables, \n
        it can be observed that when the criterion value to identify outliers was set as 1.5 or even 10, \n
        variable ‘sensor 2, 5, 13, 15, 21, 26, 27’ have obvious uni-variable novelties. Further context might be needed to determine the reason and whether they are outliers."
    })
    
    # Timeseries plot
    output$time <- renderPlot({
        cols <- input$num_cols # choose the numeric columns
        numData <- scale(dat[,cols], center = input$centre, scale = input$scale) 
        tsdat <- ts(numData, frequency=6, start=c(2001, 1), end=c(2006,12))
        plot(tsdat, main="Timeseries of Numerical Variables", type = "l", col = alpha(rainbow(ncol(numData)), 0.4), xlab = "Date", ylab = "Values" ) 
    })
    
    # Correlation 
    output$corrs <- renderPlot ({
        numData <- scale(dat[,input$num_cols], center = input$centre, scale = input$scale)
        chart.Correlation(numData)
    })
    
    # Correlation description
    output$corrstext <- renderText({
        "Description – It can be observed that there are two distinct clusters for ‘sensor2, 5, 13, 15, 21, 26, 27' and apparently there is no significant association between ‘Y’ and these potential predictors.\n
        For the rest of ‘sensor’ data, it shows some interesting patterns in their relationship. \n
        In regarding to the correlation with ‘Y’, there seems to be three groups of ‘sensor’ data. \n
        Group 1 consists of ‘sensor1, 3, 4, 6, 7, 8, 9, 10’, group 2 contains ‘sensor11,12,14, 16, 17, 18, 19, 20’, \n
        and group 3 includes ‘sensor 22, 23, 24, 25, 28, 29, 30’. Sensors in group 1 have values of the correlation with ‘Y’ over 0.5, \n
        sensors in group 2 have the values between 0.16-0.21, and group 3 around 0. Sensors within each group are more related with each other than with sensors outside the group."
    })
    
    # Bar chart for freqency distribution of factors
    output$bar <- renderUI({
        fluidRow(
            column(6, 
                   lapply(seq(1, length(input$fac_col), 2), function(i) {
                       renderPlot({
                           col <- input$fac_col[i]
                           dat.freq <- table(dat[, col])
                           barplot(dat.freq,
                                   main=paste0("Frequency Distribution of ", input$fac_col[i]),
                                   ylab="Counts",
                                   xlab=col
                           )
                       })
                   })
            ),
            if (length(input$fac_col) > 1) {
                column(6,
                   lapply(seq(2, length(input$fac_col), 2), function(i) {
                       renderPlot({
                           col <- input$fac_col[i]
                           dat.freq <- table(dat[, col])
                           barplot(dat.freq,
                                   main=paste0("Frequency Distribution of ", input$fac_col[i]),
                                   ylab="Counts",
                                   xlab=col
                           )
                       })
                   })
            )
        })
    })
    
    # Missing value distribution
    output$missing_value <- renderPlot ({
        vis_dat(dat) +
            ggtitle("Missing Value Distribution")
    })
    
    # Variables and observations over missing value ratio threshold
    output$missing_var_obs <- renderPrint({
        threshhold <- input$threshold
        line1 <- "Variable with selceted ratio of missing values in data set:\n"
        for (col in colnames(dat)) {
            if (sum(is.na(dat[,col])) / nrow(dat) > threshhold ) {
                line1 <- c(line1, paste("Variable", col, "has the missing value ratio of >", input$threshold*100, "%\n"))
            }
        }
        cat(line1)
        line2 <- "Observation with selected ratio of missing values in data set:\n"
        for (row in rownames(dat)) {
            if (sum(is.na(dat[row,])) / ncol(dat) > threshhold ) {
                line2 <- c(line2, paste("Observation", row, "has the missing value ratio of >", input$threshold*100, "%\n"))
            }
        }
        cat(line2)
    })
    
    # Missing value description
    output$Misstext <- renderText({
        "Description - Variables like ‘ID’, ‘Author’, ‘Date’, ‘Priority’ and ‘Y’ have no missing value.\n
        Only variable ‘sensor3’ has the missing value ratio more than 20% but less than 30%.\n
        There are 5 observation that have the missing value ratio over 20% but below 30%.\n
        Most variables demonstrate a pattern that most missing values only occur approximately in the first 250 observations,\n
        except that the ‘sensor3’ has the missing values distributing evenly in the whole observations."
    })
    
    # Continuity plot
    output$continuity <- renderPlot({
        cols <- input$num_cols # choose the numeric columns
        d <- scale(dat[,cols], center = input$centre, scale = input$scale) # filter out non-numeric columns and scale so they can be graphed with a shared Y axis
        for (col in 1:ncol(d)) {
            d[,col] <- d[order(d[,col]),col] #sort each column in ascending order
        }
        mypalette <- rainbow(ncol(d))
        matplot(y = d, type = "l", xlab = "Observations", ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Order Chart")
        legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    })
    
    # Continuity description
    output$Continuity <- renderText({
        "Description - There are discontinuities in values of variable ‘sensor2’, ‘sensor3’, ‘sensor5’, ‘sensor13’, ‘sensor15’, ‘sensor21’, ‘sensor26’, ‘sensor27’.\n
        For ‘sensor3’, there is a discontinuity at about observation 120, while for the rest, there is a discontinuity at about the 240th position of the observations."
    })
    
    # Homogeneity plot for numerical variables
    output$homo_num <- renderPlot({
        cols <- input$num_cols # choose the numeric columns
        numData <- scale(dat[,cols], center = input$centre, scale = input$scale) # Normalise so they can share a common y axis
        matplot(numData, type = "l", col = alpha(rainbow(ncol(numData)), 0.4), xlab = "Observations", ylab = "Values" ) #use transparency so we can see into the data
        legend(legend = colnames(numData), x = "topleft", y = "top", lty = 1, lwd = 1, col = alpha(rainbow(ncol(numData)), 0.4), ncol = round(ncol(numData)^0.3))
    })
    
    # Description of homogeneity plot for numerical variables
    output$homotext <- renderText({
        "Description - There are about 60 observations showing difference compared to other observations in the value of numerical variable ‘sensor2’, ‘sensor5’, ‘sensor13’, ‘sensor15’, ‘sensor21’, ‘sensor26’, ‘sensor27’, which is consistent with what’s been revealed in continuity."
    })
    
    # Tabplot for homogeneity
    output$homo_all <- renderPlot({
        dat <- dat[c(input$num_cols, input$fac_col)]
        dat$rownum <- rownames(dat)
        tabplot::tableplot(dat, sortCol = "rownum")
    })
    
    # Description of tabplot for homogeneity
    output$homotext2 <- renderText({
        "Description - Around 60 observations with the value of ‘XX’ in factor variable ‘Author’ occur at the same position where the novelties of ‘sensor’ variables mentioned above exist, which is a pattern worth investigating."
    })
    
    #The pca plot for numerical variables
    output$pca <- renderPlot({
        cols <- input$num_cols
        dat <- dat[, cols]
        pca <- prcomp(~., data = dat, center = input$centre, scale. = input$scale)
        plot(pca$x[,1:2], main="Multi-variate Novelties for Numeric Variables") # plot only the first two principle components
    })
    
    # The pca plot for factor variables
    output$pca2 <- renderPlot({
        cols <- input$fac_col
        dat <- dat[, cols]
        dummies <- model.matrix(~.-1, dat) # convert all variables to numeric but exclude the intercept
        pca <- prcomp(dummies, center = input$centre, scale. = input$scale)
        plot(pca$x[,1:2], main="Multi-variate Novelties for Factor Variables") # plot only the first two principle components
    })
    
    # The description of pca plot for numerical variables
    output$pcatext <- renderText({
        "Description – Numerical variables with novelties found in boxplots which are ‘sensor 2, 5, 13, 15, 21, 26, 27’ are included in the plot. It can be seen that there are 2 clusters which means they are multi-variate novelties."
    })
    
    # The description of pca plot for factor variables
    output$pcatext2 <- renderText({
        "Description – All factor variables except for ‘ID’ and ‘Date’ are included in plot, and there are no remarkable novelties."
    })
    
    # Mosaic plot
    output$mosaic <- renderPlot({
        formula <- as.formula(paste("~",paste(input$fac_col, collapse= "+")))
        mosaic(formula, data = dat, main = "Frequency novelties", sub = "Novelty shows in red",
               legend = TRUE, shade = TRUE, highlighting_fill = colors)
    })
})
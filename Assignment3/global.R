library(shiny)
library(DT)
library(corrgram)
library(visdat)

# The recipes package is central to preprocessing
library(recipes)

# We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(doParallel)

# This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(caret)

set.seed(1)

ppchoices <- c("knnimpute", "bagimpute", "medianimpute", "naomit", "YeoJohnson", "dummy", "center", "scale", "pca", "pls", "ica", "nzv", "other")
               
#               "BoxCox", "spatialSign", "corr", "medianImpute", 

startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
    registerDoParallel(clus)
    clus
  } else {
    NULL
  }
}

stopMode <- function(clus) {
  if (!is.null(clus)) {
    stopCluster(clus)
    registerDoSEQ()
  }
}

steps <- function(recipe, preprocess) {
  for (s in preprocess) {
    if (s == "knnimpute") {
      recipe <- step_knnimpute(recipe, all_predictors(), k = 5) # 5 is a reasonable guess
    } else if (s == "bagimpute") {
      recipe <- step_bagimpute(recipe, all_predictors(), seed_val = 1)
    } else if (s == "medianimpute") {
      recipe <- step_medianimpute(recipe, all_predictors(), -all_nominal())
    } else if (s == "Yeojohnson") {
      recipe <- step_YeoJohnson(recipe, all_predictors())
    } else if (s == "naomit") {
      recipe <- step_naomit(recipe, all_predictors(), skip = TRUE)
    } else if (s == "pca") {
      recipe <- step_pca(recipe, all_predictors(), -all_nominal(), threshold = 0.95)
    } else if (s == "pls") {
      recipe <- step_pls(recipe, all_predictors(), -all_nominal(), outcome = "Y")
    } else if (s == "ica") {
      recipe <- step_ica(recipe, all_predictors(), -all_nominal())
    } else if (s == "center") {
      recipe <- step_center(recipe, all_predictors(), -all_nominal())
    } else if (s == "scale") {
      recipe <- step_scale(recipe, all_predictors(), -all_nominal())
    } else if (s == "nzv") {
      recipe <- step_nzv(recipe, all_predictors(), -all_nominal())
    } else if (s == "other") {
      recipe <- step_nzv(recipe, all_predictors(), -all_numeric())
    } else if (s == "dummy") {
      recipe <- step_dummy(recipe, all_predictors(), -all_numeric(), one_hot = FALSE)
    } else if (s == "poly") {
      recipe <- step_poly(recipe, all_predictors(), -all_nominal(), options = list(degree = 2))
    }
  }
  recipe
}

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Model \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It  uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("It's characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}

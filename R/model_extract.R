#' Extract Focal Parameter from Model
#'
#'This function extracts the desired test statistic from a model object. This
#'test statistic will provide the foundation of the permutation test. During the
#'permutation test, only this parameter will be stored.
#'
#' @param Model_Object A statistical model object (change name)
#' @param Data The data that the model is built from.
#' @return The desired test statistic
#' @export

model_extract<-function(Model_Object, Data, ...){


  fit<-update(Model_Object,data=Data) #Model with new data

  invisible(capture.output(b<-summary(fit,standardize=T)[[1]])) #Summarize model

  Z<-b[b$lhs=="indirect","z"]#Extract coefficient of interest (z value)

  return(Z)
}




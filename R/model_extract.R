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

model_extract<-function(Data.ME,Model_Object.ME,Null_Model.ME,Test_Statistic.ME,...){

   #Must change the name in order for the anova function to
   #recognise fitting models with the same data object.
   data2<-Data.ME


 fitNewData<-update(Model_Object.ME,data=data2) #Fitting model with new randomized data.


 AnovaOutput<-as.data.frame(anova(Null_Model.ME,fitNewData)) #Performing an anova with
 #the model with random data, and the model without that data at all (likelyhood)
 #Ratio test


#Extracting the test statistic (TS)
TS<-AnovaOutput[2,Test_Statistic.ME] #Capturing the test statistic of choice.
 #value from the anova.


 # invisible(capture.output(b<-summary(fit,standardize=T)[[1]])) #Summarize model
 #  #
 #  Z<-b[b$lhs=="indirect","z"]#Extract coefficient of interest (z value)
 #  #
   return(TS)
}


#This function will allow the interaction to be tested. The key is to fit the
#interaction to the random data, then remove the interaction from the same
#data to perform the anova.This is different from above as the random column was
#simply removed, therefore there was no need to keep the random data in the null
#model, but now there is.
model_extract2<-function(Data.ME,Model_Object.ME,Null_Model.ME,Test_Statistic.ME,Formula.ME,...){

   #Must change the name in order for the anova function to
   #recognise fitting models with the same data object.
   data2<-Data.ME


   fitNewData<-update(Model_Object.ME,data=data2) #Fitting model with new randomized data.
   fitNewNull<-update(fitNewData,Formula.ME) #Fitting model with new randomized data.


   AnovaOutput<-as.data.frame(anova(Null_Model.ME,fitNewData)) #Performing an anova with
   #the model with random data, and the model without that data at all (likelyhood)
   #Ratio test


   #Extracting the test statistic (TS)
   TS<-AnovaOutput[2,Test_Statistic.ME] #Capturing the test statistic of choice.
   #value from the anova.


   # invisible(capture.output(b<-summary(fit,standardize=T)[[1]])) #Summarize model
   #  #
   #  Z<-b[b$lhs=="indirect","z"]#Extract coefficient of interest (z value)
   #  #
   return(TS)
}



#' Extract Focal Parameter from Model
#'
#'This function extracts the desired test statistic from a model object. This
#'test statistic will provide the foundation of the permutation test. During the
#'permutation test, only this parameter will be stored.
#'
#' @param Model_Object A statistical model object (change name)
#' @param Data The data that the model is built from.
#' @return The desired test statistic
#' keywords internal
#' @export

model_extract<-function(Data.ME,Model_Object.ME,Null_Model.ME,Test_Statistic.ME,...){

   #Must change the name in order for the anova function to
   #recognise fitting models with the same data object.
   data2<-Data.ME

   #fitting new data to the model object
   fitNewData<-update(Model_Object.ME,data=data2) #Fitting model with new randomized data.

   #Performing an anova with model without variable of interest and with randomized variable
   AnovaOutput<-as.data.frame(anova(Null_Model.ME,fitNewData))


   #Extracting the test statistic (TS)
   TS<-AnovaOutput[2,Test_Statistic.ME]

   return(TS)
}


#This function will allow the interaction to be tested. The key is to fit the
#interaction to the random data, then remove the interaction from the same
#data to perform the anova.This is different from above as the random column was
#simply removed, therefore there was no need to keep the random data in the null
#model, but now there is.
model_extract2<-function(Data.ME,Model_Object.ME,Test_Statistic.ME,Formula.ME,...){

   #Must change the name in order for the anova function to
   #recognise fitting models with the same data object.
   data2<-Data.ME


   #Fitting model with new randomized data.
   fitNewData<-update(Model_Object.ME,data=data2)


   #Fitting removing term of interest in the randomized data model
    fitNewNull<-update(fitNewData,Formula.ME)

   #Performing an anova with the models with random data, with and without the parameter of interest
      AnovaOutput<-as.data.frame(anova(fitNewNull,fitNewData))

   #Extracting the test statistic (TS)
   TS<-AnovaOutput[2,Test_Statistic.ME]

   return(TS)
}


#This function will determine the test statistic based on the output of a
#lm or GLM, LMER, and GLMER as long as the test statistic was correctly designated
model_extract3_General<-function(Data.ME,Model_Object.ME,Variable.ME,Test_Statistic.ME,...){


   #Performing the supplied model again with the random data.
   Random_Model<-update(Model_Object.ME,data=Data.ME)

   #Obtaining the output test statistic for this model.
   Output<-summary(Random_Model)[["coefficients"]][Variable.ME,Test_Statistic.ME]

return(Output)

}


#This function will determine the test statistic based on the output of a
#sem fitted in lavaan
model_extract3_Lavaan<-function(Data.ME,Model_Object.ME,Variable.ME,Test_Statistic.ME,...){

   #Performing the supplied model again with the random data.
   Random_Model<-update(Model_Object.ME,data=Data.ME)

   #Obtaining the output data frame for this model.
   invisible(capture.output(
     OutputDF<-lavaan::summary(Random_Model,standardize=T) %>% purrr::pluck("pe")
   ))

   #extracting desired coefficients.
   Output<-OutputDF %>% dplyr::filter(lhs %in% Variable.ME) %>% dplyr::select(all_of(Test_Statistic.ME))

   Output<- Output[,Test_Statistic.ME]

      return(Output)
}

#This function will determine the test statistic based on the output of a
#sem fitted in lavaan
model_extract3_Lavaan_Specific<-function(Data.ME,Model_Object.ME,Variable.ME,Test_Statistic.ME,Dependent_Variable.ME,...){

   #Performing the supplied model again with the random data.
   Random_Model<-update(Model_Object.ME,data=Data.ME)

   #Obtaining the output data frame for this model.
   invisible(capture.output(
     OutputDF<-lavaan::summary(Random_Model,standardize=T) %>% purrr::pluck("pe")
   ))

   #extracting desired coefficients. (must be a vector with the left and righthandside specified.)
   Output<-OutputDF %>% dplyr::filter(rhs %in% Variable.ME , lhs %in% Dependent_Variable.ME) %>% dplyr::select(all_of(Test_Statistic.ME))

   Output<- Output[,Test_Statistic.ME]
   return(Output)
}

#This function will determine the test statistics of multiple parameters in a lavaan model

model_extract3_Lavaan_MultiParam<-function(Data.ME,Model_Object.ME,Variable.ME,Test_Statistic.ME,...){

   #Performing the supplied model again with the random data.
   Random_Model<-update(Model_Object.ME,data=Data.ME)

   #Obtaining the output data frame for this model.
   invisible(capture.output(
     OutputDF<-lavaan::summary(Random_Model,standardize=T) %>% purrr::pluck("pe")
   ))

   #extracting desired coefficients.
   Output<-OutputDF %>% dplyr::filter(lhs %in% Variable.ME) %>% dplyr::select(lhs,all_of(Test_Statistic.ME))



   Output<-tibble::deframe(Output)



   return(Output)
}

model_extract3_Lavaan_MultiParam_Specific<-function(Data.ME,Model_Object.ME,Variable.ME,Test_Statistic.ME,Dependent_Variable.ME,...){


   #Performing the supplied model again with the random data.
   Random_Model<-update(Model_Object.ME,data=Data.ME)

   #Obtaining the output data frame for this model.
   invisible(capture.output(
     OutputDF<-lavaan::summary(Random_Model,standardize=T) %>% purrr::pluck("pe")
   ))


   #extracting desired coefficients.
   Output<-OutputDF %>% dplyr::filter(op=="~",lhs %in% Dependent_Variable.ME , rhs %in% Variable.ME) %>% dplyr::select(rhs,all_of(Test_Statistic.ME))


   Output<-tibble::deframe(Output)

   print(Output)

   return(Output)
}






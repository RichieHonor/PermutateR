#' Extract Focal Parameter from Model
#'
#'This function extracts the desired test statistic from a model object. This
#'test statistic will provide the foundation of the permutation test. During the
#'permutation test, only this parameter will be stored.
#'
#' @param Model_Object A statistical model object (change name)
#' @param Data The data that the model is built from.
#' @return The desired test statistic
#' @keywords internal
#' @export

model_extract<-function(Data.ME,Model_Object.ME,Null_Model.ME,Test_Statistic.ME,...){

   #Must change the name in order for the anova function to
   #recognise fitting models with the same data object.
   data2<-Data.ME


 fitNewData<-update(Model_Object.ME,data=data2) #Fitting model with new randomized data.

 #Performing an anova with model without variable of interest and with randomized variable

    AnovaOutput<-as.data.frame(anova(Null_Model.ME,fitNewData))


#Extracting the test statistic (TS)

tryCatch(
   #attempting to get the test statistic from the model object.
   {
      TS<-AnovaOutput[2,Test_Statistic.ME] #Capturing the test statistic of choice.
      if(is.null(TS)){
         call(error)
      }
   },
   #if there is an error, check to ensure that the test statistic matches what is available to the model object
   error=function(msg){
      TS_Options<-colnames(AnovaOutput)

      if (Test_Statistic.ME %in%  TS_Options ){
         message("Error: ensure that the Test Parameter is in the model object")
      }

      else{ message(paste("Error: Test_Statistic",Test_Statistic.ME," is not part of the model object output\nPlease pick one of:"))
         print(TS_Options)
         stop("Test_Statistic not valid")
      }
   }
)

 #value from the anova.


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

      tryCatch(
         #attempting to get the test statistic from the model object.
         {
            TS<-AnovaOutput[2,Test_Statistic.ME] #Capturing the test statistic of choice.
            if(is.null(TS)){
               call(error)
            }
         },
         #if there is an error, check to ensure that the test statistic matches what is available to the model object
         error=function(msg){
            TS_Options<-colnames(AnovaOutput)

            if (Test_Statistic.ME %in%  TS_Options ){
               message("Error: ensure that the Test Parameter is in the model object")
            }

            else{ message(paste("Error: Test_Statistic",Test_Statistic.ME," is not part of the model object output\nPlease pick one of:"))
               print(TS_Options)
               stop("Test_Statistic not valid")
            }
         }
      )


   return(TS)
}


#This function will determine the test statistic based on the output of a
#lm or GLM, LMER, and GLMER as long as the test statistic was correctly designated
model_extract3_General<-function(Data.ME,Model_Object.ME,Variable.ME,Test_Statistic.ME,...){


   #Performing the supplied model again with the random data.
         Random_Model<-update(Model_Object.ME,data=Data.ME)


   #Obtaining the output test statistic for this model.

         tryCatch(
            #attempting to get the test statistic from the model object.
            {
               Output<-summary(Random_Model)[["coefficients"]][Variable.ME,Test_Statistic.ME]
            },
            #if there is an error, check to ensure that the test statistic matches what is available to the model object
                  error=function(msg){
                     TS_Options<-colnames(summary(Random_Model)[["coefficients"]])

                     if (Test_Statistic.ME %in%  TS_Options ){
                        message("Error: ensure that the Test Parameter is available in summary(Model_Object)")
                     }

                     else{ message(paste("Error: Test_Statistic",Test_Statistic.ME," is not part of the model object output\nPlease pick one of:"))
                        print(TS_Options)
                        stop("Test_Statistic not valid")
                     }
                  }
         )

return(Output)


}



#This function will determine the test statistic based on the output of a glmmTMB model
model_extract3_GLMMTMB<-function(Data.ME,Model_Object.ME,Variable.ME,Test_Statistic.ME,...){


   #Performing the supplied model again with the random data.

         Random_Model<-update(Model_Object.ME,data=Data.ME)


   #Obtaining the output test statistic for this model.

      tryCatch(
         #attempting to get the test statistic from the model object.
         {
            Output<-summary(Random_Model)[["coefficients"]]$cond[Variable.ME,Test_Statistic.ME]
         },
         #if there is an error, check to ensure that the test statistic matches what is available to the model object
         error=function(e){
            TS_Options<-colnames(summary(Random_Model)[["coefficients"]]$cond)

            if (Test_Statistic.ME %in%  TS_Options ){
               message("Error: ensure that the Test Parameter is available in summary(Model_Object)")
            }

            else{ message(paste("Error: Test_Statistic",Test_Statistic.ME," is not part of the model object output\nPlease pick one of:"))
               print(TS_Options)
               stop("Test_Statistic not valid")
            }
         }
      )

   return(Output)
}





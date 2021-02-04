#' Perform a permutation test on a model object
#'
#' This function performs a permutation test on a model object. The permutation
#' test is based on the test statistics derived from the function "summary",
#' which are typically contrasts. One must specify the variable(s) to be randomized,
#' the test parameter(s), and the amount of replication to be performed. This
#' function can perform extract multiple test parameters from the same fitted model
#' thus allowing permutation tests of multiple parameters to be performed at once,
#' efficiently. This function works on lm, glm, lmer, glmer, glmmTMB, and Lavaan
#' classes.
#'
#' @param Model_Object A statistical model object.
#' @param Randomize_Variables A character vector indicating the variables that
#' are to be randomized in the permutation test
#'@param Test_Parameter A character string indicating the parameter that the
#'permutation test is to be performed on. For example, 'var1:var2' or "var1".
#'This function also has functionality to extract multiple test statistics from
#'a single permutation test. To perform permutation tests on multiple parameters,
#'input a vector of parameters. For example c("var1:var2","var1","var3").
#' @param Test_Statistic A Character string indicating the desired test statistic
#' to conduct the permutation test.This can be an f value, chi-squared value,
#'  p-value...etc, depending on what test statistics are available for your model
#'  object. These are different for lm, glm, glmmTMB, etc.
#' @param Data_Supplement The data frame that the model is built on.
#' This is necessary to supply for models of the class "lavaan" as there is not
#' functionality to extract the data frame from the model object.
#' @param Replication The number of simulations in the permutation test.
#' @param OutputData Should the simulated test statistics be outputted ?
#' @param Dependent_Variable This is a character vector that specifies the
#' dependent variable in a lavaan model. As a path analysis can have predictor variables
#' as the independent and dependent variable, one must specify the dependent variable here.
#' The independent variable is the character vector listed in "Test_Parameter"
#' @return A list of two items. The first is a data frame of results of the
#' permutation test. The second is a histogram displaying the sampling
#' distribution of the simulated test statistics, with a red line displaying the
#' test statistic of the original (non-randomized) data. If OutputData=T, then
#' a vector of simulated test statistics will also be included in the output list.
#' If multiple test parameters are supplied, then this output will be in a list for
#' each test parameter.
#' @export


permTest_Contrast<-function(Model_Object,Test_Parameter,Randomize_Variables,Test_Statistic,Replication,UseAllAvailableCores=TRUE,OutputData=FALSE,Data_Supplement=NULL,Dependent_Variable=NULL){

   #----------
  #Determining the method of model extraction to use, depending on the class of
  #the model object.

  if(class(Model_Object)[1]=="glmmTMB"){ #Class glmmTMB
    Model.Class<-"glmmTMB"

    model_extract_CallFunction<-function(Data.ME,...){
      model_extract3_GLMMTMB(Data.ME,...)
    }
  }


  else if(class(Model_Object)[1]=="lavaan"){ #Class lavaan
        Model.Class<-"lavaan"

        if(length(Test_Parameter)<2){#Single parameter inputted

                if(is.null(Dependent_Variable)){
                    model_extract_CallFunction<-function(Data.ME,...){
                    model_extract3_Lavaan(Data.ME,...)
                    }
                }

                else{
                    model_extract_CallFunction<-function(Data.ME,...){
                    model_extract3_Lavaan_Specific(Data.ME,...)
                    }
                }
        }

        else{ #Multiple paramter inputted.

            if(is.null(Dependent_Variable)){

                  model_extract_CallFunction<-function(Data.ME,...){
                  model_extract3_Lavaan_MultiParam(Data.ME,...)

                  }
              }

            else{

                  model_extract_CallFunction<-function(Data.ME,...){
                  model_extract3_Lavaan_MultiParam_Specific(Data.ME,...)

                  }
            }
        }
    }

  else{ #Class other : lm , glm, lmer, glmer.

    Model.Class<-"Other"

    model_extract_CallFunction<-function(Data.ME,...){
      model_extract3_General(Data.ME,...)
    }
  }



 #####
  #Preparing data for simulation and determining real test statistics
  #______________________________________________________


  #lavaan does not allow for model frames to be extracted, therefore the
  #user will need to input their data manually for this type of function.
  if(Model.Class!="lavaan"){

  #Obtaining the data frame including only the rows and columns used in the model.
  data2<- model.frame(Model_Object,drop.unused.levels=TRUE)

  #Refitting the model object with this minimal data frame.
  fit_True<-update(Model_Object,data=data2)
  }

  if(Model.Class=="lavaan"){

    #Obtaining the data frame including only the rows and columns used in the model.
    data2<- Data_Supplement

    #Refitting the model object with this minimal data frame.
    fit_True<-update(Model_Object,data=Data_Supplement)

    #Warning message to ensure that users input Data_Supplement for the class lavaann
    if(is.null(data2)){
      stop("lavaan models require a data frame as input (the Data_Supplement argument is needed).")
    }



  }


  #Determining the real test statistic
   tryCatch(
      {
      #attempting to get the test statistic from the model object.
      Real_TS<-model_extract_CallFunction(Data.ME=data2,Model_Object.ME=fit_True,Variable.ME=Test_Parameter,Test_Statistic.ME=Test_Statistic,Dependent_Variable.ME=Dependent_Variable)
      },
          #if there is an error, check to ensure that the test statistic matches what is available to the model object
          error=function(e){

            #Specififying the types of ceofficients available for the model class.
            if(Model.Class=="Other"){
              TS_Options<-colnames(summary(fit_True)[["coefficients"]])
            }
            if(Model.Class=="glmmTMB"){
              TS_Options<-colnames(summary(fit_True)[["coefficients"]]$cond)
            }
            if(Model.Class=="lavaan"){
              TS_Options<-c("se","z","pvalue")
            }

                  #Warning messages to use the appropriate test statistics available
                  #to the model class
                 if (Test_Statistic %in%  TS_Options ){
                    message("Error in model_extract_CallFunction. Ensure that the Test Parameter is available in summary(Model_Object)")
                 }

               else{ message(paste("Error: Test_Statistic",Test_Statistic," is not part of the model object output\nPlease pick one of:"))
                    print(TS_Options)
                    stop("Test_Statistic not valid")
               }

      }
   )

  #Ensuring that the correct format was inputted for lavaan classes
if(length(Real_TS)>length(Test_Parameter) ){
  stop("Multiple test statistics produced for single parameter,
  ensure that Test_Parameters are unique in summary tables. If
supplied class is lavaan, the Dependent_Variable must be specified.")
}


  #####
  #Performing simulation
  #______________________________________________________

  #Obtaining data frames with desired replication
  Data_Frames<-replicate(Replication,new_data2(data2,Column_Names=Randomize_Variables),simplify=F)


  if(UseAllAvailableCores==TRUE){# Modeling desired formula over each
    #data frame with a random permutation
    random_TS<-unlist(parallel::mclapply(Data_Frames,model_extract_CallFunction,Model_Object.ME=fit_True,Variable.ME=Test_Parameter,Test_Statistic.ME=Test_Statistic,Dependent_Variable.ME=Dependent_Variable))
  }

  else{ # Modeling desired formula over each data frame with a random permutation
    random_TS<-unlist(lapply(Data_Frames,model_extract_CallFunction,Model_Object.ME=fit_True,Variable.ME=Test_Parameter,Test_Statistic.ME=Test_Statistic,Dependent_Variable.ME=Dependent_Variable))
  }



  #####
  # Formulating output
  #______________________________________________________

  #For a permutation test of only 1 parameter:

  if( length(Test_Parameter)==1){
    out<-Format_Output(random_TS,Real_TS,OutputData,Test_Parameter)
  }

   else{
     #Creating a data frame out of the named vector.
     OutputDataFrame<-stack(random_TS)
     Real_TS_DataFrame<-stack(Real_TS)
     out<-list()
        #Generating results
        for(i in 1:length(Test_Parameter)){
          #Isolating the random TS results for the focal parameter
          random_TS<-OutputDataFrame$values[OutputDataFrame$ind==Test_Parameter[i]]

          #Isolating the real ts for the focal parameter
          Real_TS_Focal<-Real_TS_DataFrame$value[Real_TS_DataFrame$ind==Test_Parameter[i]]

          out[[i]]<-Format_Output(random_TS=random_TS,Real_TS=Real_TS_Focal,OutputData=OutputData,Test_Parameter=Test_Parameter[i])

        }
   }

  return(out)

}


Format_Output<-function(random_TS,Real_TS,OutputData,Test_Parameter){
#Obtaining p value
p_Val<-length(random_TS[abs(random_TS)>abs(Real_TS)])/length(random_TS)

#Creating a string with the p value
out_P<-paste("The simulated p-value value for test parameter",Test_Parameter,"is:",p_Val,sep=" ")
out_R<-paste("The real test statistic of the model is",Real_TS)
#Returning a histogram of z values
 p<-ggplot2::ggplot()+
   geom_histogram(aes(x=random_TS),bins = 50) +
   geom_vline(aes(xintercept=Real_TS),colour="red")+
   xlab(Test_Parameter)


if(OutputData==T){
  return(list(out_P,out_R,random_TS,p))
}

else return(list(out_P,p))

}





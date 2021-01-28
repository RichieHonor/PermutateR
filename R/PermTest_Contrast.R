#' Perform a permutation test on an interaction using the contrast specific in
#' the model output.
#'
#' This function performs a permutation test on a model object. The permutation
#' test is based on the test statistic from the summary of the model, which are
#' typically contrasts. One must specify the variables to be randomized, the test
#' parameter (i.e. the variable of interest), and the amount of replication to be
#' performed. This function works on lm, glm, lmer, glmer and glmmTMB classes.
#'
#' @param Model_Object A statistical model object.
#' @param Data The data that the model is built from.
#' @param Randomize_Variables A character vector indicating the variables that
#' are to be randomized in the permutation test
#'@param Test_Parameter A character string indicating the parameter that the
#'permutation test is to be performed on. For example, 'var1:var2'. If testing
#'the effect of a categorical variable or interaction, see the output of summary
#'of the model to determine what parameter the permutation test will be built on.
#' @param Test_Statistic A Character string indicating the desired test statistic
#' to conduct the permutation test.This can be an f value, chi-squared value,
#'  p-value...etc, depending on what test statistics are available for your model
#'  object. These are different for lm, glm, glmmTMB, etc.
#' using the 'anova' function and choose one of the output parameters.
#' @param Replication The number of simulations in the permutation test.
#' @param OutputData Should the simulated test statistics be outputted ?
#' @return A list of two items. The first is a data frame of results of the
#' permutation test. The second is a histogram displaying the sampling
#' distribution of the simulated test statistics, with a red line displaying the
#' test statistic of the original (non-randomized) data. If OutputData=T, then
#' a vector of simulated test statistics will also be included in the output list.
#' @export


permTest_Contrast<-function(Model_Object,Test_Parameter,Randomize_Variables,Test_Statistic,Replication,UseAllAvailableCores=TRUE,OutputData=FALSE){



  #####
  #Preparing data for simulation and determining real test statistics
  #______________________________________________________


  #Obtaining the data frame including only the rows and columns used in the model.
  data2<- model.frame(Model_Object,drop.unused.levels=TRUE)

  #Refitting the model object with this minimal data frame.
  fit_True<-update(Model_Object,data=data2)

  #Determining the method of model extraction to use, depending on the class of
  #the model object.

  if(class(Model_Object)[1]=="glmmTMB"){ #Class glmmTMB
    model_extract_CallFunction<-function(Data.ME,...){
      model_extract3_GLMMTMB(Data.ME,...)
    }
  }
  else{ #Class other : lm , glm, lmer, glmer.
    model_extract_CallFunction<-function(Data.ME,...){
      model_extract3_General(Data.ME,...)
    }

  }

  #Determining the real test statistic
  Real_TS<-model_extract_CallFunction(Data.ME=data2,Model_Object.ME=fit_True,Variable.ME=Test_Parameter,Test_Statistic.ME=Test_Statistic)




  #####
  #Performing simulation
  #______________________________________________________

  #Obtaining data frames with desired replication
  Data_Frames<-replicate(Replication,new_data2(data2,Column_Names=Randomize_Variables),simplify=F)


  if(UseAllAvailableCores==TRUE){# Modeling desired formula over each
    #data frame with a random permutation
    random_TS<-unlist(parallel::mclapply(Data_Frames,model_extract_CallFunction,Model_Object.ME=fit_True,Variable.ME=Test_Parameter,Test_Statistic.ME=Test_Statistic))
  }

  else{ # Modeling desired formula over each data frame with a random permutation
    random_TS<-unlist(lapply(Data_Frames,model_extract_CallFunction,Model_Object.ME=fit_True,Variable.ME=Test_Parameter,Test_Statistic.ME=Test_Statistic))
  }


  #####
  # Working the with simulated data
  #______________________________________________________


  #Obtaining p value
  p_Val<-length(random_TS[abs(random_TS)>abs(Real_TS)])/length(random_TS)

  out_P<-paste("The simulated p-value value is:",p_Val,sep=" ")#Creating a string to
  #put the p value

  #Returning a histogram of z values
  p<-ggplot2::ggplot()+
    geom_histogram(aes(x=random_TS),bins = 50) +
    geom_vline(aes(xintercept=Real_TS),colour="red")


  #####
  # Formulating output
  #______________________________________________________


  if(OutputData==T){
    return(list(out_P,p,random_TS))
  }
  else return(list(out_P,p))
}


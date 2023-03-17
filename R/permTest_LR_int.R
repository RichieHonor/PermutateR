#' Perform a permutation test on an interaction using the likelyhood ratio test
#'
#' This function performs a permutation test on an interaction in a model object
#' using a likelyhood ratio test. It does this by re-sampling the column(s)
#' indicated, and refitting two models to the new data: one with and one without
#' the interaction. The function then performs a likelyhood ratio test using the
#' anova function on these two models, creating a simulated permutation test
#' output. Because two models are needed to be created on each iteration to test
#' the interaction, this function is the slowest of the permutation tests.
#'
#' @param Model_Object A statistical model object.
#' @param Randomize_Variables A character vector of variables that are to be
#'  randomized in the permutation test. ex. c("var1","var2") or "var1".
#'@param Test_Parameter A character string indicating the parameter that the
#'permutation test is to be performed on.For example, 'var1:var2', or "var1"
#' @param Test_Statistic A character string indicating the desired test
#' statistic to conduct the permutation test. This can be an f value,
#' chi-squared value, p-value...etc, depending on what test statistics are
#' available for your model object. These are different for lm, glm, glmmTMB, etc.
#' @param Replication The number of simulations in the permutation test.
#' @param OutputData Should the simulated test statistics be outputted ?
#' @return A list of two items. The first is a data frame of results of the
#' permutation test. The second is a histogram displaying the sampling
#' distribution of the simulated test statistics, with a red line displaying the
#' test statistic of the original (non-randomized) data. If OutputData=T, then
#' a vector of simulated test statistics will also be included in the output list.
#' @export


permTest_LR_int<-function(Model_Object,Test_Parameter,Randomize_Variables,Test_Statistic,Replication,UseAllAvailableCores=TRUE,OutputData=FALSE){

  #Obtaining the data frame including only the rows and columns used in the model.
  data2<- model.frame(Model_Object,drop.unused.levels=TRUE)

  #Refitting the model object with this minimal data frame.
  fit_True<-update(Model_Object,data=data2)


  #Creating a new formula string for the update function.
  NewFormula<-as.formula(paste("~.-",Test_Parameter,sep=""))

  #Obtaining the null model without the variable for the likelyhood ratio test.
  #This is referred to as "fit_Null"
  fit_Null<-update(fit_True,NewFormula)

  #Determining the real test statistic
  AnovaOutput<-as.data.frame(anova(fit_Null,fit_True))

  Real_TS<-AnovaOutput[2,Test_Statistic]


  #Assessing that the test statistic output was correctly specified.
   if(is.null(Real_TS)){

       TS_Options<-colnames(AnovaOutput)

          if (Test_Statistic %in%  TS_Options ){
              message("Error: ensure that the Test Parameter is in the model object")
             }

           else{ message(paste("Error: Test_Statistic :",Test_Statistic," is not part of the model object output\nPlease pick one of:"))
               print(TS_Options)
               stop("Test_Statistic not valid")
           }
    }





  #Obtaining data frames with desired replication
  Data_Frames<-replicate(Replication,new_data2(data2,Column_Names=Randomize_Variables),simplify=F)






  ###Performing permutation test.




  ###Permutation Test
  ###-----

  if(UseAllAvailableCores==TRUE){# Modeling desired formula over each
    #data frame with a random permutation
    random_TS<-unlist(parallel::mclapply(Data_Frames,model_extract2,Model_Object.ME=fit_True,Test_Statistic.ME=Test_Statistic,Formula.ME=NewFormula))
  }

  else{ # Modeling desired formula over each data frame with a random permutation
    random_TS<-unlist(lapply(Data_Frames,model_extract2,Model_Object.ME=fit_True,Test_Statistic.ME=Test_Statistic,Formula.ME=NewFormula))
  }
  ###-----






  #Obtaining p value
  p_Val<-length(random_TS[abs(random_TS)>abs(Real_TS)])/length(random_TS)

  out_P<-paste("The simulated p-value value is:",p_Val,sep=" ")#Creating a string to
  #put the p value

  #Returning a histogram of z values
  p<-ggplot2::ggplot()+
    ggplot2::geom_histogram(aes(x=random_TS),bins = 50) +
    ggplot2::geom_vline(aes(xintercept=Real_TS),colour="red")

  if(OutputData==T){
    return(list(out_P,p,random_TS))
  }
  else return(list(out_P,p))
}




#' Perform a permutation test using the likelyhood ratio test
#'
#' This function performs a permutation test on a model object using a likelyhood
#' ratio test. It works by generating a specified amount of replicated
#' data frames and fits the supplied model object to each one. The permulation
#' test is based on the test statistic output of  a likelyhood ratio test.
#'  The null model is the Model_Object without the variable of interest. This
#'  function will not work on interactions, to run a permutation test on an
#'   interaction with a likelyhood ratio test, use permTest_LR_int().
#'
#' @param Model_Object A statistical model object.
#' @param Data The data that the model is built from.
#' @param Variable A character string of the variable requiring the permutation
#' test is run on.
#' @param Test_Statistic A character string of the desired test statistic to
#' conduct the permutation test.
#' @param Replication The number of simulations in the permutation test.
#' @param OutputData Should the simulated test statistics be outputted ?
#' @return A list of two items. The first is a data frame of results of the
#' permutation test. The second is a histogram displaying the sampling
#' distribution of the simulated test statistics, with a red line displaying the
#' test statistic of the original (non-randomized) data. If OutputData=T, then
#' a vector of simulated test statistics will also be included in the output list.
#' @export


permTest_LR<-function(Model_Object,Variable,Test_Statistic,Replication,UseAllAvailableCores=TRUE,OutputData=FALSE){



   #Obtaining the data frame including only the rows and columns used in the model.
   data2<- model.frame(Model_Object,drop.unused.levels=TRUE)


   #Refitting the model object with this minimal data frame.
   fit_True<-update(Model_Object,data=data2)


   #Creating a new formula string for the update function.
   NewFormula<-paste("~.-",Variable,sep="")

  #Obtaining the null model without the variable for the likelyhood ratio test.
   #This is referred to as "fit_Null"
  fit_Null<-update(fit_True,as.formula(NewFormula))



 #Determining the real test statistic
   Real_TS<-model_extract(Data.ME=data2,Model_Object.ME=fit_True,Null_Model.ME=fit_Null,Test_Statistic.ME=Test_Statistic)



   #Obtaining data frames with desired replication
   Data_Frames<-replicate(Replication,new_data(data2,Variable),simplify=F)



          if(UseAllAvailableCores==TRUE){# Modeling desired formula over each
             #data frame with a random permutation
          random_TS<-unlist(parallel::mclapply(Data_Frames,model_extract,Model_Object.ME=fit_True,Null_Model.ME=fit_Null,Test_Statistic.ME=Test_Statistic))
          }

          else{ # Modeling desired formula over each data frame with a random permutation
             random_TS<-unlist(lapply(Data_Frames,model_extract,Model_Object.ME=fit_True,Null_Model.ME=fit_Null,Test_Statistic.ME=Test_Statistic))
          }


   #Obtaining p value
    p_Val<-length(random_TS[abs(random_TS)>abs(Real_TS)])/length(random_TS)

    out_P<-paste("The simulated p-value value is:",p_Val,sep=" ")#Creating a string to
    #put the p value

    #Returning a histogram of z values
  p<-ggplot2::ggplot()+
      geom_histogram(aes(x=random_TS),bins = 50) +
     geom_vline(aes(xintercept=Real_TS),colour="red")

  if(OutputData==T){
    return(list(out_P,p,random_TS))
  }
  else return(list(out_P,p))
}





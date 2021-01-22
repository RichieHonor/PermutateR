#' Perform a permutation test
#'
#' This function performs a permutation test on a model object. It makes use of
#'  the new_data function to resample a variable in a data frame. Then, it
#'  generates a specified amount of replicated data frames and fits the supplied
#'  model to each one. The permulation test is based on the f values generated
#'  from a likelyhood ratio test. The null model is determined to be the
#'  Model_Object, updated to exclude the supplied variable.
#'
#' @param Model_Object A statistical model object (change name).
#' @param Data The data that the model is built from.
#' @param Variable The variable requiring the permutation test.
#' @param Replication The number of simulations in the permutation test.
#' @return A list of two items. The first is a data frame of results of the
#' permutation test. The second is a histogram displaying the sampling
#' distribution of the simulated test statistics, with a red line displaying the
#' test statistic of the original (non-randomized) data.
#' @export


perm_test_LR<-function(Model_Object,Data,Variable,Replication,UseAllAvailableCores=TRUE){


 if(is.character(substitute(Variable))){
    Variable<-str2lang(Variable)
    Variable<-deparse(substitute(Variable))
 }
else {

   Variable<-deparse(substitute(Variable))


}

   NewFormula<-paste("~.-",Variable,sep="")


 #Creating a formula in the form of a string to be supplied to the update function.

  print(NewFormula)

  #Obtaining the null model without the variable
  fit0<-update(Model_Object,as.formula(NewFormula))

#   #Determining the real z value
   Real_Z<-model_extract(Model_Object,fit0,Data=Data)
#
#   #Obtaining data frames with desired replication
   Data_Frames<-replicate(Replication,new_data(Data,Variable),simplify=F)



   if(UseAllAvailableCores==TRUE){

#   # Modeling desired formula over each data frame with a random permutation
   random_Z<-unlist(parallel::mclapply(Data_Frames,model_extract,Model_Object=Model_Object,Null_Model=fit0))
# #
   }

   else{

     #   # Modeling desired formula over each data frame with a random permutation
     random_Z<-unlist(lapply(Data_Frames,model_extract,Model_Object=Model_Object,Null_Model=fit0))
     # #
   }


#  #Obtaining p value
   p_Val<-length(random_Z[abs(random_Z)>abs(Real_Z)])/length(random_Z)
#
   out_P<-paste("The simulated p value is:",p_Val,sep=" ")#Creating a string to
   #put the p value

#   #Returning a histogram of z values
 p<-ggplot2::ggplot()+
     geom_histogram(aes(x=random_Z),bins = 50) +
    geom_vline(aes(xintercept=Real_Z),colour="red")
 #
   return(list(out_P,p))
}





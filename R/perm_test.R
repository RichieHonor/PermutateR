#' Perform a permutation test
#'
#' This function performs a permutation test on a model object. Given
#'  a model object, a data frame, and a variable. It makes use of
#'  the new_data function to resample a variable in a data frame. Then, it
#'  generates a specified amount of replicated data frames and fits the model to
#'  each one in order to perform a permutation test.
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


perm_test<-function(Model_Object,Data,Variable,Replication){

  #Obtaining name of Variable from environment
  Variable<-substitute(Variable)

  #Determining the real z value
  Real_Z<-model_extract(Model_Object,Data=Data)

  #Obtaining data frames with desired replication
  Data_Frames<-replicate(Replication,NewData(Data,Variable),simplify=F)
  #
  # Modeling desired formula over each data frame with a random permutation
  random_Z<-unlist(parallel::mclapply(Data_Frames,model_extract(),Data=Data,Model_Object=Model_Object))

 #Obtaining p value
  p_Val<-length(random_Z[abs(random_Z)>abs(Real_Z)])/length(random_Z)

  #Returning a histogram of z values
  p<-ggplot2::ggplot()+
    geom_histogram(aes(x=random_Z),bins = 50) +
    geom_vline(aes(xintercept=Real_Z),colour="red")

  return(list(Real_Z,p_Val,random_Z,p))
}





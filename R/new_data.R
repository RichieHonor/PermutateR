#' Resample a data frame
#'
#' This function resamples a column in a data frame. It first removes all NA's
#' from the column, the samples that column without replacement. The resampled
#' column then replaces the original column in the data frame.
#'
#' @param Data A data frame
#' @param Column_Name The name of the column within the data frame that is to be
#' resampled
#' @return The same data frame provided, but with the specified column
#' randomized.
#' @keywords internal
#' @export


new_data<-function(Data,Column_Name=NA){


  Random<-sample(Data[[Column_Name]],replace=F)#Sampling column
  Data[,Column_Name]<-Random#Assigning randomized column to data frame
  return(Data)
}

new_data2<-function(Data,Column_Names=NA){

  Random_Cols<-dplyr::select(Data,all_of(Column_Names)) #Columns that are randomized.
  Constant_Cols<-dplyr::select(Data,!all_of(Column_Names)) #Columns that are not randomized.

  Random_Cols<-apply(Random_Cols,2,sample) #Sampling only the columns specified to be randomized.

 outDat<-cbind(Constant_Cols,Random_Cols) #Rejoining the randomized columns with the non-randomized columns

  return(outDat)
}












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
#' @export


new_data<-function(Data,Column_Name=NA){ #Function to build a data frame with columns representing permutations of the given vector.

  #Making column name a string
  #Column_Name<-deparse(substitute(Column_Name))

  # stopping if column name not entered
  if(is.na(Column_Name)){
    stop("No column name provided, must be a character string")
  }

  #   #Removing Rows with NA's for the focal column.
  data2<- data[!is.na(data[[Column_Name]]),]

  #Sample column
  Random<-sample(data2[[Column_Name]],replace=F)
  #
  data2[,Column_Name]<-Random
  return(data2)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_extract.R
\name{model_extract}
\alias{model_extract}
\title{Extract Focal Parameter from Model}
\usage{
model_extract(Data.ME, Model_Object.ME, Null_Model.ME, Test_Statistic.ME, ...)
}
\arguments{
\item{Model_Object}{A statistical model object (change name)}

\item{Data}{The data that the model is built from.}
}
\value{
The desired test statistic
}
\description{
This function extracts the desired test statistic from a model object. This
test statistic will provide the foundation of the permutation test. During the
permutation test, only this parameter will be stored.
}
\keyword{internal}

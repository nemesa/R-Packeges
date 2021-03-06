% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PPBF.R
\name{PPBF.ParseDataFromRawPlateData}
\alias{PPBF.ParseDataFromRawPlateData}
\title{PPBF.ParseDataFromRawPlateData}
\usage{
PPBF.ParseDataFromRawPlateData(rowNumber, columnNumber, vectorWithRawData)
}
\arguments{
\item{rowNumber}{number of rows fo the plate}

\item{columnNumber}{number of the usefull data columns on the plate (first column is the name)}

\item{vectorWithRawData}{raw data separated with ','}
}
\value{
a DataFrame with the parsed data
}
\description{
PPBF.ParseDataFromRawPlateData
}
\examples{
PPBF.ParseDataFromRawPlateData(2,2,c("A,1,2","B,1,2"))
}

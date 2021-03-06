% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PPBF.R
\name{PPBF.Run}
\alias{PPBF.Run}
\title{PPBF.Run}
\usage{
PPBF.Run(filepath, printInfo = FALSE,
  numberOfRowsPrefixString = "Number of rows,,,,",
  numberOfColumnsPrefixString = "Number of columns,,,,",
  dataStartsAfterThisString = "Results for Meas A -  (RFU)")
}
\arguments{
\item{filepath}{The file path for the csv file (eg: c:/Work/PrestoBlue_Sample.csv)}

\item{printInfo}{default(FALSE) set this TRUE to display information messages during processing}

\item{numberOfRowsPrefixString}{default("Number of rows,,,,") a string which will be in the raw input file every char after this will be parsed to a number}

\item{numberOfColumnsPrefixString}{default("Number of columns,,,,") a string which will be in the raw input file every char after this will be parsed to a number}

\item{dataStartsAfterThisString}{default("Results for Meas A -  (RFU)") the funtion will read the raw data after this line}
}
\value{
a list(RowNum=(number of rows of the plate),ColNum=(number of coulmns of the plate),Data= (a DataFram with the data)
}
\description{
PPBF.Run
}
\examples{
parseResult <- PPBF.Run("c:/WORK/PrestoBlue_Sample.csv")

parseResult <- PPBF.Run("c:/WORK/PrestoBlue_Sample.csv",printInfo=TRUE)

rowNumber <- parseResult$RowNum
columnNumber <- parseResult$ColNum
dataFrame <- parseResult$Data
}

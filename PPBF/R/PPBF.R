#' PPBF.ParseDataFromRawPlateData
#'
#' @param rowNumber number of rows fo the plate
#' @param columnNumber number of the usefull data columns on the plate (first column is the name)
#' @param vectorWithRawData raw data separated with ','
#' @return a DataFrame with the parsed data
#' @examples
#' PPBF.ParseDataFromRawPlateData(2,2,c("A,1,2","B,1,2"))

PPBF.ParseDataFromRawPlateData<-function(rowNumber,columnNumber,vectorWithRawData){
  parsedData_DataFrame=data.frame(X00=c(0))
  
  #create empty dataframe columns
  for(columnIndex in 1:columnNumber){ 
    coumnName<-"X"
    if(columnIndex<10)
    {
      #if the actual column is in the first 10 than we add a '0' char to X, so at the end insted of 'X2' it will be 'X02'
      coumnName=paste(coumnName, "0", sep="") #>concat....
    }
    coumnName=paste(coumnName, columnIndex, sep="")
    parsedData_DataFrame[coumnName]=c(0)  
  }
  
  for(row in 1:rowNumber){
    #split the line by "," character
    parsedRow <- unlist(strsplit(vectorWithRawData[row], ",")) 
    for(column in 1:(columnNumber+1)){ 
      #set each column one by one in the result dataframe from the parsedRow
	    if(is.na(strtoi(parsedRow[column]))){
	      parsedData_DataFrame[row,column]<-parsedRow[column]
	    } else{
	      parsedData_DataFrame[row,column]<-as.numeric(parsedRow[column])
	    }      
    }
  }
  
  return(parsedData_DataFrame)
}

#' PPBF.Run
#'
#' @param filepath The file path for the csv file (eg: c:/Work/PrestoBlue_Sample.csv)
#' @param printInfo default(FALSE) set this TRUE to display information messages during processing
#' @param numberOfRowsPrefixString default("Number of rows,,,,") a string which will be in the raw input file every char after this will be parsed to a number
#' @param numberOfColumnsPrefixString default("Number of columns,,,,") a string which will be in the raw input file every char after this will be parsed to a number
#' @param dataStartsAfterThisString default("Results for Meas A -  (RFU)") the funtion will read the raw data after this line
#' @return a list(RowNum=(number of rows of the plate),ColNum=(number of coulmns of the plate),Data= (a DataFram with the data)
#' @examples
#' parseResult <- PPBF.Run("c:/WORK/PrestoBlue_Sample.csv")
#' 
#' parseResult <- PPBF.Run("c:/WORK/PrestoBlue_Sample.csv",printInfo=TRUE)
#' 
#' rowNumber <- parseResult$RowNum
#' columnNumber <- parseResult$ColNum
#' dataFrame <- parseResult$Data

PPBF.Run <- function(filepath
                                              , printInfo=FALSE
                                              , numberOfRowsPrefixString="Number of rows,,,,"
                                              , numberOfColumnsPrefixString="Number of columns,,,,"
                                              , dataStartsAfterThisString="Results for Meas A -  (RFU)") {
  
  fileConnection = file(filepath, "r") #open for read
  allLinesInFile=c()
  numberOfLinesInTheFile<-0
  #read all lines into the allLinesInFile vector
  while ( TRUE ) {
    line = readLines(fileConnection, n = 1) #read one line
    if ( length(line) == 0 ) { 
      break #exit if end of file
    }
    allLinesInFile[numberOfLinesInTheFile]<-line
    numberOfLinesInTheFile<-numberOfLinesInTheFile+1
  }
  #close file
  close(fileConnection) 
  
  if(printInfo){print("Line wich contains the number of rows:"); print(allLinesInFile[startsWith(allLinesInFile,numberOfRowsPrefixString)])}
  numberOfRows<-as.numeric(substring(allLinesInFile[startsWith(allLinesInFile,numberOfRowsPrefixString)],nchar(numberOfRowsPrefixString)+1))
  if(printInfo){print("Line wich contains the number of columns:"); print(allLinesInFile[startsWith(allLinesInFile,numberOfColumnsPrefixString)])}
  numberOfColumns<-as.numeric(substring(allLinesInFile[startsWith(allLinesInFile,numberOfColumnsPrefixString)],nchar(numberOfColumnsPrefixString)+1))
  #seach for the data in the file
  dataStartsAt<-(which(allLinesInFile == dataStartsAfterThisString)+2)
  if(printInfo){print("data starts at line:"); print(dataStartsAt)}
  indexesWhereTheDataIs<-dataStartsAt:(dataStartsAt+numberOfRows-1) #normaly this is 7:14 (from line 7 to line 14)
  
  if(printInfo){print("Raw data lines:"); print(allLinesInFile[indexesWhereTheDataIs])}
  
  if(printInfo){print("All Lines in the file:"); print(allLinesInFile)}
  
  result<-list(RowNum=numberOfRows,
			   ColNum=numberOfColumns,
			   Data=PPBF.ParseDataFromRawPlateData(numberOfRows,numberOfColumns,allLinesInFile[indexesWhereTheDataIs]))
  
  return(result)
}


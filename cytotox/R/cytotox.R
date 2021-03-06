cytotox.getSEM<-function(value){
  numberOfValues<-sum(!is.na(value)) #will count every not 'NA' in the vale parameter
  standardDeviation<-sd(value)
  return(standardDeviation/sqrt(numberOfValues))
}

cytotox.getPercentageByInterval <- function(value,intervalStart,intervalEnd) {
  # ha 20 a 100% es 10 a 0% akkor 5 az 50%
  # ugyan az mintha (20-10)= 10 a 100% es (15-10)=5 az 50%
  # tehat emiatt ((5-10)/(20-10))*100=50%
  if(is.vector(value)){
    result<-c()
    counter<-1
    for(v in value){
      result[counter]<-((v - intervalStart)/(intervalEnd - intervalStart))*100
      counter<-counter+1
    }
  }
  if(is.data.frame(value)){
    if(nrow(value)!=length(intervalStart)){
      stop(paste("error, DataFrame row count(",nrow(value),") not equals with intervalStart count(",length(intervalStart),")"))
    }
    if(nrow(value)!=length(intervalEnd)){
      stop(paste("error, DataFrame row count(",nrow(value),") not equals with intervalEnd count(",length(intervalEnd),")"))
    }
    
    result<-data.frame()
    rowCounter<-1
    for(row in 1:nrow(value))
    {
      colCounter<-1
      for(col in 1:ncol(value[,c((intervalStart*-1),(intervalEnd*-1))]))
      {
        result[rowCounter,colCounter]<-cytotox.getPercentageByInterval(value[row,col],intervalStart[rowCounter],intervalEnd[rowCounter])
        colCounter<-colCounter+1
      }
      rowCounter<-rowCounter+1
    }
  }
  return(result)
}

cytotox.DoCalculations <- function(values) {
  result<-data.frame(Mean=c(0),SD=c(0),SEM=c(0))
  
  if(is.vector(values)){
    result[1,]<-c(Mean=mean(values),SD=sd(values),SEM=cytotox.getSEM(values))
  }
  if(is.data.frame(values)){
    for(col in 1:ncol(values))
    {
      result[col,]<- cytotox.DoCalculations(values[,col])
    }
  }  
  return(result)
}


cytotox.ReadFiles <- function(input.files
                              ,input.cellLines
                              ,input.platePositionColumnIndex = 1){

  
  
  
  
  
  #res<-cytotox.ReadFiles(input.files=c("c:/_WORK_/R/Citotox/Input/cytotox1.csv"
  #                                     ,"c:/_WORK_/R/Citotox/Input/cytotox2.csv"
  #                                     ,"c:/_WORK_/R/Citotox/Input/cytotox3.csv")
  #                       ,input.cellLines=list(
  #                         list(Name="DT40 WT",PositionOnPate=c("B","C","D"))
  #                         ,list(Name="DT40 BRCA1",PositionOnPate=c("E","F","G"))))
  
  
  fileCounter<-1
  output<-list()    
  output$ByFile<-list()
  output$ByCellLine<-list()
  
  for(cellLineIterator in 1:length(input.cellLines)){
    output$ByCellLine[[cellLineIterator]]<-list()
  }
  
  columnCount<-0
  
  for(file in input.files){
    parsedData<-PPBF.Run(file)
    if(fileCounter==1){
      columnCount<-parsedData$ColNum
    }
    else
    {
      if(columnCount!=parsedData$ColNum){
        stop(paste("error at processing file:'",file,"' plate column is not matching with the first one (",columnCount,")"))
      }
    }

    output$ByFile[[fileCounter]]<-parsedData

    cellLineCounter<-1
    for(cellLineItem in input.cellLines)
    {
      data<-parsedData$Data[which(parsedData$Data[,input.platePositionColumnIndex] %in% cellLineItem$PositionOnPate),]
      output$ByCellLine[[cellLineCounter]][[fileCounter]]<-data
      cellLineCounter<-cellLineCounter+1
    }
    
    
    fileCounter<-fileCounter+1
  }
  return(output)
}

cytotox.Run<-function(input.files
                      ,input.cellLines
                      ,input.concentracions
                      ,input.platePositionColumnIndex = 1
                      ,input.backGroundColmunIndex = 2
                      ,input.controllColumnIndex = 12
                      ,input.pozitiveControllColumnIndex = 13
                      ,input.printInfo=TRUE
                      ,input.calcMode=1
){
  if(input.printInfo==TRUE){
    print("input.files")
    print(input.files)
    print("input.cellLines")
    print(input.cellLines)
    print("input.concentracions")
    print(input.concentracions)
    print("input.platePositionColumnIndex")
    print(input.platePositionColumnIndex)
    print("input.backGroundColmunIndex")
    print(input.backGroundColmunIndex)
    print("input.controllColumnIndex")
    print(input.controllColumnIndex)
    print("input.pozitiveControllColumnIndex")
    print(input.pozitiveControllColumnIndex)
    print("input.calcMode")
    print(input.calcMode)
  }
  output<-list() 
  
  rawData<-cytotox.ReadFiles(input.files,input.cellLines,input.platePositionColumnIndex)
  
  output$ParsedData<-rawData$ByFile
  output$ParsedDataByCellLine<-rawData$ByCellLine
  
  cellLineRawData <- list()
  if(input.calcMode==1)
  {
    if(input.printInfo==TRUE){print("Do calculation mode 1")}
    cellLineCounter<-1
    for (cellLine in rawData$ByCellLine)
    {
      dataCounter<-1
      newData<-data.frame()
      for(data in cellLine){
      
        for(column in 1:ncol(data)){
          if(column==1){
            newData[dataCounter,column]=dataCounter
          }
          else
          {
            newData[dataCounter,column]=mean(data[,column])
          }
        }
      
        dataCounter<-dataCounter+1  
      }
    
      cellLineRawData[[cellLineCounter]]<-newData
      cellLineCounter<- cellLineCounter+1
    }
  }
  
  if(input.calcMode==2)
  {
    if(input.printInfo==TRUE){print("Do calculation mode 2")}
    cellLineCounter<-1
    for (cellLine in rawData$ByCellLine){
      rowCounter<-1
      newData<-data.frame()
      for(data in cellLine){
        for(row in 1:nrow(data)){
          for(column in 1:ncol(data)){
            newData[rowCounter,column]=data[row,column]
          }
          rowCounter<-rowCounter+1
        }
      }
      cellLineRawData[[cellLineCounter]]<-newData
      cellLineCounter<- cellLineCounter+1
    }
  }
  
  output$CellLineRawData<-cellLineRawData
  cellLineCounter<-1
  cellLineCalculateDoseData <- list()
  cellLineCalculateControllData <- list()
  cellLineCalculatePozitiveControllData <- list()
  for(cellLine in cellLineRawData){
    
    cellLineCalculateControllData[[cellLineCounter]]<-cytotox.DoCalculations(cellLine[,input.controllColumnIndex])
    cellLineCalculatePozitiveControllData[[cellLineCounter]]<-cytotox.DoCalculations(cellLine[,input.pozitiveControllColumnIndex])
    
    notDoseDataColumnIndexes =c(input.platePositionColumnIndex,input.backGroundColmunIndex,input.controllColumnIndex,input.pozitiveControllColumnIndex)*-1
    cellLineCalculateDoseData[[cellLineCounter]]<-cytotox.DoCalculations(cellLine[,notDoseDataColumnIndexes])
    
    cellLineCounter<-cellLineCounter+1
  }
  
  output$CellLineCalculateDoseData<-cellLineCalculateDoseData
  output$CellLineCalculateControllData<-cellLineCalculateControllData
  output$CellLineCalculatePozitiveControllData<-cellLineCalculatePozitiveControllData
  
  if(input.printInfo){
    print("Calculate Mean, SD and SEM by cell line:");
    for(i in 1:length(input.cellLines)){
      print(input.cellLines[[i]]$Name);
      print("PozitiveControll data");print(cellLineCalculatePozitiveControllData[[i]])
      print("Dose data");print(cellLineCalculateDoseData[[i]])
      print("Controll data");print(cellLineCalculateControllData[[i]])
      if(i!=length(input.cellLines)){
        print("-----------------------------------")  
      }
    }
  }
  
  
  cellLineNormalizedDoseData <- cellLineCalculateDoseData
  cellLineNormalizedControllData <- cellLineCalculateControllData
  #normalize Means (change to percentages, the pozitive controll will be 0% and the controll will be 100%), SD and SEM
  for(cellLineIdex in 1:length(input.cellLines))
  {
    iStart<-cellLineCalculatePozitiveControllData[[cellLineIdex]]$Mean
    iEnd<-cellLineCalculateControllData[[cellLineIdex]]$Mean
    normalizationFactor<-100/(iEnd-iStart)
    
    cellLineNormalizedControllData[[cellLineIdex]]$Mean<-cytotox.getPercentageByInterval(value=cellLineCalculateControllData[[cellLineIdex]]$Mean
                                                                                         ,intervalStart=iStart
                                                                                         ,intervalEnd=iEnd)
    
    cellLineNormalizedDoseData[[cellLineIdex]]$Mean<-cytotox.getPercentageByInterval(value=cellLineCalculateDoseData[[cellLineIdex]]$Mean
                                                                                     ,intervalStart=iStart
                                                                                     ,intervalEnd=iEnd)
    
    
    cellLineNormalizedControllData[[cellLineIdex]]$SD<-cellLineCalculateControllData[[cellLineIdex]]$SD*normalizationFactor
    cellLineNormalizedDoseData[[cellLineIdex]]$SD<-cellLineCalculateDoseData[[cellLineIdex]]$SD*normalizationFactor
    
    cellLineNormalizedControllData[[cellLineIdex]]$SEM<-cellLineCalculateControllData[[cellLineIdex]]$SEM*normalizationFactor
    cellLineNormalizedDoseData[[cellLineIdex]]$SEM<-cellLineCalculateDoseData[[cellLineIdex]]$SEM*normalizationFactor
  }
  output$CellLineNormalizedControllData<-cellLineNormalizedControllData
  output$CellLineNormalizedDoseData<-cellLineNormalizedDoseData
  
  if(input.printInfo){
    print("Normalize Data Mean, SD and SEM by cell line:");
    for(i in 1:length(input.cellLines)){
      print(input.cellLines[[i]]$Name);
      print("Dose data");print(cellLineNormalizedDoseData[[i]])
      print("Controll data");print(cellLineNormalizedControllData[[i]])
      if(i!=length(input.cellLines)){
        print("-----------------------------------")  
      }
    }
  }
  
  cellLineModelData<-list()
  for(cellLineIdex in 1:length(input.cellLines))
  {
    cellLineModelData[[cellLineIdex]]<-data.frame(
      Means=c(cellLineNormalizedDoseData[[cellLineIdex]]$Mean,cellLineNormalizedControllData[[cellLineIdex]]$Mean)
      ,StandardDeviations=c(cellLineNormalizedDoseData[[cellLineIdex]]$SD,cellLineNormalizedControllData[[cellLineIdex]]$SD)
      ,StandardError=c(cellLineNormalizedDoseData[[cellLineIdex]]$SEM,cellLineNormalizedControllData[[cellLineIdex]]$SEM)
      ,Concentration=input.concentracions
      ,ConcentrationLog10 =log10(input.concentracions)
    )
  }
  output$CellLineModelData<-cellLineModelData
  if(input.printInfo){
    print("Create Model by cell line:");
    for(i in 1:length(input.cellLines)){
      print(input.cellLines[[i]]$Name);
      print(cellLineModelData[[i]])
      if(i!=length(input.cellLines)){
        print("-----------------------------------")  
      }
    }
  }
  
  nonlinearLeastSquaresModels<-list()
  cellLineCounter<-1
  for(cellLineModelDataItem in cellLineModelData){
    #model: y ~ a * x/(b + x)
    nonlinearLeastSquares<-nls(Means ~ SSlogis(ConcentrationLog10, Asym, xmid, scal), cellLineModelDataItem)
    nonlinearLeastSquaresModels[[cellLineCounter]]<-nonlinearLeastSquares
    if(input.printInfo){print("Summary of Nonlinear Least Squares:");print(summary(nonlinearLeastSquares))}
    
    cellLineCounter<-cellLineCounter+1
  }
  output$NonlinearLeastSquaresModels<-nonlinearLeastSquaresModels
  
  if(length(input.cellLines)>1){
    
    myPlot <-ggplot(cellLineModelData[[1]], aes(x = ConcentrationLog10, y = Means))+xlab("Contcentrations (nM)")+ylab("Means (%)")
    
    cellLineCounter<-1
    for(cellLine in input.cellLines){
      cellLineName<-cellLine$Name
      dots<-paste("geom_point(data=cellLineModelData[[",cellLineCounter,"]],aes(x=ConcentrationLog10,y=Means,color = '",cellLineName,"'))", sep="")
      myPlot <-myPlot+eval(parse(text=dots))
      errorBar<-paste("geom_errorbar(data=cellLineModelData[[",cellLineCounter,"]],aes(ymax = StandardError + Means, ymin=Means - StandardError,color = '",cellLineName,"' ),width=0.1)", sep="")
      myPlot <-myPlot+eval(parse(text=errorBar))
      line<-paste("geom_smooth(method = 'loess', data=cellLineModelData[[",cellLineCounter,"]],aes(x=ConcentrationLog10,y=predict(nonlinearLeastSquaresModels[[",cellLineCounter,"]]),color = '",cellLineName,"'), se = FALSE)", sep="")
      myPlot <-myPlot+eval(parse(text=line)) 
      
      cellLineCounter<-cellLineCounter+1
    }  
    myPlot <-myPlot+scale_colour_manual(name="", values = c("red","blue"))
    myPlot <-myPlot+
      geom_line(linetype = 2,aes(x=ConcentrationLog10,y=50),color = "black") 
    output$Plot<-myPlot
  }
  
  return(output)
  
  
}
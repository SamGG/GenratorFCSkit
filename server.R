library(shiny)
library(shinyjs)
library(FlowSOM)
library(flowCore)
library(DT)
library(markdown)
library(rhandsontable)
library(flowCore)
library(knitr)
library(shinyFiles)
library(JLutils)
library(flowAssist)
library(shinysky)
library(dplyr)
library(magrittr)
library(ggrepel)
library(Rtsne)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  options(shiny.maxRequestSize = 1024^10)

  output$Markers <-renderUI({
    ff = read.FCS((input$Files$datapath)[1])
    pd = pData(parameters(ff))
    channels = paste(pd$name, "<", pd$desc, ">", sep = "")
    if(!is.null(input$Files)){
      return(selectInput('markers','Selected Markers:',
                         choices =channels,
                         selected = channels,
                         multiple= TRUE,width="100%"))
    }
  })
    
  previous <- reactive({
    fs <- read.flowSet(input$Files$datapath)
    pop.name = input$Files$name
    pop.name <- stringr::str_replace_all(pop.name, ".fcs", "")
    pop.name
    pop.def = data.frame(id = seq(pop.name),
                         name = pop.name, stringsAsFactors = FALSE)
    pop.count = fsApply(fs, nrow)
    pop.freq = pop.count / sum(pop.count) * 100
    pop.def = cbind(pop.def, freq = as.numeric(pop.freq), freq.max = as.numeric(pop.freq))
    pop.def <- as.data.frame(pop.def)
    pop.def
  })
  
  
  Trigger_orders <- reactive({
    if(is.null(input$popdef)){return(previous())}
    else if(!identical(previous(),input$popdef)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$popdef))
    }
  })
  
  observeEvent(input$extractCSV,
               if(input$extractCSV== 1 && !is.null(input$Files)){
                 output$popdef <- renderHotable({Trigger_orders()}, readOnly = F)
               }
  )
  
  output$Pop.def.extract <- downloadHandler(
    filename = function() {
      paste("Pop.definition", ".xlsx", sep = "")
    },
    content = function(file) {
      write.table(Trigger_orders(), file,sep ="\t",row.names =FALSE)
    }
  )
  ####
  current <- reactive({
    if ((input$Transformation == "NO") && (input$Compensation == "NO")){ 
      fs <- read.flowSet(input$Files$datapath)  
      mrks = 1:length(input$markers)
      pop.def = Trigger_orders()
      # NbIter <- nrow(pop.def)
      # resultat <- rep(NA,NbIter)
      # for(i in 1:length(pop.def$freq)){
      #   f = pop.def$freq[i]
      #   if (sum(pop.def$freq) != 100 && sum(pop.def$freq.max) != 100){
      #     rest <- sum(pop.def$freq)
      #     f = f /rest * 100
      #   }
      #   resultat[i] <- f
      #   pop.def$freq <- resultat
      #   pop.def$freq.max <- resultat
      # }
      mode = "robust"
      pop.mfi = matrix(NA, nrow = length(fs), ncol = length(mrks))
      pop.sdfi = matrix(NA, nrow = nrow(pop.mfi), ncol = ncol(pop.mfi))
      for (i in 1:length(fs)) {
        matr = exprs(fs[[i]])
        for (j in mrks) {
          switch (mode, basic = {
            mfi = mean(matr[,j])
            sdfi = sd(matr[,j])
          }, robust = {
            mfi = median(matr[,j])
            sdfi = mad(matr[,j])
          }, truncgauss = {
            res = fit.truncated.gauss(matr[,j])
            mfi = res$mfi
            sdfi= res$sdfi
          }
          )
          pop.mfi[i,j] = mfi
          pop.sdfi[i,j] = sdfi
        }
      }
    }
    
    if (input$Transformation == "YES"){
      fs <- read.flowSet(input$Files$datapath)  
      fs_trans_arcsinh <- fsApply(fs,function(frame){
        
        asinh_scale <- input$cofactor
        exprs(frame)<- asinh(exprs(frame)/ asinh_scale)
        frame
      })
      fs <- fs_trans_arcsinh
      mrks = 1:length(input$markers)
      pop.def = Trigger_orders()
      # NbIter <- nrow(pop.def)
      # resultat <- rep(NA,NbIter)
      # for(i in 1:length(pop.def$freq)){
      #   f = pop.def$freq[i]
      #   if (sum(pop.def$freq) != 100 && sum(pop.def$freq.max) != 100){
      #     rest <- sum(pop.def$freq)
      #     f = f /rest * 100
      #   }
      #   resultat[i] <- f
      #   pop.def$freq <- resultat
      #   pop.def$freq.max <- resultat
      # }
      mode = "robust"
      pop.mfi = matrix(NA, nrow = length(fs), ncol = length(mrks))
      pop.sdfi = matrix(NA, nrow = nrow(pop.mfi), ncol = ncol(pop.mfi))
      for (i in 1:length(fs)) {
        matr = exprs(fs[[i]])
        for (j in mrks) {
          switch (mode, basic = {
            mfi = mean(matr[,j])
            sdfi = sd(matr[,j])
          }, robust = {
            mfi = median(matr[,j])
            sdfi = mad(matr[,j])
          }, truncgauss = {
            res = fit.truncated.gauss(matr[,j])
            mfi = res$mfi
            sdfi= res$sdfi
          }
          )
          pop.mfi[i,j] = mfi
          pop.sdfi[i,j] = sdfi
        }
      }
    }
    
    if (input$Compensation == "YES"){
      fs <- read.flowSet(input$Files$datapath)  
      
      fs <- fs_trans_arcsinh
      fs_compensate <- fsApply(fs,function(frame){
        Compensate_data <- function(fcsFile,comp = FALSE) {
          
          fcs <- read.FCS(fcsFile)
          ## compensation
          if(comp == FALSE){
            # cat(" No Compensation is applied on Dataset")
            return(fcs)
          }else if(comp == TRUE) {
            if(!is.null(fcs@description$SPILL)) {
              fcs <- applyComp(fcs, fcs@description[["SPILL"]])
              cat("    Compensation is applied on each Flowframe")
            }else if(!is.null(fcs@description$SPILLOVER)) {
              fcs <- applyComp(fcs, fcs@description[["SPILLOVER"]])
              cat("    Compensation is applied on each Flowframe ")
            }else if(!is.null(fcs@description$COMP)) {
              fcs <- applyComp(fcs, fcs@description[["COMP"]])
              cat("    Compensation is applied on each Flowframe")
            }else{
              cat("Cannot find compensation matrix in the FCS files!")
            }
            return(fcs)
          }
        }
        frame <- Compensate_data(frame@description[["FILENAME"]],comp=TRUE)
        frame
      })
      fs <- fs_compensate
      
      mrks = 1:length(input$markers)
      pop.def = Trigger_orders()
      # NbIter <- nrow(pop.def)
      # resultat <- rep(NA,NbIter)
      # for(i in 1:length(pop.def$freq)){
      #   f = pop.def$freq[i]
      #   if (sum(pop.def$freq) != 100 && sum(pop.def$freq.max) != 100){
      #     rest <- sum(pop.def$freq)
      #     f = f /rest * 100
      #   }
      #   resultat[i] <- f
      #   pop.def$freq <- resultat
      #   pop.def$freq.max <- resultat
      # }
      mode = "robust"
      pop.mfi = matrix(NA, nrow = length(fs), ncol = length(mrks))
      pop.sdfi = matrix(NA, nrow = nrow(pop.mfi), ncol = ncol(pop.mfi))
      for (i in 1:length(fs)) {
        matr = exprs(fs[[i]])
        for (j in mrks) {
          switch (mode, basic = {
            mfi = mean(matr[,j])
            sdfi = sd(matr[,j])
          }, robust = {
            mfi = median(matr[,j])
            sdfi = mad(matr[,j])
          }, truncgauss = {
            res = fit.truncated.gauss(matr[,j])
            mfi = res$mfi
            sdfi= res$sdfi
          }
          )
          pop.mfi[i,j] = mfi
          pop.sdfi[i,j] = sdfi
        }
      }
    }

    colnames(fs)[mrks] <- input$markers
    
    colnames(pop.mfi) = sprintf("%s.mfi", colnames(fs)[mrks])
    colnames(pop.sdfi) = sprintf("%s.sdfi", colnames(fs)[mrks])
    
    New.pop.def = cbind(pop.def, pop.mfi, pop.sdfi)
    New.pop.def
  })
  
  
  Trigger_orders1 <- reactive({
    if(is.null(input$Newpopdef)){return(current())}
    else if(!identical(current(),input$Newpopdef)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$Newpopdef))
    }
  })
  
  observeEvent(input$generatecsvfile,
               if(input$generatecsvfile== 1 && !is.null(input$Files)){
                 output$Newpopdef <- renderHotable({Trigger_orders1()},readOnly = F)
               }
  )
  
  
  output$Download1<-renderUI ({
    if(input$extractCSV !=0 && !is.null(input$Files)){
      return(downloadBttn(
        outputId = "Pop.def.extract",
        label = "Export Population frequencies",
        style = "bordered",
        color = "primary"
      ))
    }
  })
  
  output$Download2<-renderUI ({
    if(input$generatecsvfile!=0 && !is.null(input$Files)){
      return(downloadBttn(
        outputId = "Pop.extract",
        label = "Export CSV FILES",
        style = "bordered",
        color = "primary"
      ))
    }
  })
  
  output$Pop.extract <- downloadHandler(
    filename = function() {
      paste("Pop.definition", ".xlsx", sep = "")
    },
    content = function(file) {
      write.table(Trigger_orders1(), file,sep ="\t",row.names =FALSE)
    }
  )
  
  
  observeEvent(input$generatecsvfile,
               if(input$generatecsvfile== 1 && !is.null(input$Files)){
                 output$correlationmfi <- renderPlot({
                   pattern1 <- ".+mfi$"
                   col.mfi <- grep(pattern1, colnames(Trigger_orders1()), perl = TRUE, value = TRUE)
                   pop.def.mfi <- (Trigger_orders1()[ ,colnames(Trigger_orders1()) %in% col.mfi])
                   
                   #heatmap of all pop.def
                   pheatmap::pheatmap(pop.def.mfi,main ="Mfi of markers",cluster_row = FALSE,cluster_cols = FALSE)
                 })
               }
  )
  
  observeEvent(input$generatecsvfile,
               if(input$generatecsvfile== 1 && !is.null(input$Files)){
                 output$correlationsdfi <- renderPlot({
                   pattern1 <- ".+sdfi$"
                   col.sdfi <- grep(pattern1, colnames(Trigger_orders1()), perl = TRUE, value = TRUE)
                   pop.def.sdfi <- (Trigger_orders1()[ ,colnames(Trigger_orders1()) %in% col.sdfi])
                   
                   #heatmap of all pop.def
                   pheatmap::pheatmap(pop.def.sdfi,main ="Sdfi of markers",cluster_row = FALSE,cluster_cols = FALSE)
                 })
               }
  )

  
  
  # Create a reactive value rf2 to store the compensation matrix.
  Comp_matrix <- reactive({
    # if (is.null(input$Files)) NULL
    ## searching the compensation matrix for decompensation
    fs <- read.flowSet(input$Files$datapath)
    fs_compensate_mat <- fsApply(fs,function(fcs){
      comp<- NULL
      #fcs <- read.FCS(fcsFile)
      ## compensation
      if(is.null(fcs@description$SPILL) && is.null(fcs@description$SPILLOVER) && is.null(fcs@description$COMP)){
        comp <- NULL
        print(comp)
      }else if(!is.null(fcs@description$SPILL)) {
        comp <- fcs@description[["SPILL"]]
        print(comp)
      }
      else if(!is.null(fcs@description$SPILLOVER)) {
        comp <- fcs@description[["SPILLOVER"]]
        print(comp)
      }
      else if(!is.null(fcs@description$COMP)) {
        comp <- fcs@description[["COMP"]]
        print(comp)
      }
    })
    matrixComp <- as.character(fs_compensate_mat)
    matrixComp
    # # 1.b. Extract pop name
    # 
    # # we had to extract population name firstly define the population name by rename them to the name of the files wich correspond to the name of the population
    # 
    # pop.name = input$Files$name
    # pop.name <- stringr::str_replace_all(pop.name, ".fcs", "")
    # Comp_mat = data.frame(name = pop.name,
    #                       Compensation_Matrix = matrixComp,stringsAsFactors = FALSE)
    # # if(!is.null(input$Files))
    # #   isolate(
    # #     rf2 <<- Comp_mat
    # #   )
    # Comp_mat
  })
  
  output$Download3<-renderUI ({
    if(input$generatecsvfile!=0 && !is.null(input$Files)){
      return(downloadBttn(
        outputId = "Rdata",
        label = "Export R.data",
        style = "bordered",
        color = "primary"
      ))
    }
  })
  
  
  #Step2. Save the reactiveValue in the downloadHandler as you have done.
  # Download R.data
  output$Rdata <- downloadHandler(
    filename <- function(){
      "Rhistory.Rdata"
    },
    
    content = function(file) {
      save(Comp_matrix, file = file)
    }
  )

  
  # 3. generate
  
  # 3.a. generate copies
  
  # frequencies are generated for each replicate
  # total count is generated for each replicate

  
  output$csvmatrix <- renderDataTable(read.csv(input$FileGenerated$datapath), options = list(lengthChange = FALSE))
  
  Generate.copies <-
    function(matrix,n.replicates, n.events, n.eventmax){
    matr = NULL
    # index intensity
    idx.mfi = grep("\\.mfi$", colnames(matrix))
    idx.sdfi = grep("\\.sdfi$", colnames(matrix))
    # get frequencies
    p.freq = matrix$freq
    p.freq.max = matrix$freq.max
    
    # transform freq here
    for (i in seq(n.replicates)) {
      # pick up a total count
      if (is.null(n.eventmax)) event.count = n.events else
        event.count = round(runif(1, n.events, n.eventmax))
      # pick up frequencies
      pop.freqs = sapply(seq_len(nrow(matrix)), function(p) 
        runif(1, p.freq[p], p.freq.max[p]))
      # renormalize frequencies to 100%
      pop.freqs = pop.freqs / sum(pop.freqs) * 100
      # pick up count for each pop
      event.counts = round(event.count * pop.freqs / 100)
      # ajdust the sum of count to the chosen total count
      real.count = sum(event.counts)
      if (real.count != event.count) {
        idx = which.max(pop.freqs)
        event.counts[idx] = event.counts[idx] + (event.count - real.count)
      }
      # generate events
      for (p in seq_len(nrow(matrix))) {
        matr = rbind(matr, mvtnorm::rmvnorm(
          n = event.counts[p],
          mean = as.numeric(matrix[p,idx.mfi]),
          sigma = diag(matrix[p,idx.sdfi])
        ))
      }
      # append pop id column
      matr = cbind(matr, rep(matrix$id, times = event.counts))
      # randomize the event order
      matr = matr[sample.int(nrow(matr)),]
      # store
      return(matr)
    }
    }
  
  output$generatefcs<- downloadHandler(
    filename = function() {
      paste("ENRICHEDFILE", ".fcs", sep = "")
    },
    content = function(file) {
      
      matr <- Generate.copies(Trigger_orders1(),input$nbcopies, input$nbevents, input$nbevents)
      ##Matrix of parameterss with mfi according to pop frequencies define in pop.def
      #rename New matrix to assign each colnames to a corresponding Markers
      new_matr <- as.data.frame(matr)
      colnames(new_matr)[1:(ncol(matr)-1)] <- input$markers
      colnames(new_matr)[ncol(matr)]<- "Population"
      FCS.FILES <- flowAssist::DFtoFF(as.data.frame(new_matr))
      
      FCS.FILES@description <- 
        list(BEGINANALYSIS='0', #This field contains the byte-offset from the beginning of the data set to the beginning of the optional ANALYSIS segment. 
             #If there is no ANALYSIS segment, a '0' should be placed in this keyword value
             BEGINDATA= '0', #This field contains the byte-offset from the beginning of the data set to the beginning of the DATA segment
             BEGINSTEXT= '0',#This field contains the byte-offset from the beginning of the data set to the beginning of the supplemental TEXT segment
             BYTEORD='0', #This keyword specifies the order from numerically least significant[1] to numerically most significant[4] in which four binary data bytes are written to compose a 32-bit word in the data acquisition computer
             DATATYPE= 'I', #This keyword describes the type of data written in the DATA segment of the data set. The four allowed values are 'I', 'F', 'D', or 'A'. The DATA segment is a continuous bit stream with no delimiters. 'I' stands for unsigned binary integer
             ENDANALYSIS= '0', #This field contains the byte-offset from the beginning of the data set to the end of the ANALYSIS segment
             ENDDATA = '0', #This field contains the byte-offset from the beginning of the data set to the end of the DATA segment
             ENDSTEXT= '0', #This field contains the byte-offset from the beginning of the data set to the end of the supplemental TEXT segment
             MODE= "L", #This keyword specifies the mode in which the data were acquired
             NEXTDATA= '0', #When there is more than one data set in an FCS file, this keyword gives the byte offset from the beginning of a data set to the first byte in the HEADER of the next data set in the FCS file
             PAR= ncol(FCS.FILES),#This keyword specifies the total number of parameters stored in each event in the data set
             PnB= '0' ,#For $DATATYPE/I/(binary integers), this keyword specifies the number of bits allocated, n1, for storage of parameter n
             PnE= '0' ,#This keyword specifies whether linear or logarithmic amplifiers were used for parameter number n
             PnR= '1024', #This keyword specifies the maximum range, n1, of parameter n
             # For $MODE/L/ (list mode data), this corresponds to the ADC range, here 1024. The data values can range from 0 to 1023.
             TOT= nrow(FCS.FILES) #This keyword specifies the total number of events in the data set.
             #SPILL = matrixComp[1]
        )
      write.FCS(FCS.FILES, file)
    }
  )
  
  
  de.compensate <- function(flowframe, compmatrix)  #deCompensateFlowFrame
  {
    if(is.null(compmatrix))
    {  
      return(flowframe)
      cat("No compensation Matrix is found")
      
    }
    if(!is.null(compmatrix))
    {
      cols <- colnames(compmatrix)
      sel <- cols %in% colnames(flowframe@exprs)
      if(!all(sel)) {
        print(paste(keyword(flowframe)[["FILENAME"]], "\\nThe following parameters in the spillover matrix are not present in the flowFrame:\\n",
                    paste(cols[!sel], collapse=", ")))
      }
      exprs_mat <- exprs(flowframe)
      exprs_mat[, cols] <- exprs_mat[, cols] %*% spillover
      exprs(flowframe) = exprs_mat
      return(flowframe)
    } 
  }
  
  output$decompensatedfcs<- downloadHandler(
    filename = function() {
      paste("DecompensatedFiles", ".fcs", sep = "")
    },
    content = function(file) {
      matr <- Generate.copies(Trigger_orders1(),input$nbcopies, input$nbevents, input$nbevents)
      ##Matrix of parameterss with mfi according to pop frequencies define in pop.def
      #rename New matrix to assign each colnames to a corresponding Markers
      new_matr <- as.data.frame(matr)
      colnames(new_matr)[1:(ncol(matr)-1)] <- input$markers
      colnames(new_matr)[ncol(matr)]<- "Population"
      FCS.FILES <- flowAssist::DFtoFF(as.data.frame(new_matr))
      FCS.FILES@description <- 
        list(BEGINANALYSIS='0', #This field contains the byte-offset from the beginning of the data set to the beginning of the optional ANALYSIS segment. 
             #If there is no ANALYSIS segment, a '0' should be placed in this keyword value
             BEGINDATA= '0', #This field contains the byte-offset from the beginning of the data set to the beginning of the DATA segment
             BEGINSTEXT= '0',#This field contains the byte-offset from the beginning of the data set to the beginning of the supplemental TEXT segment
             BYTEORD='0', #This keyword specifies the order from numerically least significant[1] to numerically most significant[4] in which four binary data bytes are written to compose a 32-bit word in the data acquisition computer
             DATATYPE= 'I', #This keyword describes the type of data written in the DATA segment of the data set. The four allowed values are 'I', 'F', 'D', or 'A'. The DATA segment is a continuous bit stream with no delimiters. 'I' stands for unsigned binary integer
             ENDANALYSIS= '0', #This field contains the byte-offset from the beginning of the data set to the end of the ANALYSIS segment
             ENDDATA = '0', #This field contains the byte-offset from the beginning of the data set to the end of the DATA segment
             ENDSTEXT= '0', #This field contains the byte-offset from the beginning of the data set to the end of the supplemental TEXT segment
             MODE= "L", #This keyword specifies the mode in which the data were acquired
             NEXTDATA= '0', #When there is more than one data set in an FCS file, this keyword gives the byte offset from the beginning of a data set to the first byte in the HEADER of the next data set in the FCS file
             PAR= ncol(FCS.FILES),#This keyword specifies the total number of parameters stored in each event in the data set
             PnB= '0' ,#For $DATATYPE/I/(binary integers), this keyword specifies the number of bits allocated, n1, for storage of parameter n
             PnE= '0' ,#This keyword specifies whether linear or logarithmic amplifiers were used for parameter number n
             PnR= '1024', #This keyword specifies the maximum range, n1, of parameter n
             # For $MODE/L/ (list mode data), this corresponds to the ADC range, here 1024. The data values can range from 0 to 1023.
             TOT= nrow(FCS.FILES) #This keyword specifies the total number of events in the data set.
             #SPILL = load(input$Rdata$datapath)
        )
      
      compmatrix <- FCS.FILES@description[["SPILL"]]
      FCS.FILES <- de.compensate(FCS.FILES,compmatrix)
      
      write.FCS(FCS.FILES, file)
    }
  )
  
  
  
  detransform.asinh <- function(flowframe, channels, cofactor)
  {
    fcs <- flowframe
    fcs@exprs[,channels] <- sinh(fcs@exprs[,channels])*cofactor
    
    return(fcs)
  }
  
  output$detransformedfcs<- downloadHandler(
    filename = function() {
      paste("DetransformedFile", ".fcs", sep = "")
    },
    content = function(file) {
      
      matr <- Generate.copies(Trigger_orders1(),input$nbcopies, input$nbevents, input$nbevents)
      ##Matrix of parameterss with mfi according to pop frequencies define in pop.def
      #rename New matrix to assign each colnames to a corresponding Markers
      new_matr <- as.data.frame(matr)
      colnames(new_matr)[1:(ncol(matr)-1)] <- input$markers
      colnames(new_matr)[ncol(matr)]<- "Population"
      FCS.FILES <- flowAssist::DFtoFF(as.data.frame(new_matr))
      channels <- 1:(length(input$markers)-1) # or select markers # all the markers exept the Population can be choose because in the new fcs filecreate, the previous step of this analysis filters markers
      cofactor <- input$cofactor # for detransformation of mass cytometry data or between 150 and 250 for flow cytometry data
      FCS.FILES <- detransform.asinh(FCS.FILES, channels, cofactor)
      
      FCS.FILES@description <- 
        list(BEGINANALYSIS='0', #This field contains the byte-offset from the beginning of the data set to the beginning of the optional ANALYSIS segment. 
             #If there is no ANALYSIS segment, a '0' should be placed in this keyword value
             BEGINDATA= '0', #This field contains the byte-offset from the beginning of the data set to the beginning of the DATA segment
             BEGINSTEXT= '0',#This field contains the byte-offset from the beginning of the data set to the beginning of the supplemental TEXT segment
             BYTEORD='0', #This keyword specifies the order from numerically least significant[1] to numerically most significant[4] in which four binary data bytes are written to compose a 32-bit word in the data acquisition computer
             DATATYPE= 'I', #This keyword describes the type of data written in the DATA segment of the data set. The four allowed values are 'I', 'F', 'D', or 'A'. The DATA segment is a continuous bit stream with no delimiters. 'I' stands for unsigned binary integer
             ENDANALYSIS= '0', #This field contains the byte-offset from the beginning of the data set to the end of the ANALYSIS segment
             ENDDATA = '0', #This field contains the byte-offset from the beginning of the data set to the end of the DATA segment
             ENDSTEXT= '0', #This field contains the byte-offset from the beginning of the data set to the end of the supplemental TEXT segment
             MODE= "L", #This keyword specifies the mode in which the data were acquired
             NEXTDATA= '0', #When there is more than one data set in an FCS file, this keyword gives the byte offset from the beginning of a data set to the first byte in the HEADER of the next data set in the FCS file
             PAR= ncol(FCS.FILES),#This keyword specifies the total number of parameters stored in each event in the data set
             PnB= '0' ,#For $DATATYPE/I/(binary integers), this keyword specifies the number of bits allocated, n1, for storage of parameter n
             PnE= '0' ,#This keyword specifies whether linear or logarithmic amplifiers were used for parameter number n
             PnR= '1024', #This keyword specifies the maximum range, n1, of parameter n
             # For $MODE/L/ (list mode data), this corresponds to the ADC range, here 1024. The data values can range from 0 to 1023.
             TOT= nrow(FCS.FILES) #This keyword specifies the total number of events in the data set.
        )
      write.FCS(FCS.FILES, file)
    }
  )

})


  
  
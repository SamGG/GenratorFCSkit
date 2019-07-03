
library(shiny)
library(shinyjs)
library(FlowSOM)
library(flowCore)
library(DT)
library(markdown)
library(rhandsontable)
library(flowCore)
library(knitr)
library(shinythemes)
library(shinyFiles)
library(shinyWidgets)
library(ggcyto)
library(ggplot2)
library(JLutils)
library(plotly)
library(flowViz)
library(flowStats)
library(corrplot)
library(flowAssist)
library(shinysky)
library(shinybusy)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Import Files",
       fileInput(inputId = "Files",
                 label = "Select FCS Files",
                 multiple = TRUE,
                 accept = ".fcs"),
       fluidRow(
         uiOutput("Markers")
       ),
       fluidRow(
         column(1,(
           selectInput("Compensation","Compensate data",choices = c("YES","NO"),selected = "NO")
         ))
       ),
       fluidRow(
         column(1,(
           selectInput("Transformation","Transform data",choices = c("YES","NO"),selected= "NO")
         )),
         column(3,(
           numericInput('cofactor','Cofactor for arcsinh transformation',value=150,min =1, max= 100) 
         ))
       ),
       h3("1.Extract Population frequencies"),
       fluidRow(
       actionBttn(
         inputId = "extractCSV",
         label = "Extract Pop frequencies",
         color = "succes"
       )),
       hotable("popdef"),
       uiOutput("Download1"),
       h3("2.Extract Population Mfi and Sdfi"),
       fluidRow(
       actionBttn(
         inputId = "generatecsvfile",
         label = "Calculate MFI and sdFI",
         color = "succes"
       )),
       hotable("Newpopdef"),
       fluidRow(
         column(6,(
           plotOutput("correlationmfi")
         )),
         column(6,(
           plotOutput("correlationsdfi")
         ))
       ),
       uiOutput("Download2"),
       uiOutput("Download3")
    ),
    
    tabPanel("Generate FCS",
       fileInput(inputId = "FileGenerated",
                 label = "Select Csv files",
                 multiple = TRUE,
                 accept = ".csv"),
       dataTableOutput("csvmatrix"),
       fileInput(inputId = "Rdata",
                 label = "Enter R.data",
                 multiple = FALSE,
                 accept = ".Rhistory"),
       fluidRow(
         h3("Generate Copies")
       ),
       fluidRow(
         column(1,(
           numericInput('nbevents','Events',value=20000,min =1, max= 10000000)
         )),
         column(1,(
           numericInput('nbcopies','Copies',value=1,min =1, max= 100) 
         ))
       ),
       
       downloadBttn(
         outputId = "generatefcs",
         label = "Genrate FCS",
         style = "bordered",
         color = "primary"
       ),
       downloadBttn(
         outputId = "detransformedfcs",
         label = "Detransformed FCS",
         style = "bordered",
         color = "primary"
       ),
       downloadBttn(
         outputId = "decompensatedfcs",
         label = "Decompensated FCS",
         style = "bordered",
         color = "primary"
       )
    )
       
  )
)
)

  






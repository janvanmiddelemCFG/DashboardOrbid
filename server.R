#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library("RODBC")
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(data.table)
library(tidyr)
library(plotly)


server <- "CFGORBDAS1VP"
database<- "Imperial"
username <- "orbid"
password <- "1qnP2u0LIQFCm16mS317qq8H3h8GAkIXJKi5F8368az6uJVPnv"

connectionString <- paste0("DRIVER={SQL Server}; server=",server,"; database=",database,"; uid=",username,"; pwd=",password, sep="")
channel <-  odbcDriverConnect(connection=connectionString)



Orbid_view <- sqlFetch(channel, "VW_overzicht_Man_Dak_uren_Productie")
Orbid_view$WorkCenterCode <- trimws(Orbid_view$WorkCenterCode, "both")
PlantStructuur <- read_delim("B:/ISC/07_CE/04_BE/05_FC02/02_prod/02_PROJECTEN/55_Planning vs routing/PlantStructuur.csv", ";", trim_ws = TRUE)
Shifts <- read_delim("B:/ISC/07_CE/04_BE/05_FC02/02_prod/02_PROJECTEN/55_Planning vs routing/Shifts.csv", 
                     ";", escape_double = FALSE, col_types = cols(BeginShift = col_time(format = "%H:%M"), 
                                                                  EindShift = col_time(format = "%H:%M"), 
                                                                  Ploegwissel = col_time(format = "%H:%M")), 
                     trim_ws = TRUE)

planning <- select(Orbid_view,"PlantID","DepartmentID","WorkCenterCode","MaterialCode","ProdDate","Est_StartTime", "Est_EndTime")

### Afsplitsen producten voor roken naar nieuw workcenter 

roken <- c('1803522', '1803666','1803648','1803026','1802883')
planning$WorkCenterCode[planning$MaterialCode %in% roken]<- 'ATK2ROOK'

###


dataset <- merge(planning, PlantStructuur, by.x = "WorkCenterCode", by.y = "Work Center")
dataset <- select(dataset,"PlantID","DepartmentID","WorkCenterCode","MaterialCode","ProdDate","Est_StartTime", "Est_EndTime","Work Center2", "Labour")

colnames(dataset) <- c("PlantID","DepartmentID","WorkCenterCode","MaterialCode","ProdDate","Est_StartTime", "Est_EndTime","WorkCenterName", "Labour")

dataset$StartTime <- strptime(dataset$Est_StartTime, format= "%d/%m/%Y %H:%M:%S")
dataset$StopTime <- strptime(dataset$Est_EndTime, format= "%d/%m/%Y %H:%M:%S")
dataset$StartTime <- as.POSIXct(dataset$Est_StartTime, format= "%d/%m/%Y %H:%M:%S")
dataset$StopTime <- as.POSIXct(dataset$Est_EndTime, format= "%d/%m/%Y %H:%M:%S")
dataset$ProdDate <- as.POSIXct(dataset$ProdDate, format= "%Y-%m-%d")
dataset$DOW <- strftime(dataset$ProdDate, '%A') 





#server stuk
shinyServer(function(input, output, session) {
	
  session$onSessionEnded(function() {
        stopApp()})

  output$ui <- renderUI({
    if (is.null(input$Factory))
      return()
    
    switch(input$Factory,
           "FC01" = selectInput("sectie","Selecteer sectie:",c("IMKRUIDEN","IMPZOZOUT","IMVERPAK","IMWOAFVULLEN","IMWOCUTTER","IMWOROOK","IMZO-ONTVORMEN","IMZOSNIJ","IMZOVORMEN"),multiple = TRUE, selected = "IMKRUIDEN"),
           "FC02" = selectInput("sectie","Selecteer sectie:",c("CORAFTERTR","CORCUTTER","CORCUTTERVARK","CORKOKEN","CORKRUIDEN","CORSNACK","CORTROMMEL","CORVERSNIJDING","CORVULBUS","CORWOLFVARK"
                                    ),multiple = TRUE, selected = "CORAFTERTR"),
           "FC03" = selectInput("sectie","Selecteer sectie:",c("DAHAMINPOT","DAHAMKOKERIJ","DAHAMKRUID","DAHAMSPUIT","DAHAMUITBEEN","DAHAMVERPAK","DAHAMVERPAKHC","DAHUDIVERSEN","DAPACUTTER","DAPADEFROST","DAPAETIKET","DAPAHIGHRISK","DAPAKOOK","DAPAVULBUS","DAPEKELAANMAAK"
                                    ),multiple = TRUE, selected = "DAHAMINPOT"),
           "FC04" = selectInput("sectie","Selecteer sectie:",c("AMMIX","AMSLIKOOK","AMSLIZOUT"),multiple = TRUE, selected = "AMMIX"),
           "FC05" = selectInput("sectie","Selecteer sectie:",c("CH FINITION","CHBOURREUSE","CHEMBALLAGE","CHEPICES","CHFABRICATION","CHINOTEC","CHSALOIR"),multiple = TRUE, selected = "CH FINITION")
           )
  })
  
   
  output$distPlot <- renderPlot({
    
    plant = input$Factory
    department = input$sectie
    date_max_entry <- as.POSIXct(input$DatumRange[2], format = "%Y-%m-%d")-2*3600
    date_max <- date_max_entry
    date_min_entry <- as.POSIXct(input$DatumRange[1], format = "%Y-%m-%d")-2*3600
    date_min <- date_min_entry-24*3600

    dataset_gg <- filter(dataset, dataset$PlantID == plant, dataset$DepartmentID %in% department, dataset$ProdDate <= date_max & dataset$ProdDate >= date_min)

    
    shiftvis <- group_by(dataset_gg, dataset_gg$ProdDate)
    shiftvis <- summarize(shiftvis)
    colnames(shiftvis) <- c("ProdDate")
    shiftvis$ProdDate <- as.POSIXct(shiftvis$ProdDate, format= "%Y-%m-%d")
    shiftvis$DOW <- strftime(shiftvis$ProdDate, '%A') 
    shiftvis <- merge(shiftvis, Shifts, by = "DOW")
    
    shiftvis$BeginShift <- shiftvis$ProdDate + shiftvis$BeginShift
    shiftvis$Ploegwissel <- shiftvis$ProdDate + shiftvis$Ploegwissel
    shiftvis$EindShift <- shiftvis$ProdDate + shiftvis$EindShift
    
    planning <- dataset_gg
    
    planning$Duration <- as.double(difftime(planning$Est_EndTime, planning$Est_StartTime, units ='mins'))
    timings <- group_by(planning, planning$DepartmentID, planning$WorkCenterCode, planning$WorkCenterName)
    timings <- summarize(timings, Lijnuren = sum(Duration))
    timings$Lijnuren <- ceiling(timings$Lijnuren/6)/10
    colnames(timings) <- c("DepartmentID","WorkCenterCode","WorkCenterName","Lijnuren")
    timings$labels = paste(timings$WorkCenterName,"\n",as.character(timings$Lijnuren)," lijnuren")
    
    
    dataset_gg <- merge(dataset_gg, timings, by="WorkCenterCode", all = TRUE)
    dataset_gg <- select(dataset_gg, "PlantID","DepartmentID.x","WorkCenterCode","MaterialCode","ProdDate","Est_StartTime", "Est_EndTime","WorkCenterName.x","labels" )
    colnames(dataset_gg) <- c("PlantID","DepartmentID","WorkCenterCode","MaterialCode","ProdDate","Est_StartTime", "Est_EndTime","WorkCenterName","labels")
    
    plannedStop <- c('30601')
    dataset_gg$fill_colors <- "lightgrey"
    dataset_gg$fill_colors[dataset_gg$MaterialCode %in% plannedStop] <- 'darkorange'
    
    x_limits = c(date_min + 24*3600,date_max + 24*3600)
    
    ggplot()+
      geom_rect(data = shiftvis, aes(ymin=-0.25, ymax=1.25, xmin=BeginShift, xmax=Ploegwissel), color = "black", fill = "green", alpha=0.10)+
      geom_rect(data = shiftvis, aes(ymin=-0.25, ymax=1.25, xmin=Ploegwissel, xmax=EindShift), color = "black", fill = "green", alpha=0.10)+
      geom_rect(data = dataset_gg, aes(ymin=0, ymax=1, xmin=Est_StartTime, xmax=Est_EndTime), color = "black", fill = dataset_gg$fill_colors)+  
      facet_grid(labels~., switch="both")+ 
      theme_bw()+
      theme(panel.grid.major.x = element_line(colour = "black")) +
      theme(panel.grid.minor.x = element_line(colour = "grey")) +
      theme(strip.text.y = element_text(angle = 180))+ 
      theme(axis.text.x = element_text(angle =0, hjust = 0.5))+
      theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+
      scale_y_continuous(limits = c(-0.25,1.25), breaks = NULL)+
      scale_x_datetime(breaks = date_breaks("1 day"), minor_breaks=date_breaks("6 hours"),limits = x_limits, expand = c(0,0))
    
    
  })
  
})

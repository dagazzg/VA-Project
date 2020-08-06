###########################################################################################
#                                                                                         #
#                                 IMPORTING LIBRARIES                                     #
#                                                                                         #
###########################################################################################

library(plotly)
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(ggplot2)
library(ggridges)
library(dplyr)
library(magrittr)
library(plotly)
library(viridis)
library(RColorBrewer)
library(leaflet)
library(sp)
library(data.table)
library(rgeos)
library(raster)
library(rgdal)
library(GISTools)
library(xts)
library(tidyr)
library(colorRamps)

###########################################################################################
#                                                                                         #
#                                       SERVER CODE                                       #
#                                                                                         #
###########################################################################################
# Define server logic 
server <- function(input, output, session) {
  data <- fread('./data/Raw Consumption - EJ.csv')
  
  # Data Cleaning
  data <- transform(data, Consumption = as.numeric(Consumption))
  data$Consumption[is.na(data$Consumption)] <- 0
  data$Country[data$Country == "China Hong Kong SAR"] <- "Hong Kong"
  data$Country[data$Country == "Iran"] <- "Iran (Islamic Republic of)"
  data$Country[data$Country == "Russian Federation"] <- "Russia"
  data$Country[data$Country == "South Korea"] <- "Korea, Republic of"
  data$Country[data$Country == "US"] <- "United States"
  data$Country[data$Country == "Vietnam"] <- "Viet Nam"
  
  # Data Transformation
  data$Category <- ifelse(data$Type == "Hydroelectricity" | data$Type == "Solar" | data$Type == "Wind", "Renewable", "Nonrenewable" )
  map_data <- setDT(data)[,list(Consumption=sum(Consumption)), by = .(Country, Category), with = TRUE]
  map_data2 <- spread(map_data, Category, Consumption )
  map_data2$Total <- with(map_data2 , Renewable + Nonrenewable)
  map_data2$Proportion <- with(map_data2, Renewable / Total)
  
  
  # Load shape files
  glob_area <- readOGR('./data/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp')
  
  # Map countries to geospatial data
  shape.data <- glob_area@data
  shape.data$id  <- as.numeric(1:nrow(shape.data))
  names(shape.data)[names(shape.data) == "NAME"] <- "Country"
  new.shape.data  <- merge(shape.data, map_data2, by = "Country", all.x = TRUE)
  new.shape.data <- new.shape.data[order(new.shape.data$id,decreasing = FALSE),]
  #new.shape.data$Consumption[is.na(new.shape.data$Consumption)] <- 0
  glob_area@data <- new.shape.data
  
  #mypalette <- colorQuantile(palette="Reds", domain=map_data$Proportion, na.color="black", n = 10)
  mypalette <- colorBin(palette=colorRamps::green2red(5), domain=glob_area@data$Proportion, na.color="darkgrey", 8, reverse = TRUE)
  
  # Render Choropleth map
  output$choroplethMap <- renderLeaflet({
    leaflet(glob_area) %>%
      addMapPane(name = "maplabels", zIndex = 420) %>% 
      addTiles() %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = leafletOptions(pane = "maplabels")) %>%
      addPolygons(
        fillColor = ~mypalette(glob_area@data$Proportion),
        weight = 2,
        opacity = 1,
        fillOpacity = 0.8,
        color = 'grey'
      ) %>%
      leaflet::addLegend(pal = mypalette, values = glob_area@data$Proportion, title = "Proportion of renewable energy", position = "bottomleft")
      
  })
  
  consump_data <- fread('./data/Raw Consumption - EJ.csv')

  consumpToDate <- consump_data %>%
    filter(Year <= 2019)
  
  cleanConsumpToDate <- consump_data %>%
    filter(Year <= 2019 & Classification == "Renewable")
  
  TradConsumpToDate <- consump_data %>%
    filter(Year <= 2019 & Classification == "Traditional")
  
  NuclConsumpToDate <- consump_data %>%
    filter(Year <= 2019 & Classification == "Nuclear")
  
  output$totalEnergy <- renderInfoBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear)
    valueBox(value = paste0(ceiling(sum(consumpToDate$Consumption)), " EJ"), 
             subtitle = "Global Consumption",
             color = "yellow", icon = icon("bolt"), size = "tiny"
    )
  })
  
  output$cleanProp <- renderInfoBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear)
    cleanConsumpToDate <- subset(cleanConsumpToDate, Year == input$MaxYear)
    valueBox(value = paste0(round((sum(cleanConsumpToDate$Consumption)/sum(consumpToDate$Consumption)*100),digits = 1),"%"), 
             subtitle = "from renewable sources",
             color = "green", icon = icon("leaf"), size = "tiny"
             
             
    )
    
  })
  
  output$tradProp <- renderInfoBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear)
    TradConsumpToDate <- subset(TradConsumpToDate, Year == input$MaxYear)
    valueBox(value = paste0(round((sum(TradConsumpToDate$Consumption)/sum(consumpToDate$Consumption)*100),digits = 1),"%"), 
             subtitle = "from fossil fuels",
             color = "red", icon = shiny::icon("gas-pump"), size = "tiny"
             
             
    )
    
  })
  
  output$nuclProp <- renderInfoBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear)
    NuclConsumpToDate <- subset(NuclConsumpToDate, Year == input$MaxYear)
    valueBox(value = paste0(round((sum(NuclConsumpToDate$Consumption)/sum(consumpToDate$Consumption)*100),digits = 1),"%"), 
             subtitle = "from nuclear",
             color = "lime", icon = shiny::icon("radiation-alt", lib = "font-awesome"), size = "tiny"
             
    )
    
  })

}
###########################################################################################
#                                                                                         #
#                                 IMPORTING LIBRARIES                                     #
#                                                                                         #
###########################################################################################

library(plotly)
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
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
library(tidyr)
library(colorRamps)

###########################################################################################
#                                                                                         #
#                                       SERVER CODE                                       #
#                                                                                         #
###########################################################################################
# Define server logic 
server <- function(input, output, session) {
  rawdata <- fread('./data/Raw Consumption - EJ.csv')
  mapCountry <- 'Algeria'
  options(scipen=999)
  
  # Data Cleaning
  rawdata <- transform(rawdata, Consumption = as.numeric(Consumption))
  rawdata$Consumption[is.na(rawdata$Consumption)] <- 0
  rawdata$Country[rawdata$Country == "China Hong Kong SAR"] <- "Hong Kong"
  rawdata$Country[rawdata$Country == "Iran"] <- "Iran (Islamic Republic of)"
  rawdata$Country[rawdata$Country == "Russian Federation"] <- "Russia"
  rawdata$Country[rawdata$Country == "South Korea"] <- "Korea, Republic of"
  rawdata$Country[rawdata$Country == "US"] <- "United States"
  rawdata$Country[rawdata$Country == "Vietnam"] <- "Viet Nam"
  rawdata$Category <- ifelse(rawdata$Type == "Hydroelectricity" | rawdata$Type == "Solar" | rawdata$Type == "Wind", "Renewable", "Nonrenewable" )
  
  renderChoropleth <- reactive({
    print(input$MaxYear)
    data <- subset(rawdata, Year == input$MaxYear)
    
    # Data Transformation
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
    new.shape.data <- merge(shape.data, map_data2, by = "Country", all.x = TRUE)
    new.shape.data <- new.shape.data[order(new.shape.data$id,decreasing = FALSE),]
    #new.shape.data$Consumption[is.na(new.shape.data$Consumption)] <- 0
    glob_area@data <- new.shape.data
    
    #mypalette <- colorQuantile(palette="Reds", domain=map_data$Proportion, na.color="black", n = 10)
    mypalette <- colorBin(palette=colorRamps::green2red(5), domain=glob_area@data$Proportion, na.color="darkgrey", 8, reverse = TRUE)
    
    #popups <- ()
    
    labels <- paste(
      glob_area@data$Country, "<br />",
      "Proportion of energy that is renewable: ", round(glob_area@data$Proportion * 100, 4), "%","<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Render Choropleth map
    choroplethMap <- leaflet(glob_area) %>%
        addMapPane(name = "maplabels", zIndex = 420) %>% 
        addTiles() %>%
        setView(lat = 0, lng = 0, zoom = 1.5) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels", 
                         options = leafletOptions(pane = "maplabels")) %>%
        addPolygons(
          layerId = glob_area@data$Country,
          fillColor = ~mypalette(glob_area@data$Proportion),
          weight = 2,
          opacity = 1,
          fillOpacity = 0.8,
          color = 'grey',
          label = labels
          #popup = popups
        ) %>%
        leaflet::addLegend(pal = mypalette, values = glob_area@data$Proportion, title = "Proportion of renewable energy", position = "bottomleft")
    return (choroplethMap)
  })
  
  output$choroplethMap <- renderLeaflet(renderChoropleth())
  
  # ======================= #
  #    floating pie chart   # filter and title by selected country, algeria is hardcoded now
  # ======================= #
  renderDonut <- function(country){
    data <- subset(rawdata, Year == input$MaxYear)
    donutChart<- data %>%
      filter(Country == country) %>% select(Type, Consumption)%>%
      plot_ly(labels = ~Type, 
              values = ~Consumption,
              sort = FALSE
              ) %>%
      add_pie(hole = 0.5) %>%
      layout(title = list(
              text = paste(country, "Energy Mix")
             ),
             showlegend = TRUE,
             plot_bgcolor = '#aad3df',
             paper_bgcolor= '#aad3df',
             images = list(
               list(source = base64enc::dataURI(file = paste("./data/Flags/", tolower(country), ".png", sep = "")),
                    xref = "paper", yref = "paper",
                    x = 0.275, y = 0.72,
                    sizex = 0.45, sizey = 0.45,
                    opacity = 1
                    )
               )
             )
    
    return (donutChart)
  }
  output$donutChart <- renderPlotly(renderDonut("Algeria"))
  
  observeEvent(input$choroplethMap_shape_click, {
    event <- input$choroplethMap_shape_click
    mapCountry <- event$id
    output$donutChart <- renderPlotly(renderDonut(mapCountry))
  })
  
  
  # Keystats
  consump_data <- rawdata
  consumpToDate <- consump_data %>%
    filter(Year <= 2019)
  
  cleanConsumpToDate <- consump_data %>%
    filter(Year <= 2019 & Classification == "Renewable")
  
  TradConsumpToDate <- consump_data %>%
    filter(Year <= 2019 & Classification == "Traditional")
  
  NuclConsumpToDate <- consump_data %>%
    filter(Year <= 2019 & Classification == "Nuclear")
  
  output$totalEnergy <- renderValueBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear)
    box <- valueBox(value = paste0(ceiling(sum(consumpToDate$Consumption)), " EJ"), 
             subtitle = "Global Consumption",
             color = "yellow", icon = icon("bolt"), size = "small"
    )
    return (box)
  })
  
  output$cleanProp <- renderValueBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear) 
    cleanConsumpToDate <- subset(cleanConsumpToDate, Year == input$MaxYear)
    return(valueBox(value = paste0(round((sum(cleanConsumpToDate$Consumption)/sum(consumpToDate$Consumption)*100),digits = 1),"%"), 
             subtitle = "from renewable sources",
             color = "green", icon = icon("leaf"), size = "small"
    ))
  })
  
  output$tradProp <- renderValueBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear)
    TradConsumpToDate <- subset(TradConsumpToDate, Year == input$MaxYear)
    valueBox(value = paste0(round((sum(TradConsumpToDate$Consumption)/sum(consumpToDate$Consumption)*100),digits = 1),"%"), 
             subtitle = "from fossil fuels",
             color = "red", icon = shiny::icon("gas-pump"), size = "small"
    )
  })
 
  output$nuclProp <- renderValueBox({
    consumpToDate <- subset(consump_data, Year == input$MaxYear)
    NuclConsumpToDate <- subset(NuclConsumpToDate, Year == input$MaxYear)
    valueBox(value = paste0(round((sum(NuclConsumpToDate$Consumption)/sum(consumpToDate$Consumption)*100),digits = 1),"%"), 
             subtitle = "from nuclear",
             color = "teal", icon = shiny::icon("radiation-alt", lib = "font-awesome"), size = "small"
             
    )
  })
}


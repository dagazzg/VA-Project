###########################################################################################
#                                                                                         #
#                                 IMPORTING LIBRARIES                                     #
#                                                                                         #
###########################################################################################

require(plotly)
require(shiny)
require(shiny.semantic)
require(semantic.dashboard)
require(dplyr)
require(leaflet)
require(sp)
require(data.table)
require(colorRamps)
require(base64enc)
require(tidyr)
require(reactable)
require(tidyverse)
require(RColorBrewer)
require(rgdal)

###########################################################################################
#                                                                                         #
#                                       SERVER CODE                                       #
#                                                                                         #
###########################################################################################
# Define server logic 
server <- function(input, output, session) {
  rawdata <- fread('./data/Raw Consumption - EJ.csv')
  mapCountry <- 'United States'
  options(scipen=999)
  colorNormalize <- function(x) rgb(colorRamp(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"))(x), maxColorValue = 255)
  
  # Data Cleaning
  rawdata <- transform(rawdata, Consumption = as.numeric(Consumption))
  rawdata$Consumption[is.na(rawdata$Consumption)] <- 0
  rawdata$Country[rawdata$Country == "China Hong Kong SAR"] <- "Hong Kong"
  rawdata$Country[rawdata$Country == "Iran"] <- "Iran (Islamic Republic of)"
  rawdata$Country[rawdata$Country == "Russian Federation"] <- "Russia"
  rawdata$Country[rawdata$Country == "South Korea"] <- "Korea, Republic of"
  rawdata$Country[rawdata$Country == "US"] <- "United States"
  rawdata$Country[rawdata$Country == "Vietnam"] <- "Viet Nam"
  rawdata$Category <- ifelse(rawdata$Type == "Hydroelectricity" | rawdata$Type == "Solar" | rawdata$Type == "Wind" | rawdata$Type == "Biofuels", "Renewable", "Nonrenewable" )
  
  # Choropleth map
  renderChoropleth <- reactive({
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
    
    mypalette <- colorBin(palette=rev(brewer.pal(6,"RdYlGn")), domain=glob_area@data$Proportion, na.color="darkgrey", 5, reverse = TRUE)
    
    labels <- paste(
      glob_area@data$Country, "<br />",
      "Proportion of renewable energy: ", round(glob_area@data$Proportion * 100, 4), "%","<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Render Choropleth map
    choroplethMap <- leaflet(glob_area, options = leafletOptions(zoomControl = FALSE)) %>%
        addMapPane(name = "maplabels", zIndex = 420) %>% 
        addTiles() %>%
        setView(lat = 0, lng = 0, zoom = 1.5) %>%
        addProviderTiles("CartoDB.PositronOnlyLabels", 
                         options = leafletOptions(pane = "maplabels")) %>%
        addPolygons(
          layerId = glob_area@data$Country,
          fillColor = ~mypalette(glob_area@data$Proportion),
          weight = 2,
          fillOpacity = 0.8,
          color = 'grey',
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"),
          stroke = TRUE
        ) %>%
        leaflet::addLegend(pal = mypalette, values = glob_area@data$Proportion, title = "Proportion of renewable fuels", position = "bottomleft")
    return (choroplethMap)
  })
  
  output$choroplethMap <- renderLeaflet(renderChoropleth())
  
  # ======================= #
  #    floating pie chart   #
  # ======================= #
  renderDonut <- function(country){
    Noax <- list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE
    )
    
    if (country == "Korea, Republic of") {
      country2 <- "South Korea"
    } else {country2 <- country}
    title_format <- list(text = paste(country2, input$MaxYear, "Fuel Mix"),
                         xanchor="left",
                         xref="container",
                         x = 0)
    
    margins <- list(t = 70, l = 40)
    
    data <- subset(rawdata, Year == input$MaxYear)
    data$Type <- as.factor(data$Type)
    colorPalette <- leaflet::colorFactor(palette = brewer.pal(9,"Set3"), domain = unique(data$Type))(unique(data$Type))
    
    donutData<- data %>%
      filter(Country == country) %>% dplyr::select(Type, Consumption)
    
    if (nrow(donutData) == 8) {
      colorPalette <- colorPalette[-1]
    }
    
    donutChart <- donutData %>%  
      plot_ly(labels = ~Type, 
              values = ~Consumption,
              sort = FALSE,
              marker = list(colors = colorPalette)
              ) %>%
      add_pie(hole = 0.5) %>%
      layout(title = title_format,
             margin = margins,
             xaxis = Noax,
             yaxis = Noax,
             showlegend = TRUE,
             plot_bgcolor = '#FFFFFF',
             paper_bgcolor= '#FFFFFF',
             images = list(
               list(source = base64enc::dataURI(file = paste("./data/Flags/", tolower(country), ".png", sep = "")),
                    xref = "paper", yref = "paper",
                    x = 0.3, y = 0.725,
                    sizex = 0.46, sizey = 0.45,
                    opacity = 1
                    )
               )
             )
    
    return (donutChart)
  }
  output$donutChart <- renderPlotly(renderDonut(mapCountry))
  
  observeEvent(input$choroplethMap_shape_click, {
    event <- input$choroplethMap_shape_click
    mapCountry <- event$id
    output$donutChart <- renderPlotly(renderDonut(mapCountry))
  })
  
  # Table
  output$myTable <- renderReactable(renderProportions())
  
  renderProportions <- reactive({
    renewableconsumption <- setDT(rawdata)[,list(Consumption=sum(Consumption)), by = .(Country, Category, Year), with = TRUE]
    renewableconsumption_2 <- renewableconsumption %>% 
      group_by(Country, Year, Category) %>% 
      # filter(Category == "Renewable") %>% 
      filter(Year %in% input$dateRange[1]:input$dateRange[2]) %>%
      spread(Category, Consumption) %>%
      summarise(renewableProportion = round(Renewable / (Nonrenewable + Renewable), 4)) %>% 
      spread(Year, renewableProportion)
    
    renewableconsumption_2[is.na(renewableconsumption_2)] <- 0
    renewableconsumption_2[, "Change"] <- renewableconsumption_2[, paste(input$dateRange[2])] - renewableconsumption_2[, paste(input$dateRange[1])]
    setcolorder(renewableconsumption_2, c("Country", "Change", input$dateRange[2]:input$dateRange[1]))
    
    yearList <-list(Country = colDef(name = "Country"),
                    Change = colDef(format = colFormat(percent = TRUE, digits = 2),
                                                      style = function(value) {
                                                        color <- if (value > 0) {
                                                          "#008000"
                                                        } else if (value < 0) {
                                                          "#e00000"
                                                        } else { "#000000" }
                                                        list(fontWeight = 600, color = color)
                                                      })
    )
    
    counter <- 3
    for (year in input$dateRange[2]:(input$dateRange[1])) {
      yearList[[toString(year)]] <- colDef(format = colFormat(percent = TRUE, digits = 2), style = function(value){
        color <- colorNormalize(value)
        return(list(background = color))
      })
      counter <- counter + 1
    }

    reactable(renewableconsumption_2,
              columns = yearList,
              theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#f0f5f9",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                searchInputStyle = list(width = "100%")
              ),
              minRows = 10,
              searchable = TRUE,
              highlight = TRUE,
              showSortable= TRUE,
              bordered = TRUE,
              striped = TRUE,
              resizable = TRUE
    )
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
             subtitle = "from non-renewable sources",
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
  ###############
  # country tab #
  ###############
  rawdata2 <- fread('./data/Raw Consumption - EJ.csv')
  emissionData <- read_csv("./data/emission.csv")
  yearList <- seq(2001,2019,1)
  filterData <- function() {
    
    # Filtering by country
    if (input$country == "(All)") {
      dataSet <- rawdata2
      country <- "all countries"
    } else {
      dataSet <- rawdata2 %>% filter(Country == input$country)
      country <- input$country
    }
    
    # Filtering by fuel type
    chosenFuels <- ""
    
    if (!input$isSumOil) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Oil"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Oil"
    } else {
      chosenFuels <- paste(chosenFuels, "Oil", sep= ", ")
    }
    
    if (!input$isSumGas) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Gas"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Gas"
    } else {
      chosenFuels <- paste(chosenFuels, "Gas", sep= ", ")
    }
    
    if (!input$isSumCoal) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Coal"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Coal"
    } else {
      chosenFuels <- paste(chosenFuels, "Coal", sep= ", ")
    }
    
    if (!input$isSumBiofuels) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Biofuels"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Biofuels"
    } else {
      chosenFuels <- paste(chosenFuels, "Biofuels", sep= ", ")
    }
    
    if (!input$isSumHydroelectricity) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Hydroelectricity"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Hydroelectricity"
    } else {
      chosenFuels <- paste(chosenFuels, "Hydroelectricity", sep= ", ")
    }
    
    if (!input$isSumNuclear) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Nuclear"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Nuclear"
    } else {
      chosenFuels <- paste(chosenFuels, "Nuclear", sep= ", ")
    }
    
    if (!input$isSumSolar) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Solar"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Solar"
    } else {
      chosenFuels <- paste(chosenFuels, "Solar", sep= ", ")
    }
    
    if (!input$isSumWind) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Wind"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Wind"
    } else {
      chosenFuels <- paste(chosenFuels, "Wind", sep= ", ")
    }
    
    if (!input$isSumOthers) {
      dataSet <- dataSet %>% 
        filter(!(Type == "Others"))
    } else if (chosenFuels == "") {
      chosenFuels <- "Others"
    } else {
      chosenFuels <- paste(chosenFuels, "Others", sep= ", ")
    }
    
    if (chosenFuels == "Oil, Gas, Coal, Biofuels, Hydroelectricity, Nuclear, Solar, Wind, Others") {
      chosenFuels <- "all fuels"
    }
    
    dataSet <- dataSet %>%
      group_by(Year, Type) %>%
      replace_na(list(Consumption = 0)) %>%
      summarise("totalConsumption" = sum(Consumption))
    
    return(list(country, chosenFuels, dataSet))
  }
  
  # Fuel consumption plot
  output$fuelPlot <- renderPlotly({
    
    # Filtering data
    filterResults <- filterData()
    country <- filterResults[1]
    chosenFuels <- filterResults[2]
    dataSet <- filterResults[[3]]
    
    if (input$levelOfDetail == "Cumulative") {
      if (chosenFuels == "") {
        fuelPlotTitle <- "No fuel type chosen."
        
        # Empty graph
        fuelPlot <- ggplot() +
          geom_col() +
          theme_classic() +
          labs(title = fuelPlotTitle, x="Year", y="Consumption (EJ)") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'right',
                plot.margin = unit(c(0,0,0,2), "cm")) +
          scale_x_continuous(breaks = seq(2000, 2019, 1))
      } else {
        fuelPlotTitle <- paste0("Consumption of ", chosenFuels, " for ", country, " over time")
        
        fuelPlot <- ggplot(data = dataSet, aes(x = Year, y = totalConsumption, fill = Type)) +
          geom_area() +
          theme_classic() +
          labs(title = fuelPlotTitle, x="Year", y="Consumption (EJ)") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'right',
                plot.margin = unit(c(0,0,0,2), "cm")) +
          scale_fill_brewer(palette = "Set3") +
          scale_x_continuous(breaks = seq(2000, 2019, 1))
      }
    } else if (input$levelOfDetail == "Discrete"){
      if (chosenFuels == "") {
        fuelPlotTitle <- "No fuel type chosen."
        # Empty graph
        fuelPlot <- ggplot() +
          geom_col() +
          theme_classic() +
          labs(title = fuelPlotTitle, x="Year", y="Consumption (EJ)") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'right',
                plot.margin = unit(c(0,0,0,2), "cm"))
      } else {
        discreteFuelPlotTitle <- paste0("Consumption of fuels for ", country, " over time")
        fuelPlot <- ggplot(data = dataSet, aes(x = Year, y = totalConsumption, fill = Type)) +
          geom_col() +
          theme_classic() +
          labs(title = discreteFuelPlotTitle, x="\n \n \nYear", y="Consumption (EJ)") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'none',
                plot.margin = unit(c(0,0,0,2), "cm"),
                strip.text.x = element_text(margin = margin(0.6,0,0.6,0, "mm"))) +
          scale_fill_brewer(palette = "Set3") +
          facet_grid(. ~ Type)
      }
    }
    fuelPlot
  }
  )
  
  # Percentage change in consumption plot
  output$percentChangePlot <- renderPlotly({
    percentChangeData <- data.frame(Year=as.numeric(character()),
                                    Type=character(), 
                                    totalConsumption=as.numeric(character()), 
                                    percentChangeFromPrevYear=as.numeric(character())) 
    
    # Filtering data
    filterResults <- filterData()
    country <- filterResults[1]
    chosenFuels <- filterResults[2]
    dataSet <- filterResults[[3]]
    
    # Calculating percentage change values
    if (chosenFuels != "") {
      for (year in yearList) {
        yearData <- dataSet %>% filter(Year == year)
        
        prevYear <- year - 1
        prevYearData <- dataSet %>% filter(Year == prevYear)
        
        yearData$percentChangeFromPrevYear <- ifelse(prevYearData[,3] == 0, 0, round((yearData[,3] - prevYearData[,3])/prevYearData[,3] * 100, 2)[,1])
        percentChangeData <- rbind.data.frame(percentChangeData, yearData)
      }
    }

    
    if (input$levelOfDetail == "Cumulative") {
      if (nrow(percentChangeData) == 0){
        fuelPlotTitle <- "No fuel type chosen."
        # Empty graph
        percentChangePlot <- ggplot() +
          geom_col() +
          theme_classic() +
          labs(title = fuelPlotTitle, x="\n \n \nYear", y="% Change") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'none',
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                plot.margin = unit(c(0,0,0,2), "cm")) +
          scale_x_continuous(breaks = seq(2000, 2019, 1))
        
      } else {
        percentChangePlot <- ggplot(percentChangeData, aes(x=Year, y=percentChangeFromPrevYear, fill = Type),show.legend = FALSE) +
          geom_col() +
          theme_classic() +
          labs(title = "Percentage Change in Consumption from Previous Year (%)", x="Year", y="% Change") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'right',
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                plot.margin = unit(c(0,0,0,2), "cm")) +
          scale_fill_brewer(palette = "Set3") +
          scale_x_continuous(breaks = seq(2000, 2019, 1))
      }
    } else if (input$levelOfDetail == "Discrete"){
      if (nrow(percentChangeData) == 0){
        fuelPlotTitle <- "No fuel type chosen."
        # Empty graph
        percentChangePlot <- ggplot() +
          geom_col() +
          theme_classic() +
          labs(title = fuelPlotTitle, x="\n \n \nYear", y="% Change") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'none',
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                plot.margin = unit(c(0,0,0,2), "cm"))
        
      } else {
        percentChangePlot <- ggplot(percentChangeData, aes(x=Year, y=percentChangeFromPrevYear, fill = Type),show.legend = FALSE) +
          geom_col() +
          theme_classic() +
          labs(title = "Percentage Change in Consumption from Previous Year (%)", x="\n \n \nYear", y="% Change") +
          theme(plot.title = element_text(size=18, hjust=0.5),
                axis.title.x = element_text(size=10),
                axis.text.x = element_text(angle=45, hjust=1),
                axis.title.y = element_text(size=10),
                legend.position = 'none',
                strip.background = element_blank(),
                strip.text.x = element_blank(),
                plot.margin = unit(c(0,0,0,2), "cm")) +
          scale_fill_brewer(palette = "Set3") +
          facet_grid(. ~ Type)
      }
      
    }
    percentChangePlot
  })
  
  # Fuel emission plot
  output$emissionPlot <- renderPlotly({
    
    # Filtering data
    if (input$country == "(All)") {
      dataSetEmission <- emissionData %>%
        dplyr::select(Year, Emission) %>% 
        group_by(Year) %>% 
        summarise(Emission = sum(Emission, na.rm=TRUE))
      country <- "all countries"
    } else {
      dataSetEmission <- emissionData %>% filter(Country == input$country)
      country <- input$country
    }
    
    title <- paste0("Carbon Emission for ", country, " over time")
    
    emissionPlot <- ggplot(dataSetEmission, aes(x=Year, y=Emission)) +
      labs(title = paste(strwrap(title), collapse = "\n"), x="\nYear", y="Carbon Emission\n(Million tonnes of CO2)\n \n") +
      geom_segment( aes(x=Year, xend=Year, y=0, yend=Emission), color="skyblue") +
      geom_point(color="orange", size=4) +
      theme_classic() +
      theme(plot.title = element_text(size=18, hjust=0.5),
            axis.title.x = element_text(size=10),
            axis.text.x = element_text(angle=45, hjust=1),
            axis.title.y = element_text(size=10),
            panel.grid.major.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank()) + 
      scale_x_continuous(breaks = seq(2000, 2019, 1))
  })
}
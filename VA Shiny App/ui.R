###########################################################################################
#                                                                                         #
#                                 IMPORTING LIBRARIES                                     #
#                                                                                         #
###########################################################################################
usePackage<-function(p){
  # load a package if installed, else load after installation.
  # Args:
  #   p: package name in quotes
  
  if (!is.element(p, installed.packages()[,1])){
    print(paste('Package:',p,'Not found, Installing Now...'))
    install.packages(p, dep = TRUE)}
  print(paste('Loading Package :',p))
  require(p, character.only = TRUE)  
}

usePackage("leaflet")
usePackage("shiny")
usePackage("dplyr")
usePackage("data.table")
usePackage("sp")
usePackage("rgeos")
usePackage("raster")
usePackage("rgdal")
usePackage("GISTools")
usePackage("ShinyApp")
usePackage("plotly")
usePackage("shiny.semantic")
usePackage("semantic.dashboard")
usePackage("ggplot2")
usePackage("ggridges")
usePackage("lubridate")
usePackage("dbplyr")
usePackage("dygraphs")
usePackage("xts")
usePackage("forcats")
usePackage("png")
usePackage("base64enc")

###########################################################################################
#                                                                                         #
#                                    USER-INTERFACE CODE                                  #
#                                                                                         #
###########################################################################################
# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "BP World Energy Survey 2019", inverted = TRUE),
  dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "overall", "Overall View"),
      menuItem(tabName = "country", "Country View")
    )
  ),
  dashboardBody(
    tags$style(".pusher.container .ui.segment .ui.stackable.container.grid {margin:0px!important;}"),
    tabItems(
      selected = 1,
      tabItem(
        tabName = "overall",
        div(style="width:100%",
          fluidRow(
            div(style="display: inline-block; margin-top:25px; margin-right:20px; vertical-align:top; width:20%", valueBoxOutput('totalEnergy')),
            div(style="display: inline-block; margin-top:25px; margin-right:20px; vertical-align:top; width:20%", valueBoxOutput('tradProp')),
            div(style="display: inline-block; margin-top:25px; margin-right:20px; vertical-align:top; width:20%", valueBoxOutput('cleanProp')),
            div(style="display: inline-block; margin-top:25px; margin-right:20px; vertical-align:top; width:20%", valueBoxOutput('nuclProp'))
            ),
          fluidRow(
            div(style="display: inline-flex; margin-top:20px; margin-right:20px",
              sliderInput("MaxYear",
                          "Year",
                          min = 2000,
                          max = 2019,
                          value = 2019,
                          sep = "" ),
              selectInput('energyType', 'Energy Type', 
                          choices =  c(`All` = 'all',
                                       `Renewable`='renewable',
                                       `Non-renewable`='nonrenewable'),
                          multiple = FALSE),
            )
          ),
          fluidRow(
            box(
              leafletOutput("choroplethMap")
            )
          )
        )
      ),
      tabItem(
        tabName = "country",
        div(style="display:inline-block",
          selectInput(
            inputId = 'ops_time_factor', label= 'Time Factor', 
            choices =  c("Month" = 'month',
                         "Day of week" ='day',
                         "Time Group" ='time_bin',
                         "Holiday"='holiday',
                         "Season" ='season'),
            multiple = FALSE), 
        )
      )
    )
  ), theme = "cosmo",
  suppress_bootstrap = TRUE
)
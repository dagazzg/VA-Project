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

usePackage("plotly")
usePackage("leaflet")
usePackage("shiny")
usePackage("shiny.semantic")
usePackage("semantic.dashboard")
usePackage("dplyr")
usePackage("data.table")
usePackage("sp")
usePackage("rgeos")
usePackage("raster")
usePackage("rgdal")
usePackage("GISTools")
usePackage("ShinyApp")
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
    h1("ISSS608 Visual Analytics and Applications"),
    tags$style(".pusher.container .ui.segment .ui.stackable.container.grid {margin:0px!important;}"),
    tabItems(
      selected = 1,
      tabItem(
        tabName = "overall",
        div(style="width:100%",
          fluidRow(
            tags$style(".ui.yellow.card {background: #fbbd08}"),
            div(style="display: inline-block; margin-top:25px; margin-right:10px; vertical-align:top; width:23%", valueBoxOutput('totalEnergy')),
            tags$style(".ui.red.card {background: #db2828}"),
            div(style="display: inline-block; margin-top:25px; margin-right:10px; vertical-align:top; width:23%", valueBoxOutput('tradProp')),
            tags$style(".ui.green.card {background: #21ba45}"),
            div(style="display: inline-block; margin-top:25px; margin-right:10px; vertical-align:top; width:23%", valueBoxOutput('cleanProp')),
            tags$style(".ui.teal.card {background: #00b5ad}"),
            div(style="display: inline-block; margin-top:25px; margin-right:10px; vertical-align:top; width:23%", valueBoxOutput('nuclProp'))
            ),
          br(),
          fluidRow(
            div(style="display: inline-flex; margin-top:20px;",
              sliderInput("MaxYear",
                          "Year",
                          min = 2000,
                          max = 2019,
                          value = 2019,
                          sep = "" ),
              #selectInput('energyType', 'Energy Type', 
              #            choices =  c(`All` = 'all',
              #                         `Renewable`='renewable',
              #                         `Non-renewable`='nonrenewable'),
              #            multiple = FALSE),
            )
          ),
          br(),
          fluidRow(id = 'choromap',
                   tags$style('#choromap { background-color: #aad3df; }'),
            div(style="display: inline-block; margin-top:25px; margin-right:0px; vertical-align:top; width:60%",
              leafletOutput("choroplethMap")
            ),
            div(style="display: inline-block; margin-top:25px; margin-left:0px; vertical-align:top; width:37%",
                plotlyOutput("donutChart")
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
  ), theme = "material",
  suppress_bootstrap = TRUE
)
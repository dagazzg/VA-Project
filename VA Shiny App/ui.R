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
      menuItem(tabName = "overall", "All Energy Sources"),
      menuItem(tabName = "renewables", "Renewable Energy"),
      menuItem(tabName = "nonrenewables", "Non-renewable Energy")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "overall",
        div(style="width:1200px",
          fluidRow(
            div(style="display: inline-block; margin-top:20px; margin-right:20px",
            selectInput('choice1', 'Time Factor', 
                        choices =  c(`Month` = 'Month',
                                     `Day of Week`='Day_of_Week',
                                     `Time Group`='time_indicator',
                                     `Holiday`='Holiday',
                                     `Season`='Season'),
                        multiple = FALSE),
            )
          ),
        )
      ),
      tabItem(
        tabName = "renewables",
        div(
        )
      ),
      tabItem(
        tabName = "nonrenewables",
        div(style="display:inline-block",
          selectInput(
            inputId = 'ops_time_factor', label= 'Time Factor', 
            choices =  c("Month" = 'month',
                         "Day of week" ='day',
                         "Time Group" ='time_bin',
                         "Holiday"='holiday',
                         "Season" ='season'),
            multiple = FALSE), 
        ),
      )
    )
  ), theme = "cosmo"
)



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
usePackage("shiny")
usePackage("shiny.semantic")
usePackage("semantic.dashboard")
usePackage("dplyr")
usePackage("leaflet")
usePackage("sp")
usePackage("data.table")
usePackage("base64enc")
usePackage("colorRamps")
usePackage("tidyr")
usePackage("reactable")
usePackage("tidyverse")
usePackage("RColorBrewer")
usePackage("rgdal")

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
      menuItem(tabName = "home", "Home"),
      menuItem(tabName = "overall", "Overall View"),
      menuItem(tabName = "country", "Detailed View")
    )
  ),
  dashboardBody(
    h1("World Fuel Consumption"),
    tags$style(".pusher.container .ui.segment .ui.stackable.container.grid {margin:0px!important;}"),
    tabItems(
      selected = 1,
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 16,
            align = "center",
            box(
              div(style="padding: 60px; height: 750px; font-size: 24px", "ISSS608 Visual Analytics and Applications - G2 Group 6",
                  br(),
                  list(img(src=base64enc::dataURI(file="img/green-energy.png", mime="image/png"),height='200px')),
                  fluidRow(
                    div(style="font-size: 20px; margin-bottom: 35px; margin-top: 50px",
                        HTML("<b><u>INTRODUCTION</u></b><br>
                             The global demand for energy has been steadily increasing over the past few decades, with the rise of nations like China and India. However, the reliance of fossil fuels has led to
                             issues such as climate change and excessive carbon emissions causing harm to environment. In recent years, there has been a pushback against the usage of traditional, non-renewable
                             fuel sources with one of the most important solutions for fighting climate change being indentified as the need to move away from fossil fuels and towards renewable energy sources.")
                    ),
                    div(style="font-size: 20px",
                        "With the pressing need to switch to cleaner fuel sources, our team found ourselves asking these questions. 
                        What fuel sources are countries currently reliant on? Which countries are ahead in the push towards green energy? 
                        Are there any trends or patterns we can discern from the available data? With reference to BP's 2019 Statistical Review of World Energy, 
                        we attempt to visualize the overall fuel consumption of countries on a global scale."
                    ),
                    br(),
                    div(style="font-size: 20px",
                        " This dashboard is best viewed using a resolution of 1920x1080"
                    ),
                    div(style="font-size: 20px; margin-top: 25px",
                        HTML(paste("<table style='width:100%'>
                                    <tr>
                                      <th><u>OBJECTIVES</u><br></th>
                                    </tr>
                                    <tr>
                                      <td>&emsp;</td>
                                    </tr>
                                    <tr>
                                      <td>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;1.&emsp;&emsp; Visualise the fuel consumption in the past 20 years on a global and by-country level</td>
                                    </tr>
                                    <tr>
                                      <td>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;2.&emsp;&emsp; Explore developments in consumption and fuel mix</td>
                                    </tr>
                                    <tr>
                                      <td>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;3.&emsp;&emsp; Identify differences in the move towards renewable energy between countries</td>
                                    </tr>
                                    <tr>
                                      <td>&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;4.&emsp;&emsp; Explore the impact of crises on fuel consumption</td>
                                    </tr>

                                  </table>"))
                    )
                  )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "overall",
        div(style="width:100%",
          box(style = "height: 700px",
            title = "World Fuel Consumption - Yearly",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
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
            fluidRow(
              tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:1000;}"))),
              div(style="display: inline-flex; margin-top:20px;",
                selectInput("MaxYear",
                            "Year",
                            choices = c(2019:2000),
                            selected = 2019)
              )
            ),
            fluidRow(id = 'choromap',
                     tags$style('#choromap { background-color: #FFFFFF; }'),
                     div(style="display: inline-block; margin-right:0px; vertical-align:top; width:60%; ",
                         box(title = "Choropleth Map", color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                           leafletOutput("choroplethMap", height = 450)
                         )
                     ),
                     div(style="display: inline-block; margin-left:0px; vertical-align:top; width:37%; ",
                         box(title = "Fuel Type Distribution", color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                           plotlyOutput("donutChart", height = 450)
                         )
                     ),
                     tags$style(".leaflet-container {background: #FFFFFF; }")
            ),
          ),
          br(),
          box(title = "Yearly Data (% Renewable Fuel Consumption)",
            color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
            fluidRow(
              sliderInput("dateRange",
                          "Year",
                          min = 2000,
                          max = 2019,
                          value = c(2000,2019),
                          sep = "",
                          width = "40%"
              ),
              reactableOutput("myTable")
            )
          )
        )
      ),
      tabItem(
        tabName = "country",
        div(style="width:100%",
            div(style="display: inline-block; width:15%; ",
              box(width = 2,
                title = "Options",
                color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                selectInput(
                  inputId = "country",
                  label = "Country",
                  choices = c("(All)","Austria", "Azerbaijan", "Bangladesh", "Belarus", 
                              "Belgium", "Brazil", "Bulgaria", "Canada", 
                              "Central America", "Chile", "China", 
                              "China Hong Kong SAR", "Colombia", "Croatia", 
                              "Cyprus", "Czech Republic", "Denmark", "Eastern Africa", 
                              "Ecuador", "Egypt", "Estonia", "Finland", "France", 
                              "Germany", "Greece", "Hungary", "Iceland", "India", 
                              "Indonesia", "Iran", "Iraq", "Ireland", "Israel", 
                              "Italy", "Japan", "Kazakhstan", "Kuwait", "Latvia", 
                              "Lithuania", "Luxembourg", "Malaysia", "Mexico", 
                              "Middle Africa", "Morocco", "Netherlands", 
                              "New Zealand", "North Macedonia", "Norway", "Oman", 
                              "Other Asia Pacific", "Other Caribbean", "Other CIS", 
                              "Other Europe", "Other Middle East", 
                              "Other Northern Africa", "Other S. & Cent. America", 
                              "Other South America", "Other Southern Africa", 
                              "Pakistan", "Peru", "Philippines", "Poland", "Portugal", 
                              "Qatar", "Romania", "Russian Federation", 
                              "Saudi Arabia", "Singapore", "Slovakia", "Slovenia", 
                              "South Africa", "South Korea", "Spain", "Sri Lanka", 
                              "Sweden", "Switzerland", "Taiwan", "Thailand", 
                              "Trinidad & Tobago", "Turkey", "Turkmenistan", 
                              "Ukraine", "United Arab Emirates", "United Kingdom", 
                              "US", "Uzbekistan", "Venezuela", "Vietnam", "Western Africa"),
                  selected = "(All)"),
                radioButtons(inputId = "typeOfDetail",
                             label = strong("Type of Detail"), 
                             choices = c("Fuel Consumption", "Carbon Emission"), 
                             selected = "Fuel Consumption"),
                conditionalPanel(
                  condition = "input.typeOfDetail == 'Fuel Consumption'",
                  radioButtons(inputId = "levelOfDetail",
                               label = strong("Level of Detail"), 
                               choices = c("Discrete", "Cumulative"), 
                               selected = "Discrete"),
                  tags$style('.shiny-input-checkboxgroup label ~ .shiny-options-group, .shiny-input-radiogroup label ~ .shiny-options-group { margin-top:0px; }'),
                  fluidRow(
                    column(3, strong("Non-renewable"),
                           checkboxInput(
                             inputId = "isSumOil",
                             label = "Oil",
                             value = TRUE),
                           checkboxInput(
                             inputId = "isSumGas",
                             label = "Gas",
                             value = FALSE),
                           checkboxInput(
                             inputId = "isSumCoal",
                             label = "Coal",
                             value = FALSE),
                           checkboxInput(
                             inputId = "isSumNuclear",
                             label = "Nuclear",
                             value = FALSE)),
                    column(3, strong("Renewable"),
                           checkboxInput(
                             inputId = "isSumBiofuels",
                             label = "Biofuels",
                             value = FALSE),
                           checkboxInput(
                             inputId = "isSumHydroelectricity",
                             label = "Hydroelectricity",
                             value = FALSE),
                           checkboxInput(
                             inputId = "isSumSolar",
                             label = "Solar",
                             value = FALSE),
                           checkboxInput(
                             inputId = "isSumWind",
                             label = "Wind",
                             value = FALSE),
                           checkboxInput(
                             inputId = "isSumOthers",
                             label = "Others",
                             value = FALSE)))),
              )
              ),
              
            # Show a plot of the generated distribution
            div(style="display: inline-block; margin-right:0px; vertical-align:top; width:80%; ",
              box(title= "Country Data",
                color = "black", ribbon = FALSE, title_side = "top", collapsible = FALSE,
                conditionalPanel(condition = "input.typeOfDetail == 'Fuel Consumption'", 
                                 plotlyOutput('fuelPlot', height = "400px"),
                                 plotlyOutput('percentChangePlot', height = "400px")),
                conditionalPanel(condition = "input.typeOfDetail == 'Carbon Emission'", plotlyOutput('emissionPlot', height = "400px"))
              )
            )
        )
      )
    )
  ), theme = "cosmo",
  suppress_bootstrap = TRUE
)
library(tidyverse)
library(readxl)
library(magrittr)
library(sf)
library(shiny)
library(shinyjs)
library(bslib)
library(data.table)
library(ggspatial)
library(magrittr)
library(plotly)
library(crosstalk)
library(shinythemes)
library(shinycssloaders)
# library(listviewer)
# library(jsonlite)
library(viridis)
library(colorspace)



##### LEAFLET MAP/SHINY APP PREREQS
## THE SHINY APP #####
ui <- fluidPage(
  
  # App title ----
  
  # Sidebar layout with input and output definitions ----
    column(2,
           br(),br(),br(),
    # Sidebar panel for inputs ----=
      # Input: Select the random distribution type ----
      radioButtons("colourradio", "Colour Palette:",
                   choices = list("Rainbow" = 1, "Viridis" = 2),
                   selected = 1),
      br(),
      
      # br() element to introduce extra vertical spacing ---
      
      HTML(
        "<b>Double click</b> on a legend language to (de)isolate it from the rest. 
            <b>Shift click</b> on another language to add it to current selection. <b>Double click</b>
            on white space to return map to its regular zoom state. "
      )
      # Input: Slider for the number of observations to generate ----
    ),
    # Main panel for displaying outputs ----
    column(10,
      h3(
        "Top Non-official Language Spoken at Home",
        style="text-align: center"
      ),
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Canada (Provincial)", 
                           sidebarLayout(
                             sidebarPanel(
                               width=4,
                               fluidRow(
                                 plotlyOutput(
                                   outputId = "langbar_PROV", 
                                   width = "100%",
                                   height = "80vh"
                                 ) %>% withSpinner(color="#3e3f3a")
                               )
                             ),
                             mainPanel(
                               width = 8,
                               align = "center",
                               plotlyOutput(
                                 outputId = "map_PROV", 
                                 width = "100%",
                                 height = "80vh"
                               ) %>% withSpinner(color="#3e3f3a")
                             )
                           )),
                  tabPanel("Canada (Census Divisions)",
                           sidebarLayout(
                             sidebarPanel(
                               width=4,
                               fluidRow(
                                 plotlyOutput(
                                   outputId = "langbar_CD", 
                                   width = "100%",
                                   height = "80vh"
                                 ) %>% withSpinner(color="#3e3f3a")
                               )
                             ),
                             mainPanel(
                               width = 8,
                               align = "center",
                               plotlyOutput(
                                 outputId = "map_CD", 
                                 width = "100%",
                                 height = "80vh"
                               ) %>% withSpinner(color="#3e3f3a")
                             )
                           )),
                  tabPanel(
                    "Waterloo",
                    sidebarLayout(
                      sidebarPanel(
                        width=4,
                        fluidRow(
                          plotlyOutput(
                            outputId = "langbar_KWRegion",
                            width = "100%",
                            height = "80vh"
                          ) %>% withSpinner(color="#3e3f3a")
                        )
                      ),
                      mainPanel(
                        width = 8,
                        align = "center",
                        plotlyOutput(
                          outputId = "map_KWRegion",
                          width = "100%",
                          height = "80vh"
                        ) %>% withSpinner(color="#3e3f3a")
                      )
                    )
                  ),
                  tabPanel(
                    "London",
                    sidebarLayout(
                      sidebarPanel(
                        width=4,
                        fluidRow(
                          plotlyOutput(
                            outputId = "langbar_London",
                            width = "100%",
                            height = "80vh"
                          ) %>% withSpinner(color="#3e3f3a")
                        )
                      ),
                      mainPanel(
                        width = 8,
                        align = "center",
                        plotlyOutput(
                          outputId = "map_London",
                          width = "100%",
                          height = "80vh"
                        ) %>% withSpinner(color="#3e3f3a")
                      )
                    )
                  ),
                  tabPanel(
                    "Toronto",
                    sidebarLayout(
                      sidebarPanel(
                        width=4,
                        fluidRow(
                          plotlyOutput(
                            outputId = "langbar_Toronto",
                            width = "100%",
                            height = "80vh"
                          ) %>% withSpinner(color="#3e3f3a")
                        )
                      ),
                      mainPanel(
                        width = 8,
                        align = "center",
                        plotlyOutput(
                          outputId = "map_Toronto",
                          width = "100%",
                          height = "80vh"
                        ) %>% withSpinner(color="#3e3f3a")
                      )
                    )
                  )
                  
                  
      )
      
    )
)




# navbarPage(
#   "2021 Census Language Data",
#   theme = shinytheme("sandstone"),
#   tags$style(
#     HTML(
#       ".radio {margin-bottom: 42px;}"
#     )
#   ),
#   ,
#     "Canada (Provincial)",
#     tags$style(
#       type='text/css', 
#       ".tab-content, plot-container plotly {background-color: #f8f5f0;}
#        .col-sm-8 {padding-top: 50px;padding-bottom:50px;padding-right:30px;}"
#     ),
#     
#   ),
#   tabPanel(
#     "Canada (Census Divisions)",
#     tags$style(
#       type='text/css', 
#       ".tab-content, plot-container plotly {background-color: #f8f5f0;}
#        .col-sm-8 {padding-top:50px;padding-bottom:50px;padding-right:30px;}"
#     ),
#             
#   ),
#   navbarMenu(

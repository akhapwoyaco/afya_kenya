#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(bslib)
library(sf)
library(stringr)
#
load('geo_data.RData')
load('health_facilities_data.RData')
load('counties_data_aggregate.RData')
#
counties_data_aggregate <- counties_data_aggregate |>
  select(name, facility_count) |>
  as.data.frame() |> 
  bind_rows(
    data.frame(
      name = "ALL", 
      facility_count = sum(counties_data_aggregate$facility_count))
  ) |>
  arrange(desc(facility_count))
#
maxWidth <- max(str_length(counties_data_aggregate[, 1]))
counties_unique <- counties_data_aggregate[, 1]
names(counties_unique) <- paste0( 
  str_pad(
    counties_data_aggregate[, 1], 
    width = maxWidth, 
    side="right"), 
  "|", counties_data_aggregate[, 2])


# navbar_js <- "@media (max-width: 1200px) {
#   
# }"
#
navbarPage(
  "Health Facilities", 
  id="health_navigation_id", 
  # bootswatch_themes()
  theme = bslib::bs_theme(preset = "zephyr",
                          bg = "#e5e5e5", fg = "#0d0c0c"),
  # bg = "#101010",
  # fg = "#FFF"),
  header = tags$head(includeCSS("../common.css")),
  # tab 1 of map
  tabPanel(
    "Interactive map",
    div(
      class="outer",
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput(
        outputId = "leaflet_map", width="100%", height="500px"
      ), 
      
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(
        id = "controls", 
        class = "panel panel-default", fixed = TRUE, 
        draggable = TRUE, top = 60, left = "auto", 
        right = 20, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Select Counties"),
        
        
        selectInput(
          inputId = "county", 
          label = "County", 
          choices = counties_unique, 
          selected = counties_unique[1]
        ),
        conditionalPanel(
          "input.county != 'ALL'", 
          # Only prompt for when map is of counties
          uiOutput(
            "wards_selector_ui"
          )
        )
      )
    ),
    
    tags$div(
      id="cite",
      'Data Source', tags$em('https://afya360.co.ke/'), 
      '.'
    ), 
    br(),
    div(
      class = "footer",
      style='height:50px;background:gray54;margin-top:auto;margin-bottom:auto;
                    text-align:center;',
      HTML(
        '<footer class="footer">
              Copyright &copy; 2024 &nbsp; Website: <a href="https://scalableanalytics.co.ke/"
              target="_blank">Scalable Analytics</a> &nbsp; &nbsp;
              Github Account: <a href="https://github.com/akhapwoyaco"
              target="_blank">akhapwoyaco</a>
              </footer>'
      )
    )
    
  ),
  
  tabPanel(
    "Data Explorer",
    hr(),
    DT::dataTableOutput("hospital_table")
  )
)

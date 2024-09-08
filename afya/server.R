#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
load('geo_data.RData')
load('health_facilities_data.RData')
#
clean_health_facilities <- final_data |> 
  dplyr::select(id, name, facility_type_name, county_name, twenty_four_hour, 
                open_weekends, get_latitude, get_longitude, 
                ward, constituency, county) |> 
  na.omit(get_latitude)
#
library(shiny)
library(leaflet)
#
kenya_county <- kenya_county |> rename(uid = shapeID)
#

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # County Data
  #
  county_layer <- reactive({
    #
    pattern_to_search = stringr::str_sub(input$county, 1,4)
    #
    if (grepl(pattern = "ALL" , x = input$county)){
      kenya_county
    } else {
      kenya_caw |> 
        filter(
          grepl(
            pattern = pattern_to_search, 
            x = county)
        )
    }
  })
  # get county data and stats
  #
  health_county_select <- reactive({
    if (grepl(pattern = "ALL" , x = input$county)){
      facilities = clean_health_facilities
    } else {
      facilities <- clean_health_facilities |> 
        dplyr::filter(county == input$county) # county_name
    }
    # st_as_sf(
    facilities#, 
    #   coords = c("get_longitude", "get_latitude"),
    #   crs = 4326
    # )
  })
  # render the ward selection input
  output$wards_selector_ui <- renderUI({
    req(input$county)
    # kenya_caw[grepl(pattern = stringr::str_sub(
    # string = "BUNGOMA", 1,4), x = kenya_caw$county),]$ward
    selectInput(
      'ward_s', 'WARDS', 
      choices = county_layer()$ward
    )
  })
  #
  # get totals by ward
  #
  # ward_aggregates <- reactive({
  #   req(input$ward_s)
  #   #
  #   health_county_select() |> 
  #     group_by(ward, facility_type_name) |>
  #     summarise(
  #       n = n()
  #       ) 
  #     # mutate(
  #     #   ward = trimws(toupper(ward))
  #     # ) |>
  #     # dplyr::filter(ward == input$ward_s)
  #   #
  # })
  #
  #
  output$leaflet_map <- renderLeaflet({
    #
    leaflet() |>
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(noWrap = TRUE)
      ) |>
      addTiles() |> 
      addPolygons(
        data = county_layer(), 
        stroke = T,
        color = 'black', weight = 1, layerId = ~uid) |> 
      addCircleMarkers(
        radius = 1, 
        
        data = health_county_select(), 
        lat = ~get_latitude, lng = ~get_longitude, 
        stroke = T, fill = T,
        color = "black", opacity = 0.5,
        layerId = ~id,
        fillOpacity = 0.95,
        popup = ~paste(
          paste(name, sep = '')
        )
      )
  })
  #
  observeEvent(input$ward_s, {
    req(input$ward_s)
    #
    facility_type_name_category <- as.character(unique(
      health_county_select()$facility_type_name)
    )
    #
    category_palette <- colorFactor(  
      palette = 'Set1',
      domain = health_county_select()$facility_type_name
    )
    #
    #get the selected polygon and extract the label point 
    selected_polygon <- county_layer() |>
      filter(ward == input$ward_s)
    
    # polygon_labelPt <- selected_polygon@polygons[[1]]@labpt
    # 
    # #remove any previously highlighted polygon
    leafletProxy("leaflet_map") |>
      removeShape(layerId = "selected") |>
      clearMarkers() |>
      # # 
      # # #center the view on the polygon 
      # # leafletProxy("leaflet_map") |>
      # #   setView(lng=polygon_labelPt[1],lat=polygon_labelPt[2],zoom=7)
      # # 
      # #add a slightly thicker red polygon on top of the selected one
      #leafletProxy("leaflet_map") |>
      addPolygons(
        stroke = TRUE, weight = 2, #color = "red",
        fill = 'blue', opacity = 1,
        data = selected_polygon, 
        layerId = "selected"
      ) |> 
      #
      addCircleMarkers(
        radius = 2, 
        data = health_county_select(), 
        lat = ~get_latitude, lng = ~get_longitude, 
        opacity = 0.5,
        fillOpacity = 0.95,
        popup = ~paste(
          paste(name, sep = '')
        ),
        layerId = ~id, 
        group = ~facility_type_name, 
        color = ~category_palette(facility_type_name)
      ) |> 
      #
      addLayersControl(
        overlayGroups = facility_type_name_category,
        options = layersControlOptions(collapsed = !FALSE))
    #
  })
  #
  output$hospital_table <- DT::renderDT({
    health_county_select() |> 
      select(
        name, facility_type_name, 
        twenty_four_hour, open_weekends,
        ward, constituency, county)
  })
  
}

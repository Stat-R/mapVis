lapply(list('shiny','googleVis','leaflet','sp','rgdal','htmltools','shinydashboard','magrittr'),require,character.only = T)
library(shinydashboard)
library(leaflet)

shinyApp(ui = dashboardPage(
         dashboardHeader(title = "Testing Dashboard",titleWidth = 200),
         dashboardSidebar(
           list(HTML('<img src="TTI_logo.jpg"/>')),
           sliderInput(inputId = 'nummo',label = 'Choose how many years to display: ',value = 4, min = 1, max = 5),
           textInput(inputId = 'code', label = 'Enter code...', value = "", width = NULL),
           selectInput(inputId = 'select_year',selected = 2010,label = 'Select Year (for ttt Map)',choices = c(2010,2015)),
           verbatimTextOutput('some_text')
         ),
         dashboardBody(
           # Boxes need to be put in a row (or column)
           fluidRow(
             leafletOutput("ttt_map"),
             htmlOutput('ttt_plot'),
             verbatimTextOutput('CONVERTED_text')
           ))
         ),
server = function(input, output){
  output$some_text <- renderText({
    paste0('You selected this year # ',input$select_year)
  })
  
  spDF <- reactive(readOGR(dsn = 'ShapeFiles',layer = 'Old_New'))
  code1 <- reactive(input$code)
  map_layer = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}'
  output$CONVERTED_text <- renderText({paste0('Your spDF names is converted number is # ',names(spDF()))})
  output$ttt_map <- renderLeaflet({
    if(code1() == "GJH9Z1QDFATNECQ1UE22762LT821U2"){ 
    shp_data <- spDF()
    shp_data <- spTransform(shp_data,CRS('+init=epsg:4326'))  
    shp_data = shp_data[shp_data$Pleca == input$select_year,]
    
    map_bounds <- shp_data@bbox %>% as.matrix()
    leaflet(data = shp_data)  %>% fitBounds(map_bounds[1,1], map_bounds[2,1], map_bounds[1,2], map_bounds[2,2]) %>%
      addTiles() %>%
      addTiles(urlTemplate =map_layer,options = providerTileOptions(opacity = 0.35)) %>%
      setView(lng = center_lon, lat = center_lat, zoom = 12) %>%
      addPolygons(data=shp_data,weight=4,color = col_vacant,opacity = .5)
  }
  })
  
  output$ttt_plot <- renderGvis({
    if(code1() == 'abc123'){
    gvisComboChart(readr::read_csv('TRZ_New.csv',col_names = T) %>% dplyr::filter(year == 2009),#input$select_year),
                   xvar="Land_Type", yvar=c("taxable_val","total_area"),
                   options =  list(seriesType="bars",legend = 'none',title = 'TEST',fontSize='15')) 
    }
  })
}
)# %>% print # this will run shinyApp just by sourcing this file

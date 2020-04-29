library(shinydashboard)
library(shiny)
library(shinyWidgets)     # For cool buttons, sliders, checkboxes, etc.
library(leaflet)          # For interactive maps
library(DT)               # For fancy interactive data table 
library(shinycssloaders)  # For spinner wheel 
library(sf)               # Reading and manipulating shapes
library(htmltools)
library(RColorBrewer)


#load("map_dept.RData")



header <- dashboardHeader(
  title = "French Mortality Database", titleWidth = 400
  #navbarPage(header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 20pt; }")))
)

body <- dashboardBody(
  
  chooseSliderSkin(skin = "Flat", color = "#3c8dbc"),   # awesomeSlider appearance
  
  fluidRow(
    
    # Some CSS styling options
    tags$style(type = "text/css", "
               .irs-grid-text {font-size: 13px; color: black;}
               span.irs-single {font-size: 14px;}
               span.irs-min {font-size: 14px;}
               span.irs-max {font-size: 14px;}
               span.irs-single {background-color: #3c8dbc;}
               .bttn-unite.bttn-default {color: #3c8dbc; border-color: #3c8dbc;}
               #map {height: calc(100vh - 80px) !important;}
               "),
    
    column(width = 9, 
           box(width = NULL, solidHeader = TRUE,
               withSpinner(leafletOutput("FMDB", height = 900), type = 6),
               HTML(" <em> Source: French Mortality Database</em>. University of California, Berkeley (USA), INED (Paris, France). 
               Disponible sur <a href='https://mortality.org'>mortality.org</a>", 
                    paste0(". Les données ont été affichées à ", as.POSIXct(Sys.time()), ".")),
               style = "height:100vh - 30vh;"
               )
          ),
    
    column(width = 3,
           box(width = NULL,
               p(enc2utf8("Faites vos choix ci-dessous, ensuite cliquez 'Actualiser' pour rafraîchir la carte"), style = "color: grey; font-size: 16px"),
               awesomeRadio("lt.column", h4("Indicateur"),
                            choices = list("Taux de mortalité" = "mx", "Espérance de vie" = "ex"), selected = "mx", inline = TRUE),
               awesomeRadio("sex", h4("Sexe"), choices = list("Hommes" = 1, "Femmes" = 2), inline = TRUE),
               
               sliderInput("age", h4("Âge"), min = 0, max = 110, value = 0, sep = "", step = 1),
               sliderInput("year", h4("Année (période)"), min = min(years), max = max(years), value = max(years), sep = "", step = 1),
               actionBttn("applyInput", "Actualiser", style = "unite", block = TRUE)
              )
           
          ),
    
    column(width = 3, 
           box(width = NULL, DT::dataTableOutput("table.lt"), style = "height:400px; overflow-y: scroll;"
           
              )
          )

     )
 )




ui <- dashboardPage(
                    skin = "blue",
                    header,
                    dashboardSidebar(disable = TRUE),
                    body
                  )


server <- function(input, output, session) {
  
  # Set up state-level life table data to be called based on user input
  spdf.data <- reactive({
    
    input$applyInput # Asserting dependency on pressing of the Apply Changes button
    
    sex.vec <- c("MLT.", "FLT.")
    df <- get(paste0(sex.vec[isolate(as.numeric(input$sex))], isolate(input$year)))
    df1 <- df[df$Age == isolate(as.character(input$age)), ]
    df2 <- merge(if (isolate(input$year) < 1968) {dept.before.1968.shp} else {dept.after.1968.shp}, df1, by.x = "code_dept", by.y = "PopName")
    
    names(st_geometry(df2)) <- NULL # Trivial, but necessary setup to avoid sf shapefile bug. See here for more details:
    ## https://stackoverflow.com/questions/53227205/polygons-not-getting-plotted-in-leaflet-r-map-since-update
    return(df2)
  })
  

  # Showing life table
  output$table.lt <- DT::renderDataTable({
    
    input$applyInput      # Asserting dependency on pressing of the Apply Changes button
    
    table.data <- st_set_geometry(spdf.data(), NULL) # Getting rid of the geometry column of sf to be able to extract tabular data
    
    datatable(table.data[order(table.data[, "nom_dept"]), c("nom_dept", "Age", "mx", "ex")], 
              rownames = FALSE, colnames = c("Département", "Âge (x)", "Décès pour 1000 habitants", "L'espérance de vie à X an(s)"),
              class = "cell-border compact hover", 
              caption = htmltools::tags$caption(paste0("Table de mortalité départementale pour les ", 
                                                       ifelse(isolate(input$sex) == "1", "hommes",
                                                                            ifelse(isolate(input$sex) == "2", "femmes", stopApp())),
                                                       ifelse(isolate(input$sex) == 1, " agés ", " agées "), isolate(input$age), " en ", isolate(input$year)), 
                                                style = "color:red; font-size:16px;"), 
              options = list(searching = FALSE, paging = FALSE)
    )
  })  
  
  
  # Creating a map
  output$FMDB <- renderLeaflet({
    
    input$applyInput  # Asserting dependency on pressing of the Apply Changes button
    
    lt.col <- reactive({isolate(input$lt.column)})
    
    map.dept <- spdf.data()
    
    # Creating quantile breakdown of mx or ex
    quantColor <- if (lt.col() == "mx") {
                    unique(quantile(map.dept$mx, n = 5, na.rm = T))
                  } else if (lt.col() == "ex") {
                    unique(quantile(map.dept$ex, n = 5, na.rm = T))}
                  
    map1 <- leaflet() %>%
      
      addProviderTiles("Stamen.TonerLite",  
                       options = providerTileOptions(minZoom = 4, maxZoom = 9)) %>%
      
      setView(lat = 46.70, lng = 3.30, zoom = 6) %>%                       
      
      
      addPolygons(data = map.dept,
                  color = "white",
                  weight = 2,
                  opacity = 0.5,
                  fillColor = brewer.pal(n = length(quantColor), if (lt.col() == "mx") {"Reds"} 
                                                                 else if (lt.col() == "ex") {"Blues"}),
                  fillOpacity = 0.65,
                  popup = paste0("<b>", map.dept$nom_dept, " (", isolate(input$year), ", ", 
                                 isolate(ifelse(input$sex == "1", "hommes",
                                                ifelse(input$sex == "2", "femmes", NA))), ")", "</b>",
                                 "<br>", "Nombre de décès à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                   " pour 1000 habitants = ",  round(map.dept$mx, 2),
                                 "<br>", "L'espérance de vie à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                   " = ", round(map.dept$ex, 2), " ans"),
                  highlightOptions = highlightOptions(fillColor = "yellow", fillOpacity = 0.6, weight = 2)) %>%
      
      addLegend(pal = colorQuantile(if (lt.col() == "mx") {"Reds"} else if (lt.col() == "ex") {"Blues"}, 
                                    seq(min(quantColor, na.rm = T), max(quantColor, na.rm = T), 
                                        (max(quantColor, na.rm = T) - min(quantColor, na.rm = T))/5), 
                                    n = 5),
                values = round(seq(min(quantColor), max(quantColor), (max(quantColor) - min(quantColor))/5), 2),
                title = paste0(isolate(ifelse(input$lt.column == "mx", paste0("Nombre de décès à ", map.dept$Age[1], 
                                                                              if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                                                              " pour 1000 habitants "),
                                              paste0("L'espérance de vie à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"}))),
                               "<br>", "(", isolate(input$year), ", ",
                               isolate(ifelse(input$sex == "1", "hommes",
                                              ifelse(input$sex == "2", "femmes", stopApp()))), "):"),
                position = "bottomleft",
                opacity = 1,
                labFormat = function(type, cuts, p) {
                  n <- length(cuts)
                  cuts <- paste0(format(cuts[-n], big.mark = ","),
                                 " - ", format(cuts[-1], big.mark = ","))}) %>%
      
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topright',
        # width = 175, height = 175,
        toggleDisplay = FALSE,
        # aimingRectOptions = list(color = 'grey'), shadowRectOptions = list(color = 'orange'),
        
        width = 300, height = 300,
        centerFixed = c(46.70, 3.30), 
        zoomLevelFixed = 7,
        aimingRectOptions = list(color = "grey", clickable = TRUE, weight = 1), shadowRectOptions = list(color = NA)
        
        
        )
    
    map1
    
  })
  
  # ### WIP ()
  # # Updating the polygons upon options selection without reloading the basemap

}

shinyApp(ui = ui, server = server)

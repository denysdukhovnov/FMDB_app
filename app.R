library(shiny)              # Base shiny
library(shinydashboard)     # Dashboard version of shiny
library(shinydashboardPlus) # Dashboard extras (mainly right sidebar)
library(shinyWidgets)       # For cool buttons, sliders, checkboxes, etc.
library(leaflet)            # For interactive maps
library(DT)                 # For fancy interactive data table 
library(shinycssloaders)    # For spinner wheel 
library(sf)                 # Reading and manipulating shapes
library(htmltools)          # Custom HTML control
library(RColorBrewer)       # Color palettes
library(plotly)             # For interactive plots

source("./global.R")


header <- dashboardHeaderPlus(disable = FALSE,
  title = "French Mortality Database", titleWidth = 400,
  enable_rightsidebar = FALSE, rightSidebarIcon = "gears", 
  left_menu = tagList(
    dropdownBlock(
      id = "additional_options",
      title = "Réglages",
      icon = icon("sliders"),
      prettySwitch(
        inputId = "colorscale_options",
        label = "Activer l'échelle des couleurs thématique (décile)",
        fill = TRUE,
        bigger = TRUE,
        status = "primary",
        value = FALSE
      )
    )
  )

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
               span.label.label-danger {display: none !important;}
               .modebar {display: none !important;}
               .leaflet-popup-content-wrapper {background-color: rgba(255, 255, 255, 0.85)}
               "),
    
    column(width = 6, 
           box(width = NULL, solidHeader = TRUE,
               withSpinner(leafletOutput("FMDB", height = 900), type = 6),
               HTML(" <em> Source: French Mortality Database</em>. University of California, Berkeley (USA), INED (Paris, France). 
               Disponible sur <a href='https://mortality.org'>mortality.org</a>", 
                    paste0(". Les données ont été affichées à ", as.POSIXct(Sys.time()), ".")),
               style = "height: 100vh - 30vh;"
               )
          ),
    
    column(width = 3,
           box(width = NULL, solidHeader = TRUE, 
               h3(strong("Paris et les environs"), style = "color: #3c8dbc; text-align: center; margin-top: 0px"),
               withSpinner(leafletOutput("FMDB.Paris", height = 380), type = 6),
               style = "height: 43vh;"
           ),
           
           box(width = NULL, solidHeader = TRUE,
               plotlyOutput("plotly_histogram", height = 380),
               style = "height: 40vh;"
           )
    ),
    
    column(width = 3,
           box(width = NULL, solidHeader = TRUE,
               p(enc2utf8("Veuillez choisir l'indicateur, l'âge et le sexe, et cliquez sur 'Actualiser' pour rafraîchir la carte"), 
                 style = "color: grey; font-size: 16px"),
               awesomeRadio("lt.column", h4("Indicateur"),
                            choices = list("Taux de mortalité" = "mx", "Espérance de vie" = "ex"), selected = "mx", inline = TRUE),
               awesomeRadio("sex", h4("Sexe"), choices = list("Hommes" = 1, "Femmes" = 2), inline = TRUE),
               
               sliderInput("age", h4("Âge"), min = 0, max = 110, value = 0, sep = "", step = 1),
               sliderInput("year", h4("Année (période)"), min = min(years), max = max(years), value = max(years), sep = "", step = 1),
               actionBttn("applyInput", "Actualiser", style = "unite", block = TRUE)
              )
           
          ),
    
    column(width = 3, 
           box(width = NULL, solidHeader = TRUE, DT::dataTableOutput("table.lt"), style = "height:400px;" #overflow-y: scroll;"
           
              )
          )

     )
 )




ui <- dashboardPagePlus(
                    skin = "blue",
                    header = header,
                    sidebar = dashboardSidebar(disable = TRUE, width = 0),
                    # rightsidebar = rightSidebar(width = 400,
                    #                             rightSidebarTabContent(
                    #                               id = 1,
                    #                               icon = "info",
                    #                               active = TRUE
                    #                             )
                    #                ),
                    sidebar_fullCollapse = TRUE,
                    body = body
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
              rownames = FALSE, colnames = c("Département", "Âge (x)", "Décès pour 1000 habitants", "L'espérance de vie à (x) an(s)"),
              class = "cell-border compact hover", 
              caption = htmltools::tags$caption(paste0("Table de mortalité départementale pour les ", 
                                                       ifelse(isolate(input$sex) == "1", "hommes",
                                                                            ifelse(isolate(input$sex) == "2", "femmes", stopApp())),
                                                       ifelse(isolate(input$sex) == 1, " agés ", " agées "), isolate(input$age), " en ", isolate(input$year)), 
                                                style = "color: #3c8dbc; font-size:18px; text-align: center; margin-top: 0px; font-weight: 700"), 
              options = list(searching = FALSE, paging = FALSE, scrollY = "200px")
    )
  })  
  
  
  # Plotly histogram
  output$plotly_histogram <- renderPlotly({
    
    lt.col <- reactive({isolate(input$lt.column)})
    
    map.dept <- if (lt.col() == "mx") {
                    spdf.data()$mx
                  } else if (lt.col() == "ex") {
                    spdf.data()$ex
                  }
    
    labs.x <- if (lt.col() == "mx") {
                "Taux de mortalité (pour 1000 habitants)"
              } else if (lt.col() == "ex") {
                "Espérance de vie (ans)"
              }
    
    layout(
      
      plot_ly(
              x = map.dept,
              autobinx = TRUE,
              xbins = list(start = min(map.dept), end = max(map.dept), size = 0.25),
              alpha = 0.6,
              type = "histogram",
              histnorm = "percent"
      ),
           #title = "Nombre de départements",
           xaxis = list(title = labs.x, 
                        zeroline = FALSE, 
                        dtick = 1, 
                        tick0 = 0, 
                        tickmode = "linear", 
                        range = c(min(map.dept), max(map.dept))
                    ),
      
      
           yaxis = list(title = "%", zeroline = FALSE),
           bargap = 0.05
      
      
    )
    
  })
  
  
  # Pre-define map function to be called later
  make_leaflet_map <- function(opts = NULL) { 
  
    input$applyInput  # Asserting dependency on pressing of the Apply Changes button
    
    lt.col <- reactive({isolate(input$lt.column)})
    
    map.dept <- spdf.data()
    
    # Creating quantile breakdown of mx or ex
    quantColor <- if (lt.col() == "mx") {
                    unique(quantile(map.dept$mx, n = 5, na.rm = T))
                  } else if (lt.col() == "ex") {
                    unique(quantile(map.dept$ex, n = 5, na.rm = T))}
                  
    map1 <- leaflet(options = opts) %>%
      
      addMapPane("background", zIndex = 410) %>% 
      addMapPane("polygons", zIndex = 420) %>% 
      addMapPane("maplabels", zIndex = 430) %>% 
      
      addProviderTiles("CartoDB.PositronNoLabels", options = pathOptions(pane = "background")) %>%
      addProviderTiles("Stamen.TonerLabels", options = pathOptions(pane = "maplabels", opacity = 0.4)) %>%
      
      #setView(lat = 46.70, lng = 3.30, zoom = 6) %>%                       
      
      addPolygons(data = map.dept,
                  options = pathOptions(pane = "polygons"),
                  color = "white",
                  weight = 2,
                  opacity = 0.5,
                  fillColor = brewer.pal(n = length(quantColor), if (lt.col() == "mx") {"Reds"} 
                                                                 else if (lt.col() == "ex") {"Blues"}),
                  fillOpacity = 0.65,
                  popup = paste0("<b>", map.dept$nom_dept, " (", isolate(input$year), ", ", 
                                 isolate(ifelse(input$sex == "1", "hommes",
                                                ifelse(input$sex == "2", "femmes", NA))), ", chiffres arrondis)", "</b>",
                                 "<br>", "Nombre de décès à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                   " pour 1000 habitants = ",  round(map.dept$mx, 2),
                                 "<br>", "L'espérance de vie à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                   " = ", round(map.dept$ex, 2), " ans"),
                  popupOptions = popupOptions(autoPan = FALSE, keepInView = TRUE, alpha = 0.5), 
                  highlightOptions = highlightOptions(fillColor = "cyan", fillOpacity = 0.6, weight = 2)) %>%
      
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
                                 " - ", format(cuts[-1], big.mark = ","))}) 
      

    return(map1)
}
  
  
  # Rendering the main map
  output$FMDB <- renderLeaflet({
    
    make_leaflet_map(opts = leafletOptions()) %>% 
      
      setView(lat = 46.70, lng = 2.4, zoom = 6) %>% 
      
    addMiniMap(
      tiles = providers$CartoDB.Positron,
      position = 'topright',
      width = 175, height = 175,
      toggleDisplay = FALSE,
      aimingRectOptions = list(color = 'orange')
    )
    
  })
  
  # Rendering the Paris close-up map
  output$FMDB.Paris <- renderLeaflet({
    
    make_leaflet_map(opts = leafletOptions(dragging = FALSE, zoomControl = FALSE, minZoom = 9, maxZoom = 9)) %>% 
      setView(lat = 48.85, lng = 2.36, zoom = 9) %>% 
      clearControls()
    
  }) 
  
  # ### WIP ()
  # # Updating the polygons upon options selection without reloading the basemap

}

shinyApp(ui = ui, server = server)

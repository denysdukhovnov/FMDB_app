library(shiny)              # Base shiny
library(shinydashboard)     # Dashboard version of shiny
library(shinythemes)        # Pretty-up the UI design
library(shinyWidgets)       # For cool buttons, sliders, checkboxes, etc.
library(leaflet)            # For interactive maps
library(DT)                 # For fancy interactive data table 
library(shinycssloaders)    # For spinner wheel 
library(sf)                 # Reading and manipulating shapes
library(htmltools)          # Custom HTML control
library(RColorBrewer)       # Color palettes
library(plotly)             # For interactive plots
library(dplyr)              # For data management

source("./global.R")


ui <- navbarPage(title = "French Mortality Database",
                 id = "nav", selected = "1", 
                 theme = shinytheme("spacelab"),                       # Dashboard theme
  
  tabPanel(title = "Carte", value = "1", icon = icon("map-marked-alt"),
      
           chooseSliderSkin(skin = "Nice", color = "#3c8dbc"),         # Slider style and appearance
           
           tags$style(type = "text/css", "
                      
                      /* Slider and button appearance */
                          .irs-grid-text {
                                          font-size: 14px; 
                                          color: black; 
                                          -webkit-transform: rotate(-45deg);
                                          padding: 0 20px 0 0;
                                         }
                      
                          span.irs-single {font-size: 14px; 
                                           background-color: #3c8dbc;}
                          
                          span.irs-min {font-size: 14px;}
                          span.irs-max {font-size: 14px;}

/*.shiny-input-container[id='age'].span.irs-max {
                                          color: transparent; 
                                          font-size: 14px; 
                                          background: 0;
                                       }
                          .shiny-input-container[id='age'].span.irs-max:after {
                                                position: relative !important; 
                                                content: '105+'; 
                                                font-size: 14px; 
                                                color: #999; 
                                                background-color: rgba(0, 0, 0, 0.1);
                                                padding: 1px 3px;
                                                border-radius: 3px;
                                                line-height: 1.333;
                                                text-shadow: none;
                                             }
*/                        

                          .form-group.shiny-input-container {margin-bottom: 30px;}
                          
                          .bttn-unite.bttn-default {color: #3c8dbc; border-color: #3c8dbc;}
                          span.label.label-danger {display: none !important;}
                          .modebar {display: none !important;}
                      
                      /* leaflet popups and other map settings */
                          .leaflet-popup-content-wrapper {background-color: rgba(255, 255, 255, 0.85)}
                          html, body {width: 100%; height: 100%}
                      
                      /* Setting for the maps to fill/scale to the window size */ 
                          #FMDB {height: calc(100vh - 80px) !important;}
                          #FMDBreport {height: 20vh;}

                      /* Setting the navbar and containter-fluid padding to 0 for the map tab only */             
                          .navbar {margin-bottom: 0;}
                          .container-fluid:nth-child(2) {padding-left: 0; padding-right: 0;}
                          .tab-pane:nth-of-type(2) {padding-left: 25px; padding-right: 25px;}
                      
                      /* Appearance of the controls box */
                          #controls {  
                                      background-color: white;
                                      padding: 5px 20px 20px 20px;

                                  /* Fade out while not hovering */
                                      opacity: 0.70;
                                      transition: opacity 250ms 0s;
                                      overflow: auto;
                                    }
                          
                          #controls:hover {
                                        /* Fade in while hovering */
                                            opacity: 1;
                                            transition-delay: opacity 250ms 0s;
                                            overflow: auto;
                          }
                        
                      /* Top padding for the DOWNLOAD BUTTON 
                          .tab-pane:nth-of-type(2), .row:nth-of-type(3) {margin-top: 15px;}
                      */

                      /* Data Table height */
                          .dataTables_scrollBody {min-height: 30vh;}
                      
                      /* Data Table prevent row deselection (i.e. at least one row should be selected at any given time) */
                          table.dataTable tbody tr.selected {pointer-events: none;}

                      /* Data Table remove info row [Showing x of n entries] from the bottom and adding bottom margins to space elements */
                          .dataTables_info {display: none;}
                          .dataTables_scroll {margin-bottom: 20px;}

                      "),
           
           tags$div(id = "cite",
                    
                    HTML("Source: French Regional Database. Paris School of Economics (France), Ecole Normale Supérieure Paris-Saclay (France), University of California, Berkeley (USA), INED (France). 
                         Disponible sur <a href='https://mortality.org'>mortality.org</a>"), 
                                             
                    style = "position: absolute; bottom: 10px; left: 10px; font-size: 14px;" 
                    
           ),
           
    withSpinner(proxy.height = "100vh - 80px",
                leafletOutput("FMDB", width = "100%", height = 1000), 
                type = 6),
        
    
     absolutePanel(
            id = "controls",
            class = "panel panel-default", 
            fixed = TRUE,
            draggable = FALSE,
            top = 60,
            left = "auto",
            right = 20,
            bottom = "auto",
            width = 500,
            height = "auto",
            
            wellPanel(em(enc2utf8("Veuillez choisir l'indicateur, l'âge et le sexe, puis cliquer sur 'Actualiser' pour rafraîchir la carte"), 
              style = "color: grey50; font-size: 16px; justified: center;")),      
            
            awesomeRadio("lt.column", h4("Indicateur"),
                         choices = list("Taux de mortalité" = "mx", "Espérance de vie" = "ex"), selected = "mx", inline = TRUE),
            awesomeRadio("sex", h4("Sexe"), choices = list("Hommes" = 1, "Femmes" = 2), inline = TRUE),
            
            sliderInput("age", h4("Âge"), min = 0, max = 105, value = 0, sep = "", step = 1, width = "100%"),
            sliderInput("year", h4("Année (période)"), min = min(years), max = max(years), value = max(years), sep = "", step = 1, width = "100%"),
            actionBttn("applyInput", "Actualiser", style = "unite", block = TRUE)
            
     )
  ),

  tabPanel(title = "Explorateur de données", value = "2", icon = icon("table"),

       fluidRow(
         column(width = 10,
                wellPanel(em(enc2utf8("Veuillez cliquer sur une des lignes du tableau pour actualiser les graphiques et la carte. 
                                      Pour changer l'indicateur ou les autres réglages, revenez à l'onglet principal de la carte et
                                      effectuez votre sélection"), 
                             style = "color: grey50; font-size: 16px; justified: left; border: none; margin-bottom: 0;"))
         ),
         
         column(width = 2,
                wellPanel(downloadButton("report", "Générer un rapport"), 
                          style = "padding: 10px; background-color: transparent; -webkit-box-shadow: none; border: none; box-shadow: none;")
         )
       ),
       
       fluidRow(
          
         DT::dataTableOutput("tableLT", width = "100%", height = "100%")
       
       ), 
       
       fluidRow(
         column(width = 3,
                plotlyOutput("plotly_histogram", width = "100%", height = "100%")
         ),
         
         column(width = 5, 
                plotlyOutput("plotly_graph", width = "100%", height = "100%")
         ),
         
         column(width = 4,
                leafletOutput("FMDBreport", width = "100%", height = 450) 
         )
       )
  )
) 



server <- function(input, output, session) {
  
  # This section ensures that changes are not applied to any indicators UNTIL the Apply Changes button is clicked!!!
  lt.col <- eventReactive(input$applyInput, {input$lt.column}, ignoreNULL = FALSE)
  age <- eventReactive(input$applyInput, {input$age}, ignoreNULL = FALSE)
  sex <- eventReactive(input$applyInput, {input$sex}, ignoreNULL = FALSE)
  year <- eventReactive(input$applyInput, {input$year}, ignoreNULL = FALSE)
  
  
  # Set up department-level life table data to be called based on user input
  spdf.data <- reactive({
    
    input$applyInput # Asserting dependency on pressing of the Apply Changes button
    
    sex.vec <- c("MLT.", "FLT.")
    df <- get(paste0(sex.vec[isolate(as.numeric(sex()))], isolate(year())))
    df1 <- df[df$Age == isolate(as.character(age())), ] 
    df2 <- merge(if (isolate(year()) < 1968) {dept.before.1968.shp} else {dept.after.1968.shp}, df1, by.x = "code_dept", by.y = "PopName")
    df3 <- df2[order(df2$nom_dept),] # This is necessary to maintain concordance with the DataTable order of rows
    rownames(df3) <- 1:nrow(df3)     # and this helps to match the rows though user selection to the correct shape
    
    names(st_geometry(df3)) <- NULL # Trivial, but necessary setup to avoid sf shapefile bug. See here for more details:
    ## https://stackoverflow.com/questions/53227205/polygons-not-getting-plotted-in-leaflet-r-map-since-update
    return(df3)
  })
  
  
  # Data for graph plotting and comparison (not linked to leaflet)
  scatter.data <- reactive({

    input$applyInput # Asserting dependency on pressing of the Apply Changes button

    sex.vec <- c("MLT", "FLT")
    df <- get(sex.vec[isolate(as.numeric(sex()))])
    df1 <- df[df$Age == isolate(as.character(age())), ]
    df2 <- merge(if (isolate(year()) < 1968) {
                     dept.before.1968.shp[, c("code_dept", "nom_dept")]
                 } else {
                     dept.after.1968.shp[, c("code_dept", "nom_dept")]
                 }, 
                 df1, 
                 by.x = "code_dept",
                 by.y = "PopName")
    st_geometry(df2) <- NULL                                   # <---- Must be set to null, otherwise not treated like data.frame in some instances
    df3 <- df2[order(as.numeric(df2$code_dept), df2$Year), ]   # This is necessary to maintain concordance with the DataTable order of rows
                                                               # and this helps to match the rows though user selection to the correct shape
    return(df3) 
  })
  

  # Showing life table
  output$tableLT <- DT::renderDataTable({
    
    input$applyInput      # Asserting dependency on pressing of the Apply Changes button
    
    table.data <- st_set_geometry(spdf.data(), NULL) %>%    # Getting rid of the geometry column of sf to be able to extract tabular data
                    mutate(ex = format(ex, big.mark = ".", decimal.mark = ","))     # Formatting column to show 2 decimal places and comma instead of decimal point
    
    
    datatable(table.data[order(table.data[, "nom_dept"]), c("nom_dept", "code_dept", "Age", "mx", "ex")], 
              
                  rownames = FALSE, colnames = c("Département", "Code", "Âge", "Taux de mortalité pour 1000 habitants", "Espérance de vie à (x) an(s)"),
              
                  class = "cell-border compact hover", 
              
                  caption = htmltools::tags$caption(paste0("Tables de mortalité départementales pour l'année ", isolate(year())," (", 
                                                          ifelse(isolate(sex()) == "1", "hommes",
                                                                 ifelse(isolate(sex()) == "2", "femmes", stopApp())),
                                                          ifelse(isolate(sex()) == 1, " agés de ", " agées de "), isolate(age()), " ans)"), 
                                                    style = "color: #3c8dbc; font-size:24px; text-align: center; margin-top: 0px; font-weight: 700"), 
                  options = list(searching = FALSE,
                                 paging = FALSE, 
                                 scrollY = "200px",
                                 list(order = list(0, 'desc')),
                                 columnDefs = list(list(className = 'dt-right', targets = 1:4)) # Right-justifying numeric columns
                            ), 
              
                  selection = list(mode = "single", selected = 1)
             )
    
  })  
  
  
  # Plotly histogram
  output$plotly_histogram <- renderPlotly({

    plot.dept <- if (lt.col() == "mx") {
                    spdf.data()$mx
                  } else if (lt.col() == "ex") {
                    spdf.data()$ex
                  }

    labs.x <- if (lt.col() == "mx") {"Taux de mortalité pour 1000 habitants (mx)"} else if (lt.col() == "ex") {"Espérance de vie (ex)"}

    layout(

      plot_ly(
              x = plot.dept,
              autobinx = TRUE,
              alpha = 0.6,
              type = "histogram",
              histnorm = "percent"
      ),
           title = paste0("Histogramme des valeurs en ", isolate(year())),
           xaxis = list(title = labs.x,
                        zeroline = FALSE,
                        automargin = FALSE,
                        tickmode = "linear",
                        range = c(min(plot.dept) - 0.5, max(plot.dept) + 0.5),
                        fixedrange = TRUE,
                        showticklabels = TRUE, 
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside'
                    ),

           yaxis = list(title = "%", 
                        zeroline = FALSE,
                        fixedrange = TRUE,
                        showticklabels = TRUE, 
                        tickcolor = 'rgb(127,127,127)', 
                        ticks = 'outside'
                    ),
           bargap = 0.05
    )

  })
  
  
  
  # Setup the process to record the current and last clicks of the DataTable
  clickDT <- reactiveValues(current.click = 1, last.click = NULL)
  
  observeEvent(input$tableLT_row_last_clicked, {
    clickDT$last.click <- clickDT$current.click
    clickDT$current.click <- input$tableLT_row_last_clicked
  })
  
  current.clickDT <- reactive({
    req(input$tableLT_rows_selected) 
    input$tableLT_rows_selected
    clickDT$current.click
  })
  
  last.clickDT <- reactive({
    req(input$tableLT_row_last_clicked)
    input$tableLT_row_last_clicked
    clickDT$last.click
  })
  

  # Data for plotly line graph
  plot.df <- reactive({
                  
                  {input$tableLT_rows_selected}
                  
                  input$applyInput
                  
                  plot.data <- scatter.data() %>%
                    group_by(Year) %>% 
                    mutate(Min = if (lt.col() == "mx") {min(mx, na.rm = T)} else {min(ex, na.rm = T)},
                           Pctile25 = if (lt.col() == "mx") {unname(quantile(mx, na.rm = T)[2])
                                      } else {unname(quantile(ex, na.rm = T)[2])},
                           Pctile75 = if (lt.col() == "mx") {unname(quantile(mx, na.rm = T)[4])
                                      } else {unname(quantile(ex, na.rm = T)[4])},
                           Max = if (lt.col() == "mx") {max(mx, na.rm = T)} else {max(ex, na.rm = T)}) %>%
                    summarise(Min = mean(Min),
                              Pctile25 = mean(Pctile25),
                              Pctile75 = mean(Pctile75),
                              Max = mean(Max))
                  
                  sex.vec <- c("MLT", "FLT")
                  plot.data.france <- get(sex.vec[isolate(as.numeric(sex()))]) %>% 
                    filter(PopName == "98", Age == isolate(as.character(age()))) %>% 
                    select(Year, lt.col()) %>%
                    rename_at(2, ~"France")
                  
                 
                  plot.data.dept <- scatter.data() %>%
                    filter(code_dept == if_else(isolate(as.numeric(year())) >= 1968, 
                                                match.sel.row.dept.after.1968[current.clickDT(), "code_dept"],
                                                match.sel.row.dept.before.1968[current.clickDT(), "code_dept"])
                    ) %>% 
                    select(Year, nom_dept, lt.col()) %>% 
                    rename_at(3, ~"value")
                  
                  plot.data.join <- plot.data %>% 
                    left_join(plot.data.france, by = "Year") %>% 
                    left_join(plot.data.dept, by = "Year") %>% 
                    select(Year, Min, Pctile25, Pctile75, Max, France, nom_dept, value)

                  return(plot.data.join)                
  
  })
  
  
  # Plotly graph
  output$plotly_graph <- renderPlotly({
    
    plot.data <- plot.df()
    
    labs.y <- if (lt.col() == "mx") {
      "Taux de mortalité (mx)"
    } else if (lt.col() == "ex") {
      "Espérance de vie (ex)"
    }
    
  layout(
    
    
    plot.data %>%
      plot_ly() %>% 
      add_trace(x = ~Year,
                y = ~Min,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(150,175,255,0)'),
                showlegend = FALSE,
                name = 'Min-Max',                             # <---- THIS IS A FAKE TITLE (FOR EASIER WORKAROUND WITH LEGEND LABELS)
                hoverinfo = "x+y",                            # Only shows x and y values on hover
                hoverlabel = list(bgcolor = "rgba(150,175,255,0.4)"),
                hovertemplate = paste0(plot.data$Year, ": ", format(round(plot.data$Min, 2), decimal.mark = ","), "<extra></extra>")
                ) %>% 
      add_trace(x = ~Year,
                y = ~Max,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor = 'rgba(150,175,255,0.4)',
                line = list(color = 'rgba(150,175,255,0)'),
                showlegend = TRUE, 
                name = 'Min-Max',                            # <---- THIS IS A FAKE TITLE (FOR EASIER WORKAROUND WITH LEGEND LABELS)
                hoverinfo = "x+y",                           # Only shows x and y values on hover
                hoverlabel = list(bgcolor = 'rgba(150,175,255,0.4)'),
                hovertemplate = paste0(plot.data$Year, ": ", format(round(plot.data$Max, 2), decimal.mark = ","), "<extra></extra>")
                ) %>%                        
      add_trace(x = ~Year,
                y = ~Pctile25,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(90,190,255,0)'),
                showlegend = FALSE, 
                name = 'Intervalle inter-quartile',          # <---- THIS IS A FAKE TITLE (FOR EASIER WORKAROUND WITH LEGEND LABELS),
                hoverinfo = "x+y",                           # Only shows x and y values on hover
                hoverlabel = list(bgcolor = 'rgba(90,190,255,0.4)'),
                hovertemplate = paste0(plot.data$Year, ": ", format(round(plot.data$Pctile25, 2), decimal.mark = ","), "<extra></extra>")
                ) %>% 
      add_trace(x = ~Year,
                y = ~Pctile75,
                type = 'scatter',
                mode = 'lines',
                fill = 'tonexty',
                fillcolor = 'rgba(90,190,255,0.4)',
                line = list(color = 'rgba(90,190,255,0))'),
                showlegend = TRUE, 
                name = 'Intervalle inter-quartile',          # <---- THIS IS A FAKE TITLE (FOR EASIER WORKAROUND WITH LEGEND LABELS)
                hoverinfo = "x+y",                           # Only shows x and y values on hover
                hoverlabel = list(bgcolor = 'rgba(90,190,255,0.4)'),
                hovertemplate = paste0(plot.data$Year, ": ", format(round(plot.data$Pctile75, 2), decimal.mark = ","), "<extra></extra>")
                ) %>% 
      add_trace(x = ~Year,
                y = ~France,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(120,120,255,1)'),
                showlegend = TRUE, 
                name = 'FRANCE',
                hoverinfo = "x+y+name",                      # Shows x, y, and name values on hover
                hoverlabel = list(bgcolor = 'rgba(120,120,255,0.8)'),
                hovertemplate = paste0(plot.data$Year, ": ", format(round(plot.data$France, 2), decimal.mark = ","))
                ) %>% 
      add_trace(x = ~Year,
                y = ~value,
                type = 'scatter',
                mode = 'lines',
                line = list(color = 'rgba(255,0,0,0.7)'),
                showlegend = TRUE,
                name = ~nom_dept,
                hoverinfo = "x+y+name",                      # Shows x, y, and name values on hover
                hoverlabel = list(bgcolor = 'rgba(255,0,0,0.6)'),
                hovertemplate = paste0(plot.data$Year, ": ", format(round(plot.data$value, 2), decimal.mark = ","))
                ),

      plot_bgcolor = 'rgb(229,229,229)',
      title = "Séries chronologiques",
      xaxis = list(title = "Année", zeroline = FALSE, fixedrange = FALSE, 
                   gridcolor = 'rgb(255,255,255)', showticklabels = TRUE, tickcolor = 'rgb(127,127,127)', ticks = 'outside',
                   showspikes = TRUE, spikemode = "toaxis+marker", spikethickness = 2, spikedash = "solid"),
      yaxis = list(title = labs.y, zeroline = FALSE, fixedrange = FALSE, 
                   gridcolor = 'rgb(255,255,255)', showticklabels = TRUE, tickcolor = 'rgb(127,127,127)', ticks = 'outside',
                   showspikes = TRUE, spikemode = "toaxis+marker", spikethickness = 2, spikedash = "solid"),
      legend = list(x = 0.825, y = 0.975, bgcolor = 'rgba(255,255,255,0.35)', xanchor = "right", orientation = "h")

    )
    
  })
  
  
  # Pre-define map function to be called later
  make_leaflet_map <- function(opts = NULL) { 
  
    input$applyInput  # Asserting dependency on pressing of the Apply Changes button
    
    map.dept <- spdf.data() 
    
    # Creating quantile breakdown of mx or ex
    quantColor <- if (lt.col() == "mx") {
      colorBin("Reds", seq(min(map.dept$mx, na.rm = TRUE), 
                           max(map.dept$mx, na.rm = TRUE), 
                           length.out = 5), pretty = TRUE, 
               na.color = NA) #unique(quantile(map.dept$mx, n = 5, na.rm = T))
    } else if (lt.col() == "ex") {
      colorBin("Blues", seq(min(map.dept$ex, na.rm = TRUE), 
                            max(map.dept$ex, na.rm = TRUE), 
                            length.out = 5), pretty = TRUE, 
               na.color = NA) #unique(quantile(map.dept$ex, n = 5, na.rm = T))}
    }
    
    
    ## THIS WORKS BUT DOESN"T BREAK CLEANLY
    # quantColor <- if (lt.col() == "mx") {
    #   colorQuantile("Reds", seq(min(map.dept$mx), max(map.dept$mx), length.out = 5), n = 5, na.color = NA) #unique(quantile(map.dept$mx, n = 5, na.rm = T))
    #               } else if (lt.col() == "ex") {
    #   colorQuantile("Blues", seq(min(map.dept$ex), max(map.dept$ex), length.out = 5), n = 5, na.color = NA) #unique(quantile(map.dept$ex, n = 5, na.rm = T))}
    #               }
    
    # quantColor <- if (lt.col() == "mx") {
    #   colorQuantile("Reds", map.dept$mx, na.color = "transparent")}
    # else if (lt.col() == "ex") {
    #   colorQuantile("Blues", map.dept$ex, na.color = "transparent")}
                  
    map1 <- leaflet(options = opts, sizingPolicy = leafletSizingPolicy(browser.fill = TRUE)) %>%
      
      addMapPane("background", zIndex = 410) %>% 
      addMapPane("polygons", zIndex = 420) %>% 
      addMapPane("maplabels", zIndex = 430) %>% 
      
      addProviderTiles("CartoDB.PositronNoLabels", options = pathOptions(pane = "background")) %>%
      addProviderTiles("Stamen.TonerLabels", options = pathOptions(pane = "maplabels", opacity = 0.4)) %>%
      
      addPolygons(data = map.dept,
                  options = pathOptions(pane = "polygons"),
                  color = "white",
                  weight = 2,
                  opacity = 0.5,
                  fillColor = ~quantColor(if (lt.col() == "mx") {mx} else if (lt.col() == "ex") {ex}),
                                            #brewer.pal(n = length(quantColor), if (lt.col() == "mx") {"Reds"} 
                                            #                     else if (lt.col() == "ex") {"Blues"}),
                                            
                  fillOpacity = 0.65,
                  popup = paste0("<b>", map.dept$nom_dept, " (", isolate(year()), ", ", 
                                 isolate(ifelse(sex() == "1", "hommes",
                                                ifelse(sex() == "2", "femmes", NA))), ", chiffres arrondis)", "</b>",
                                 "<br>", "Nombre de décès à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                   " pour 1000 habitants = ",  round(map.dept$mx, 2),
                                 "<br>", "Espérance de vie à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                   " = ", format(round(map.dept$ex, 2), decimal.mark = ","), " ans"),
                  popupOptions = popupOptions(autoPan = FALSE, keepInView = TRUE, alpha = 0.5), 
                  highlightOptions = highlightOptions(fillColor = "gold", fillOpacity = 0.6, weight = 2),
                  layerId = ~code_dept) %>%
      
      addLegend(pal = quantColor,
                      values = if (lt.col() == "mx") {map.dept$mx} else if (lt.col() == "ex") {map.dept$ex},
                  
                #   colorQuantile(if (lt.col() == "mx") {"Reds"} else if (lt.col() == "ex") {"Blues"}, 
                #                     seq(min(quantColor, na.rm = T), max(quantColor, na.rm = T), 
                #                         (max(quantColor, na.rm = T) - min(quantColor, na.rm = T))/5), 
                #                     n = 5),
                # values = round(seq(min(quantColor), max(quantColor), (max(quantColor) - min(quantColor))/5), 2),
                title = paste0(isolate(ifelse(input$lt.column == "mx", paste0("Nombre de décès à ", map.dept$Age[1], 
                                                                              if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"},
                                                                              " pour 1000 habitants "),
                                              paste0("Espérance de vie à ", map.dept$Age[1], if (map.dept$Age[1] %in% c(0,1)) {" an"} else {" ans"}))),
                               "<br>", "(", isolate(year()), ", ",
                               isolate(ifelse(sex() == "1", "hommes",
                                              ifelse(sex() == "2", "femmes", stopApp()))), "):"),
                position = "bottomleft",
                opacity = 1,
                labFormat = function(type, cuts, p) {
                  n <- length(cuts)
                  cuts <- paste0(format(round(cuts[-n], 2), big.mark = ".", decimal.mark = ","),
                                 " - ", format(round(cuts[-1], 2), big.mark = ".", decimal.mark = ","))
                  }) 
      

    return(map1)
}
  
  
  # Rendering the main map
  output$FMDB <- renderLeaflet({
    
    make_leaflet_map(opts = leafletOptions()) %>% 
      
      #setView(lat = 46.70, lng = 2.4, zoom = 6) %>% 
      clearBounds() %>% 
      
      addMiniMap(
        tiles = providers$CartoDB.Positron,
        position = 'topleft',
        width = 175, height = 175,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = 'orange'),
        autoToggleDisplay = TRUE
    )

  })
  
  
  # Rendering the map for the report tab
  output$FMDBreport <- renderLeaflet({

    make_leaflet_map(opts = leafletOptions(dragging = TRUE, zoomControl = FALSE)) %>%
      #setView(lat = 46.7, lng = 2.4, zoom = 5) %>%
      clearBounds() %>% 
      clearControls()
    
  })
  
  
  
  # Observer for proxies
  observe({  
    
    map.dept <- spdf.data()
 
    if (is.null(current.clickDT())) {
      
      return()
      
    } else if (!is.null(current.clickDT())) {
      
      leafletProxy("FMDBreport") %>%        
        
        flyToBounds(lng1 = unname(st_bbox(map.dept[current.clickDT(),])[1]),
                    lng2 = unname(st_bbox(map.dept[current.clickDT(),])[3]),
                    lat1 = unname(st_bbox(map.dept[current.clickDT(),])[2]),
                    lat2 = unname(st_bbox(map.dept[current.clickDT(),])[4])
        )
    }
    
  })
  
  
  
  # WIP : Hold off for the time being
  
  # # Create a PDF to export
  # output$report = downloadHandler(
  #   filename = function() {"FMDB_report.pdf"},
  #   content = function(file) {
  #     pdf(file, onefile = TRUE)
  #     grid.arrange(grobs = lapply(list(report.elements$FMDB,
  #                                      report.elements$tableLT,
  #                                      report.elements$plotly_histogram,
  #                                      report.elements$plotly_graph,
  #                                      report.elements$FMDBreport), 
  #                                 grobTree), 
  #                  ncol = 2) 
  #     dev.off()
  #   }
  # )
  
  #
  # # Make and download the report card
  # output$report <- downloadHandler(
  #   
  #   filename = "report.pdf",
  #   
  #   content = function(file) {
  #     
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(n = input$slider)
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  # })
  
  
}

shinyApp(ui = ui, server = server)

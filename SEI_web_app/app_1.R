
# Caricamento librerie ----
library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library(dplyr)
library(readxl)
library(rpart)
library(rpart.plot)
library(ResourceSelection)
library(shinythemes)
library(sp)
library(terra)
library(sf)
library(plotly)
library(raster)
library(tidyverse)
library(stars)
library(ggspatial)


# Caricamento dati ----
dati_csv <- read.csv("Data/Dati.csv", sep = ";", header = TRUE, 
                 na.strings = "", dec = ",")
dati_xls <- read_excel("Data/Dati.xlsx")

robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## WORLD SHAPEFILE ----
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

world_sf <- rnaturalearth::ne_countries(
    type = "countries", scale = "small"
) |>
    sf::st_as_sf(wkt = "geometry") |>
    sf::st_transform(crsLONGLAT)|>
    sf::st_set_crs(crsLONGLAT)
st_crs(world_sf)
head(world_sf)
names(world_sf)
plot(sf::st_geometry(world_sf))

world_sf_no_antartica <- world_sf |>
    dplyr::filter(region_un != "Antarctica") |>
    dplyr::select(iso_a2, name)

plot(sf::st_geometry(world_sf_no_antartica))

world_sf_no_antartica[44,]$iso_a2 <-  "FR"
world_sf_no_antartica[22,]$iso_a2 <-  "NO"
world_sf_no_antartica[167,]$iso_a2 <-  "SO"

# Trasformazioni dei dati ----

# JOIN ----
join <- left_join(world_sf_no_antartica, dati_xls, by = c("iso_a2" = "ISO"), multiple = "all")
join <- join |>
  st_as_sf() |>
  st_transform(robinson_crs)


Info <- paste0(
  "\nStato: ", join$name,
  "\nAnno dell'evento: ", join$Year,
  "\nMega evento: ", join$Event,
  "\nImpatto dell'evento: ", join$`Impatto totale`)
Info[1:2]

join$Info <- Info

# Fin qua ci siamo, devo capire come rendere selezionabile un mega evento alla volta

as.numeric(join$`Impatto totale`)
vmin <- min(join$`Impatto totale`, na.rm = TRUE, finite = TRUE)
vmax <- max(join$`Impatto totale`, na.rm = TRUE, finite = TRUE)
brk <- round(classInt::classIntervals(
    join$`Impatto totale`,
  n = 3,
  style = "quantile"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- c('#ff0000', '#ffa500', '#72d65d', '#00a2d2')

# Definizione dell'interfaccia Shiny
ui <- fluidPage(
  titlePanel("Web App Mappa Eventi"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mega_evento", "Seleziona Mega Evento:", choices = unique(join$Event))
    ),
    
    mainPanel(
      leafletOutput("mappa"),
      dataTableOutput("tabella")
    )
  )
)

# Funzione di server Shiny
server <- function(input, output) {
  filtered_join <- reactive({
    filter(join, Event == input$mega_evento)
  })
  
  output$mappa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addPolygons(data = filtered_join(), fillColor = "red", fillOpacity = 0.8,
                  color = "black", weight = 1)
  })
  
  output$tabella <- renderDataTable({
    filtered_join() %>%
      dplyr::select(Stato = name, `Anno dell'evento` = Year, `Mega evento` = Event, `Impatto dell'evento` = `Impatto totale`)
  })
}

# Esecuzione dell'applicazione Shiny
shinyApp(ui = ui, server = server)


# IL MIGLIORE PER ORA-----
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(plotly)

# Caricamento dati
dati_xls <- read_excel("Data/Dati.xlsx")

robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

world_sf <- rnaturalearth::ne_countries(type = "countries", scale = "small") |>
  sf::st_as_sf(wkt = "geometry") |>
  sf::st_transform(crsLONGLAT) |>
  sf::st_set_crs(crsLONGLAT)

world_sf_no_antartica <- world_sf |>
  dplyr::filter(region_un != "Antarctica") |>
  dplyr::select(iso_a2, name)

world_sf_no_antartica[44, ]$iso_a2 <- "FR"
world_sf_no_antartica[22, ]$iso_a2 <- "NO"
world_sf_no_antartica[167, ]$iso_a2 <- "SO"

# Trasformazioni dei dati
join <- left_join(world_sf_no_antartica, dati_xls, by = c("iso_a2" = "ISO"), multiple = "all")
join <- join |>
  st_as_sf() |>
  st_transform(robinson_crs)

Info <- paste0(
  "\nStato: ", join$name,
  "\nAnno dell'evento: ", join$Year,
  "\nMega evento: ", join$Event,
  "\nImpatto dell'evento: ", join$`Impatto totale`
)
Info[1:2]

join$Info <- Info

as.numeric(join$`Impatto totale`)
vmin <- min(join$`Impatto totale`, na.rm = TRUE, finite = TRUE)
vmax <- max(join$`Impatto totale`, na.rm = TRUE, finite = TRUE)
brk <- round(classInt::classIntervals(
  join$`Impatto totale`,
  n = 3,
  style = "quantile"
)$brks, 1) |>
  head(-1) |>
  tail(-1) |>
  append(vmax)
breaks <- c(vmin, brk)

cols <- c('#ff0000', '#ffa500', '#72d65d', '#00a2d2')

# Definizione dell'interfaccia Shiny
ui <- fluidPage(
  titlePanel("Web App Mappa Eventi"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mega_evento", "Seleziona Mega Evento:", choices = c("Africa Cup", "Alpine Skiing World", "Asian Beach Games", "Asian Games", 
                                                                       "Commonwealth Games", "European Games", "Expo", "Fisu University Summer",
                                                                       "Fisu University Winter", "Mediterranean Games", "Olympic Summer",
                                                                       "Olympic Winter", "World Cup", "World Games", "World Swimming", "Youth Games"), selected = "Olympic Summer")
    ),
    mainPanel(
      plotlyOutput("mappa"),
      dataTableOutput("tabella")
    )
  )
)

# Funzione di server Shiny
server <- function(input, output) {
  filtered_join <- reactive({
    filter(join, Event == input$mega_evento)
  })
  
  output$mappa <- renderPlotly({
    ggsf_filtered <- ggplot(filtered_join()) +
      geom_sf(aes(fill = `Impatto totale`, label = Info)) +
      scale_fill_gradient2(low = "#ff0000", mid = '#ffa500', high = '#72d65d', na.value = "lightgray") +
      labs(title = "Sport Event Impact", fill = "Impatto calcolato") +
      theme_minimal()
    
    ggplotly(ggsf_filtered, tooltip = "label")
  })
  
  output$tabella <- renderDataTable({
    filtered_join() %>%
      dplyr::select(Stato = name, `Anno dell'evento` = Year, `Mega evento` = Event, `Impatto dell'evento` = `Impatto totale`,
                    `Impatto economico` = `impatto economico anno dell'evento`, `Impatto ambientale` = `impatto ambientale anno dell'evento`,
                    `Impatto sociale` = `impatto sociale anno dell'evento`)
  })
}

# Esecuzione dell'applicazione Shiny
shinyApp(ui = ui, server = server)
























# Chat GPT 2 ----
library(shiny)
library(leaflet)
library(DT)
library(sf)
library(dplyr)

# Caricamento dati
dati_csv <- read.csv("Data/Dati.csv", sep = ";", header = TRUE, na.strings = "", dec = ",")
dati_xls <- read_excel("Data/Dati.xlsx")

robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# Trasformazioni dei dati
world_sf_no_antartica <- rnaturalearth::ne_countries(type = "countries", scale = "small") |>
  sf::st_as_sf(wkt = "geometry") |>
  sf::st_transform(crs = robinson_crs) |>
  dplyr::filter(region_un != "Antarctica") |>
  dplyr::select(iso_a2, name)

join <- left_join(world_sf_no_antartica, dati_xls, by = c("iso_a2" = "ISO"), multiple = "all") |>
  st_as_sf() |>
  st_transform(robinson_crs)
join <- join |> st_transform(crsLONGLAT)

# Definizione dell'interfaccia Shiny
ui <- fluidPage(
  titlePanel("Web App Mappa Eventi"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mega_evento", "Seleziona Mega Evento:", choices = c("", unique(join$Event)))
    ),
    
    mainPanel(
      leafletOutput("mappa"),
      dataTableOutput("tabella")
    )
  )
)

# Funzione di server Shiny
server <- function(input, output) {
  filtered_join <- reactive({
    if (input$mega_evento == "") {
      return(NULL)
    } else {
      filter(join, Event == input$mega_evento)
    }
  })
  
  output$mappa <- renderLeaflet({
    if (!is.null(filtered_join())) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addPolygons(data = filtered_join(), fillColor = ~colorNumeric(palette = "RdYlBu", domain = join$`Impatto totale`)(`Impatto totale`), fillOpacity = 0.8,
                    color = "black", weight = 1)
    }
  })
  
  output$tabella <- renderDataTable({
    if (!is.null(filtered_join())) {
      filtered_join() %>%
        dplyr::select(-geometry)
    }
  })
}

# Esecuzione dell'applicazione Shiny
shinyApp(ui = ui, server = server)












# Chat GPT 1 ----


library(shiny)
library(leaflet)
library(DT)
library(readxl)
library(sf)
library(plotly)
library(dplyr)
library(shinyjs)

# Caricamento dati ----
dati_csv <- read.csv("Data/Dati.csv", sep = ";", header = TRUE, na.strings = "", dec = ",")
dati_xls <- read_excel("Data/Dati.xlsx")

robinson_crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

world_sf <- rnaturalearth::ne_countries(type = "countries", scale = "small") |>
  sf::st_as_sf(wkt = "geometry") |>
  sf::st_transform(crsLONGLAT) |>
  sf::st_set_crs(crsLONGLAT)

world_sf_no_antartica <- world_sf |>
  dplyr::filter(region_un != "Antarctica") |>
  dplyr::select(iso_a2, name)

world_sf_no_antartica[44,]$iso_a2 <- "FR"
world_sf_no_antartica[22,]$iso_a2 <- "NO"
world_sf_no_antartica[167,]$iso_a2 <- "SO"

# JOIN ----
join <- left_join(world_sf_no_antartica, dati_xls, by = c("iso_a2" = "ISO"), multiple = "all") |>
  st_as_sf() |>
  st_transform(robinson_crs)

Info <- paste0(
  "\nStato: ", join$name,
  "\nAnno dell'evento: ", join$Year,
  "\nMega evento: ", join$Event,
  "\nImpatto dell'evento: ", join$`Impatto totale`)
join$Info <- Info

# Creazione della mappa con Plotly ----
mappa <- plot_ly(data = join, color = ~`Impatto totale`, colors = "YlOrRd",
                 split = ~Event, alpha = 1, type = "scatter", mode = "markers",
                 text = ~Info, hoverinfo = "text", marker = list(size = 6)) %>%
  layout(geo = list(projection = list(type = "robinson")))

# Creazione dell'app Shiny ----
ui <- fluidPage(
  useShinyjs(),  # Carica il pacchetto shinyjs
  
  titlePanel("Mappa degli eventi"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("evento", "Seleziona un evento:", choices = unique(dati_csv$Event),
                  selected = unique(dati_csv$Event)[1])  # Imposta il primo evento come selezionato predefinito
    ),
    
    mainPanel(
      plotlyOutput("mappa_output", height = "500px")  # Imposta un'altezza per la mappa
    )
  )
)

server <- function(input, output, session) {
  # Aggiungi un'etichetta di id alla mappa
  output$mappa_output <- renderPlotly({
    join <- join %>%
      st_geometry() %>%
      st_centroid()  # Calcola il centroide di ciascun poligono
    
    plot_ly(data = join, type = "scattermapbox", mode = "markers",
            symbol = ~Event, symbolmap = list(unique(dati_csv$Event)),
            text = ~Info, hoverinfo = "text", marker = list(size = 6)) %>%
      add_trace(
        type = "scattermapbox",
        mode = "lines",
        lat = ~st_coordinates(st_geometry(join))[,"Y"],  # Estrarre le coordinate Y
        lon = ~st_coordinates(st_geometry(join))[,"X"],  # Estrarre le coordinate X
        fill = ~`Impatto totale`,
        colorscale = "Viridis",
        hoverinfo = "skip",
        showlegend = FALSE
      ) %>%
      layout(
        mapbox = list(projection = list(type = "robinson")),
        hovermode = "closest"
      ) %>%
      config(displayModeBar = FALSE)  # Nascondi la barra dei comandi della mappa
  })
  
  # Aggiungi l'evento "click" alla mappa
  observeEvent(input$mappa_output_click, {
    # Ottieni i dati selezionati dall'evento "click" sulla mappa
    selected_data <- event_data("plotly_click", source = "mappa_output")
    
    # Esegui le operazioni desiderate con i dati selezionati
    # ...
  })
}

shinyApp(ui = ui, server = server)
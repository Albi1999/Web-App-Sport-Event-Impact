library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(plotly)

# Caricamento dati
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
            selectInput("mega_evento", "Seleziona Mega Evento:", choices = c("Olympic Summer", "Olympic Winter", "Expo", "World Cup"), selected = "Olympic Summer")
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
























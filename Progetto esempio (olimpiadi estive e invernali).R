# PROGETTO D'ESEMPIO ----
# Mappa dinamica
# Tabella dinamica
# Olimpiadi estive
# Olimpiadi invernali

# CARICAMENTO LIBRERIE ----
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
library(sf)
library(plotly)
library(tidyverse)
library(stars)
library(ggspatial)
library(htmltools)
library(knitr)
library(kableExtra)
library(gt)
library(gtExtras)
library(ggflags)
library(paletteer)
library(systemfonts)

# CARICAMENTO DATI ----
## Totale ----
dati_csv <- read.csv("Data/Dati.csv", sep = ";", header = TRUE, na.strings = "", dec = ",")

## Olimpiadi estive ----
sum <- dati_csv %>%
  dplyr::filter(Event == "Olympic Summer")

## Olimpiadi invernali ----
win <- dati_csv %>%
  dplyr::filter(Event == "Olympic Winter")

# WORLD SHAPEFILE ----
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

plot(world_sf_no_antartica)

# OLIMPIADI ESTIVE ----

## TRASFORMAZIONE DEI DATI ----
join_sum <- left_join(world_sf_no_antartica, sum, by = c("iso_a2" = "ISO"), multiple = "all")
join_sum <- join_sum |>
  st_as_sf() |>
  st_transform(robinson_crs)

Info <- paste0(
  "\nStato: ", join_sum$name,
  "\nAnno dell'evento: ", join_sum$Year,
  "\nMega evento: ", join_sum$Event,
  "\nImpatto dell'evento: ", join_sum$Impatto.totale
)
Info[1:2]

join_sum$Info <- Info


## MAPPA DINAMICA ----
ggsf_sum <- ggplot(join_sum) +
  geom_sf(aes(fill = Impatto.totale, label = Info)) +
  scale_fill_gradient2(low = "#ff0000", mid = '#72d65d', high = '#00a2d2', na.value = "lightgray") +
  labs(title = "Impatto Olimpiadi Estive", fill = "Impatto\nTotale") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5), 
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggplotly(ggsf_sum, tooltip = "label")

## TABELLA DINAMICA ----




sum <- sum %>%
  mutate(
  flag = case_when(
    ISO == "CA" ~ "Flag/ca.png",
    ISO == "US" ~ "Flag/us.png",
    ISO == "RU" ~ "Flag/ru.png",
    ISO == "MX" ~ "Flag/mx.png",
    ISO == "BR" ~ "Flag/br.png",
    ISO == "FR" ~ "Flag/fr.png",
    ISO == "KR" ~ "Flag/kr.png",
    ISO == "DE" ~ "Flag/de.png",
    ISO == "GR" ~ "Flag/gr.png",
    ISO == "ES" ~ "Flag/es.png",
    ISO == "AU" ~ "Flag/au.png",
    ISO == "CN" ~ "Flag/cn.png",
    ISO == "IT" ~ "Flag/it.png",
    ISO == "GB" ~ "Flag/gb.png",
    ISO == "FI" ~ "Flag/fi.png",
    ISO == "JP" ~ "Flag/jp.png",
))%>%
  select(flag, everything())
datatable(sum)

datatable(sum,
          options = list(paging = TRUE,    ## paginate the output
                         pageLength = 15,  ## number of rows to output for each page
                         scrollX = TRUE,   ## enable scrolling on X axis
                         scrollY = TRUE,   ## enable scrolling on Y axis
                         autoWidth = TRUE, ## use smart column width handling
                         server = FALSE,   ## use client-side processing
                         dom = 'Bfrtip',
                         buttons = c('csv', 'excel'),
                         columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                           list(targets = c(0, 8, 9), visible = FALSE))
          ),
          extensions = 'Buttons',
          selection = 'single', ## enable selection of a single row
          filter = 'bottom',              ## include column filters at the bottom
          rownames = FALSE                ## don't show row numbers/names
)


table<-sum%>%
  select(flag, ISO, Year, Event, impatto.economico.anno.dell.evento, impatto.ambientale.anno.dell.evento, impatto.sociale.anno.dell.evento, Impatto.totale)%>%
  gt()%>%
  gt_img_rows(flag)%>%
  gt_color_rows(Impatto.totale, palette = "ggsci::blue_material")%>%
  gt_theme_espn()%>%
  cols_align(
    align = "center",
    columns = c(Impatto.totale, flag)
  )%>%
  tab_footnote("Impatto olimpiadi estive",
               locations = cells_column_labels(columns = ISO))%>%
  tab_options(heading.title.font.size = 20)%>%
  tab_header(title = "IMPATTO OLIMPIADI ESTIVE",
             subtitle= "Impatto calcolato")%>%
  tab_source_note("Data from SBL Consultancy | Table Graphic by Alberto Calabrese")

table

# Da modificare




# OLIMPIADI INVERNALI ----

## TRASFORMAZIONE DEI DATI ----
join_win <- left_join(world_sf_no_antartica, win, by = c("iso_a2" = "ISO"), multiple = "all")
join_win <- join_win |>
  st_as_sf() |>
  st_transform(robinson_crs)

Info <- paste0(
  "\nStato: ", join_win$name,
  "\nAnno dell'evento: ", join_win$Year,
  "\nMega evento: ", join_win$Event,
  "\nImpatto dell'evento: ", join_win$Impatto.totale
)
Info[1:2]

join_win$Info <- Info

## MAPPA DINAMICA ----
ggsf_win <- ggplot(join_win) +
  geom_sf(aes(fill = Impatto.totale, label = Info)) +
  scale_fill_gradient2(low = "#ff0000", mid = '#72d65d', high = '#00a2d2', na.value = "lightgray") +
  labs(title = "Impatto Olimpiadi Invernali", fill = "Impatto\nTotale") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5), 
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggplotly(ggsf_win, tooltip = "label")

## TABELLA DINAMICA ----





# WEB APP ----

# Definizione dell'interfaccia Shiny
ui <- fluidPage(
  titlePanel("Web App Mappa Eventi"),
  theme = shinytheme("cerulean"),
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




# BANDIERE ----

# Lista delle coppie ISO e URL delle bandiere mancanti
flag_data <- data.frame(
  ISO = c("BG", "BY", "CH", "CI", "CL", "CM", "CO", "CZ", "DZ", "EC", "EG",
          "ET", "GA", "GH", "GN", "GQ", "HR", "HU", "ID", "IN", "IR", "NG",
          "NL", "NO", "OM", "PH", "PL", "РТ", "GA", "RO", "RS", "SD", "SG",
          "SK", "SN", "SY", "TH", "TN", "TR", "VN", "ZA"),
  flag = c("Flag/bg.png", "Flag/by.png", "Flag/ch.png", "Flag/ci.png",
           "Flag/cl.png", "Flag/cm.png", "Flag/co.png", "Flag/cz.png",
           "Flag/dz.png", "Flag/ec.png", "Flag/eg.png", "Flag/et.png",
           "Flag/ga.png", "Flag/gh.png", "Flag/gn.png", "Flag/gq.png",
           "Flag/hr.png", "Flag/hu.png", "Flag/id.png", "Flag/in.png",
           "Flag/ir.png", "Flag/ng.png", "Flag/nl.png", "Flag/no.png",
           "Flag/om.png", "Flag/ph.png", "Flag/pl.png", "Flag/pt.png",
           "Flag/ga.png", "Flag/ro.png", "Flag/rs.png", "Flag/sd.png",
           "Flag/sg.png", "Flag/sk.png", "Flag/sn.png", "Flag/sy.png",
           "Flag/th.png", "Flag/tn.png", "Flag/tr.png", "Flag/vn.png",
           "Flag/za.png")
)

# Uniamo i dati usando left_join
data_join <- left_join(join, flag_data, by = c("iso_a2" = "ISO"), relationship = "many-to-many")
# Problemi ma più o meno ci siamo !!


# Visualizziamo la tabella
datatable(sum)

join <- join %>%
  mutate(
    flag = case_when(
      iso_a2 == "AE" ~ "Flag/ae.png",
      iso_a2 == "AO" ~ "Flag/ao.png",
      iso_a2 == "AR" ~ "Flag/ar.png",
      iso_a2 == "AT" ~ "Flag/at.png",
      iso_a2 == "AU" ~ "Flag/au.png",
      iso_a2 == "AZ" ~ "Flag/az.png",
      iso_a2 == "BA" ~ "Flag/ba.png",
      iso_a2 == "BF" ~ "Flag/bf.png",
      iso_a2 == "BG" ~ "Flag/bg.png",
      iso_a2 == "BR" ~ "Flag/br.png",
      iso_a2 == "BY" ~ "Flag/by.png",
      iso_a2 == "CA" ~ "Flag/ca.png",
      iso_a2 == "CH" ~ "Flag/ch.png",
      iso_a2 == "CI" ~ "Flag/ci.png",
      iso_a2 == "CL" ~ "Flag/cl.png",
      iso_a2 == "CM" ~ "Flag/cm.png",
      iso_a2 == "CN" ~ "Flag/cn.png",
      iso_a2 == "CO" ~ "Flag/co.png",
      iso_a2 == "CZ" ~ "Flag/cz.png",
      iso_a2 == "DE" ~ "Flag/de.png",
      iso_a2 == "DZ" ~ "Flag/dz.png",
      iso_a2 == "EC" ~ "Flag/ec.png",
      iso_a2 == "EG" ~ "Flag/eg.png",
      iso_a2 == "ES" ~ "Flag/es.png",
      iso_a2 == "ET" ~ "Flag/et.png",
      iso_a2 == "FI" ~ "Flag/fi.png",
      iso_a2 == "FR" ~ "Flag/fr.png",
      iso_a2 == "GA" ~ "Flag/ga.png",
      iso_a2 == "GB" ~ "Flag/gb.png",
      iso_a2 == "GH" ~ "Flag/gh.png",
      iso_a2 == "GN" ~ "Flag/gn.png",
      iso_a2 == "GQ" ~ "Flag/gq.png",
      iso_a2 == "GR" ~ "Flag/gr.png",
      iso_a2 == "HR" ~ "Flag/hr.png",
      iso_a2 == "HU" ~ "Flag/hu.png",
      iso_a2 == "ID" ~ "Flag/id.png",
      iso_a2 == "IN" ~ "Flag/in.png",
      iso_a2 == "IR" ~ "Flag/ir.png",
      iso_a2 == "IT" ~ "Flag/it.png",
      iso_a2 == "JM" ~ "Flag/jm.png",
      iso_a2 == "JP" ~ "Flag/jp.png",
      iso_a2 == "KR" ~ "Flag/kr.png",
      iso_a2 == "KZ" ~ "Flag/kr.png",
      iso_a2 == "LB" ~ "Flag/lb.png",
      iso_a2 == "LU" ~ "Flag/lu.png",
      iso_a2 == "LY" ~ "Flag/ly.png",
      iso_a2 == "MA" ~ "Flag/ma.png",
      iso_a2 == "ML" ~ "Flag/ml.png",
      iso_a2 == "MX" ~ "Flag/mx.png",
      iso_a2 == "MY" ~ "Flag/my.png",
      iso_a2 == "NG" ~ "Flag/ng.png",
      iso_a2 == "NL" ~ "Flag/nl.png",
      iso_a2 == "NO" ~ "Flag/no.png",
      iso_a2 == "NZ" ~ "Flag/nz.png",
      iso_a2 == "OM" ~ "Flag/om.png",
      iso_a2 == "PH" ~ "Flag/ph.png",
      iso_a2 == "PL" ~ "Flag/pl.png",
      iso_a2 == "PT" ~ "Flag/pt.png",
      iso_a2 == "QA" ~ "Flag/qa.png",
      iso_a2 == "RO" ~ "Flag/ro.png",
      iso_a2 == "RS" ~ "Flag/rs.png",
      iso_a2 == "RU" ~ "Flag/ru.png",
      iso_a2 == "SD" ~ "Flag/sd.png",
      iso_a2 == "SG" ~ "Flag/sg.png",
      iso_a2 == "SK" ~ "Flag/sk.png",
      iso_a2 == "SN" ~ "Flag/sn.png",
      iso_a2 == "SY" ~ "Flag/sy.png",
      iso_a2 == "TH" ~ "Flag/th.png",
      iso_a2 == "TN" ~ "Flag/tn.png",
      iso_a2 == "TR" ~ "Flag/tr.png",
      iso_a2 == "US" ~ "Flag/us.png",
      iso_a2 == "VN" ~ "Flag/vn.png",
      iso_a2 == "ZA" ~ "Flag/za.png"
    ))%>%
  select(flag, everything())
datatable(join)


table<-join%>%
  select(flag, iso_a2, Year, Event, `impatto economico anno dell'evento`, 
         `impatto ambientale anno dell'evento` , `impatto sociale anno dell'evento`, 
         `Impatto totale`)%>%
  gt()%>%
  gt_img_rows(flag)%>%
  gt_color_rows(`Impatto totale`, palette = "ggsci::blue_material")%>%
  gt_theme_espn()%>%
  cols_align(
    align = "center",
    columns = c(`Impatto totale`, flag)
  )%>%
  tab_footnote("Impatto maga eventi",
               locations = cells_column_labels(columns = iso_a2))%>%
  tab_options(heading.title.font.size = 20)%>%
  tab_header(title = "SPORT EVENT IMPACT",
             subtitle= "Impatto calcolato")%>%
  tab_source_note("Data from SBL Consultancy | Table Graphic by Alberto Calabrese")

table
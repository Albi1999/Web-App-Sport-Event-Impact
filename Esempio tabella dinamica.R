library(dplyr)
items <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv'
  ) %>% select(-num_id,-sell_currency,-buy_currency,-games_id,-id_full) %>%
  unique() %>%
  head(100)  
villagers <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv'
  ) %>% 
  select(-row_n) %>% 
  unique()

library(DT)
datatable(villagers[,1:8])

datatable(villagers,
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

library(shiny)
icon("thumbs-up", lib = "font-awesome")
as.character(icon("thumbs-up", lib = "font-awesome"))

icon_villagers <- villagers %>%
  mutate(gender = ifelse(gender == "female",
                         as.character(icon("venus", lib = "font-awesome")),
                         as.character(icon("mars", lib = "font-awesome"))))
datatable(icon_villagers[,1:8],
          options = list(scrollX = TRUE),
          escape = FALSE)

pic_villagers <- villagers %>%
  mutate(
    picture = paste0(
      "<img src=\"",
      url,
      "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"",
      species,
      "\"></img>"
    )
  ) %>%
  select(picture, name, gender, species, birthday, personality, phrase) %>%
  unique()

datatable(pic_villagers,
          options = list(scrollX = TRUE),
          escape = FALSE,
          rownames = FALSE)

ui <- fluidPage(titlePanel("DT table in Shiny"),
                mainPanel(width = 12,
                          DT::dataTableOutput("mytable")))

server <- function(input, output) {
  output$mytable <- DT::renderDataTable(villagers,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)



library(formattable)

## Prepare the item data
pic_items <- items %>%
  mutate(picture = ifelse(!is.na(image_url), 
                          paste0("<img src=\"",image_url, "\" height=\"30\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"", name, "\"></img>"), 
                          as.character(icon("question", lib = "font-awesome")))) %>%
  select(picture, name, category, orderable, sell_value, buy_value) %>%
  unique()

## A formatter function to format TRUE/FALSE values
true_false_formatter <-
  formatter("span",
            style = x ~ style(
              font.weight = "bold",
              color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
            ))

## Use formattable
formattable(
  head(pic_items),
  list(
    ## colour this column in a white to pink gradiant by value
    `sell_value` = color_tile("white", "pink"),
    
    ## a coloured bar with length proportional to value
    `buy_value` = color_bar("lightblue"),
    
    ## use custom formatter for TRUE/FALSE values
    orderable = true_false_formatter
  )
)


formattable(
  pic_items,
  list(
    `sell_value` = color_tile("white", "pink"),
    `buy_value` = color_bar("lightblue"),
    orderable = true_false_formatter
  )
) %>%
  as.datatable(escape = FALSE,
               options = list(scrollX = TRUE),
               rownames = FALSE)


library(reactable)
reactable(items[, 2:7],
          columns = list(
            sell_value = colDef(align = "right",
                                na = "-",
                                format = colFormat(suffix = " Bells"))
          ))

## Colour and values for table colour formatting
brks <- seq(5, 320000, 1000)
clrs <- colorRampPalette(c("white", "#6baed6"))(length(brks) + 1)


datatable(pic_items,
          options = list(scrollX = TRUE,
                         order = list(list(5, 'desc'))),
          escape = FALSE,
          rownames = FALSE) %>%
  formatStyle(c("sell_value", "buy_value"), backgroundColor = styleInterval(brks, clrs))


datatable(pic_items,
          options = list(scrollX = TRUE,
                         columnDefs = list(
                           list(
                             targets = 1,
                             render = JS(
                               "function(data, type, row, meta) {",
                               "return type === 'display' && data.length > 10 ?",
                               "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                               "}")))),
          escape = FALSE,
          rownames = FALSE)


headerCallbackRemoveHeaderFooter <- c(
  "function(thead, data, start, end, display){",
  "  $('th', thead).css('display', 'none');",
  "}"
)

datatable(
  my_pic_villagers,
  options = list(
    dom = "t",
    ordering = FALSE,
    paging = FALSE,
    searching = FALSE,
    headerCallback = JS(headerCallbackRemoveHeaderFooter)
  ),
  selection = 'none',
  callback = JS(
    "$('table.dataTable.no-footer').css('border-bottom', 'none');"
  ),
  class = 'row-border',
  escape = FALSE,
  rownames = FALSE,
  filter = "none",
  width = 500
)

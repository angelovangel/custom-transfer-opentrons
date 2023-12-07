library(rhandsontable)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(tibble)
library(stringr)
library(reactable)
library(dplyr)
library(rmarkdown)
library(vroom)
library(plater)

wells_colwise <- function(cols, rows)
{
  lapply(1:cols, function(x) {str_c(LETTERS[1:rows], x)}) %>% 
    unlist()
}

# used to generate a plater::view_plate()
make_plate <- function(cols, rows) {
  nwells <- cols * rows
  df <- tibble(
    id = wells_colwise(cols, rows),
    label = seq(nwells)
  )
  plate <- plater::view_plate(df, well_ids_column = 'id', columns_to_display = 'label', plate_size = nwells)
  plate$label
}

# used to generate main table - hot
make_hot <- function(scols, srows, dcols, drows) {
  swells <- scols * srows
  dwells <- dcols * drows
  
  data.frame(
    source_well = wells_colwise(scols, srows),
    dest_well = wells_colwise(dcols, drows),
    vol = 0
  )
}

tab1 <- fluidRow(
  box(width = 12, status = "warning", solidHeader = FALSE, title = 'Source and destination labware',
      fluidRow(
        column(6,
               pickerInput(
                 'source_labware', 'Source labware', 
                 choices = select_labware
                 #choicesOpt = list(content = labware$img_icon)
                 )
               #imageOutput('source_img')
               ),
        column(6, 
               pickerInput(
                 'dest_labware', 'Destination labware', 
                 choices = select_labware
                 #choicesOpt = list(content = labware$img_icon)
                )
               #imageOutput('dest_img', width = 200, height = 200)
               )
      ),
      fluidRow(
        column(6, rHandsontableOutput('hot')),
        column(6,
               tags$p('Source preview'),
               reactableOutput('source_plate'),
               tags$p('Destination preview'),
               reactableOutput('dest_plate')
               )
      )
  )
)
ui = dashboardPage(
        skin = 'red',
        header = dashboardHeader(),
        sidebar = dashboardSidebar(),
        body = dashboardBody(
          useShinyjs(),
          tabsetPanel(
            tabPanel(title = "Enter data", icon = icon("list"), tab1)
          )
        )
      )

server = function(input, output, session) {
  
  protocol_url <- "https://raw.githubusercontent.com/angelovangel/opentrons/main/protocols/10-custom-transfer.py"
  if (curl::has_internet()) {
    con <- url(protocol_url)
    protocol_template <- readLines(con, warn = F)
    close(con)
  } else {
    protocol_template <- readLines('10-custom-transfer.py', warn = F)
  }
  
  # Reactives
  
  source_react <- reactive({
    selected_labware <- labware[labware$id == input$source_labware, , drop = FALSE]
    cols <- selected_labware$cols
    rows <- selected_labware$rows
    make_plate(cols = cols, rows = rows)
  })
  
  dest_react <- reactive({
    selected_labware <- labware[labware$id == input$dest_labware, , drop = FALSE]
    cols <- selected_labware$cols
    rows <- selected_labware$rows
    make_plate(cols = cols, rows = rows)
  })
  
  hot <- reactive({
    selected_src <- labware[labware$id == input$source_labware, , drop = FALSE]
    selected_dest <- labware[labware$id == input$dest_labware, , drop = FALSE]
    make_hot(
      scols = selected_src$cols, 
      srows = selected_src$rows, 
      dcols = selected_dest$cols, 
      drows = selected_dest$rows
      )
  })
  
  # Outputs
  output$source_plate <- renderReactable({
    reactable(
      source_react(),
      highlight = T, wrap = F, bordered = T, compact = T, fullWidth = F, sortable = F, pagination = F,
      columns = list(.rownames = colDef(style = list(color = 'black', fontSize = '90%'))),
      defaultColDef =
        colDef(
          style = list(color = 'grey', fontSize = '90%'),
          minWidth = 40,
          html = TRUE,
          headerStyle = list(background = "#f7f7f8", fontSize = '90%')
        )
    )
  })
  
  output$dest_plate <- renderReactable({
    reactable(
      dest_react(),
      highlight = T, wrap = F, bordered = T, compact = T, fullWidth = F, sortable = F, pagination = F,
      columns = list(.rownames = colDef(style = list(color = 'black', fontSize = '90%'))),
      defaultColDef =
        colDef(
          style = list(color = 'grey', fontSize = '90%'),
          minWidth = 40,
          html = TRUE,
          headerStyle = list(background = "#f7f7f8", fontSize = '90%')
        )
    )
  })
  
  output$hot <- renderRHandsontable({
    rhandsontable(hot())
  })
}

shinyApp(ui, server)

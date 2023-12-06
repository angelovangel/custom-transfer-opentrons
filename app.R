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

wells_colwise <- function(cols, rows)
{
  lapply(1:cols, function(x) {str_c(LETTERS[1:rows], x)}) %>% 
    unlist()
}

tab1 <- fluidRow(
  box(width = 12, status = "warning", solidHeader = FALSE, title = 'Source and destination labware',
      fluidRow(
        column(6,
               pickerInput(
                 'source_labware', 'Source labware', 
                 choices = labware$id,
                 choicesOpt = list(content = labware$img_icon)
                 )
               #imageOutput('source_img')
               ),
        column(6, 
               pickerInput(
                 'dest_labware', 'Destination labware', 
                 choices = labware$id,
                 choicesOpt = list(content = labware$img_icon)
                )
               #imageOutput('dest_img', width = 200, height = 200)
               )
      ),
      fluidRow(
        column(6, rHandsontableOutput('hot1')),
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
  # OUTPUTS
  # output$source_img <- renderImage({
  #   imgpath <- labware[labware$id == input$source_labware, , drop = FALSE]
  #   list(
  #     src = file.path('www', imgpath$img),
  #     contentType = "image/jpeg", width = 200, height = 200
  #   )  
  # })
  # 
  output$hot1 <- renderRHandsontable({
    rhandsontable(mtcars)
  })
}

shinyApp(ui, server)

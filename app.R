# bslib version
library(rhandsontable)
library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(tibble)
library(stringr)
library(reactable)
library(dplyr)
library(rmarkdown)
library(vroom)
library(plater)
library(shinyjs)
library(processx)

# call wells_colwise(12, 1) to get columns only
wells_colwise <- function(cols, rows)
{
  l <- lapply(1:cols, function(x) {str_c(LETTERS[1:rows], x)}) %>% unlist()
  l
  #factor(l, levels = l)
}

# used to generate a plater::view_plate()
make_plate <- function(cols, rows, content, multi) {
  nwells <- cols * rows
  if(multi & rows == 16) {
    # special case for 384 well plate, pipetting is done A1, B1, A2, B2 ...
    content <- lapply(seq(2, cols*2, by = 2), function(x) rep(content[(x-1):x], times = 8)) %>% unlist
  } else if (multi) {
    content <- rep(content, each = rows)
  } else {
    content <- content
  }
  length(content) <- nwells
  #wellcontent <- str_replace_na(wellcontent, replacement = ".")
  df <- tibble(
    id = wells_colwise(cols, rows),
    label = seq(nwells),
    wellcontent = content
  )
  plate <- plater::view_plate(df, well_ids_column = 'id', columns_to_display = 'wellcontent', plate_size = nwells)
  plate$wellcontent
}

# used to generate main table - hot
make_hot <- function(scols, srows, dcols, drows) {
  swells <- scols * srows
  dwells <- dcols * drows
  # # fill with NA, let the user decide how to fill shorter labware
  source_well <- wells_colwise(scols, srows)
  dest_well <- wells_colwise(dcols, drows)
  if (swells > dwells) {
    length(dest_well) <- length(source_well) 
  } else if (swells < dwells) {
    length(source_well) <- length(dest_well)
  }
  tibble(
    source_well = source_well,
    dest_well = dest_well,
    vol = 0
  )
}
# input controls
pipets <- list(
  selectizeInput(
    'active_pipet', 
    label = tooltip(
      trigger = list('Active pipet', bs_icon('info-circle')), 'Select the pipet to use'
      ), 
    choices = list('Left (single channel)' = 'left', 'Right (multi channel)' = 'right'), 
    selected = 'left'),
selectizeInput('left_pipette', 'Left', choices = c('p20_single_gen2', 'p300_single_gen2')),
selectizeInput('right_pipette', 'Right', choices = c('p20_multi_gen2', 'p300_multi_gen2'))
)

pipetting <- list(
  selectizeInput(
    'pipetting_type', 
    'Pipetting type',
    choices = c('transfer', 'distribute', 'consolidate'), 
    selected = 'transfer', multiple = F),
  uiOutput('mix'),
  numericInput('airgap', 'Air gap', value = 0, min = 0, max = 200, step = 1),
  selectizeInput('newtip', 'New tip', choices = c('always', 'once'), selected = 'always')
)
sidebar <- sidebar(
  accordion(
    open = T,
    accordion_panel(title = 'Select pipet', icon = bsicons::bs_icon('crosshair'), pipets),
    accordion_panel(title = 'Adjust pipetting', icon = bsicons::bs_icon('sliders2'), pipetting)
  )
  #downloadButton('download_script', 'Download script') #style = 'margin-left:15px; margin-top:15px; color: #444;'),
)


panel1 <- list(
  
  pickerInput(
    width = '90%',
    'source_labware', 'Source labware', 
    choices = select_labware, selected = 'biorad_96_wellplate_200ul_pcr'
    #choicesOpt = list(content = labware$img_icon)
  ),
  pickerInput(
    width = '90%',
    'dest_labware', 'Destination labware', 
    choices = select_labware, selected = 'biorad_96_wellplate_200ul_pcr'
    #choicesOpt = list(content = labware$img_icon)
  ),
  
    #card_header('Pipetting scheme'),
    tags$div(
      tags$a('Volumes', tooltip(bs_icon("info-circle"), 'Enter source/destination wells and volumes to be pipetted')),
      rHandsontableOutput('hot')
    ),
    
    
    tags$div(
      tags$a('Source preview', tooltip(bs_icon("info-circle"), 'Total volumes aspirated for each well are shown in red')),
      reactableOutput('source_plate'),
      tags$hr(),
      tags$a('Destination preview', tooltip(bs_icon("info-circle"), 'Total volumes dispensed for each well are shown in green')),
      reactableOutput('dest_plate')
    )
)


ui <- page_navbar(
  useShinyjs(),
  fillable = T,
  title = 'Custom transfer OT2',
  theme = bs_theme(font_scale = 0.9, bootswatch = 'yeti', primary = '#2E86C1'),
  sidebar = sidebar,
  #nav_spacer(),
  nav_panel(
    'Labware and volumes', 
    layout_columns(
      col_widths = c(6, 6, 3, 9), panel1[[1]], panel1[[2]], panel1[[3]], panel1[[4]])
    ),
  nav_panel('Protocol preview', verbatimTextOutput('protocol_preview')),
  nav_panel(
      'Simulate run',
      actionButton('simulate', 'Run simulation', width = '25%'),
      uiOutput('pipet_valuebox'),
      verbatimTextOutput('stdout')
  ),
  nav_panel('Deck view', htmlOutput('deck')),
  nav_panel('Download script', 
            downloadButton('download_script', 'Download script', style = 'width: 25%'))
  #nav_item(downloadLink('download_script', 'Download script'))
)

server <- function(input, output, session) {
  # add opentrons_simulate path
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(old_path, Sys.getenv('OPENTRONS_PATH'), sep = ":"))
  
  protocol_template <- readLines('10-custom-transfer.py', warn = F)
  
  # Reactives
  hot <- reactive({
    selected_src <- labware[labware$id == input$source_labware, , drop = FALSE]
    selected_dest <- labware[labware$id == input$dest_labware, , drop = FALSE]
    make_hot(
      scols = selected_src$cols,
      # columns only if multichannel
      srows = 
        case_when(
          input$active_pipet == 'right' & selected_src$nwells == 384 ~ 2,
          input$active_pipet == 'right' ~ 1, 
          TRUE ~ selected_src$rows), 
      dcols = selected_dest$cols, 
      drows = 
        case_when(
          input$active_pipet == 'right' & selected_dest$nwells == 384 ~ 2,
          input$active_pipet == 'right' ~ 1, 
          TRUE ~ selected_dest$rows)
    )
  })
  
  source_react <- reactive({
    selected_labware <- labware[labware$id == input$source_labware, , drop = FALSE]
    cols <- selected_labware$cols
    rows <- selected_labware$rows
    
    if (!is.null(input$hot)) {
      ht <- as_tibble(hot_to_r(input$hot))
      agg <- aggregate(vol ~ source_well, data = ht, sum)
      
      if (input$active_pipet == 'right' & selected_labware$nwells == 384) {
        allwells <- tibble(source_well = wells_colwise(cols, 2))
      } else if (input$active_pipet == 'right') {
        allwells <- tibble(source_well = wells_colwise(cols, 1))
      } else {
        allwells <- tibble(source_well = wells_colwise(cols, rows))
      }
      
      allvols <- left_join(allwells, agg, by = 'source_well')
      #print(allvols, n=100)
      content <- str_replace_na(allvols$vol, '0')
      
      make_plate(
        cols = cols, 
        rows = rows, 
        #content = ht$vol,
        content = content, 
        multi = if_else(input$active_pipet != 'left', TRUE, FALSE)
      ) 
    }
  })
  
  dest_react <- reactive({
    selected_labware <- labware[labware$id == input$dest_labware, , drop = FALSE]
    cols <- selected_labware$cols
    rows <- selected_labware$rows
    if(!is.null(input$hot)) {
      ht <- as_tibble(hot_to_r(input$hot))
      agg <- aggregate(vol ~ dest_well, data = ht, sum)
      
      if (input$active_pipet == 'right' & selected_labware$nwells == 384) {
        allwells <- tibble(dest_well = wells_colwise(cols, 2))
      } else if (input$active_pipet == 'right') {
        allwells <- tibble(dest_well = wells_colwise(cols, 1))
      } else {
        allwells <- tibble(dest_well = wells_colwise(cols, rows))
      }
      
      allvols <- left_join(allwells, agg, by = 'dest_well')
      content <- str_replace_na(allvols$vol, '0')
      
      make_plate(
        cols = cols, 
        rows = rows, 
        content = content,
        multi = if_else(input$active_pipet != 'left', TRUE, FALSE)
      )
    }
  })
  
  # CORE functionality
  myvalues <- reactive({
    hot_table <- as_tibble(hot_to_r(input$hot))
    swells <- hot_table$source_well %>% str_replace_na(replacement = '')
    dwells <- hot_table$dest_well %>% str_replace_na(replacement = '')
    vols <-str_replace_na(hot_table$vol, '0')
    
    # if(input$active_pipet == 'Right (multi channel)') {
    #   swells <- str_extract(swells, pattern = 'A[0-9]+')
    #   dwells <- str_extract(dwells, pattern = 'A[0-9]+')
    # }
    if( input$pipetting_type == 'distribute' ) {
      swells <- swells[swells != ''] #%>% unique()
    }
    if( input$pipetting_type == 'consolidate' ) {
      dwells <- dwells[dwells != ''] #%>% unique()
    }
    c(
      str_flatten(swells, collapse = "','", na.rm = T),
      str_flatten(dwells, collapse = "','", na.rm = T),
      str_flatten(vols, collapse = ", ")
    )
  })
  
  myprotocol <- reactive({
    str_replace(string = protocol_template, 
                pattern = 'source_wells =.*', 
                replacement = paste0("source_wells = ['", myvalues()[1], "']")
    ) %>%
      str_replace(pattern = 'dest_wells =.*', 
                  replacement = paste0("dest_wells = ['", myvalues()[2], "']")
      ) %>%
      str_replace(pattern = 'volumes =.*', 
                  replacement = paste0("volumes = [", myvalues()[3], "]") 
      ) %>%
      str_replace(pattern = "pipetting_type = .*", 
                  replacement = paste0("pipetting_type = ", "'", input$pipetting_type, "'")
      ) %>%
      str_replace(pattern = "newtip = .*", 
                  replacement = paste0("newtip = ", "'",input$newtip, "'")
      ) %>%
      str_replace(pattern = "dest_type = .*", 
                  replacement = paste0("dest_type = ", "'", input$dest_labware, "'")
      ) %>%
      str_replace(pattern = "source_type = .*", 
                  replacement = paste0("source_type = ", "'", input$source_labware, "'")
      ) %>%
      str_replace(pattern = "right_mount = .*", 
                  replacement = paste0("right_mount = ", "'", input$right_pipette, "'")
      ) %>%
      str_replace(pattern = "left_mount = .*", 
                  replacement = paste0("left_mount = ", "'", input$left_pipette, "'")
      ) %>%
      str_replace(pattern = "active_pip = .*", 
                  replacement = paste0("active_pip = ", "'", input$active_pipet, "'")) %>%
      str_replace(pattern = "mytips = .*", 
                  replacement = case_when(
                    input$active_pipet == 'left' & input$left_pipette == 'p20_single_gen2' ~ "mytips = 'opentrons_96_filtertiprack_20ul'",
                    input$active_pipet == 'right' & input$right_pipette == 'p20_multi_gen2' ~ "mytips = 'opentrons_96_filtertiprack_20ul'",
                    TRUE ~ "mytips = 'opentrons_96_filtertiprack_200ul'"
                    )
      ) %>%
      str_replace(pattern = 'mbefore = .*', 
                  replacement = paste0("mbefore = (", input$btimes, ",", input$bmix_vol, ")")) %>%
      str_replace(pattern = 'mafter = .*', 
                  replacement = paste0("mafter = (", input$atimes, ",", input$amix_vol, ")")) %>%
      str_replace(pattern = 'agap = .*', 
                  replacement = paste0('agap = ', input$airgap))
    
  })
  
  # Outputs
  # add appropriate mix according to pipetting_type
  output$mix <- renderUI({
    mixchoices <- c('no mix' = 0, '2x' = 2, '3x' = 3, '5x' = 5, '10x' = 10, '20x' = 20)
    if (input$pipetting_type == 'transfer') {
      tagList(fluidRow(
        style = "margin-top: -20px;",
        column(6, selectizeInput('btimes', 'Mix before', choices = mixchoices, selected = 0), style='padding-right:0px;'), 
        column(6, numericInput('bmix_vol', 'Mix vol', value = 0, min = 0, max = 200, step = 1), style='padding-left:0px;'),
      ), fluidRow(
        #style = "margin-top: -20px;",
        column(6, selectizeInput('atimes', 'Mix after', choices = mixchoices, selected = 0), style='padding-right:0px;'),
        column(6, numericInput('amix_vol', 'Mix vol', value = 0, min = 0, max = 200, step = 1), style='padding-left:0px;'),
      ))
    } else if (input$pipetting_type == 'distribute') {
      fluidRow(
        style = "margin-top: -20px;",
        column(6, selectizeInput('btimes', 'Mix before', choices = mixchoices, selected = 0), style='padding-right:0px;'),
        column(6, numericInput('bmix_vol', 'Mix vol', value = 0), style='padding-left:0px;'),
      )
    } else if (input$pipetting_type == 'consolidate') {
      fluidRow(
        style = "margin-top: -20px;",
        column(6, selectizeInput('atimes', 'Mix after', choices = mixchoices, selected = 0), style='padding-right:0px;'),
        column(6, numericInput('amix_vol', 'Mix vol', value = 0), style='padding-left:0px;'),
      )
    }
  })
  
  output$source_plate <- renderReactable({
    selected_labware <- labware[labware$id == input$source_labware, , drop = FALSE]
    # exclude invalid combinations
    if (input$active_pipet == 'right' &&  selected_labware$nwells < 96 ) {
      validate('Not possible to use a multi-channel pipette with this labware')
    }
    DF = source_react()
    if(!is.null(DF)) {
      reactable(
        DF,
        highlight = T, wrap = F, bordered = T, compact = T, fullWidth = F, sortable = F, pagination = F,
        columns = list(.rownames = colDef(style = list(color = 'black', fontSize = '90%'))),
        defaultColDef =
          colDef(
            style = function(value) {
              if (value > 0) {
                color <- "#FF5733"
                fw <- "bold"
              } else {
                color <- 'grey'
                fw <- "lighter"
              }
              list(color = color, fontWeight = fw, fontSize = '90%')
            },
            minWidth = 40,
            html = TRUE,
            headerStyle = list(background = "#f7f7f8", fontSize = '90%')
          )
      )
    }
  })
  
  output$dest_plate <- renderReactable({
    selected_labware <- labware[labware$id == input$dest_labware, , drop = FALSE]
    # exclude invalid combinations
    if (input$active_pipet == 'right' &&  selected_labware$nwells < 96 ) {
      validate('Not possible to use a multi-channel pipette with this labware')
    }
    
    DF = dest_react()
    if(!is.null(DF)) {
      reactable(
        DF,
        highlight = T, wrap = F, bordered = T, compact = T, fullWidth = F, sortable = F, pagination = F,
        columns = list(.rownames = colDef(style = list(color = 'black', fontSize = '90%'))),
        defaultColDef =
          colDef(
            style = function(value) {
              if (value > 0) {
                color <- "#229954"
                fw <- "bold"
              } else {
                color <- 'grey'
                fw <- "lighter"
              }
              list(color = color, fontWeight = fw, fontSize = '90%')
            },
            minWidth = 40,
            html = TRUE,
            headerStyle = list(background = "#f7f7f8", fontSize = '90%')
          )
      )
    }
  })
  
  # renders first column well in grey for better plate overview
  rendergrey <- function() {
    "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      tbl = this.HTMLWidgets.widgets[0]
      
      if (/^A/.test(value)) {
        td.style.color = '#2980B9';
        td.style.background = '#F2F4F4';
      }
      
      return td;
  }"
  }
  
  output$hot <- renderRHandsontable({
    DF = hot()
    if(!is.null(DF)) {
      rhandsontable(hot()) %>%
        hot_col('source_well', readOnly = F, type = 'dropdown',
                source = unique(hot()$source_well), 
                renderer = rendergrey()) %>%
        hot_col('dest_well', type = 'dropdown', 
                source = unique(hot()$dest_well), 
                renderer = rendergrey()) %>%
        hot_col('vol', type = 'numeric', allowInvalid = F)
    }
  })
  
  output$protocol_preview <- renderPrint({
    write(myprotocol(), file = "")
  })
  
  output$deck <- renderUI({
    HTML('<img src="deck.png" height="600">')
  })
  
  output$pipet_valuebox <- renderUI({
    pipette_title <- if(input$active_pipet == 'left'){
      input$left_pipette
    } else {
      input$right_pipette
    }
    ht <- as_tibble(hot_to_r(input$hot))
    nsteps <- sum(ht$vol != 0, na.rm = T)
    totalvol <- if(input$active_pipet == 'left') {
       sum(ht$vol, na.rm = T)
    } else {
       sum(ht$vol, na.rm = T) * 8
    }
    
    vbs <- list(
      value_box(
        height = '70px',
        title = pipette_title,
        value = input$active_pipet,
        showcase = bsicons::bs_icon('crosshair', size = '70%'), 
        theme_color = 'primary'
      ),
      value_box(
        height = '70px',
        title = paste0(input$pipetting_type ,' steps:'),
        value = nsteps,
        showcase = bsicons::bs_icon('sliders2', size = '70%'), 
        theme_color = 'primary'
        #p("bslib ain't one", bs_icon("emoji-smile")),
        #p("hit me", bs_icon("suit-spade"))
      ),
      value_box(
        height = '70px',
        title = 'total volume:',
        value = paste0(formatC(totalvol, big.mark = ",", format = 'd', digits = 0), ' ul'),
        #p('Source: ', input$source_labware),
        #p('Destination: ', input$dest_labware)
        showcase = bsicons::bs_icon('water', size = '70%'),
        theme_color = 'primary'
      )
    )
    layout_column_wrap(width = '250px', !!!vbs)
  })
  # observers
  observeEvent(input$simulate, {
    # clear stdout
    shinyjs::html(id = "stdout", "")
    
    # check if opentrons_simulate is in path
    if (system2('which', args = 'opentrons_simulate') == 1) {
      shinyjs::html(id = 'stdout', "opentrons_simulate executable not found. Set the OPENTRONS_PATH variable to the opentrons path.")
      return()
      }
    
    # change button
    shinyjs::disable(id = 'simulate')
    shinyjs::html(id = 'simulate', "Working...")
    tmp <- tempfile('protocol', fileext = '.py')
    write(myprotocol(), file = tmp)
    ht <- as_tibble(hot_to_r(input$hot))
    
    withCallingHandlers({
      if (all(ht$vol == 0)) {
        processx::run(
          'echo', args = ("All volumes are 0, cannot simulate this!"),
          stderr_to_stdout = TRUE, 
          error_on_status = FALSE,
          stdout_line_callback = function(line, proc) {message(line)}
        )
      } else {
        processx::run(
          'opentrons_simulate', 
          args = tmp,
          stderr_to_stdout = TRUE, 
          error_on_status = FALSE,
          stdout_line_callback = function(line, proc) {message(line)}, 
        )
      }
      shinyjs::enable(id = 'simulate')
      shinyjs::html(id = 'simulate', "Run simulation")
    },
    message = function(m) {
      shinyjs::html(id = "stdout", html = m$message, add = TRUE); 
      runjs("document.getElementById('stdout').scrollTo(0,1e9);") 
      # scroll the page to bottom with each message, 1e9 is just a big number
    }
    )
  })
  
  observeEvent(input$active_pipet, {
    if (input$active_pipet == 'left') {
      shinyjs::hide(id = 'right_pipette')
      shinyjs::show(id = 'left_pipette')
    } else if (input$active_pipet == 'right') {
      shinyjs::hide(id = 'left_pipette')
      shinyjs::show(id = 'right_pipette') 
    }
  })
  
  # download
  output$download_script <- downloadHandler(
    filename = function() {
      paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-custom-transfer.py')
    },
    content = function(con) {
      # at download time, replace name so that it appears on the Opentrons app
      replacement <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), '-custom-transfer.py')
      write(myprotocol() %>%
              str_replace(pattern = "10-custom-transfer.py", 
                          replacement = replacement), 
            con)
    }
  )
  
}

shinyApp(ui, server)

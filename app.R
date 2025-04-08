pacman::p_load(shiny, shinyWidgets, readxl, dplyr, tidyr, ggplot2, lubridate,
               DT, plotly, viridis, ggcorrplot, jsonlite, openxlsx, rhandsontable)

anc.files <- list.files("anc/", 
                        pattern = "*.R$", full.names = TRUE, 
                        recursive = TRUE)
sapply(anc.files, source, .GlobalEnv)

addResourcePath("www", "www")
addResourcePath("lib", "lib")

ui <- navbarPage(
  id = "tabs",
  title = div(
    class = "navbar-brand",
    tags$img(src = "www/logo.png", height = "50px"),  
    span("Validatietool SAM")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$script(HTML("
      window.onbeforeunload = function(event) {
        return 'Weet u zeker dat u deze pagina wilt verlaten? Niet-opgeslagen wijzigingen gaan verloren.';
      };
    "))
  ),
  tabPanel("Import", value = "Import",
    fluidPage(
      fluidRow(
        column(4, 
          selectInput("meetnet", "Selecteer Meetnet:", choices = c("57", "59"))
        ),
        column(4, 
          fileInput("file", "Upload CSV File", accept = c(".csv"))
        ),
        column(4,
          uiOutput("meetpostopstelling_ui")
        )
      )
    )
  ),
  tabPanel("Verwerking", value = "Verwerking",
    fluidPage(
      h3(textOutput("process_title")),
      uiOutput("process")
    )
  ),
  tabPanel("Data", value = "Data",
    fluidPage(
      h3(textOutput("data_title")),
      dataTableOutput("data_summary"),
      tags$style(HTML("
        table {
          width: 100%;
          table-layout: auto;
        }
        th, td {
          white-space: nowrap;
        }
      "))
    )
  ),
  tabPanel("Statistieken", value = "Statistieken",
    fluidPage(
      h3(textOutput("stats_title")),
      uiOutput("stats")
    )
  ),
  tabPanel("Plots", 
    fluidPage(
      h3(textOutput("plot_title")),
      uiOutput("plots")
    )
  ),
  tabPanel("Validatie", value = "Validatie", 
    fluidPage(
      h3(textOutput("validation_title")),
      fluidRow(
        column(6, 
          fileInput("val-file", label = NULL, width = '500px', buttonLabel = "Tussentijdse Validatie Importeren", accept = c(".csv"))
        ),
        column(6,
          tags$div(style = "text-align: right;", 
            downloadButton("save_button", "Huidige Validatie Downloaden")
          )
        )
      ),
      tags$hr(style = "border-top: 5px solid #E7E5E5;"),
      tags$div(style = "height:500px; overflow-y: scroll; overflow-x: scroll;", DT::dataTableOutput("dtable")),
      tags$hr(style = "border-top: 5px solid #E7E5E5;"),
      rHandsontableOutput("selected_table"),
      tags$div(style = "height: 20px;") 
    )
  ),
  tabPanel("Export", value = "Export",
    fluidPage(
      fluidRow(
        column(3, selectInput("file_type", "Selecteer bestandstype:", choices = c("csv", "xlsx", "svpol"))),
        column(3, dateInput("start_date", "Startdatum:", value = Sys.Date())),
        column(3, dateInput("end_date", "Einddatum:", value = Sys.Date())),
        column(3, pickerInput("meetposten", "Selecteer Meetposten:", choices = NULL, multiple = TRUE))
      ),
      downloadButton("downloadData", "Exporteer Gevalideerde Data"),
      downloadButton("downloadReport", "Download Rapport")
    )
  ),
  tabPanel("Info", value = "Info",
    fluidPage(
      tags$div(
        style = "text-align: justify; font-size: 16px;",
        HTML("<p>
          Deze <b><a href='https://shiny.posit.co/'>R Shiny app</a></b> werd ontwikkeld binnen de kern Lucht van de Vlaamse Milieumaatschappij 
          om het valideren van de data komende van verscheidene semi-automatische meetnetten te vergemakkelijken.
          De applicatie is in continue aanbouw, zowel qua functionaliteiten als het toevoegen van nieuwe meetnetten. <br>
          Voor de nieuwste versie, zie de 
          <b><a href='https://github.com/BramVerbeek/sampler-validatietool'>GitHub repository</a></b>.
          Bij vragen, mail naar 
          <b><a href='mailto:b.verbeek@vmm.be'>b.verbeek@vmm.be</a></b>. </p>"),
        tags$hr(style = "border-top: 5px solid #E7E5E5;"),
        HTML("<p>
            <b>Gebruikshandleiding:</b> <br>   
        </p>")
      )
    )
  )
)

server <- function(input, output, session) {

  file_path <- reactive({
    req(input$file)
    path <- input$file$datapath
    return(path)  
  })

  uploaded_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    return(df)  
  })

  processed_data <- reactive({
    req(uploaded_data(), input$meetnet)
    df <- uploaded_data()
    processed_df <- get(paste0("process_meetnet_", input$meetnet))(df)$output 
    return(processed_df)
  })

  output$validation_title <- renderText({
    req(input$meetpostopstelling)
    paste("Validatie voor meetnet ", input$meetnet, " en meetpost ", input$meetpostopstelling, ":", sep = "")
  })
  output$plot_title <- renderText({
    req(input$meetpostopstelling)
    paste("Plots voor meetnet ", input$meetnet, " en meetpost ", input$meetpostopstelling, ":", sep = "")
  })
  output$stats_title <- renderText({
    req(input$meetpostopstelling)
    paste("Statistieken voor meetnet ", input$meetnet, " en meetpost ", input$meetpostopstelling, ":", sep = "")
  })
  output$process_title <- renderText({
    req(input$meetpostopstelling)
    paste("Verwerking voor meetnet ", input$meetnet, " en meetpost ", input$meetpostopstelling, ":", sep = "")
  })
  output$data_title <- renderText({
    req(input$meetpostopstelling)
    paste("Ruwe data voor meetnet ", input$meetnet, " en meetpost ", input$meetpostopstelling, ":", sep = "")
  })

  output$meetpostopstelling_ui <- renderUI({
    tagList(
      selectInput("meetpostopstelling", "Selecteer de te valideren meetpost:", choices = sort(names(processed_data())))
    )
  })

  output$data_summary <- renderDataTable({
    req(input$meetpostopstelling)
    df <- processed_data()[[input$meetpostopstelling]]
    df <- df[, c("Monsternummer", "Begindatum", "Einddatum", 
           setdiff(sort(names(df)), c("Monsternummer", "Begindatum", "Einddatum", "Commentaar", "CommentaarAnt", "Labovalidatie")), 
           "Labovalidatie" ,"Commentaar", "CommentaarAnt")]
    datatable(df, options = list(paging = FALSE))
  })

  output$plots <- renderUI({
    req(input$meetpostopstelling)
    plot_list <- get(paste0("plot_meetnet_", input$meetnet))(processed_data(), input$meetpostopstelling)
    
    plot_output_list <- lapply(seq_along(plot_list), function(i) {
      plotname <- paste("plot", i, sep="")
      tagList(
        h4(plot_list[[i]]$title), 
        plotlyOutput(plotname, height = "500px")
      )
    })
    
    do.call(tagList, plot_output_list)
  })

  observe({
    req(input$meetpostopstelling)
    plot_list <- get(paste0("plot_meetnet_", input$meetnet))(processed_data(), input$meetpostopstelling)
    
    for (i in seq_along(plot_list)) {
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        output[[plotname]] <- renderPlotly({
          plot_list[[my_i]]$plot 
        })
      })
    }
  })

  output$stats <- renderUI({
    req(input$meetpostopstelling)
    stat_list <- get(paste0("stat_meetnet_", input$meetnet))(uploaded_data(), processed_data(), input$meetpostopstelling)
    
    stat_output_list <- lapply(seq_along(stat_list), function(i) {
      statname <- paste("stat", i, sep="")
      tagList(
        h4(stat_list[[i]]$title), 
        dataTableOutput(statname)
      )
    })
    
    do.call(tagList, stat_output_list)
  })

  observe({
    req(input$meetpostopstelling)
    stat_list <- get(paste0("stat_meetnet_", input$meetnet))(uploaded_data(), processed_data(), input$meetpostopstelling)
    
    for (i in seq_along(stat_list)) {
      local({
        my_i <- i
        statname <- paste("stat", my_i, sep="")
        output[[statname]] <- renderDataTable({
          stat_list[[my_i]]$data 
        })
      })
    }
  })

  selected_df <- reactiveVal(data.frame(Monsternummer = character(), Parameter = character(), Waarde = double(), 
                                        Validatiecode = integer(), Validatiecommentaar = character()))

  output$dtable <- renderDataTable({
    req(input$meetpostopstelling)
    df <- processed_data()[[input$meetpostopstelling]]
    unique_parameters <- as.vector(unique(uploaded_data()$Parameter))
    df_with_buttons <- df
    df_with_buttons$Monsternummer <- paste0( df$Monsternummer,
      '<br><button id="select_all_', df$Monsternummer, '" 
      onclick="Shiny.setInputValue(\'select_all_click\', {row: \'', df$Monsternummer, '\'}, {priority: \'event\'})">Select All</button>',
      '<br><button id="deselect_all_', df$Monsternummer, '" 
      onclick="Shiny.setInputValue(\'deselect_all_click\', {row: \'', df$Monsternummer, '\'}, {priority: \'event\'})">Deselect All</button>'
    )
    current_selected <- selected_df()
    for (col in unique_parameters) { 
      df_with_buttons[[col]] <- mapply(function(value, row) {
        if (any(current_selected$Monsternummer == row & current_selected$Parameter == col)) {
          paste0('<span style="color:#1a708d; font-weight:900;">', value, '</span>')
        } else {
          value
        }
      }, df[[col]], df$Monsternummer)
      df_with_buttons[[col]] <- paste0(
        df_with_buttons[[col]], 
        '<br><button id="select_', col, '_', df$Monsternummer, '" 
        onclick="Shiny.setInputValue(\'button_click\', {row: \'', df$Monsternummer, '\', col: \'', col, '\'}, {priority: \'event\'})">Select</button>',
        '<br><button id="deselect_', col, '_', df$Monsternummer, '" 
        onclick="Shiny.setInputValue(\'button_deselect_click\', {row: \'', df$Monsternummer, '\'}, {priority: \'event\'})">Deselect</button>'
      )
    }
    datatable(df_with_buttons, escape = FALSE, selection = 'none', options = list(paging = FALSE))
  })

  observeEvent(input$select_all_click, {
    row_name <- input$select_all_click$row  

    df <- processed_data()[[input$meetpostopstelling]]
    unique_parameters <- as.vector(unique(uploaded_data()$Parameter))

    current_selected <- selected_df()
    for (col_name in unique_parameters) {
      cell_value <- as.double(df[df$Monsternummer == row_name, col_name])
      if (!any(current_selected$Monsternummer == row_name & current_selected$Parameter == col_name)) {
        updated_selected <- rbind(current_selected, data.frame(Monsternummer = row_name, Parameter = col_name, Waarde = cell_value, Validatiecode = NA, Validatiecommentaar = ""))
        current_selected <- updated_selected
      }
    }
    selected_df(current_selected)
  })

  observeEvent(input$deselect_all_click, {
    row_name <- input$deselect_all_click$row  

    df <- processed_data()[[input$meetpostopstelling]]
    unique_parameters <- as.vector(unique(uploaded_data()$Parameter))

    current_selected <- selected_df()
    for (col_name in unique_parameters) {
      updated_selected <- current_selected[!(current_selected$Monsternummer == row_name & current_selected$Parameter == col_name), ]
      current_selected <- updated_selected
    }
    selected_df(current_selected)
  })

  observeEvent(input$button_click, {
    row_name <- input$button_click$row  
    col_name <- input$button_click$col  
    
    df <- processed_data()[[input$meetpostopstelling]]

    cell_value <- as.double(df[df$Monsternummer == row_name, col_name])
    
    current_selected <- selected_df()
    
    if (!any(current_selected$Monsternummer == row_name & current_selected$Parameter == col_name)) {
      updated_selected <- rbind(current_selected, data.frame(Monsternummer = row_name, Parameter = col_name, Waarde = cell_value, Validatiecode = NA, Validatiecommentaar = ""))
      selected_df(updated_selected)
    }
  })

  observeEvent(input$button_deselect_click, {
    row_name <- input$button_deselect_click$row  
    col_name <- input$button_deselect_click$col  
    
    current_selected <- selected_df()
    updated_selected <- current_selected[!(current_selected$Monsternummer == row_name & current_selected$Parameter == col_name), ]
    selected_df(updated_selected)
  })

  output$selected_table <- renderRHandsontable({
    rhandsontable(selected_df(), rowHeaders = NULL,
      colHeaders = c("Monsternummer", "Parameter", "Waarde", "Validatiecode", "Validatiecommentaar"),
      overflow = 'visible') %>% 
      hot_cols(columnSorting = TRUE) %>%
      hot_col("Monsternummer", type = "text", readOnly = TRUE) %>%
      hot_col("Parameter", type = "text", readOnly = TRUE) %>%
      hot_col("Waarde", type = "numeric", readOnly = TRUE) %>% 
      hot_col("Validatiecode", type = "numeric") %>% 
      hot_col("Validatiecommentaar", type = "text") %>% 
      hot_table(stretchH = "all", colHeaders = TRUE, contextMenu = FALSE)
  })

  observeEvent(input$`val-file`, {
    req(input$`val-file`)
    val_data <- read.csv(input$`val-file`$datapath)
    current_selected <- selected_df()
    updated_selected <- bind_rows(current_selected, val_data) %>%
      arrange(is.na(Validatiecode)) %>%  
      distinct(Monsternummer, Parameter, .keep_all = TRUE)
    selected_df(updated_selected)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("validated_data_", input$meetnet, "_", Sys.Date(), ".", input$file_type, sep = "")
    },
    content = function(file) {
      df <- get(paste0("export_meetnet_", input$meetnet))(uploaded_data(), selected_df())
      df <- df %>% filter(toestelopstelling %in% input$meetposten)
      df <- df %>% filter(as.Date(date, format = "%d-%m-%Y") >= input$start_date & as.Date(enddate, format = "%d-%m-%Y") <= input$end_date)
      if (input$file_type == "xlsx") {
        write.xlsx(df, file)
      } else if (input$file_type == "csv") {
        write.csv(df, file, row.names = FALSE)
      } else if (input$file_type == "svpol") {
        write.table(df, file, sep = ";", row.names = FALSE)
      }
    }
  )

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("report_", input$meetnet, "_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("www/report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        meetnet = input$meetnet,
        valcodes = selected_df(),
        start_date = input$start_date,
        end_date = input$end_date
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )

  observeEvent(uploaded_data(),{
    req(uploaded_data())
    import_data <- uploaded_data()
    unique_toestelopstelling <- sort(unique(import_data$MeetpostOpstelling))
    earliest_date <- min(as.Date(import_data$Begindatum, format = "%Y-%m-%d"))
    last_date <- max(as.Date(import_data$Begindatum, format = "%Y-%m-%d")) 
    updatePickerInput(session, "meetposten", choices = unique_toestelopstelling)
    updateDateInput(session, "start_date", value = earliest_date)
    updateDateInput(session, "end_date", value = last_date)
  })

  observeEvent(input$save_button, {
    write.csv(hot_to_r(input$selected_table), row.names = FALSE)
    showNotification("Selected table saved successfully!", type = "message")
  })

  output$save_button <- downloadHandler(
    filename = function() {
      paste("tussentijdse_validatie_meetnet_", input$meetnet,"_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hot_to_r(input$selected_table), file, row.names = FALSE)
    }
  )

  observeEvent(input$selected_table, {
    req(input$selected_table)
    updated_df <- hot_to_r(input$selected_table) 
    selected_df(updated_df)  
  })

  observeEvent(uploaded_data(), {
    req(uploaded_data())
    data <- uploaded_data()
    invalid_rows <- data[data$Validatiecode != "V", ]
    if (nrow(invalid_rows) > 0) {
      current_selected <- selected_df()
      new_rows <- data.frame(
        Monsternummer = invalid_rows$Monsternummer,
        Parameter = invalid_rows$Parameter,
        Waarde = invalid_rows$Resultaat,
        Validatiecode = rep(NA, nrow(invalid_rows)),
        Validatiecommentaar = paste("[ Labocode: ", invalid_rows$Validatiecode, ", Labocommentaar: ", invalid_rows$Commentaar, "]")
      )
      updated_selected <- bind_rows(current_selected, new_rows)  %>%
        arrange(is.na(Validatiecode)) %>%  
        distinct(Monsternummer, Parameter, .keep_all = TRUE)
      selected_df(updated_selected)
    }
  })

  observe({
    req(selected_df())
    visited_validatie <- reactiveVal(FALSE)  

    observeEvent(input$tabs, {
      if (input$tabs == "Validatie") {
        visited_validatie(TRUE)  
      } else if (visited_validatie() && input$tabs != "Validatie") {
        if (any(is.na(selected_df()$Validatiecode))) {
          showModal(modalDialog(
            title = "⚠ Waarschuwing",
            "Er zijn lege validatiecodes. Ga naar 'Validatie' en vul deze aan voordat u verdergaat.",
            easyClose = FALSE,
            footer = modalButton("OK")
          ))
        }
      }
    })
  })

}

shinyApp(ui = ui, server = server)

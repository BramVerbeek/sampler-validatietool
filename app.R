pacman::p_load(shiny, shinyWidgets, readxl, dplyr, tidyr, ggplot2, lubridate, miceadds,
               DT, plotly, viridis, ggcorrplot, jsonlite, xlsx, rhandsontable)
source.all("anc/")
addResourcePath("www", "www")
addResourcePath("lib", "lib")

ui <- navbarPage(
  title = div(
    class = "navbar-brand",
    tags$img(src = "www/logo.png", height = "50px"),  
    span("Validatietool Semi-automatische Metingen")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  ),
  tabPanel("Data Invoer", 
    fluidPage(
      fluidRow(
        column(4, 
          selectInput("meetnet", "Selecteer Meetnet:", choices = c("59"))
        ),
        column(4, 
          fileInput("file", "Upload CSV File", accept = c(".csv"))
        ),
        column(4,
          uiOutput("meetpostopstelling_ui"))
      ),
      tags$hr(style = "border-top: 5px solid #E7E5E5;"),
      h4(textOutput("data_title")),
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
  tabPanel("Statistieken", 
    fluidPage(
      h4(textOutput("stats_title")),
      uiOutput("stats")
    )
  ),
  tabPanel("Plots", 
    fluidPage(
      h4(textOutput("plot_title")),
      uiOutput("plots")
    )
  ),
  tabPanel("Validatie", 
    fluidPage(
      h4(textOutput("validation_title")),
      fluidRow(
        column(6, 
          fileInput("val-file", label = NULL, width = '500px', buttonLabel = "Tussentijdse Validatie Importeren", accept = c(".csv"))
        ),
        column(6,
            tags$div(style = "text-align: right;", 
            downloadButton("save_button", "Huidige Validatie Downloaden")))
      ),
      tags$hr(style = "border-top: 5px solid #E7E5E5;"),
      DTOutput("dtable"),
      tags$hr(style = "border-top: 5px solid #E7E5E5;"),
      rHandsontableOutput("selected_table"),
      tags$div(style = "height: 20px;") 
    )
  ),
  tabPanel("Data Export", 
    fluidPage(
      fluidRow(
        column(3, selectInput("file_type", "Selecteer bestandstype:", choices = c("csv", "xlsx", "svpol"))),
        column(3, dateInput("start_date", "Startdatum:", value = Sys.Date())),
        column(3, dateInput("end_date", "Einddatum:", value = Sys.Date())),
        column(3, pickerInput("meetposten", "Selecteer Meetposten:", choices = NULL, multiple = TRUE))
      ),
      downloadButton("downloadData", "Download")
    )
  ),
  tabPanel("Info",
    fluidPage(
      tags$div(
        style = "text-align: justify; font-size: 16px;",
        HTML("<p>
          Deze <b><a href='https://shiny.posit.co/'>R Shiny app</a></b> werd ontwikkeld bij de dienst Lucht van de Vlaamse Milieumaatschappij 
          om het valideren van de data komende van verscheidene semi-automatische meetnetten te vergemakkelijken. De applicatie is in continue aanbouw, zowel qua functionaliteiten als het toevoegen van nieuwe meetnetten.
          Voor de nieuwste versie, zie de 
          <b><a href='https://github.com/BramVerbeek/sampler-validatietool'>GitHub repository</a></b>.
          Bij vragen, mail naar 
          <b><a href='mailto:b.verbeek@vmm.be'>b.verbeek@vmm.be</a></b>. </p> "),
        tags$hr(style = "border-top: 5px solid #E7E5E5;"),
        HTML("<p>
          Na het importeren van de data wordt de validatie per meetpost uitgevoerd. Hierna kan de data
          geëxporteerd worden naar een csv, xlsx of svpol-bestand met een vast bepaalde formatting. <br>
          <b>Opgelet:</b>  het opnieuw laden van de pagina zal al je voortgang wissen. Zorg ervoor dat je de data hebt geëxporteerd voordat je de pagina herlaadt. <br><br>
          Hierna gaat u <b>terug naar het tabblad &quot;Data Invoer&quot;</b> om de volgende meetpost te selecteren. <br>
          Indien alle meetposten zijn behandeld, gaat u <b>verder naar &quot;Data Export&quot;</b>. <br>   
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

    selected <- selected_df()

    df <- uploaded_data()
    processed_df <- get(paste0("process_meetnet_", input$meetnet))(df)
    selected <- selected_df()
    
    if (nrow(selected) > 0 && any(!is.na(selected$Validatiecode) & selected$Validatiecode != "")) {
      for (i in seq_len(nrow(selected))) {
        row <- selected[i, ]
        if (!is.na(row$Validatiecode) && as.numeric(row$Validatiecode) > 100) {
          trdf <- processed_df[[sub("-.*$", "", as.character(row$Monsternummer))]]
          monster <- row$Monsternummer
          param <- row$Parameter
          row_index <- which(trdf[["Monsternummer"]] == monster)
          trdf[[param]][row_index] <- NA
        }
      }
    }
    return(processed_df)
  })

  output$validation_title <- renderText({
    req(input$meetpostopstelling)
    paste("Validatie voor Meetnet ", input$meetnet, " en Meetpostopstelling ", input$meetpostopstelling, ":", sep = "")
  })
  output$plot_title <- renderText({
    req(input$meetpostopstelling)
    paste("Plots voor Meetnet ", input$meetnet, " en Meetpostopstelling ", input$meetpostopstelling, ":", sep = "")
  })
  output$stats_title <- renderText({
    req(input$meetpostopstelling)
    paste("Statistieken voor Meetnet ", input$meetnet, " en Meetpostopstelling ", input$meetpostopstelling, ":", sep = "")
  })
  output$data_title <- renderText({
    req(input$meetpostopstelling)
    paste("Data voor Meetnet ", input$meetnet, " en Meetpostopstelling ", input$meetpostopstelling, ":", sep = "")
  })

  output$meetpostopstelling_ui <- renderUI({
    tagList(
      selectInput("meetpostopstelling", "Selecteer de te valideren meetpost:", choices = names(processed_data()))
    )
  })

  output$data_summary <- renderDataTable({
    req(input$meetpostopstelling)
    datatable(processed_data()[[input$meetpostopstelling]], options = list(pageLength = 50))
  })

  output$plots <- renderUI({
    req(input$meetpostopstelling)
    plot_list <- get(paste0("plot_meetnet_", input$meetnet))(processed_data(), input$meetpostopstelling)
    plot_output_list <- lapply(seq_along(plot_list), function(i) {
      plotname <- paste("plot", i, sep="")
      plotlyOutput(plotname, height = "500px")
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
          plot_list[[my_i]]
        })
      })
    }
  })

  output$stats <- renderUI({
    req(input$meetpostopstelling)
    stat_list <- get(paste0("stat_meetnet_", input$meetnet))(uploaded_data(), processed_data(), input$meetpostopstelling)
    stat_output_list <- lapply(seq_along(stat_list), function(i) {
      statname <- paste("stat", i, sep="")
      dataTableOutput(statname)
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
          stat_list[[my_i]]
        })
      })
    }
  })

  selected_df <- reactiveVal(data.frame(Monsternummer = character(), Parameter = character(), Waarde = character(), 
                                        Validatiecode = numeric(), Validatiecommentaar = character()))

  output$dtable <- renderDataTable({
    req(input$meetpostopstelling)
    df <- get(paste0("process_meetnet_", input$meetnet))(uploaded_data())[[input$meetpostopstelling]]
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
          paste0('<span style="color:#F25757; font-weight:bold;">', value, '</span>')
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
    datatable(df_with_buttons, escape = FALSE, selection = 'none', options = list(pageLength = 5))
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

  validatiecodes <- reactive({
    valcodes <- read.csv("lib/validatiecodesSAM.csv", sep = ";")
    paste0(valcodes$Code, ": ", valcodes$Omschrijving)
  })

  output$selected_table <- renderRHandsontable({
    rhandsontable(selected_df(), rowHeaders = NULL,
      colHeaders = c("Monsternummer", "Parameter", "Waarde", "Validatiecode", "Validatiecommentaar"),
      overflow = 'visible') %>% 
      hot_cols(columnSorting = TRUE) %>% 
      hot_col("Validatiecode", type = "numeric") %>% 
      hot_col("Validatiecommentaar", type = "text") %>% 
      hot_table(stretchH = "all", colHeaders = TRUE)
  })

  observe({
    validatiecodes()
  })

  observeEvent(input$selected_table, {
    updated_df <- hot_to_r(input$selected_table)
    selected_df(updated_df)   
    processed_data() 
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

  observe({
    updateSelectInput(session, "meetposten", choices = names(processed_data()))
  })

  observeEvent(input$save_button, {
    write.csv(hot_to_r(input$selected_table), row.names = FALSE)
    showNotification("Selected table saved successfully!", type = "message")
  })

  output$save_button <- downloadHandler(
    filename = function() {
      paste("tussentijdse_validatie_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hot_to_r(input$selected_table), file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)

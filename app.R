pacman::p_load(shiny, readxl, dplyr, tidyr, ggplot2, lubridate, miceadds,
               DT, plotly, viridis, ggcorrplot, jsonlite, xlsx, rhandsontable)
source.all("anc/")
addResourcePath("www", "www")
addResourcePath("lib", "lib")



ui <- navbarPage(
  title = div(
    class = "navbar-brand",
    tags$img(src = "www/logo.png", height = "50px"),  
    span("Validatietool Samplermetingen")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('update_checkbox', function(message) {
        var row = message.row;
        var col = message.col;
        var value = message.value;
        
        // Update the checkbox using the provided row and column
        var checkboxId = '#' + col + '_' + row;
        $(checkboxId).prop('checked', value);
      });
    "))
  ),
  tabPanel("Data Invoer", 
           fluidPage(
            fluidRow(
              column(6, 
                     selectInput("meetnet", "Selecteer Meetnet:",
                                 choices = c("59"))
              ),
              column(6, 
                     fileInput("file", "Upload Excel File", 
                               accept = c(".xls", ".xlsx"))
              )
            ),
            uiOutput("meetpostopstelling_ui"),
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
            tags$div(
        style = "text-align: justify; font-size: 16px;",
        HTML("
          <p>
        <b>Selecteer hieronder</b> de waarden voor deze meetpost die u wenst een <b>andere validatiecode</b> dan &quot;<i>10:valid</i>&quot; te geven. <br> 
          </p>
        ")
      ),
      DTOutput("dtable"),
                  tags$div(
        style = "text-align: justify; font-size: 16px;",
        HTML("
          <p>
         In de volgende tabel kan u de <b>validatiecode</b> en <b>commentaar</b> aanpassen. Dit doet u door op de bijhorende cel in de tabel te klikken. <br>
         Hierna gaat u <b>terug naar het tabblad &quot;Data Invoer&quot;</b> om de volgende meetpost te selecteren. <br>
         Indien alle meetposten zijn behandeld, gaat u <b>verder naar &quot;Data Export&quot;</b>. <br>   
          </p>
        ")
      ),
      rHandsontableOutput("selected_table"),
      tags$div(style = "height: 20px;")  # Add vertical space
    )
  ),
  tabPanel("Data Export", 
    fluidPage(
      tags$div(
        style = "text-align: justify; font-size: 16px;",
        HTML("
          <p>
        Druk op de knop hieronder om een <b>excel file te downloaden met de gevalideerde data</b>. <br>
      Deze data is uw oorspronkelijk bestand, herwerkt naar een standaardformaat en met de 
        validatiecodes en commentaren die u heeft ingegeven. <br>
        Alle metingen zonder expliciete validatiecode krijgen de code &quot;<i>10:valid</i>&quot; mee. <br>
          </p>
        ")
        ),
      downloadButton("downloadData", "Download de Gevalideerde Data")
    )
  ),
  tabPanel("Info",
    fluidPage(
      tags$div(
        style = "text-align: justify; font-size: 16px;",
      HTML("<p>
        Deze <b><a href='https://shiny.posit.co/'>R Shiny app</a></b> werd ontwikkeld bij de dienst Lucht van de Vlaamse Milieumaatschappij 
        om het valideren van de data komende van verscheidene sampler-meetnetten te vergemakkelijken. <br>
        Na het importeren van de data wordt de validatie per meetpost uitgevoerd. Hierna kan de data
        geëxporteerd worden naar een Excel-bestand met een vast bepaalde formatting. <br>
        <b>Opgelet:</b>  het opnieuw laden van de pagina zal al je voortgang wissen. Zorg ervoor dat je de data hebt geëxporteerd voordat je de pagina herlaadt. <br><br>

        De applicatie is in continue aanbouw, zowel qua functionaliteiten als het toevoegen van nieuwe meetnetten.
        Voor de nieuwste versie, zie de 
        <b><a href='https://github.com/BramVerbeek/sampler-validatietool'>GitHub repository</a></b>. <br>
        Bij vragen, mail naar 
        <b><a href='mailto:b.verbeek@vmm.be'>b.verbeek@vmm.be</a></b>.
      </p>")
        )
      )
    )
)




server <- function(input, output, session) {

###################################################################################################################
# DATA IMPORT AND PROCESSING
###################################################################################################################

  uploaded_data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    return(df)  
  })

processed_data <- reactive({
    req(uploaded_data(), input$meetnet)
    df <- uploaded_data()
    processed_df <- get(paste0("process_meetnet_", input$meetnet))(df)
    selected <- selected_df()
    
    if (nrow(selected) > 0 && any(!is.na(selected$Validatiecode) & selected$Validatiecode != "")) {
        for (i in seq_len(nrow(selected))) {
            row <- selected[i, ]
            if (!is.na(row$Validatiecode) && as.numeric(row$Validatiecode) > 100) {
                processed_df[[as.character(row$Monsternummer)]][[row$Parameter]] <- NA
            }
        }
    }
    return(processed_df)
})

  observe({
    processed_data()
    output$data_summary <- renderDataTable({
        req(input$meetpostopstelling)
        datatable(processed_data()[[input$meetpostopstelling]])
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
    output$stats <- renderUI({
        req(input$meetpostopstelling)
        stat_list <- get(paste0("stat_meetnet_", input$meetnet))(uploaded_data(), input$meetpostopstelling)
        stat_output_list <- lapply(seq_along(stat_list), function(i) {
            statname <- paste("stat", i, sep="")
            dataTableOutput(statname)
        })
        do.call(tagList, stat_output_list)
    })
  })

###################################################################################################################
# INTERACTIVE TITLES
###################################################################################################################

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

###################################################################################################################
# UI ELEMENTS
###################################################################################################################

  output$meetpostopstelling_ui <- renderUI({
    tagList(
      tags$div(
        style = "text-align: justify; font-size: 16px;",
        HTML("
          <p>
        Hier <b>selecteert u de meetpost</b> waarmee u in de rest van het script aan de slag gaat. 
        De validatie <b>gaat verder op het volgende tabblad</b>. <br> <b>Om de meetpost aan te passen, keert u terug naar dit tabblad.</b> <br>
        Ten slotte kan u de <b>volledige gevalideerde dataset exporteren</b> naar een Excel-bestand op het Data Export tabblad.
          </p>
        ")
      ),
    selectInput("meetpostopstelling", "Selecteer Meetpost:",
                choices = names(processed_data()))
    )
  })

###################################################################################################################
# DATA TABLES
###################################################################################################################

  output$data_summary <- renderDataTable({
    req(input$meetpostopstelling)
    datatable(processed_data()[[input$meetpostopstelling]])
  })

###################################################################################################################
# PLOTS
###################################################################################################################

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

###################################################################################################################
# STATS
###################################################################################################################

  output$stats <- renderUI({
    req(input$meetpostopstelling)
    stat_list <- get(paste0("stat_meetnet_", input$meetnet))(uploaded_data(), input$meetpostopstelling)
    stat_output_list <- lapply(seq_along(stat_list), function(i) {
      statname <- paste("stat", i, sep="")
      dataTableOutput(statname)
    })
    do.call(tagList, stat_output_list)
  })

  observe({
    req(input$meetpostopstelling)
    stat_list <- get(paste0("stat_meetnet_", input$meetnet))(uploaded_data(), input$meetpostopstelling)
    for (i in seq_along(stat_list)) {
      local({
        my_i <- i
        statname <- paste("stat", my_i, sep="")
        output[[statname]] <- renderDT({
          stat_list[[my_i]]
        })
      })
    }
  })

###################################################################################################################
# VALIDATION SELECTION
###################################################################################################################

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
    for (col in unique_parameters) { 
        df_with_buttons[[col]] <- paste0(
            df[[col]], 
            '<br><input type="checkbox" id="', col, '_', df$Monsternummer, '" 
            onchange="Shiny.setInputValue(\'checkbox_click\', {row: \'', df$Monsternummer, '\', col: \'', col, '\', value: this.checked}, {priority: \'event\'})">'
        )
    }
    datatable(df_with_buttons, escape = FALSE, selection = 'none', options = list(pageLength = 5))
})

observeEvent(input$select_all_click, {
    row_name <- input$select_all_click$row  

    df <- processed_data()[[input$meetpostopstelling]]
    unique_parameters <- as.vector(unique(uploaded_data()$Parameter))

    for (col_name in unique_parameters) {
        session$sendCustomMessage(type = "update_checkbox", message = list(row = row_name, col = col_name, value = TRUE))
    }

    current_selected <- selected_df()
    for (col_name in unique_parameters) {
        cell_value <- as.double(df[df$Monsternummer == row_name, col_name])
        updated_selected <- rbind(current_selected, data.frame(Monsternummer = row_name, Parameter = col_name, Waarde = cell_value, Validatiecode = NA, Validatiecommentaar = ""))
        current_selected <- updated_selected
    }
    selected_df(current_selected)
})

observeEvent(input$deselect_all_click, {
    row_name <- input$deselect_all_click$row  

    df <- processed_data()[[input$meetpostopstelling]]
    unique_parameters <- as.vector(unique(uploaded_data()$Parameter))

    for (col_name in unique_parameters) {
        session$sendCustomMessage(type = "update_checkbox", message = list(row = row_name, col = col_name, value = FALSE))
    }

    current_selected <- selected_df()
    for (col_name in unique_parameters) {
        updated_selected <- current_selected[!(current_selected$Monsternummer == row_name & current_selected$Parameter == col_name), ]
        current_selected <- updated_selected
    }
    selected_df(current_selected)
})

observeEvent(input$checkbox_click, {
    row_name <- input$checkbox_click$row  
    col_name <- input$checkbox_click$col  
    is_checked <- input$checkbox_click$value 
    
    df <- processed_data()[[input$meetpostopstelling]]

    cell_value <- as.double(df[df$Monsternummer == row_name, col_name])
    
    current_selected <- selected_df()
    
    if (is_checked) {
        updated_selected <- rbind(current_selected, data.frame(Monsternummer = row_name, Parameter = col_name, Waarde = cell_value, Validatiecode = NA, Validatiecommentaar = ""))
        selected_df(updated_selected)
    } else {
        updated_selected <- current_selected[!(current_selected$Monsternummer == row_name & current_selected$Parameter == col_name), ]
        selected_df(updated_selected)
    }
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

###################################################################################################################
# DOWNLOADING DATA
###################################################################################################################

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("validated_data_", input$meetnet,"_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      df <- get(paste0("export_meetnet_", input$meetnet))(uploaded_data(), selected_df())
      write.xlsx(df, file)
    }
  )

}

shinyApp(ui = ui, server = server)

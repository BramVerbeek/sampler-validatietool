pacman::p_load(shiny, readxl, dplyr, tidyr, ggplot2, lubridate, miceadds,
               DT, plotly, viridis, corrplot)
source.all("anc/")
addResourcePath("www", "www")


callback <- c(
  "var id = $(table.table().node()).closest('.datatables').attr('id');",
  "$.contextMenu({",
  "  selector: '#' + id + ' td.factor input[type=text]',",
  "  trigger: 'hover',",
  "  build: function($trigger, e){",
  "    var levels = $trigger.parent().data('levels');",
  "    if(levels === undefined){",
  "      var colindex = table.cell($trigger.parent()[0]).index().column;",
  "      levels = table.column(colindex).data().unique();",
  "    }",
  "    var options = levels.reduce(function(result, item, index, array){",
  "      result[index] = item;",
  "      return result;",
  "    }, {});",
  "    return {",
  "      autoHide: true,",
  "      items: {",
  "        dropdown: {",
  "          name: 'Edit',",
  "          type: 'select',",
  "          options: options,",
  "          selected: 0",
  "        }",
  "      },",
  "      events: {",
  "        show: function(opts){",
  "          opts.$trigger.off('blur');",
  "        },",
  "        hide: function(opts){",
  "          var $this = this;",
  "          var data = $.contextMenu.getInputValues(opts, $this.data());",
  "          var $input = opts.$trigger;",
  "          $input.val(options[data.dropdown]);",
  "          $input.trigger('change');",
  "        }",
  "      }",
  "    };",
  "  }",
  "});"
)

createdCell <- function(levels){
  if(missing(levels)){
    return("function(td, cellData, rowData, rowIndex, colIndex){}")
  }
  quotedLevels <- toString(sprintf("\"%s\"", levels))
  c(
    "function(td, cellData, rowData, rowIndex, colIndex){",
    sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
    "}"
  )
}

ui <- navbarPage(
  title = div(
    class = "navbar-brand",
    tags$img(src = "www/logo.png", height = "50px"),  
    span("Validatietool Samplermetingen")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css")
  ),


  tabPanel("Data Invoer", 
           fluidPage(
#            h4("Inladen ruwe data"),
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
  tabPanel("Plots", 
    fluidPage(
      h4(textOutput("plot_title")),
      plotlyOutput("plot1"),
      plotlyOutput("plot2"),
      plotOutput("plot3", height = "600px"),
      plotlyOutput("plot4"),
      plotlyOutput("plot5")
    )
  ),
  tabPanel("Validatie", 
    fluidPage(
      h4(textOutput("validation_title")),
      DTOutput("dtable")
    )
  ),
  tabPanel("Data Export", 
    fluidPage(
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
        Deze tool kwam tot stand met behulp van generatieve AI. Bij vragen, mail naar 
        <b><a href='mailto:b.verbeek@vmm.be'>b.verbeek@vmm.be</a></b>.
      </p>")
        )
      )
    )
)


server <- function(input, output, session) {

  uploaded_data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    return(df)  
  })

  processed_data <- reactive({
    df <- uploaded_data()  
    processed_df <- get(paste0("process_meetnet_", input$meetnet))(df)
    return(processed_df)
  })

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

  output$data_summary <- renderDataTable({
    req(input$meetpostopstelling)
    datatable(processed_data()[[input$meetpostopstelling]])
  })

  output$plot1 <- renderPlotly({
    plot1 <- get(paste0("plot_meetnet_", input$meetnet))(uploaded_data(),input$meetpostopstelling)
    plot1[[1]]
  })

  output$plot2 <- renderPlotly({
    plot2 <- get(paste0("plot_meetnet_", input$meetnet))(uploaded_data(),input$meetpostopstelling)
    plot2[[2]] 
  })

  output$plot3 <- renderPlot({
    plot3 <- get(paste0("plot_meetnet_", input$meetnet))(uploaded_data(),input$meetpostopstelling)
    plot3[[3]] 
  })

  output$plot4 <- renderPlotly({
    plot4 <- get(paste0("plot_meetnet_", input$meetnet))(uploaded_data(),input$meetpostopstelling)
    plot4[[4]] 
  })

  output$plot5 <- renderPlotly({
    plot5 <- get(paste0("plot_meetnet_", input$meetnet))(uploaded_data(),input$meetpostopstelling)
    plot5[[5]] 
  })

  output$validation_title <- renderText({
    req(input$meetpostopstelling)
    paste("Validatie voor Meetnet ", input$meetnet, " en Meetpostopstelling ", input$meetpostopstelling, ":", sep = "")
  })
  output$plot_title <- renderText({
    req(input$meetpostopstelling)
    paste("Plots voor Meetnet ", input$meetnet, " en Meetpostopstelling ", input$meetpostopstelling, ":", sep = "")
  })

  output$dtable <- renderDataTable({
    req(input$meetpostopstelling)
    datatable(processed_data()[[input$meetpostopstelling]])
  })

}

shinyApp(ui = ui, server = server)

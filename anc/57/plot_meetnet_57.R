plot_meetnet_57 <- function(df, meetpost) {

pdf <- df[[meetpost]]

parameter_lijst <- colnames(pdf)
parameter_lijst <- setdiff(parameter_lijst, 
c("Monsternummer", "Begindatum", "Einddatum",
  "Labovalidatie", "Commentaar", "CommentaarAnt"))

longdf <- pdf %>%
      pivot_longer(cols = parameter_lijst, names_to = "Parameter", values_to = "Resultaat") %>%
      filter(!is.na(Resultaat)) %>%
      mutate(Resultaat = as.numeric(Resultaat), date = Einddatum)

colors <- viridis(24)

TSPlot <- plot_ly(longdf, 
                x = ~date,
                y = ~Resultaat, 
                type = 'scatter',
                mode = 'lines+markers',
                color = ~Parameter,   
                colors = colors,                      
                marker = list(size = 5),
                hoverinfo = "x+y+text",              
                text = ~paste("Parameter:", Parameter)) %>%
        layout(title = list(text = "<b>Tijdsreeks van de metingen voor elke parameter</b>",
                            x = 0,                                     
                            xanchor = "left",                            
                            font = list(                         
                                size = 16                      
                            )
        ),
            xaxis = list(title = "Begindatum", type = "date", autorange = TRUE),  
            yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
            margin = list(l = 40, r = 40, t = 40, b = 40),
            showlegend = TRUE)


PBoxplot <- plot_ly(longdf, 
                x = ~Parameter, 
                y = ~Resultaat, 
                type = 'box',
                color = ~Parameter,                
                colors = colors,                  
                marker = list(size = 5),
                boxpoints = "all",
                jitter = 0.5, 
                pointpos = 0,
                hoverinfo = "all") %>%
        layout(title = list(text = "<b>Boxplot van de metingen voor elke parameter</b>",
                            x = 0,                                     
                            xanchor = "left",                            
                            font = list(                         
                                size = 16                      
                            )
        ),
            xaxis = list(title = "Parameter", autorange = TRUE),
            yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
            margin = list(l = 40, r = 40, t = 40, b = 40),
            showlegend = FALSE)


df2 <- df %>%
    lapply(function(x) {
      x <- x[, !names(x) %in% c("Labovalidatie", "Commentaar", "CommentaarAnt")]
      x <- x %>%
        pivot_longer(cols = -c(Monsternummer, Begindatum, Einddatum), names_to = "Parameter", values_to = "Resultaat") %>%
        filter(!is.na(Resultaat)) %>%
        mutate(Resultaat = as.numeric(Resultaat), date = Einddatum)
      return(x)
    })

bigdf2 <- dplyr::bind_rows(df2, .id = 'MeetpostOpstelling')

greys_palette <- colorRampPalette(c("black", "grey"))(n = length(unique(bigdf2$MeetpostOpstelling)))

TSPlot2 <- plot_ly(bigdf2 %>% filter(MeetpostOpstelling != meetpost), 
                    x = ~date,
                    y = ~Resultaat,
                    color = ~MeetpostOpstelling,
                    colors = greys_palette, 
                    type = 'scatter',
                    mode = 'lines+markers',
                    frame = ~Parameter,                      
                    marker = list(size = 5),
                    hoverinfo = "x+y+text",              
                    text = ~paste("Meetpost:", MeetpostOpstelling)) %>% 
                    layout(title = list(text = "<b>Tijdreeks van de metingen per parameter voor alle meetposten</b>",
                                        x = 0,                                     
                                        xanchor = "left",                            
                                        font = list(                         
                                            size = 16                      
                                        )
                    ),
                        xaxis = list(title = "Begindatum", type = "date", autorange = TRUE),  
                        yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
                        margin = list(l = 40, r = 40, t = 40, b = 40),
                        showlegend = TRUE) %>%  
                    add_trace(data = bigdf2 %>% filter(MeetpostOpstelling == meetpost),
                              marker = list(color = '#F25757'),
                              line = list(color = '#F25757'))


BBoxplot <- plot_ly(bigdf2 %>% filter(MeetpostOpstelling != meetpost), 
                            x = ~MeetpostOpstelling, 
                            y = ~Resultaat, 
                            type = 'box',
                            color = ~MeetpostOpstelling,
                            colors = greys_palette,
                            frame = ~Parameter,                 
                            marker = list(size = 5),
                            boxpoints = "all",
                            jitter = 0.5, 
                            pointpos = 0,
                            hoverinfo = "all") %>%
                    layout(title = list(text = "<b>Boxplot van de metingen per parameter voor alle meetposten</b>",
                                        x = 0,                                     
                                        xanchor = "left",                            
                                        font = list(                         
                                            size = 16                      
                                        )
                    ),
                        xaxis = list(title = "Meetpost", autorange = TRUE),
                        yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
                        margin = list(l = 40, r = 40, t = 40, b = 40),
                        showlegend = FALSE) %>%  
                    add_trace(data = bigdf2 %>% filter(MeetpostOpstelling == meetpost),
                              marker = list(color = '#F25757'),
                              line = list(color = '#F25757'),
                              fillcolor = '#ff9595')

yearly_avg <- bigdf2 %>%
  mutate(Year = lubridate::year(date)) %>%
  group_by(Year, MeetpostOpstelling, Parameter) %>%
  summarise(AvgResultaat = mean(Resultaat, na.rm = TRUE), .groups = "drop")

YearlyBarPlot <- plot_ly(yearly_avg %>% filter(MeetpostOpstelling != meetpost),
                         x = ~Year,
                         y = ~AvgResultaat,
                         color = ~MeetpostOpstelling,
                         frame = ~Parameter,
                         type = 'bar',
                         colors = greys_palette,
                         hoverinfo = "text") %>%
  layout(title = list(text = "<b>Jaarlijkse gemiddelden per parameter en meetpost</b>",
                      x = 0,
                      xanchor = "left",
                      font = list(size = 16)),
         xaxis = list(title = "Jaar"),
         yaxis = list(title = "Gemiddelde Concentratie (µg/m³)"),
         margin = list(l = 40, r = 40, t = 40, b = 40),
         showlegend = TRUE)
        YearlyBarPlot <- YearlyBarPlot %>%
            add_trace(data = yearly_avg %>% filter(MeetpostOpstelling == meetpost),
                                marker = list(color = '#F25757'))


return(list(TSPlot, PBoxplot, TSPlot2, BBoxplot, YearlyBarPlot))
}


plot_meetnet_59 <- function(df, meetpost) {

df <- lapply(df, function(x) {
        x <- x[, colSums(x != 0, na.rm = TRUE) > 0 | names(x) %in% c("Monsternummer", "Begindatum", "Einddatum", "Labovalidatie", "Commentaar", "CommentaarAnt")]
        return(x)
    })

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
        layout(
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
        layout(
            xaxis = list(title = "Parameter", autorange = TRUE),
            yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
            margin = list(l = 40, r = 40, t = 40, b = 40),
            showlegend = FALSE)

M <- pdf %>%
    select(all_of(parameter_lijst)) %>%
    mutate(across(everything(), as.numeric))

if (sum(complete.cases(M)) > 1) {
    CORplot <- ggcorrplot(cor(M, use = "complete.obs"), hc.order = TRUE, type = "lower", tl.cex = 8) +
        ggtitle("") +
        theme(plot.title = element_text(hjust = 0, size = 14, face = "bold", color = "#5b5b5b"))
} else {
    CORplot <- ggplot() + 
        ggtitle("Not enough complete pairs to compute correlation matrix") +
        theme(plot.title = element_text(hjust = 0, size = 14, face = "bold", color = "#5b5b5b"))
}

df2 <- df %>%
    lapply(function(x) {
      x <- x[, !names(x) %in% c("Labovalidatie", "Commentaar", "CommentaarAnt")]
      x <- x %>%
        pivot_longer(cols = -c(Monsternummer, Begindatum, Einddatum), names_to = "Parameter", values_to = "Resultaat") %>%
        filter(!is.na(Resultaat)) %>%
        mutate(Resultaat = as.numeric(Resultaat), date = Begindatum)
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
                    layout(
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
                    layout(
                        xaxis = list(title = "Meetpost", autorange = TRUE),
                        yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
                        margin = list(l = 40, r = 40, t = 40, b = 40),
                        showlegend = FALSE) %>%  
                    add_trace(data = bigdf2 %>% filter(MeetpostOpstelling == meetpost),
                              marker = list(color = '#F25757'),
                              line = list(color = '#F25757'),
                              fillcolor = '#ff9595')



big_avg <- bigdf2 %>%
    group_by(MeetpostOpstelling, Parameter) %>%
    summarise(AvgResultaat = mean(Resultaat, na.rm = TRUE), .groups = "drop")

YearlyBarPlotMP <- plot_ly(big_avg %>% filter(MeetpostOpstelling == meetpost),
                         x = ~Parameter,
                         y = ~AvgResultaat,
                         color = ~Parameter,
                         type = 'bar',
                         colors = colors,
                         hoverinfo = "text") %>%
  layout(
         xaxis = list(title = ""),
         yaxis = list(title = "Gemiddelde Concentratie (µg/m³)"),
         margin = list(l = 40, r = 40, t = 40, b = 40),
         showlegend = FALSE)

YearlyBarPlot <- plot_ly(big_avg %>% filter(MeetpostOpstelling != meetpost),
                         x = ~MeetpostOpstelling,
                         y = ~AvgResultaat,
                         color = ~MeetpostOpstelling,
                         frame = ~Parameter,
                         type = 'bar',
                         colors = greys_palette,
                         hoverinfo = "text") %>%
  layout(
         xaxis = list(title = ""),
         yaxis = list(title = "Gemiddelde Concentratie (µg/m³)"),
         margin = list(l = 40, r = 40, t = 40, b = 40),
         showlegend = FALSE)
        YearlyBarPlot <- YearlyBarPlot %>%
            add_trace(data = big_avg %>% filter(MeetpostOpstelling == meetpost),
                                marker = list(color = '#F25757'))

heatmap_data <- pdf %>%
    select(c(all_of(parameter_lijst),"Monsternummer")) %>%
    pivot_longer(cols = -c(Monsternummer), names_to = "Parameter", values_to = "Value")

HeatmapPlot <- plot_ly(
  heatmap_data,
  x = ~Parameter,
  y = ~Monsternummer,
  z = ~Value,
  type = "heatmap",
  colorscale = "Jet"
) %>%
  layout(
    xaxis = list(title = "Parameter"),
    yaxis = list(title = "Monsternummer"),
    margin = list(l = 40, r = 40, t = 40, b = 40)
  )

return(list(
    list(title = paste("Tijdsreeks van de metingen voor meetpost ", meetpost), plot = TSPlot),
    list(title = paste("Boxplot van de metingen voor meetpost ", meetpost), plot = PBoxplot),
    list(title = paste("Gemiddelden per parameter voor meetpost ", meetpost), plot = YearlyBarPlotMP),
    list(title = paste("Correlatiematrix van parameters voor meetpost ", meetpost), plot = CORplot),
    list(title = paste("Heatmap van waarden voor meetpost ", meetpost), plot = HeatmapPlot),
    list(title = "Tijdreeks van de metingen per parameter voor alle meetposten", plot = TSPlot2),
    list(title = "Boxplot van de metingen per parameter voor alle meetposten", plot = BBoxplot),
    list(title = "Gemiddelden per parameter voor alle meetposten", plot = YearlyBarPlot)
))
}


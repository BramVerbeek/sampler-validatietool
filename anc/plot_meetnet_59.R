plot_meetnet_59 <- function(df, meetpost) {

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
        layout(title = list(text = "",
                            x = 0,                                     
                            xanchor = "left",                            
                            font = list(                         
                                size = 16                      
                            )
        ),
            xaxis = list(title = "Einddatum", type = "date", autorange = TRUE),  
            yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
            margin = list(l = 40, r = 40, t = 40, b = 40),
            showlegend = FALSE)


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
        layout(title = list(text = "",
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
                    layout(title = list(text = "<b>Tijdreeks van de metingen per parameter</b>",
                                        x = 0,                                     
                                        xanchor = "left",                            
                                        font = list(                         
                                            size = 16                      
                                        )
                    ),
                        xaxis = list(title = "Einddatum", type = "date", autorange = TRUE),  
                        yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
                        margin = list(l = 40, r = 40, t = 40, b = 40),
                        showlegend = FALSE) %>%  
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
                    layout(title = list(text = "<b>Boxplot van de metingen per parameter</b>",
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


return(list(TSPlot, PBoxplot, CORplot, TSPlot2, BBoxplot))
}


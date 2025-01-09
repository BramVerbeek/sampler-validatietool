plot_meetnet_59 <- function(df, meetpost) {

pdf <- df[[meetpost]]

parameter_lijst <- colnames(pdf)
parameter_lijst <- setdiff(parameter_lijst, 
c("Monsternummer", "Begindatum", "Einddatum", "Labovalidatie", "Commentaar", "CommentaarAnt"))

longdf <- pdf %>%
      pivot_longer(cols = parameter_lijst, names_to = "Parameter", values_to = "Resultaat") %>%
      filter(!is.na(Resultaat)) %>%
      mutate(Resultaat = as.numeric(Resultaat), date = as.Date(Einddatum, format = "%d-%m-%Y"))

#df2 <- df %>%
    #mutate(date = as.Date(Einddatum, format = "%d-%m-%Y")) %>%
    #arrange(date)

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
        layout(title = list(text = "<b>Tijdreeks per parameter voor de gekozen meetpost</b>",
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
        layout(title = list(text = "<b>Boxplot per parameter voor de gekozen meetpost</b>",
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
    mutate(across(everything(), as.numeric)) %>%
    cor(use = "complete.obs")

CORplot <- ggcorrplot(M, hc.order = TRUE, type = "lower", tl.cex = 8) +
    ggtitle("Correlatie tussen de parameters voor de gekozen meetpost") +
    theme(plot.title = element_text(hjust = 0, size = 14, face = "bold", color = "#5b5b5b"))


greys_palette <- colorRampPalette(c("black", "grey"))(n = length(unique(df$MeetpostOpstelling)))

#TSPlot2 <- plot_ly(df2 %>% filter(MeetpostOpstelling != meetpost), 
                    #x = ~date,
                    #y = ~Resultaat,
                    #color = ~MeetpostOpstelling,
                    #colors = greys_palette, 
                    #type = 'scatter',
                    #mode = 'lines+markers',
                    #frame = ~Parameter,                      
                    #marker = list(size = 5),
                    #hoverinfo = "x+y+text",              
                    #text = ~paste("Meetpost:", MeetpostOpstelling)) %>% 
                    #layout(title = list(text = "<b>Tijdreeks van de metingen per parameter</b>",
                                        #x = 0,                                     
                                        #xanchor = "left",                            
                                        #font = list(                         
                                            #size = 16                      
                                        #)
                    #),
                        #xaxis = list(title = "Einddatum", type = "date", autorange = TRUE),  
                        #yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
                        #margin = list(l = 40, r = 40, t = 40, b = 40),
                        #showlegend = FALSE) %>%  
                    #add_trace(data = df2 %>% filter(MeetpostOpstelling == meetpost),
                              #marker = list(color = '#ff0000'),
                              #line = list(color = '#ff0000'))


#BBoxplot <- plot_ly(df %>% filter(MeetpostOpstelling != meetpost), 
                            #x = ~MeetpostOpstelling, 
                            #y = ~Resultaat, 
                            #type = 'box',
                            #color = ~MeetpostOpstelling,
                            #colors = greys_palette,
                            #frame = ~Parameter,                 
                            #marker = list(size = 5),
                            #boxpoints = "all",
                            #jitter = 0.5, 
                            #pointpos = 0,
                            #hoverinfo = "all") %>%
                    #layout(title = list(text = "<b>Boxplot van de metingen per parameter</b>",
                                        #x = 0,                                     
                                        #xanchor = "left",                            
                                        #font = list(                         
                                            #size = 16                      
                                        #)
                    #),
                        #xaxis = list(title = "Meetpost", autorange = TRUE),
                        #yaxis = list(title = "Concentratie (µg/m³)", autorange = TRUE),
                        #margin = list(l = 40, r = 40, t = 40, b = 40),
                        #showlegend = FALSE) %>%  
                    #add_trace(data = df %>% filter(MeetpostOpstelling == meetpost),
                              #marker = list(color = '#ff0000'),
                              #line = list(color = '#ff0000'),
                              #fillcolor = '#ff9595')


return(list(TSPlot, PBoxplot, CORplot))  #, TSPlot2, BBoxplot))
}

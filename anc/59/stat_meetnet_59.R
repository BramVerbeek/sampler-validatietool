stat_meetnet_59 <- function(bigdf, df, meetpost) {
  
  df_list <- split(bigdf, bigdf$MeetpostOpstelling)
  bigpdf <- df_list[[meetpost]]

  pdf <- df[[meetpost]]
  
  days_table <- bigpdf %>%
    mutate(Begindatum = as.Date(Begindatum, format = "%Y-%m-%d"),
           Einddatum = as.Date(Einddatum, format = "%Y-%m-%d"),
           Analysedatum = as.Date(Analysedatum, format = "%Y-%m-%d"),
           Tijdsinterval = as.numeric(difftime(Analysedatum, Begindatum, units = "days")),
           Houdbaarheid = ifelse(Tijdsinterval > 30, "Niet OK", "OK")) %>%
    select(Monsternummer, Parameter, Begindatum, Analysedatum, Tijdsinterval, Houdbaarheid) %>%
    filter(Houdbaarheid == "Niet OK")
  
  parameter_columns <- setdiff(colnames(pdf), c("Monsternummer", "Begindatum", "Einddatum", "Analysedatum", "Labovalidatie", "Commentaar", "CommentaarAnt"))
  
  numeric_columns <- parameter_columns[sapply(pdf[parameter_columns], is.numeric)]
  
  stats_table <- pdf %>%
    select(all_of(numeric_columns)) %>%
    summarise(across(everything(), list(
      mean = ~round(mean(.x, na.rm = TRUE), 2),
      median = ~round(median(.x, na.rm = TRUE), 2),
      sd = ~round(sd(.x, na.rm = TRUE), 2),
      max = ~round(max(.x, na.rm = TRUE), 2),
      p90 = ~round(quantile(.x, 0.90, na.rm = TRUE), 2),
      min = ~round(min(.x, na.rm = TRUE), 2),
      p10 = ~round(quantile(.x, 0.10, na.rm = TRUE), 2)
    ))) %>%
    pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value") %>%
    separate(Parameter, into = c("Parameter", "Statistiek"), sep = "_") %>%
    pivot_wider(names_from = Parameter, values_from = Value)
  
  return(list(days_table, stats_table))
}

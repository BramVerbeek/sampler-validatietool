stat_meetnet_59 <- function(df, meetpost) {

pdf <- process_meetnet_59(df)[[meetpost]]

parameter_lijst <- colnames(pdf)
parameter_lijst <- setdiff(parameter_lijst, 
c("Monsternummer", "Begindatum", "Einddatum", "Labovalidatie", "Commentaar", "CommentaarAnt"))

longdf <- pdf %>%
      pivot_longer(cols = parameter_lijst, names_to = "Parameter", values_to = "Resultaat") %>%
      filter(!is.na(Resultaat)) %>%
      mutate(Resultaat = as.numeric(Resultaat), date = as.Date(Einddatum, format = "%d-%m-%Y"))



return(list())
}

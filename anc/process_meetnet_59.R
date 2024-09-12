process_meetnet_59 <- function(df) {
  df_list <- split(df, df$MeetpostOpstelling)
  
  df_list <- lapply(df_list, function(x) {
    x <- x[, !names(x) %in% c("MeetpostOpstelling", "Veldblanco", "Meetnet", "Labo", "Teken",
                              "Eenheid", "Ag", "Bg", "U", "Periode", "Frequentie", "Analysedatum",
                              "Jaar", "Referentie", "Erkenningscode")]
   
    date_columns <- c("Begindatum", "Einddatum")
    x[date_columns] <- lapply(x[date_columns], function(col) {
      as.Date(col)
    })
    x <- x  %>% mutate_if(is.Date,~format(.,"%d-%m-%Y"))
    
    x <- x %>%
      group_by(Monsternummer) %>%
      mutate(
      combined_comment = if (all(is.na(Commentaar))) {
        ""
      } else {
        paste(
          paste("[", Parameter[!is.na(Commentaar)], "]: ", Commentaar[!is.na(Commentaar)], sep = ""),
          collapse = "; ")
      },
      combined_comment_ant = if (all(is.na(CommentaarAnt))) {
        ""
      } else {
        paste(
          paste("[", Parameter[!is.na(CommentaarAnt)], "]: ", CommentaarAnt[!is.na(CommentaarAnt)], sep = ""),
          collapse = "; ")
      },
      combined_validatiecode = if (all(Validatiecode == "V")) {
        ""
      } else {
        paste(
          paste("[", Parameter[Validatiecode != "V"], "]: ", Validatiecode[Validatiecode != "V"], sep = ""),
          collapse = "; ")
      }
      ) %>%
    ungroup() %>%
    mutate(Commentaar = combined_comment,
          CommentaarAnt = combined_comment_ant,
          Validatiecode = combined_validatiecode) %>%
    select(-combined_comment, -combined_comment_ant, -combined_validatiecode)  

    x <- x %>%
      pivot_wider(names_from = Parameter, values_from = Resultaat)

    x <- x %>%
      relocate("Validatiecode", .after = last_col()) %>%
      relocate("Commentaar", .after = last_col()) %>%
      relocate("CommentaarAnt", .after = last_col())

    return(x)
    })
    
  return(df_list)
}

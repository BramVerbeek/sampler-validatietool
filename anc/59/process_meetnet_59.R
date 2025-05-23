process_meetnet_59 <- function(df) {

  df <- df %>% 
  mutate(Commentaar = ifelse(Commentaar == " ", NA, Commentaar)) %>%
  mutate(Commentaar = ifelse(Commentaar == "", NA, Commentaar))

  df1 <- df %>% filter(!(Veldblanco %in% c("V")))
  df2 <- df %>% filter((Veldblanco %in% c("V")))

  df_list <- split(df1, df1$MeetpostOpstelling)
  
  df_list <- lapply(df_list, function(x) {
    x <- x[, !names(x) %in% c("MeetpostOpstelling", "Veldblanco", "Meetnet", "Labo", "Teken",
                              "Eenheid", "Ag", "Bg", "U", "Periode", "Frequentie",
                              "Jaar", "Referentie", "Erkenningscode", "X", "Analysedatum")]
   
    date_columns <- c("Begindatum", "Einddatum")
    x[date_columns] <- lapply(x[date_columns], function(col) {
      as.Date(col, "%Y-%m-%d")
    })
    x <- x  %>% mutate_if(is.Date,~format(.,"%Y-%m-%d"))
    
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
          Labovalidatie = combined_validatiecode) %>% 
    select(-combined_comment, -combined_comment_ant, -combined_validatiecode, -Validatiecode)  

    x <- x %>% 
      pivot_wider(names_from = Parameter, values_from = Resultaat) 

    x <- x %>%
      relocate("Labovalidatie", .after = last_col()) %>%
      relocate("Commentaar", .after = last_col()) %>%
      relocate("CommentaarAnt", .after = last_col())

    x <- x %>% arrange(Monsternummer) 

    return(x)
    })

    erased_list <- split(df2, df2$MeetpostOpstelling)
    
  return(list(
    output = df_list,
    process = list(title = "Gewiste Veldblanco's",
                  data = erased_list))
    )
}

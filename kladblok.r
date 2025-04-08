 selected <- selected_df()
    
    if (nrow(selected) > 0 && any(!is.na(selected$Validatiecode) & selected$Validatiecode != "")) {
      for (i in seq_len(nrow(selected))) {
        row <- selected[i, ]
        if (!is.na(row$Validatiecode) && as.numeric(row$Validatiecode) > 100) {
          trdf <- processed_df[[sub("-.*$", "", as.character(row$Monsternummer))]]
          monster <- row$Monsternummer
          param <- row$Parameter
          row_index <- which(trdf[["Monsternummer"]] == monster)
          trdf[[param]][row_index] <- NA
        }
      }
    }
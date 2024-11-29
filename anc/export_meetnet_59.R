export_meetnet_59 <- function(df, valdf) {

saroad59 <- read_excel("C:/Users/b.verbeek/Documents/sampler-validatietool/lib/SAROAD-59.xlsx")
get_saroad_code <- function(name, saroad59) {
  result <- saroad59 %>%
    filter(naam == name) %>%
    select(code) %>%
    pull()
  
  return(result)
}

data <- df
date_columns <- c("Begindatum", "Einddatum")
data[date_columns] <- lapply(data[date_columns], 
    function(col) {
      as.Date(col)
    })
data <- data  %>% mutate_if(is.Date,~format(.,"%d-%m-%Y"))

data <- data %>%
    mutate(enh_id = 80, time = "00:00:00", endtime = "23:59:59") %>%
    select(-Erkenningscode,-Veldblanco, -Frequentie, -Labo, -Ag, -Bg, -U, -Validatiecode,
           -Analysedatum, -Jaar, -Meetnet, -Referentie, -Periode)

valdata <- valdf %>%
    mutate(Validatiecode = ifelse(grepl(":", Validatiecode), sub(":.*", "", Validatiecode), Validatiecode))

data <- data %>%
  left_join(valdata %>% select(Monsternummer, Parameter, Validatiecode, Validatiecommentaar), 
      by = c("Monsternummer", "Parameter")) %>%
  mutate(Validatiecode = coalesce(Validatiecode, "10"),
         Validatiecommentaar = coalesce(Validatiecommentaar, ""))

data$saroadcode <- sapply(data$Parameter, get_saroad_code, saroad59 = saroad59)

data <- data %>%
    select(-Monsternummer) %>%
    rename(date = Begindatum, enddate = Einddatum, time = time, endtime = endtime,
           toestelopstelling = MeetpostOpstelling, parameter = Parameter,
           saroadcode = saroadcode, resultaat = Resultaat, teken = Teken,
           eenheid = Eenheid, enh_id = enh_id, validatiecode = Validatiecode,
           commentaar = Commentaar, commentaarAnt = CommentaarAnt, commentaarVal = Validatiecommentaar)

return(data)
}

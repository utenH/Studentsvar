options(OutDec = ",")
options("openxlsx.numFmt" = "#,#0.00")

# Hente Sisteårsdata
portefolje_prep_sistears <- function() {
  SA20 <- read_excel("C:\\Users\\kyrremat\\OneDrive - OsloMet\\Dokumenter\\Studieportefølje\\Sisteårs\\Sisteårs2020.xlsx")
  SA21 <- read_excel("C:\\Users\\kyrremat\\OneDrive - OsloMet\\Dokumenter\\Studieportefølje\\Sisteårs\\Sisteårs2021.xlsx")
  SA22 <- read_excel("C:\\Users\\kyrremat\\OneDrive - OsloMet\\Dokumenter\\Studieportefølje\\Sisteårs\\Sisteårs2022.xlsx")
  SA20  <- SA20 %>% select(Studieprogramkode = `fs-kode`, 
                           overordnet_tilfreds = `Alt i alt, hvor fornøyd er du med studieprogrammet du går på?`) %>% mutate(Årstall = "2020")
  SA21  <- SA21 %>% select(Studieprogramkode = `fs-kode`, overordnet_tilfreds = `Alt i alt, hvor fornøyd er du med studieprogrammet du går på?`) %>% mutate(Årstall = "2021")
  SA22  <- SA22 %>% select(Studieprogramkode = `fs-kode`, overordnet_tilfreds = `Alt i alt, hvor fornøyd er du med studieprogrammet du går på?`) %>% mutate(Årstall = "2022")
  sdf <- bind_rows(SA20, SA21, SA22)
  nivå <- c("1 - Ikke fornøyd", "2", "3", "4", "5 - Svært fornøyd")
  sdf <- sdf %>% OM_text_to_factor(overordnet_tilfreds, overordnet_tilfreds, nivå)
  return(sdf)
}

# Hente Kandidatdata
portefolje_prep_kandidat <- function() {
  KU16 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Kandidat/Kandidat2016_master_20.06.xlsx")
  KU20 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Kandidat/Kandidat2020_alleprog.xlsx")
  
  KU16  <- KU16 %>% select(Studieprogramkode = studieprogramkode, utdanning_relevant_arbeid = `Hvor relevant er masterutdanningen for oppgavene du utfører i stillingen din-`) %>% mutate(Årstall = "2016")
  KU20  <- KU20 %>% select(Studieprogramkode = studprogkode, utdanning_relevant_arbeid = `Hvor relevant er masterutdanningen for oppgavene du utfører i stillingen din?`) %>% mutate(Årstall = "2020")

  # sdf <- bind_rows(KU16, KU20)
  
  nivå <- c("1 - Ikke relevant", "2", "3", "4", "5 - Svært relevant")
  KU20 <- KU20 %>% OM_text_to_factor(utdanning_relevant_arbeid, utdanning_relevant_arbeid, nivå)
  return(KU20)
}

# Hent gjennomføringsdata
portefolje_prep_normert <- function() {
  # For 
  # •	2016, 2017, 2018 (BA + GLU)
  # •	2017, 2018, 2019 (MA)
  # 
  # Hent ut andel kvalifikasjonar for åra
  # •	2020
  # •	2021
  # •	2022
  # 
  ##** 
  ##* Les inn filer, fyller opp tomme celler med Studieprogramnamn, 
  ##* filtrerer bort GLU bortsett frå i eitt sett
  ##* TODO left_join på Studieprogram for å få tabell for MA, GLU, BA med fullførtprosent 2020, 2021, 2022
  ##* TODO bind_rows på dei tre datasetta, for å få ein samla tabell å bruke XLOOKUP på
  BA2016 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2016 BA mm Tallene - studieprogram.xlsx")
  BA2016 <- BA2016 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  BA2016 <- BA2016 %>% filter(`År termin` == "2020 HØST") %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  BA2017 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2017 BA mm Tallene - studieprogram.xlsx")
  BA2017 <- BA2017 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  BA2017 <- BA2017 %>% filter(`År termin` == "2021 HØST") %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  BA2018 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2018 BA mm Tallene - studieprogram.xlsx")
  BA2018 <- BA2018 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  BA2018 <- BA2018 %>% filter(`År termin` == "2022 HØST") %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  BA2016_2018 <- list(BA2016, BA2017, BA2018) %>% reduce(left_join, by = "Studieprogram")
  
  GLU2016 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2016 BA mm Tallene - studieprogram.xlsx")
  GLU2016 <- GLU2016 %>% fill(Studieprogram) %>% filter(grepl("GLU", Studieprogram))
  GLU2016 <- GLU2016 %>% filter(`År termin` == "2022 HØST") %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  MA2017 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2019 MA Tallene - studieprogram.xlsx")
  MA2017 <- MA2017 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  MA2017 <- MA2017 %>% filter(`År termin` == "2020 HØST") %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  MA2018 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2019 MA Tallene - studieprogram.xlsx")
  MA2018 <- MA2018 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  MA2018 <- MA2018 %>% filter(`År termin` == "2021 HØST") %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  MA2019 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2019 MA Tallene - studieprogram.xlsx")
  MA2019 <- MA2019 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  MA2019 <- MA2019 %>% filter(`År termin` == "2022 HØST") %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  MA2017_2019 <- list(MA2017, MA2018, MA2019) %>% reduce(left_join, by = "Studieprogram")
  
  kvalifikasjoner_sett <- bind_rows(BA2016_2018, GLU2016, MA2017_2019)
  return(kvalifikasjoner_sett)
  # Lag tabell med Studieprogramnavn, Andel kvalifikasjoner 2020, Andel kvalifikasjoner 2021, Andel kvalifikasjoner 2022 
}

portefoljedata <- function() {
  soknadsdata <- dbh_data(379, filters = c("Institusjonskode"="1175"), 
                       group_by = c("Avdelingskode", "Årstall", "Studieprogramkode", "Opptakstype", 
                                    "Kvalifisert", "Prioritet", "Møtt til studiestart"), 
                       variables = c("Avdelingskode", "Årstall", "Studieprogramkode", "Opptakstype", 
                                    "Kvalifisert", "Prioritet", "Møtt til studiestart"))
  soknadsdata_slim <- soknadsdata %>% select(Studieprogramkode, Årstall, `Møtt til studiestart`) %>% 
    mutate(prog_ar = paste(Studieprogramkode, Årstall))
  # dbh_379 <- dbh_data(379)
  # dbh_379 %>% filter(Institusjonskode == 1175)
  
  dbh_370 <- dbh_data(370)
  studieplassdata <- dbh_370 %>% filter(Institusjonskode == 1175)
  studieplassdata_slim <- studieplassdata %>% select(Institusjonskode, 
                     Institusjonsnavn, 
                     Avdelingskode, 
                     Avdelingsnavn, 
                     Studieprogramkode, 
                     Årstall, 
                     `Antall studieplasser`) %>% 
    mutate(prog_ar = paste(Studieprogramkode, Årstall))
  
  # OM_programvar <- read_excel("base/OsloMet_programvariabler.xlsx")
  # soknadsdata_namn <- left_join(soknadsdata, OM_programvar, "Studieprogramkode") %>% filter(!is.na(Studieprogram_instnr))
  OM_register <- bind_rows(soknadsdata_slim, studieplassdata_slim)
  return(OM_register)
}

# Lagar og returnerer snittabell for gitt variabel, over eit ukjent tal år
# Brukar (mean(as.numeric( for å handtere faktorar
studieportefolje_serie <- function(sdf, variabel, gruppe_ar_var) {
  grupperingsvariabel <- sdf %>% group_vars()
  min_svar <- 4
  
  # lagrar liste med årstala å gruppere over, for å kunne loope gjennom
  ar_liste <- sdf %>% ungroup %>% select({{gruppe_ar_var}}) %>% unique %>% arrange({{gruppe_ar_var}})
  df_ut <- NULL
  df_ut_n <- NULL
  # TODO Det må vere ein betre måte å loope gjennom
  for (ar_nr in 1:nrow(ar_liste)) {
    ar <- ar_liste[ar_nr,] %>% as.character()
    N_ar <- paste("N", ar_liste[ar_nr,])
    
    df_ar <- sdf %>% filter(!is.na(.data[[variabel]])) %>%
      summarise(
        !!ar := mean(as.numeric(.data[[variabel]][.data[[gruppe_ar_var]] == ar]), na.rm = T)
        # !!ar := sum(.data[[variabel]][.data[[gruppe_ar_var]] == ar], na.rm = T)
      ) 
    
    df_ar_N <- sdf %>% filter(!is.na(.data[[variabel]])) %>%
      summarise(
        !!N_ar := sum(!is.na(.data[[variabel]][.data[[gruppe_ar_var]] == ar]))
      ) 
    
    if (is.null((df_ut))) {
      df_ut <- df_ar
      df_ut_n <- df_ar_N
    } else {
      df_ut <- left_join(df_ut, df_ar, by = {{grupperingsvariabel}})
      df_ut_n <- left_join(df_ut_n, df_ar_N, by = {{grupperingsvariabel}})
    }
  }
  
  df_ut <- left_join(df_ut, df_ut_n, by = {{grupperingsvariabel}})
  
  # Legge til kolonne ved å slå saman første kolonne og N-kolonne, fjernar første kolonne
  # df_ut <- df_ut %>% mutate(`Studium / N` = paste0(.data[[grupperingsvariabel]], " (", .data[[nkol_siste]] , "/", .data[[nkol_eldre]], ")"))
  # df_ut <- df_ut %>% select(-1) %>% relocate(last_col(), 1)
  
  # unngå #NUM! i excel
  df_ut[df_ut == "NaN"] <- NA
  
  ##** Ikkje relevant for studieportefølje
  # #* fjerne data med lav N
  # # fjernar resultat der det er færre enn fire minimumsgrensa har svart
  kolonner <- ncol(df_ut_n)
  for (x in 2:kolonner) {
    df_ut[x][df_ut[(kolonner - 1) + x] < min_svar] <- NA
  }
  # filtrerer bort linjer som ikkje har nok svar siste år
  df_ut <- df_ut %>% filter(df_ut[ncol(df_ut)] >= min_svar)
  
  # print(df_ut)
  return(df_ut)
} # end print_snitt_as_num_serie
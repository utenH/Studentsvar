options(OutDec = ",")
options("openxlsx.numFmt" = "#,#0.00")

##**
##* Rapportkode
##* 
##* # deler fritekstdata i filer gruppert på institutt
OM_fritekst_xlsx_2022 <- function(sdf, fritekstvariabler, grupperingsvariabel, surveynamn, data_ar) {
  tabell_stil <- createStyle(wrapText = T, valign = "top")
  svarkol_stil <- createStyle(numFmt = "TEXT")
  
  sdf <- sdf %>% group_by(!!grupperingsvariabel) %>% select(!!!fritekstvariabler) %>% group_split
  
  arbeidsboker <- list()
  
  surveyinfo <- paste(surveynamn, "fritekst", data_ar)
  dir.create(surveyinfo)
  
  for (gruppe in sdf) {
    gruppenamn <- gruppe[{{grupperingsvariabel}}] %>% unique() %>% as.character()
    print(gruppenamn)
    
    # ta bort grupperingsvariabel og kolonner som er heilt tomme
    gruppe <- gruppe %>% select(-{{grupperingsvariabel}})
    not_all_na <- function(x) any(!is.na(x))
    gruppe <- gruppe %>% select_if(not_all_na)
    gruppe <- gruppe %>% mutate(across(where(is.character), str_trim))
    gruppe <- gruppe %>% arrange(1)
    
    arbeidsbok <- createWorkbook()
    addWorksheet(arbeidsbok, surveyinfo)
    writeDataTable(arbeidsbok, 1, gruppe)
    
    ## stil
    datalengde <- NROW(gruppe) + 1
    # TODO Finn ein måte å bestemme dette frå Shiny-appen
    setColWidths(arbeidsbok, 1, cols = 1:7, widths = c(40, 14, 65, 10, "auto", "auto", "auto"))
    # addStyle(arbeidsbok, 1, cols = 5, rows = 2:datalengde, svarkol_stil, gridExpand = T, stack = T)
    addStyle(arbeidsbok, 1, cols = 1:7, rows = 1:datalengde, tabell_stil, gridExpand = T, stack = T)
    
    ## Save workbook
    # TODO gjer om slik at utskrift skjer i anna funksjon
    saveWorkbook(wb = arbeidsbok,
                 file = paste0(surveyinfo, "/", gruppenamn, " ",  surveynamn, " ", data_ar, ".xlsx"),
                 overwrite = TRUE, )
    arbeidsboker <- append(arbeidsboker, arbeidsbok)
  }
  return(arbeidsboker)
}

##** Skriv ut statistikk per variabel - basert på Kandidatundersøkinga
##*
OM_indikator_print_2022 <- function(sdf, malfil = "", survey = "test", aggregert = F) {
  
  # loop gjennom fakultet, ein eigen for aggregert
  
  # Bestem minste tal på svar som blir tatt med
  min_svar <- 4
  
  ##** Hent mal frå xlsx
  utskriftsmal <- read.xlsx(malfil)
  
  ##** Skriv ut basert på xlsx-mal
  # Finn lengde på mal
  malrader <- NROW(utskriftsmal)
  
  # lagre årstal til variabel
  arstal <- sdf %>% select(gruppe_ar) %>% arrange(desc(gruppe_ar)) %>% unique() 
  arstal_siste <- paste(arstal[1,1])
  arstal_eldre <- paste(arstal[2,1])
  
  # Tar vare på eit "ufiltrert" datasett for rapportering på aggregert nivå
  sdf_full <- sdf %>% filter(!is.na(Fakultetsnavn))
  
  # Filtrerer bort program som ikkje er med i siste datasett
  sdf <- sdf %>% group_by(Studieprogram_instnamn) %>% filter(arstal_siste %in% gruppe_ar) %>% ungroup
  
  surveyinfo <- paste0("Rapportfiler/",survey, " indikatorrapport ", arstal_siste)
  dir.create(surveyinfo)
  
  if (nrow(arstal) > 2) {
    print(paste("Datasettet har fleire enn to årstal, brukar desse:", arstal[1,1], "/", arstal[2,1]))
    print(paste(arstal[1,1], arstal[2,1]))
  }
  
  # Lagar og returnerer fordelingstabell for gitt variabel 
  print_fordeling <- function(sdf, variabel) {
    grupperingsvariabel <- sdf %>% group_vars()
    
    # Lage fordelingstabell
    df_fordeling <- sdf %>% select(.data[[variabel]]) %>% table %>% prop.table(1)
    
    # Gjere om til dataframe
    df_fordeling <- df_fordeling %>% as.data.frame.matrix()
    
    # Gjer radnamn til kolonne, for å kunne slå saman med N
    df_fordeling <- rownames_to_column(df_fordeling, var={{grupperingsvariabel}})
    
    # Rekne ut N
    df_n <- sdf %>% summarise(N = sum(!is.na(.data[[variabel]])))
    
    # Slå saman fordelingstabell og N
    df_ut <- left_join(df_fordeling, df_n, {{grupperingsvariabel}})
    
    # Legge til kolonne ved å slå saman første kolonne og N-kolonne
    df_ut <- df_ut %>% mutate(`Studium / år / N` = paste0(.data[[grupperingsvariabel]], 
                                                          " (", N , ")"))
    
    # Fjernar første kolonne og flyttar den nye kolonna først
    df_ut <- df_ut %>% select(-1) %>% relocate(last_col(), 1)
    
    # unngå #NUM! i excel
    df_ut[df_ut == "NaN"] <- NA
    
    # filtrerer bort linjer som ikkje har nok svar siste år
    df_ut <- df_ut %>% filter(df_ut[ncol(df_ut)] >= min_svar)
    
    return(df_ut)
  } # end print_fordeling
  
  # Lagar og returnerer snittabell for gitt variabel
  # Brukar (mean(as.numeric( for å handtere faktorar
  print_snitt_as_num <- function(sdf, variabel, arstal_eldre, arstal_siste) {
    grupperingsvariabel <- sdf %>% group_vars()
    # mellomlagre kolonnenamn for N
    kol_eldre <- paste(arstal_eldre)
    kol_siste <- paste(arstal_siste)
    nkol_eldre <- paste("N", arstal_eldre)
    nkol_siste <- paste("N", arstal_siste)
    
    df_ut <- sdf %>% filter(!is.na(.data[[variabel]])) %>%
      summarise(
        !!kol_eldre := mean(as.numeric(.data[[variabel]][gruppe_ar == kol_eldre]), na.rm = T),
        !!kol_siste := mean(as.numeric(.data[[variabel]][gruppe_ar == kol_siste]), na.rm = T),
        !!nkol_eldre := sum(!is.na(.data[[variabel]][gruppe_ar == kol_eldre])),
        !!nkol_siste := sum(!is.na(.data[[variabel]][gruppe_ar == kol_siste])),
        "p_verdi" = tryCatch(t.test(as.numeric(.data[[variabel]])[gruppe_ar == kol_eldre], 
                                    as.numeric(.data[[variabel]][gruppe_ar == kol_siste]),
                                    var.equal = F)$p.value, error = function(e) {return(NA)}, 
                             silent = TRUE)
      )
    
    if (variabel == "brutto_arslonn_vasket") {
      df_ut <- df_ut %>% mutate(across(c(2, 3), round, -3))
    }
    
    # Legg til kolonne med * for signifikans
    df_ut <- df_ut %>% mutate(pstjerne = case_when(
      p_verdi < 0.001 ~ "***",
      p_verdi < 0.01 ~ "**",
      p_verdi < 0.05 ~ "*",
      T ~ ""
    ))
    
    # Legge til kolonne ved å slå saman første kolonne og N-kolonne
    df_ut <- df_ut %>% mutate(`Studium / N` = 
                                paste0(.data[[grupperingsvariabel]], 
                                       " (", .data[[nkol_siste]] , "/", .data[[nkol_eldre]], ")",
                                       pstjerne))
    
    # Fjernar første kolonne og flyttar den nye kolonna først
    df_ut <- df_ut %>% select(-1) %>% relocate(last_col(), 1)
    
    # unngå #NUM! i excel
    df_ut[df_ut == "NaN"] <- NA
    
    # #* fjerne data med lav N
    # # fjernar resultat der det er færre enn minimumsgrensa har svart
    # kolonner <- ncol(df_ut) - 5
    for (x in 2:3) {
      df_ut[x][df_ut[x + 2] < min_svar] <- NA
    }
    # filtrerer bort linjer som ikkje har nok svar siste år
    df_ut <- df_ut %>% filter(df_ut[ncol(df_ut) - 2] >= min_svar)
    
    return(df_ut)
  } # end print_snitt_as_num
  
  # Lagar og returnerer snittabell for gitt variabel, berre for siste år
  # Til bruk der det er stor endring i spørsmål eller berre finst data for eitt år
  # Brukar (mean(as.numeric( for å handtere faktorar
  print_snitt_as_num_single <- function(sdf, variabel, arstal_siste) {
    grupperingsvariabel <- sdf %>% group_vars()
    # mellomlagre kolonnenamn for N
    kol_siste <- paste(arstal_siste)
    nkol_siste <- paste("N", arstal_siste)
    
    df_ut <- sdf %>% filter(!is.na(.data[[variabel]])) %>%
      summarise(
        !!kol_siste := mean(as.numeric(.data[[variabel]][gruppe_ar == kol_siste]), na.rm = T),
        !!nkol_siste := sum(!is.na(.data[[variabel]][gruppe_ar == kol_siste]))
      )
    
    if (variabel == "brutto_arslonn_vasket") {
      df_ut <- df_ut %>% mutate(across(c(2), round, -3))
    }
    
    # Legge til kolonne ved å slå saman første kolonne og N-kolonne
    df_ut <- df_ut %>% mutate(`Studium / N` = paste0(.data[[grupperingsvariabel]], 
                                                     " (", .data[[nkol_siste]] , ")"))
    
    # Fjernar første kolonne og flyttar den nye kolonna først
    df_ut <- df_ut %>% select(-1) %>% relocate(last_col(), 1)
    
    # unngå #NUM! i excel
    df_ut[df_ut == "NaN"] <- NA
    
    # #* fjerne data med lav N
    # filtrerer bort linjer som ikkje har nok svar siste år
    df_ut <- df_ut %>% filter(df_ut[ncol(df_ut)] >= min_svar)
    
    return(df_ut)
  } # end print_snitt_as_num_single
  
  # Lagar og returnerer snittabell for gitt variabel, over eit ukjent tal år
  # Brukar (mean(as.numeric( for å handtere faktorar
  print_snitt_as_num_serie <- function(sdf, variabel, gruppe_ar_var) {
    grupperingsvariabel <- sdf %>% group_vars()
    
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
    
    # unngå #NUM! i excel
    df_ut[df_ut == "NaN"] <- NA
    
    #* fjerne data med lav N
    # fjernar resultat der det er færre enn fire minimumsgrensa har svart
    kolonner <- ncol(df_ut_n)
    for (x in 2:kolonner) {
      df_ut[x][df_ut[(kolonner - 1) + x] < min_svar] <- NA
    }
    # filtrerer bort linjer som ikkje har nok svar siste år
    df_ut <- df_ut %>% filter(df_ut[ncol(df_ut)] >= min_svar)
    
    # print(df_ut)
    return(df_ut)
  } # end print_snitt_as_num_serie
  
  print_aggregert <- function() {
    
    
  } # END print_aggregert
  
  print_groups <- function() {
    if (aggregert) {
      # Utskrift av fakultet og OsloMet-nivå
      # Test å bruke ein intern funksjon her
      # print_aggregert() 
      
      # lag ny arbeidsbok sn == arknummer
      sn <- 1
      arbeidsbok <- createWorkbook()
      
      # loop gjennom rad for rad, legg til fane i arbeidsbok
      for (rad in seq(from = 1, nrow((utskriftsmal)))) {
        # sr = radnummer
        sr <- 1
        # sc = kolonnenummer
        sc <- 1
        # Legg til arkfane med tittel
        addWorksheet(arbeidsbok, sheetName = utskriftsmal[rad, "Arkfanetittel"])
        
        # Skriv ut spørsmålstekst til arket
        writeData(arbeidsbok, sheet = sn, x = utskriftsmal[rad, "Spørsmålstekst"], startCol = sc, startRow = sr)
        
        if (!is.na(utskriftsmal[rad, "Kommentar"])) {
          sr <- sr + 1
          writeData(arbeidsbok, sheet = sn, x = utskriftsmal[rad, "Kommentar"], startCol = sc, startRow = sr)
        }
        sr <- sr + 2
        
        # velg utskrift basert på utskriftsmal[rad, "Svartype"]
        if (utskriftsmal[rad, "Svartype"] == "fordeling") {
          df_ut_fakultet <- print_fordeling(sdf_full %>% group_by(Fakultet_ar), utskriftsmal[rad, "Variabel"])
          df_ut_OM <- print_fordeling(sdf_full %>% group_by(OM_ar), utskriftsmal[rad, "Variabel"])
        } else if(utskriftsmal[rad, "Svartype"] == "snitt_as_num" | utskriftsmal[rad, "Svartype"] == "snitt") {
          df_ut_fakultet <- print_snitt_as_num(sdf_full %>% group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
          df_ut_OM <- print_snitt_as_num(sdf_full %>% group_by(Institusjon), utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
        } else if (utskriftsmal[rad, "Svartype"] == "fordeling_single") {
          df_ut_fakultet <- print_fordeling(sdf_full %>% filter(gruppe_ar == arstal_siste) %>% group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"])
          df_ut_OM <- print_fordeling(sdf_full %>% filter(gruppe_ar == arstal_siste) %>% group_by(Institusjon), utskriftsmal[rad, "Variabel"])
        } else if(utskriftsmal[rad, "Svartype"] == "snitt_as_num_single") {
          df_ut_fakultet <- print_snitt_as_num_single(sdf_full %>% group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"], arstal_siste)
          df_ut_OM <- print_snitt_as_num_single(sdf_full %>% group_by(Institusjon), utskriftsmal[rad, "Variabel"], arstal_siste)
        } else if(utskriftsmal[rad, "Svartype"] == "snitt_as_num_serie") {
          df_ut_fakultet <- print_snitt_as_num_serie(sdf_full %>% group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"], "undersokelse_ar")
          df_ut_OM <- print_snitt_as_num_serie(sdf_full %>% group_by(Institusjon), utskriftsmal[rad, "Variabel"], "undersokelse_ar")
        }
        
        # Fakultetsnivå
        # Snur tabellen for å få rett rekkefølgje i graf
        # df_ut_fakultet <- df_ut_fakultet %>% arrange(desc(across(1)))
        
        writeData(arbeidsbok, sheet = sn, x = df_ut_fakultet, startCol = sc, startRow = sr, colNames = T, keepNA = T)
        sr <- sr + nrow(df_ut_fakultet) + 1
        
        # OsloMet-nivå
        # Snur tabellen for å få rett rekkefølgje i graf
        # df_ut_OM <- df_ut_OM %>% arrange(desc(across(1)))
        writeData(arbeidsbok, sheet = sn, x = df_ut_OM, startCol = sc, startRow = sr, colNames = F, keepNA = T)
        
        # Formater breidde første kolonne
        setColWidths(
          arbeidsbok,
          sn,
          cols = 1,
          widths = 45
        )
        
        # gjer klar til neste ark
        sn <- sn + 1
        
        # Til bruk i formatering - om det er snitt eller prosent, f.eks.
        if (F) {
          
        } 
      } # END LOOP
      
      # Lagre arbeidsbok    
      rapportfil <- paste0(surveyinfo, "/", "aggregert", " ",  survey, " ", arstal_siste, ".xlsx")
      print(rapportfil)
      saveWorkbook(wb = arbeidsbok,
                   file = rapportfil,
                   overwrite = TRUE)
    } else {
      # loopar gjennom fakultet, skriv ut arbeidsbok
      for (fakultet in sdf$Fakultetsnavn %>% unique %>% sort) {
        # lag ny arbeidsbok sn == arknummer
        sn <- 1
        arbeidsbok <- createWorkbook()
        
        print(fakultet)
        
        sdf_fakultet <- sdf_full %>% filter(Fakultetsnavn == !!fakultet)
        sdf_fakultet_studium_ar <- sdf_studium_ar %>% filter(Fakultetsnavn == !!fakultet)
        sdf_fakultet_studieprogramnavn <- sdf_studieprogramnavn %>% filter(Fakultetsnavn == !!fakultet)
        print(paste("studium_ar", nrow(sdf_fakultet_studium_ar)))
        print(paste("studieprogramnavn", nrow(sdf_fakultet_studieprogramnavn)))
        
        # Prøve å dele opp datasett etter institutt
        instituttliste <- sdf_fakultet$Institutt %>% unique %>% sort
        
        # loop gjennom rad for rad, legg til fane i arbeidsbok
        for (rad in seq(from = 1, nrow((utskriftsmal)))) {
          # sr = radnummer
          sr <- 1
          # sc = kolonnenummer
          sc <- 1
          # Legg til arkfane med tittel
          addWorksheet(arbeidsbok, sheetName = utskriftsmal[rad, "Arkfanetittel"])
          
          # Skriv ut spørsmålstekst til arket
          writeData(arbeidsbok, sheet = sn, x = utskriftsmal[rad, "Spørsmålstekst"], startCol = sc, startRow = sr)
          
          if (!is.na(utskriftsmal[rad, "Kommentar"])) {
            sr <- sr + 1
            writeData(arbeidsbok, sheet = sn, x = utskriftsmal[rad, "Kommentar"], startCol = sc, startRow = sr)
          }
          sr <- sr + 2
          
          df_ut <- NULL
          df_ut_2 <- NULL
          
          # velg utskrift basert på utskriftsmal[rad, "Svartype"]
          if (utskriftsmal[rad, "Svartype"] == "fordeling") {
            # df_ut <- print_fordeling(sdf_fakultet %>% group_by(Studieprogram_instnamn_ar), utskriftsmal[rad, "Variabel"])
            df_ut <- print_fordeling(sdf_fakultet %>% 
                                       filter(Institutt == instituttliste[1] | Institutt == instituttliste[2]) %>%
                                       group_by(Studieprogram_instnamn_ar), utskriftsmal[rad, "Variabel"])
            df_ut_2 <- print_fordeling(sdf_fakultet %>% 
                                         filter(Institutt != instituttliste[1] & Institutt != instituttliste[2]) %>%
                                         group_by(Studieprogram_instnamn_ar), utskriftsmal[rad, "Variabel"])
            df_ut_fakultet <- print_fordeling(sdf_fakultet %>% group_by(Fakultet_ar), utskriftsmal[rad, "Variabel"])
            # df_ut_OM <- print_fordeling(sdf %>% group_by(OM_ar), utskriftsmal[rad, "Variabel"])
          } else if(utskriftsmal[rad, "Svartype"] == "snitt_as_num" | utskriftsmal[rad, "Svartype"] == "snitt") {
            # df_ut <- print_snitt_as_num(sdf_fakultet_studieprogramnavn, utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
            df_ut <- print_snitt_as_num(sdf_fakultet_studieprogramnavn %>% 
                                          filter(Institutt == instituttliste[1] | Institutt == instituttliste[2]), 
                                        utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
            df_ut_2 <- print_snitt_as_num(sdf_fakultet_studieprogramnavn %>% 
                                            filter(Institutt != instituttliste[1] & Institutt != instituttliste[2]), 
                                          utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
            df_ut_fakultet <- print_snitt_as_num(sdf_fakultet %>% group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
            # df_ut_OM <- print_snitt_as_num(sdf %>% group_by(Institusjon), utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
          } else if (utskriftsmal[rad, "Svartype"] == "fordeling_single") {
            df_ut <- print_fordeling(sdf_fakultet %>% 
                                       filter(gruppe_ar == arstal_siste,
                                              Institutt == instituttliste[1] | Institutt == instituttliste[2]) %>%
                                       group_by(Studieprogram_instnamn), utskriftsmal[rad, "Variabel"])
            df_ut_2 <- print_fordeling(sdf_fakultet %>% 
                                         filter(gruppe_ar == arstal_siste,
                                                Institutt != instituttliste[1] & Institutt != instituttliste[2]) %>%
                                         group_by(Studieprogram_instnamn), utskriftsmal[rad, "Variabel"])
            df_ut_fakultet <- print_fordeling(sdf_fakultet %>% filter(gruppe_ar == arstal_siste) %>% 
                                                group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"])
          } else if(utskriftsmal[rad, "Svartype"] == "snitt_as_num_single") {
            # df_ut <- print_snitt_as_num(sdf_fakultet_studieprogramnavn, utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
            df_ut <- print_snitt_as_num_single(sdf_fakultet_studieprogramnavn %>% 
                                                 filter(Institutt == instituttliste[1] | Institutt == instituttliste[2]), 
                                               utskriftsmal[rad, "Variabel"], arstal_siste)
            df_ut_2 <- print_snitt_as_num_single(sdf_fakultet_studieprogramnavn %>% 
                                                   filter(Institutt != instituttliste[1] & Institutt != instituttliste[2]), 
                                                 utskriftsmal[rad, "Variabel"], arstal_siste)
            df_ut_fakultet <- print_snitt_as_num_single(sdf_fakultet %>% group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"], arstal_siste)
            # df_ut_OM <- print_snitt_as_num(sdf %>% group_by(Institusjon), utskriftsmal[rad, "Variabel"], arstal_eldre, arstal_siste)
          } else if(utskriftsmal[rad, "Svartype"] == "snitt_as_num_serie") {
            df_ut <- print_snitt_as_num_serie(sdf_fakultet_studieprogramnavn, utskriftsmal[rad, "Variabel"], "undersokelse_ar")
            df_ut_fakultet <- print_snitt_as_num_serie(sdf_fakultet %>% group_by(Fakultetsnavn), utskriftsmal[rad, "Variabel"], "undersokelse_ar")
            # df_ut_OM <- print_snitt_as_num_serie(sdf %>% group_by(Institusjon), utskriftsmal[rad, "Variabel"], "undersokelse_ar")
          }
          
          # Snur tabellen for å få rett rekkefølgje i graf
          df_ut <- df_ut %>% arrange(desc(across(1)))
          
          # tar bort fakultetnummer og instituttforkorting frå første kolonne
          firstColName <- colnames(df_ut)[1]
          df_ut <- df_ut %>% mutate(!!firstColName := gsub("^\\S+\\s", "", .data[[firstColName]]))
          
          # Skriv ut spørsmålstekst til arket
          # Første halvdel av institutta
          writeData(arbeidsbok, sheet = sn, x = df_ut, startCol = sc, startRow = sr, colNames = T, keepNA = T)
          sr <- sr + nrow(df_ut) + 1
          
          # Fakultet
          # Snur tabellen for å få rett rekkefølgje i graf
          df_ut_fakultet <- df_ut_fakultet %>% arrange(desc(across(1)))
          
          writeData(arbeidsbok, sheet = sn, x = df_ut_fakultet, startCol = sc, startRow = sr, colNames = F, keepNA = T)
          sr <- sr + nrow(df_ut_fakultet) + 1
          
          # Andre halvdel av institutta
          if (!is.null(df_ut_2)) {
            # Snur tabellen for å få rett rekkefølgje i graf
            df_ut_2 <- df_ut_2 %>% arrange(desc(across(1)))
            
            # tar bort fakultetnummer og instituttforkorting frå første kolonne
            firstColName <- colnames(df_ut_2)[1]
            df_ut_2 <- df_ut_2 %>% mutate(!!firstColName := gsub("^\\S+\\s", "", .data[[firstColName]]))
            
            sr <- sr + 2
            # Andre halvdel av institutta
            writeData(arbeidsbok, sheet = sn, x = df_ut_2, startCol = sc, startRow = sr, colNames = T, keepNA = T)
            sr <- sr + nrow(df_ut_2) + 1
            
            # Fakultet
            # Snur tabellen for å få rett rekkefølgje i graf
            df_ut_fakultet <- df_ut_fakultet %>% arrange(desc(across(1)))
            
            writeData(arbeidsbok, sheet = sn, x = df_ut_fakultet, startCol = sc, startRow = sr, colNames = F, keepNA = T)
            sr <- sr + nrow(df_ut_fakultet) + 1
          }
          
          # Formater breidde første kolonne
          setColWidths(
            arbeidsbok,
            sn,
            cols = 1,
            widths = 45
          )
          
          # gjer klar til neste ark
          sn <- sn + 1
          
          # Til bruk i formatering - om det er snitt eller prosent, f.eks.
          if (F) {
            
          } 
        } # END LOOP
        
        # Lagre arbeidsbok    
        rapportfil <- paste0(surveyinfo, "/", fakultet, " ",  survey, " ", arstal_siste, ".xlsx")
        print(rapportfil)
        saveWorkbook(wb = arbeidsbok,
                     file = rapportfil,
                     overwrite = TRUE)
      } # END fakultetloop
    } #END fakultetutskrift
  } # END print_group
  
  # Førebu datasett
  # TODO flytt group_by til metodekall for fordeling og snitt?
  sdf_studium_ar <- sdf %>% group_by(Studieprogram_instnamn_ar)
  sdf_studieprogramnavn <- sdf %>% group_by(Studieprogram_instnamn)
  
  # Skriv ut ei fil per fakultet - kan sikkert endrast til å bestemme grupperinga etter argument
  print_groups()
  
  # return(arbeidsbok) Bruk denne når shinykoplingane er klare

} # END OM_indikator_print_2022

##** 
##* Lagar arbeidsbokobjekt, med faner per fakultet, samanliknar snitt med året før

OM_print_2023 <- function(survey, source_df, source_df_forrige, malfil = "", nivå = "Alle") {
  # Set kva år som skal samanliknast - blir også brukt til å lage samanslått df for t.test
  sisteår <- source_df$undersøkelse_år %>% unique
  forrigeår <- source_df_forrige$undersøkelse_år %>% unique
  print(paste("Samanliknar", sisteår, "med", forrigeår))
  
  ##* Filtrering:
  #* ta bort dei utan fakultet
  #* ta bort program som ikkje er i source_df frå source_df_forrige
  # Tar bort tredjeåret, for å ha likt samanlikningsgrunnlag
  source_df <- source_df %>% filter(!is.na(FAKNAVN), STUDIEAR != 3 | is.na(STUDIEAR))
  source_df_forrige <- source_df_forrige %>% filter(!is.na(FAKNAVN), STUDIEAR != 3 | is.na(STUDIEAR))
  
  # Kan brukast for å samanlikne andreåret mot dei på tredjeåret
  # source_df <- source_df %>% filter(!is.na(FAKNAVN), STUDIEAR != 3)
  # source_df_forrige <- source_df_forrige %>% filter(!is.na(FAKNAVN), STUDIEAR == 3)
  
  print(source_df %>% NROW)  
  ##** Skiljer mellom Master og Bachelor
  ##* Endringar i 2022-kode: Sørga for å kode Bachelor/Master i dataframes, sparer kompleksitet her
  ##* 2023: tilbake til Studieprogram_instnamn
  ##* nivå kan vere Bachelor, Master eller Annet
  print(source_df %>% select(Nivå) %>% unique())
  if (nivå != "Alle") {
    source_df <- source_df %>% filter(Nivå == nivå)
    source_df_forrige <- source_df_forrige %>% filter(Nivå == nivå)
  }
  
  # Print
  # lag ny arbeidsbok sn == arknummer
  sn <- 1
  ab <- createWorkbook()
  
  ##** Hent mal frå xlsx
  utskriftsmal <- read.xlsx(malfil)
  
  for (fak in source_df$FAKNAVN %>% unique %>% sort) {
    # handterer manglande data, for å ha jamt tal på linjer
    utd_df <- source_df %>% filter(FAKNAVN == fak) %>% group_by(Studieprogram_instnamn, .drop=FALSE) 
    
    # Sikrar like mange linjer med data
    prog_n <- utd_df %>% select(Studieprogram_instnamn) %>% unique %>% NROW
    fak_df <- source_df %>% filter(FAKNAVN == fak) %>% group_by(FAKNAVN)
    
    # Kunne gjort dette utanfor loop, men då må det lagrast til ny variabel 
    source_df <- source_df %>% group_by(instnr)
    
    # Slår saman datasetta til bruk i samanlikning
    df_bound <- bind_rows(source_df, source_df_forrige)
    
    # Filtrerer bort program som ikkje finst i nyaste datasett
    utd_df_forrige <- source_df_forrige %>% 
      filter(FAKNAVN == fak, Studieprogram_instnamn %in% source_df$Studieprogram_instnamn) %>% 
      group_by(Studieprogram_instnamn, .drop=FALSE)
    
    # Slår saman datasetta til bruk i samanlikning
    df_utd_bound <- bind_rows(utd_df, utd_df_forrige)   
    
    fak_df_forrige <- source_df_forrige %>% filter(FAKNAVN == fak) %>% group_by(FAKNAVN)
    source_df_forrige <- source_df_forrige %>% group_by(instnr)
    
    # Slår saman datasetta til bruk i samanlikning
    # TODO Kan kanskje bruke berre samanslått sett, og bruke undersøkelse_år for å skilje?
    df_fak_bound <- bind_rows(fak_df, fak_df_forrige)  
    
    # sr == radnummer
    sr <- 3
    # sc == kolonnenummer
    sc <- 1
    # siste_likert_kol - siste kolonne å farge basert på femdelt skala
    siste_likert_kol <- 2
    
    # Arknamn + tittel
    print(fak_df[1,]$FAKNAVN)
    
    addWorksheet(ab, sheetName = strtrim(fak_df[1,]$FAKNAVN, 30))
    writeData(ab, sheet = sn, x = utskriftsmal[1, "Blokkoverskrift"], startCol = sc, startRow = 1, colNames = FALSE)
    writeData(ab, sheet = sn, x = paste(nivå, fak, sep = " - "), startCol = sc, startRow = 2, colNames = FALSE)
    
    # Første rad - program/fakultet/OM for alle blokkene
    uthevkol <- c(sc)
    skillekol <- c()
    SB_print_fc(utd_df, fak_df, source_df, sn, sc, sr, ab)
    sc <- sc + 1
    
    # rader til spesialformatering
    prosentkol <- c()
    sumtidkol <- NULL
    progresjonkol <- NULL
    
    ##** Skriv ut basert på xlsx-mal
    # Finn lengde på mal
    malrader <- NROW(utskriftsmal)
    # loop gjennom rad for rad
    for (rad in seq(from = 2, nrow((utskriftsmal)))) {
      if (!is.na(utskriftsmal[rad, "Utheving"]) & utskriftsmal[rad, "Utheving"]) {
        uthevkol <- append(uthevkol, sc)
      }
      if (!is.na(utskriftsmal[rad, "Blokkoverskrift"])) {
        # om det er overskriftsrad, legg til skillekolonne
        skillekol <- append(skillekol, sc)
        sc <- sc + 1
        SB_print_batteryheader(utskriftsmal[rad, "Blokkoverskrift"], utskriftsmal[rad, "Ingress"], ab, sn, sc)
      } else if (!is.na(utskriftsmal[rad, "Spørsmålstekst"])) {
        # om det er spørsmålsrad
        # SB_sub_print(utd_df, fak_df, source_df, source_df_forrige, ab, sn, sc, sr, prog_n, 
        SB_sub_print(utd_df, fak_df, source_df, utd_df_forrige, fak_df_forrige, source_df_forrige, ab, sn, sc, sr, prog_n, 
                     utskriftsmal[rad, "Spørsmålstekst"], utskriftsmal[rad, "Variabel"], utskriftsmal[rad, "Tid"])
        
        # Prosentformatering av enkelte kolonner
        if (!is.na(utskriftsmal[rad, "Format"]) & utskriftsmal[rad, "Format"] == "prosent") {
          prosentkol <- append(prosentkol, sc)
        }
        
        # Lagre kolonne med progresjon - for å kunne formatere Sum tid studier
        if (!is.na(utskriftsmal[rad, "Variabel"]) & utskriftsmal[rad, "Variabel"] == "progresjon") {
          progresjonkol <- sc
        }
        
        # Lagre kolonne med Sum tid studier - for å kunne formatere denne
        if (!is.na(utskriftsmal[rad, "Variabel"]) & utskriftsmal[rad, "Variabel"] == "sum_tid_studier") {
          sumtidkol <- sc
        }
        
        # Set siste kolonne som kan formaterast etter femdelt skala
        if (siste_likert_kol < 3 & utskriftsmal[rad, "Tid"]) {
          siste_likert_kol <- sc - 1
        }
        sc <- sc + 1
      }
      
      # Til bruk i formatering - om det er snitt eller prosent, f.eks.
      if (F) {
      }
    } # END loop gjennom malark
    
    qtextRow <- 3
    firstDataRow <- 4
    firstDataCol <- 2
    lastDataCol <- sc - 1
    qRowHeight <- 90
    
    # sr <- NROW(tmp_utd_df) + 7
    sr <- prog_n + 7
    # Farge øvst
    addStyle(ab, sn, SB_style_headerbg, rows = 1:qtextRow, cols = 1:lastDataCol, gridExpand = T)
    # addStyle(ab, sn, SB_style_wrap, rows = 2:qtextRow, cols = 1:lastDataCol, gridExpand = T, stack = T)
    addStyle(ab, sn, SB_style_header, rows = 1:2, cols = 1, gridExpand = T, stack = T)
    addStyle(ab, sn, SB_style_bold, rows = 1:2, cols = 1:lastDataCol, gridExpand = T, stack = T)
    addStyle(ab, sn, SB_style_normal, rows = 3, cols = 1, gridExpand = T, stack = T)
    
    addStyle(ab, sn, SB_style_bold, rows = qtextRow, cols = 2, gridExpand = T, stack = T)
    addStyle(ab, sn, SB_style_spm, rows = qtextRow, cols = 1:lastDataCol, gridExpand = T, stack = T)
    
    # Bakgrunnsfarge
    addStyle(ab, sn, SB_style_lgrey, rows = firstDataRow:sr, cols = 1:lastDataCol, gridExpand = T, stack = T)
    addStyle(ab, sn, SB_style_num, rows = firstDataRow:sr, cols = firstDataCol:lastDataCol, gridExpand = T, stack = T)
    
    # Farge på utheva kolonner 
    for (x in uthevkol) {
      addStyle(ab, sn, SB_style_headerbg, rows = firstDataRow:sr, cols = x, gridExpand = T, stack = T)
      # addStyle(ab, sn, SB_style_lgrey, rows = firstDataRow:sr, cols = x, gridExpand = T, stack = T)
      addStyle(ab, sn, SB_style_bold, rows = qtextRow:sr, cols = x, gridExpand = T, stack = T)
      addStyle(ab, sn, SB_style_batteryborder, rows = 1:qtextRow, cols = x, gridExpand = T, stack = T)
      addStyle(ab, sn, SB_style_num, rows = firstDataRow:sr, cols = x, gridExpand = T, stack = T)
    }
    
    # Farge på skillekolonner 
    for (x in skillekol) {
      addStyle(ab, sn, SB_style_lgrey, rows = 1:sr, cols = x, gridExpand = T)
    }
    
    # Prosentformatering}
    for (x in prosentkol) {
      addStyle(ab, sn, SB_style_perc, rows = firstDataRow:sr, cols = x, gridExpand = T, stack = T)
    }
    
    # https://www.rdocumentation.org/packages/openxlsx/versions/4.2.3/topics/conditionalFormatting
    # Conditional formatting
    # diff_celle <- firstDataRow + (NROW(tmp_utd_df) + 7) * 2
    diff_celle <- firstDataRow + (prog_n + 7) * 2
    # p_celle <- firstDataRow + (NROW(tmp_utd_df) + 7) * 3
    p_celle <- firstDataRow + (prog_n + 7) * 3
    print(paste("d", diff_celle, "p", p_celle))
    
    ndif <- paste("B", diff_celle, "<0", sep = "")
    pdif <- paste("B", diff_celle, "<0", sep = "")
    pdif <- paste("B", p_celle, ">=0.05", sep = "")
    
    # Regel for negativ
    diff_neg <- paste("B", diff_celle, "<0", sep = "")
    # diff_neg <- paste("AND(", ndif, ";", pdif, ")", sep = "")
    conditionalFormatting(ab, sn,
                          cols = firstDataCol:sc,
                          rows = firstDataRow:sr,
                          rule = diff_neg,
                          style = SB_style_sig_neg_ramme
    )
    
    # Regel for positiv
    diff_pos <- paste("B", diff_celle, ">0", sep = "")
    # diff_pos <- paste("AND(", pdif, ";", pdif, ")", sep = "")
    conditionalFormatting(ab, sn,
                          cols = firstDataCol:sc,
                          rows = firstDataRow:sr,
                          rule = diff_pos,
                          style = SB_style_sig_pos_ramme
    )
    
    # Regel for ikkje-signifikant
    non_sig <- paste("B", p_celle, ">=0.05", sep = "")
    conditionalFormatting(ab, sn,
                          cols = firstDataCol:sc,
                          rows = firstDataRow:sr,
                          rule = non_sig,
                          style = SB_style_default
    )
    
    # Regel for gode resultat på 1-5 likert
    godt_resultat <- c(4, 5)
    conditionalFormatting(ab, sn,
                          cols = firstDataCol:siste_likert_kol,
                          rows = firstDataRow:sr,
                          type = "between",
                          rule = godt_resultat,
                          style = SB_style_pos_res_fyll
    )
    
    # Regel for gode resultat på 1-5 likert
    svakt_resultat <- c(1, 3)
    conditionalFormatting(ab, sn,
                          cols = firstDataCol:siste_likert_kol,
                          rows = firstDataRow:sr,
                          type = "between",
                          rule = svakt_resultat,
                          style = SB_style_svakt_res_fyll
    )
    
    ##**
    ##* Usikker på om denne fører til god diskusjon, vi bør ha ein større samtale om tidsbruk
    ##* Formatering av Sum tid studier
    ##* Dersom Sum tid studier / progresjon < landssnitt studier -> svakt_res
    ##* Dersom Sum tid studier / progresjon er over venta tidsbruk for 1500 t/år -> pos_res
    landsnitt_tid <- 31
    # tid_minimum <- 34.5
    # tid_maksimum <- 41.5
    # 
    # sum_tid_svakt <- paste0(int2col(sumtidkol), firstDataRow, "/", 
    #                         int2col(progresjonkol), firstDataRow, "<",
    #                         landsnitt_tid)
    # 
    # conditionalFormatting(ab, sn,
    #                       cols = sumtidkol,
    #                       rows = firstDataRow:sr,
    #                       rule = sum_tid_svakt,
    #                       style = SB_style_svakt_res_fyll
    # )
    # 
    # Dersom fagleg tidsbruk er over landssnitt,
    # rekna på heiltidsstudentar med total tidsbruk mellom 11 og 61, marker grønt
    sum_tid_pos <- paste0(int2col(sumtidkol), firstDataRow, "/",
                            int2col(progresjonkol), firstDataRow, ">",
                          landsnitt_tid)

    conditionalFormatting(ab, sn,
                          cols = sumtidkol,
                          rows = firstDataRow:sr,
                          rule = sum_tid_pos,
                          style = SB_style_pos_res_fyll
    )
    # 
    # sum_tid_sus <- paste0(int2col(sumtidkol), firstDataRow, "/", 
    #                       int2col(progresjonkol), firstDataRow, ">",
    #                       tid_maksimum)
    # 
    # conditionalFormatting(ab, sn,
    #                       cols = sumtidkol,
    #                       rows = firstDataRow:sr,
    #                       rule = sum_tid_sus,
    #                       style = SB_style_sus_res_fyll
    # )
    
    # Lesbarheit
    setRowHeights(
      ab,
      sn,
      rows = qtextRow,
      heights = qRowHeight
    )
    
    # Første
    setColWidths(
      ab,
      sn,
      cols = 1,
      widths = 60
    )
    
    setColWidths(
      ab,
      sn,
      cols = firstDataCol:sc,
      widths = 17
    )
    
    # Skillekolonner
    setColWidths(
      ab,
      sn,
      cols = skillekol,
      widths = 2
    )
    
    freezePane(
      ab,
      sn,
      firstActiveRow = firstDataRow,
      firstActiveCol = 2
    )
    
    # formatering av ekstra blokker
    # blokkoverskrifter
    # N
    overskriftsrad <- firstDataRow + (prog_n + 5)
    addStyle(ab, sn, SB_style_bold, rows = overskriftsrad, cols = 1, gridExpand = T, stack = T)
    
    # diff
    overskriftsrad <- overskriftsrad + (prog_n + 7)
    diffblokkLast <- overskriftsrad + (prog_n + 5)
    addStyle(ab, sn, SB_style_bold, rows = overskriftsrad, cols = 1, gridExpand = T, stack = T)
    addStyle(ab, sn, SB_style_differanseblokk, rows = overskriftsrad:diffblokkLast, cols = 1:lastDataCol, gridExpand = T, stack = T)
    
    # p-verdi
    overskriftsrad <- overskriftsrad + (prog_n + 7)
    addStyle(ab, sn, SB_style_bold, rows = overskriftsrad, cols = 1, gridExpand = T, stack = T)
    
    sn <- sn + 1
  } #END sheet loop
  
  # TODO Denne krevjer XLConnect + JDK, men JDK-installasjonen feilar
  # Hentar inn forklaringsark frå mal
  # forklaringsmal <- "malfiler/Studiebarometeret veiledningsfane.xlsx"
  #  
  # forklaring_innhald <- loadWorkbook(forklaringsmal) 
  # forklaring_fanenamn <- "Forklaring"
  # addWorksheet(ab, forklaring_fanenamn)
  # writeData(ab, sheet = forklaring_fanenamn, x = forklaring_innhald)
  
  return(ab)
} # END OM_print_2023


# Datapakke til kvalitetsrapport --------------------------------------------------------------

# Eksportere datapakke per fakultet til programrapport - fane for kvar utdanning
# tidsserie - liste med datasett Studiebarometeret per siste tre år
# SA - datasett for siste tre år i Sisteårsstudenten
# SA_tidsserie21 <- rbind(SA22, SA21, SA20)
# tidsserie <- list(SB19, SB20, SB21)
# 2022 august: bytta studprog_kod til Studieprogramkode, studieprogram_instkode til StudiumID
datapakke_print_2022 <- function(tidsserie, SA, SB_fil = "", part = "") {
  # Plan: 
  # Splitt på fakultet
  # Lag filnamn per fakultet
  # For kvart fakultet - filtrer bort utdanningar som ikkje finst i siste år
  # For kvar utdanning: Nytt ark, skriv ut gjennomsnitt for variablar i kolonne for kvart år
  # Lagre fil, gå til neste fakultet
  
  # Del opp datasett, eldst til nyast
  # bruk tidsserie[[x]] for å velje
  # OBS oppdatere årstal
  Y1 <- tidsserie[[1]] %>% mutate(år = "2019")
  Y2 <- tidsserie[[2]] %>% mutate(år = "2020")
  Y3 <- tidsserie[[3]] %>% mutate(år = "2021")
  
  # ved å sette vektoren indikatorliste og funksjonen slim_variables til år-spesifikke versjonar,
  # kan resten av koden vere lik frå år til år
  indikatorliste <- indikatorliste22
  slim_variables <- slim_variables22
  
  Y1 <- slim_variables(Y1)
  Y2 <- slim_variables(Y2)
  Y3 <- slim_variables(Y3)
  
  # Tar bort utdanningar som ikkje finst i nyaste datasett
  Y1_sub <- Y1 %>% filter(Studieprogramkode %in% Y3$Studieprogramkode)
  Y2_sub <- Y2 %>% filter(Studieprogramkode %in% Y3$Studieprogramkode)
  
  # Slå saman datasett
  ys <- rbind(Y1_sub, Y2_sub, Y3)
  
  # Grupper på fakultet
  ys_fak <- ys %>% group_by(fakultet) %>% group_split()
  
  # for (fak in Y3$fakultet %>% unique %>% sort) {
  for (fak in ys_fak) {
    fak_n <- fak[1,]$fakultet
    path <- "C:\\Users\\kyrremat\\OneDrive - OsloMet\\Dokumenter\\statistikk\\Kvalitetsrapport datapakke R\\rapportfiler\\"
    nameroot <- "datapakke 2022"
    # if (part == "") part <- " testrapport"
    suf <- ".xlsx"
    # print(fak_n)
    SB_fil <- paste(path, fak_n, " ", nameroot, part, suf, sep = "")
    # print(SB_fil)
    
    # Grupper og splitt på program
    fak <- fak %>% group_by(Studieprogramkode) %>% group_split()
    # # Forsøkt Endra 2022 - nokre programkoder blir behandla felles, denne variabelen har dei slått saman
    # Svikta på at fanenamn i Excel blir like. Gikk tilbake til Studieprogramkode, sidan det truleg er det nivået ein skriv rapport for
    # fak <- fak %>% group_by(studieprogram_instkode) %>% group_split()  
    
    # Print
    # lag ny arbeidsbok sn == arknummer
    sn <- 1
    wb <- createWorkbook()
    
    for (utd in fak) {
      utd_id <- utd[1,]$Studieprogramkode
      # utd_id <- utd[1,]$studieprogram_instkode # Endra 2022 - nokre programkoder blir behandla felles, denne variabelen har dei slått saman
      # print(utd_id)
      # TODO tenk ut noko smart, trunkering gir duplikatnamn
      addWorksheet(wb, utd_id)
      # Studiebarometeret
      # writeData(wb, sn, paste(utd_id, " - ", utd[1,]$studieprogram_instkode))
      # prøver noko nytt for å handtere program som har ulike langnamn og same kode - som M1GLU
      arktittel <- paste(utd_id, " - ", paste(utd$StudiumID %>% unique(), collapse = " - "))
      # print(arktittel)
      writeData(wb, sn, arktittel)
      # writeData(wb, sn, utd[1,]$studieprogram_instkode) # Endra 2022 - nokre programkoder blir behandla felles, denne variabelen har dei slått saman
      # Forklaringstekst
      forklaring <- paste('Indikatorene er gjennomsnitt på en femdelt skala der høy verdi er positivt.\n',
                          'Når svaralternativene er Ja/Nei, er indikatoren andel som har svart "Ja".\n',
                          'De som har svart "Vet ikke" eller "Usikker" er ikke med i beregningene.')
      forklaring_n <- paste("Til høyre for gjennomsnittene står antall respondenter (N) per indikator.\n",
                            "Dersom færre enn fire har svart, har vi ikke tatt med resultatet.\n",
                            # "Vi har bare tatt med tall for programkoder der vi har data fra Studiebarometeret 2021.\n",
                            "Det er viktig å huske at tallene i Sisteårsundersøkelsen og Studiebarometeret ikke gjelder samme studentkull.\n",
                            "En kan derfor ikke forvente å observere sammenfallende trender i de to undersøkelsene.\n",
                            "Årstall viser til når undersøkelsen ble sendt ut.")
      forklaring_tid <- paste("Når NOKUT/Rambøll presenterer tidsbruk, tar de med alle svar. OsloMet har hatt en praksis med å filtrere bort svar",
                              "der summen av tid til organisert læring, egenstudier og arbeid blir under 11 eller over 60 timer per uke.",
                              "Vi tar her med verdier for begge varianter, og har markert versjonene der vi tar med også svar som gir samlet tidsbruk under 11 og over 60 timer.")
      forklaring_variabler <- paste("Indeksene er satt sammen av flere spørsmål, man må ha svart på de fleste av spørsmålene om et tema for at svarene skal bli inkludert i indeksen.")
      
      writeData(wb, sn, forklaring, startRow = 2)
      writeData(wb, sn, forklaring_n, startRow = 3)
      addStyle(wb, sn, cols = 1, rows = 2:3, style = SB_style_wrap, gridExpand = T, stack = T)
      
      utrad <- 5
      ##**
      ##* Lag tabell for spørsmål frå Studiebarometeret
      ##* 
      # Snitt
      # Summering av snitt, gruppert på år
      # utdata <- utd %>% group_by(år) %>% summarise(across(4:26, ~mean(., na.rm = T))) 
      # print(utd)
      # break()
      utdata <- utd %>% group_by(år) %>% summarise(across(4:(ncol(utd)-1), ~mean(., na.rm = T))) 
      # mellomlagre kolonne med årstal, for å kunne transformere tabell så variablane kjem som rader
      utdata_år <- utdata %>% select(1)
      # fjerner årstalkolonne og transformerer tabellen til vertikalt format
      utdata <- utdata %>% subset(select= -1) %>% t %>% as.data.frame() 
      # namngir årstalkolonner med årstal
      colnames(utdata) <- utdata_år[["undersøkelse_år"]]
      # gjer om radnamn til namngitt kolonne, for vidare behandling
      utdata <- utdata %>% rownames_to_column(., var="Variabel") 
      
      # Legg til spørsmålstekst
      utdata <- utdata %>% mutate(Indikator = indikatorliste)
      utdata <- relocate(utdata, Indikator, .after = Variabel)
      utdata <- utdata %>% subset(select = -Variabel)
      
      # N gruppert på år
      # utdata_n <- utd %>% group_by(år) %>% summarise(across(4:26, ~sum(!is.na(.) )))
      utdata_n <- utd %>% group_by(år) %>% summarise(across(4:(ncol(utd)-1), ~sum(!is.na(.) )))
      # mellomlagre kolonne med årstal, for å kunne transformere tabell så variablane kjem som rader
      # Legg til "N" før årstal for å kunne slå saman til ein tabell
      utdata_år <- utdata_n %>% select(1)
      utdata_år$undersøkelse_år <- paste("N", utdata_år$undersøkelse_år)
      # fjerner årstalkolonne og transformerer tabellen til vertikalt format
      utdata_n <- utdata_n %>% subset(select= -1) %>% t %>% as.data.frame() 
      # namngir årstalkolonner med årstal
      colnames(utdata_n) <- utdata_år[["undersøkelse_år"]]
      # tar bort variabelkolonne, den trengst ikkje i utskrift
      rownames(utdata_n) <- NULL
      
      # p-verdi endring mellom siste to år - viste seg ikkje vere så nyttig eller lett å presentere
      # utdata_p <- SB_utrad_p(utd, 2021, 2020)
      # print(utdata_p)
      # break()
      
      ##**
      ##* Lag tabell for spørsmål frå Sisteårsstudenten
      ##* 
      sa_utd <- SA %>% filter(Studieprogramkode == utd_id)
      # sa_utdata <- c()
      # print(utd_id)
      # Treng ikkje gjere dette viss det ikkje finst sisteårsdata
      if (NROW(sa_utd) > 0) {
        forklaring_variabler <- paste("Indeksene er satt sammen av flere spørsmål, man må ha svart på de fleste av spørsmålene om et tema for at svarene skal bli inkludert i indeksen.")
        # paste("Praksisindeksen er satt sammen av disse variablene, man må ha svart på minst tre av dem for at svarene skal bli inkludert i indeksen:\n",
        #                             "Hvor enig er du i disse påstandene?  [Jeg var godt forberedt til praksisperioden(e)];\n",
        #                             "Hvor enig er du i disse påstandene?  [Det var godt samarbeid mellom praksisstedet og OsloMet];\n",
        #                             "Hvor enig er du i disse påstandene?  [Teoriopplæringen var relevant for praksisutøvelsen];\n",
        #                             "Hvor enig er du i disse påstandene?  [Praksisutøvelsen ble brukt som grunnlag for diskusjon/refleksjon i undervisningen]\n")
        # 
        # Summering av snitt, gruppert på år
        # Det hadde gått an å bruke ein av desse, for å kunne kode Nei=0, Usikker=1, Ja=2:
        # summarise(mean(flerkulturell_kompetanse == 1, na.rm = T))
        # summarise(1-mean((flerkulturell_pre == "Ja" | flerkulturell_pre == "Yes"), na.rm = T))
        sa_utdata <- sa_utd %>% group_by(undersøkelse_år) %>% summarise(
          mean(flerkulturell_kompetanse, na.rm = T), 
          mean(nettbasert_internasjonalt, na.rm = T), 
          mean(indx_praksis4, na.rm = T))
        # mean(nyttigeemner, na.rm = T), 
        #   mean(godtlub, na.rm = T))
        # mellomlagre kolonne med årstal, for å kunne transformere tabell så variablane kjem som rader
        sa_utdata_år <- sa_utdata %>% select(1)
        # fjerner årstalkolonne og transformerer tabellen til vertikalt format
        sa_utdata <- sa_utdata %>% subset(select= -1) %>% t %>% as.data.frame() 
        # namngir årstalkolonner med årstal
        colnames(sa_utdata) <- sa_utdata_år[["undersøkelse_år"]]
        # gjer om radnamn til namngitt kolonne, for vidare behandling
        sa_utdata <- sa_utdata %>% rownames_to_column(., var="Variabel")
        
        # Legg til spørsmålstekst
        sa_utdata <- 
          sa_utdata %>% mutate(Indikator = 
                                 c("Studentenes opplevelse av læring som gir internasjonal og flerkulturell kompetanse (Andel svart ja)",
                                   "Studentenes deltagelse i nettbaserte grupper med studenter fra andre land (Sisteårsstudenten, kun stilt i 2022)",
                                   "Studentenes tilfredshet med praksisstudiene (indeks Sisteårsstudenten)")) 
        # c("Har alle emnene i studieprogrammet ditt vært nyttige (andel Ja)",
        #    "Jeg er godt fornøyd med læringsutbyttet jeg har hatt på studieprogrammet"))
        
        sa_utdata <- relocate(sa_utdata, Indikator, .after = Variabel)
        sa_utdata <- sa_utdata %>% subset(select = -Variabel)
        # Gjer om til NA, for å rydde bort #NUM! i excelfila
        sa_utdata[sa_utdata == "NaN"] <- NA
        
        # sa_utdata <- sa_utd %>% group_by(år) %>% 
        #   summarise(
        #       mean(nyttigeemner, na.rm = T), 
        #       mean(godtlub, na.rm = T)) %>% 
        #   data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
        
        # N gruppert på år
        sa_utdata_n <- sa_utd %>% group_by(år) %>% summarise(
          sum(!is.na(flerkulturell_kompetanse)), 
          sum(!is.na(nettbasert_internasjonalt)),
          sum(!is.na(indx_praksis4)))
        # sum(!is.na(nyttigeemner)), 
        #   sum(!is.na(godtlub)))
        # mellomlagre kolonne med årstal, for å kunne transformere tabell så variablane kjem som rader
        # Legg til "N" før årstal for å kunne slå saman til ein tabell
        utdata_år <- sa_utdata_n %>% select(1)
        utdata_år$undersøkelse_år <- paste("N", utdata_år$undersøkelse_år)
        # fjerner årstalkolonne og transformerer tabellen til vertikalt format
        sa_utdata_n <- sa_utdata_n %>% subset(select= -1) %>% t %>% as.data.frame() 
        # namngir årstalkolonner med årstal
        colnames(sa_utdata_n) <- utdata_år[["undersøkelse_år"]]
        # tar bort variabelkolonne, den trengst ikkje i utskrift
        rownames(sa_utdata_n) <- NULL
        
        # Slår saman snitt og N og fjernar resultat der det er færre enn fire som har svart
        sa_utdata_full <- cbind(sa_utdata, sa_utdata_n)
        kolonner <- ncol(sa_utdata)
        for (x in 2:kolonner) {
          sa_utdata_full[x][sa_utdata_full[(kolonner - 1) + x] < 4] <- NA
        }
        
        # Sisteårsstudenten - skriv ut berre om det finst data
        writeData(wb, sn, paste("Sisteårsstudenten", sa_utd[1,]$programkode), startRow = utrad)
        utrad <- utrad + 1
        writeDataTable(wb, sn, sa_utdata_full, startRow = utrad, keepNA = T, na.string = "-")
        utrad <- utrad + 1
        addStyle(wb, sn, cols = 2:NCOL(sa_utdata), rows = utrad:(utrad + 1), style = pct_stil, gridExpand = T, stack = T)
        utrad <- utrad + 2
        snitt_sluttkol <- NCOL(sa_utdata)
        addStyle(wb, sn, cols = 2:snitt_sluttkol, rows = utrad, style = desimal_stil, gridExpand = T, stack = T)
        
        utrad <- utrad + 2
      }
      
      # Slår saman snitt og N og fjernar resultat der det er færre enn fire som har svart
      sb_utdata_full <- cbind(utdata, utdata_n)
      kolonner <- ncol(utdata)
      for (x in 2:kolonner) {
        sb_utdata_full[x][sb_utdata_full[(kolonner - 1) + x] < 4] <- NA
      }
      
      writeData(wb, sn, paste("Studiebarometeret", utd[1,]$Studieprogramkode), startRow = utrad)
      utrad <- utrad + 1
      # Snitt
      # writeDataTable(wb, sn, cbind(utdata, utdata_n, utdata_p), startRow = utrad, keepNA = T, na.string = "-")
      writeDataTable(wb, sn, sb_utdata_full, startRow = utrad, keepNA = T, na.string = "-")
      snitt_startrad <- utrad
      snitt_sluttrad <- NROW(utdata) + utrad
      snitt_sluttkol <- NCOL(utdata)
      addStyle(wb, sn, cols = 2:snitt_sluttkol, rows = snitt_startrad:snitt_sluttrad, style = desimal_stil, gridExpand = T, stack = T)
      # N
      nkol <- NCOL(utdata) + 1
      # writeDataTable(wb, sn, utdata_n, startRow = utrad, startCol = nkol)
      
      utrad <- NROW(utdata) + utrad + 2
      writeData(wb, sn, forklaring_variabler, startRow = utrad)
      
      # Stil
      # setColWidths(wb, sn, cols = 1:2, widths = c(18, 63))
      setColWidths(wb, sn, cols = 1, widths = c(100))
      addStyle(wb, sn, cols = 1, rows = 1, style = SB_style_h1, gridExpand = T, stack = T)
      addStyle(wb, sn, cols = 1, rows = utrad, style = SB_style_wrap, gridExpand = T, stack = T)
      
      # setColWidths(wb, sn, cols = 6, widths = c(63))
      sn <- sn + 1
    }
    
    # Lagar dataframe med resultat frå Sisteårs som _ikkje_ er med i Studiebarometeret
    SA_not_in_SB <- SA %>% filter(programkode %!in% (ys$Studieprogramkode))
    SA_not_in_SB <- SA_not_in_SB %>% filter(fakultet == fak_n)
    
    # Hente Studieprogramnavn frå DBH
    # TODO: skriv om til å bruke dbh_add_programnavn()
    SA_not_in_SB <- left_join(SA_not_in_SB, dbh_hent_programdata() %>% 
                                select(Studieprogramkode, Studieprogramnavn), 
                              by=c("programkode" = "Studieprogramkode")) 
    
    SA_not_in_SB <- SA_not_in_SB %>% group_by(programkode) %>% group_split()
    #** 
    #* Skriv ut for dei som ikkje finst i Studiebarometeret
    for (utd in SA_not_in_SB) {
      utd_id <- utd[1,]$programkode
      # print(utd_id)
      # print(utd)
      addWorksheet(wb, utd_id)
      arktittel <- paste(utd_id, " - ", paste(utd$Studieprogramnavn %>% unique(), collapse = " - "))
      # print(arktittel)
      writeData(wb, sn, arktittel)
      
      # Forklaringstekst
      forklaring <- paste('Indikatorene er gjennomsnitt på en femdelt skala der høy verdi er positivt.\n',
                          'Når svaralternativene er Ja/Nei, er indikatoren andel som har svart "Ja".\n',
                          'De som har svart "Vet ikke" eller "Usikker" er ikke med i beregningene.')
      forklaring_n <- paste("Til høyre for gjennomsnittene står antall respondenter (N) per indikator.\n",
                            "Dersom færre enn fire har svart, har vi ikke tatt med resultatet.\n",
                            "For denne programkoden har vi ikke data fra Studiebarometeret 2021.\n",
                            "Årstall viser til når undersøkelsen ble sendt ut.")
      
      writeData(wb, sn, forklaring, startRow = 2)
      writeData(wb, sn, forklaring_n, startRow = 3)
      addStyle(wb, sn, cols = 1, rows = 2:3, style = SB_style_wrap, gridExpand = T, stack = T)
      
      utrad <- 5
      
      ##**
      ##* Lag tabell for spørsmål frå Sisteårsstudenten
      ##* 
      sa_utd <- SA %>% filter(Studieprogramkode == utd_id)
      # sa_utdata <- c()
      # print(utd_id)
      
      forklaring_variabler <- paste("Indeksene er satt sammen av flere spørsmål, man må ha svart på de fleste av spørsmålene om et tema for at svarene skal bli inkludert i indeksen.")
      # paste("Praksisindeksen er satt sammen av disse variablene, man må ha svart på minst tre av dem for at svarene skal bli inkludert i indeksen:\n",
      #                             "Hvor enig er du i disse påstandene?  [Jeg var godt forberedt til praksisperioden(e)];\n",
      #                             "Hvor enig er du i disse påstandene?  [Det var godt samarbeid mellom praksisstedet og OsloMet];\n",
      #                             "Hvor enig er du i disse påstandene?  [Teoriopplæringen var relevant for praksisutøvelsen];\n",
      #                             "Hvor enig er du i disse påstandene?  [Praksisutøvelsen ble brukt som grunnlag for diskusjon/refleksjon i undervisningen]\n")
      
      # Summering av snitt, gruppert på år
      # Det hadde gått an å bruke ein av desse, for å kunne kode Nei=0, Usikker=1, Ja=2:
      # summarise(mean(flerkulturell_kompetanse == 1, na.rm = T))
      # summarise(1-mean((flerkulturell_pre == "Ja" | flerkulturell_pre == "Yes"), na.rm = T))
      sa_utdata <- sa_utd %>% group_by(undersøkelse_år) %>% summarise(
        mean(flerkulturell_kompetanse, na.rm = T), 
        mean(nettbasert_internasjonalt, na.rm = T), 
        mean(indx_praksis4, na.rm = T))
      # mean(nyttigeemner, na.rm = T), 
      #   mean(godtlub, na.rm = T))
      # mellomlagre kolonne med årstal, for å kunne transformere tabell så variablane kjem som rader
      sa_utdata_år <- sa_utdata %>% select(1)
      # fjerner årstalkolonne og transformerer tabellen til vertikalt format
      sa_utdata <- sa_utdata %>% subset(select= -1) %>% t %>% as.data.frame() 
      # namngir årstalkolonner med årstal
      colnames(sa_utdata) <- sa_utdata_år[["undersøkelse_år"]]
      # gjer om radnamn til namngitt kolonne, for vidare behandling
      sa_utdata <- sa_utdata %>% rownames_to_column(., var="Variabel")
      
      # Legg til spørsmålstekst
      sa_utdata <- 
        sa_utdata %>% mutate(Indikator = 
                               c("Studentenes opplevelse av læring som gir internasjonal og flerkulturell kompetanse (Andel svart ja)",
                                 "Studentenes deltagelse i nettbaserte grupper med studenter fra andre land (Sisteårsstudenten, kun stilt i 2022)",
                                 "Studentenes tilfredshet med praksisstudiene (indeks Sisteårsstudenten)")) 
      # c("Har alle emnene i studieprogrammet ditt vært nyttige (andel Ja)",
      #    "Jeg er godt fornøyd med læringsutbyttet jeg har hatt på studieprogrammet"))
      
      sa_utdata <- relocate(sa_utdata, Indikator, .after = Variabel)
      sa_utdata <- sa_utdata %>% subset(select = -Variabel)
      # Gjer om til NA, for å rydde bort #NUM! i excelfila
      sa_utdata[sa_utdata == "NaN"] <- NA
      
      # sa_utdata <- sa_utd %>% group_by(år) %>% 
      #   summarise(
      #       mean(nyttigeemner, na.rm = T), 
      #       mean(godtlub, na.rm = T)) %>% 
      #   data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
      
      # N gruppert på år
      sa_utdata_n <- sa_utd %>% group_by(undersøkelse_år) %>% summarise(
        sum(!is.na(flerkulturell_kompetanse)), 
        sum(!is.na(nettbasert_internasjonalt)),
        sum(!is.na(indx_praksis4)))
      # sum(!is.na(nyttigeemner)), 
      #   sum(!is.na(godtlub)))
      # mellomlagre kolonne med årstal, for å kunne transformere tabell så variablane kjem som rader
      # Legg til "N" før årstal for å kunne slå saman til ein tabell
      utdata_år <- sa_utdata_n %>% select(1)
      utdata_år$undersøkelse_år <- paste("N", utdata_år$undersøkelse_år)
      # fjerner årstalkolonne og transformerer tabellen til vertikalt format
      sa_utdata_n <- sa_utdata_n %>% subset(select= -1) %>% t %>% as.data.frame() 
      # namngir årstalkolonner med årstal
      colnames(sa_utdata_n) <- utdata_år[["undersøkelse_år"]]
      # tar bort variabelkolonne, den trengst ikkje i utskrift
      rownames(sa_utdata_n) <- NULL
      
      # Slår saman snitt og N og fjernar resultat der det er færre enn fire som har svart
      sa_utdata_full <- cbind(sa_utdata, sa_utdata_n)
      kolonner <- ncol(sa_utdata)
      for (x in 2:kolonner) {
        # TODO ta høgde for ulike lengder på df
        sa_utdata_full[x][sa_utdata_full[(kolonner - 1) + x] < 4] <- NA
      }
      
      # Sisteårsstudenten - skriv ut berre om det finst data
      writeData(wb, sn, paste("Sisteårsstudenten", sa_utd[1,]$programkode), startRow = utrad)#, startCol = 6)
      utrad <- utrad + 1
      writeDataTable(wb, sn, sa_utdata_full, startRow = utrad, keepNA = T, na.string = "-")#, startCol = 6)
      utrad <- utrad + 1
      addStyle(wb, sn, cols = 2:NCOL(sa_utdata), rows = utrad:(utrad + 1), style = pct_stil, gridExpand = T, stack = T)
      utrad <- utrad + 2
      snitt_sluttkol <- NCOL(sa_utdata)
      addStyle(wb, sn, cols = 2:snitt_sluttkol, rows = utrad, style = desimal_stil, gridExpand = T, stack = T)
      
      utrad <- utrad + 2
      
      
      # Stil
      # setColWidths(wb, sn, cols = 1:2, widths = c(18, 63))
      setColWidths(wb, sn, cols = 1, widths = c(100))
      addStyle(wb, sn, cols = 1, rows = 1, style = SB_style_h1, gridExpand = T, stack = T)
      addStyle(wb, sn, cols = 1, rows = utrad, style = SB_style_wrap, gridExpand = T, stack = T)
      
      # setColWidths(wb, sn, cols = 6, widths = c(63))
      sn <- sn + 1
    } # END Sisteårs ikkje med i Studiebarometeret
    
    # Lagre filer
    saveWorkbook(wb, SB_fil, overwrite = TRUE)
    # *****************
  } # END print loop
  # *****************
  
} # END datapakke_print_2022

# Signifikanstestar endring i snitt mellom år
# varc er variabelen ein skal samanlikne, varc2 er grupperingsvariabel
# SB_utrad_p(fak_df, fak_df_forrige, varnamn, fakultet)
datapakke_utrad_p <- function(sdf, sisteår, forrigeår) {
  # TODO across(5:27, kan det bli meir elegant?
  # IKkje sikkert vi bør anta lik varians ...
  df_p <- sdf %>% summarise(across(5:NCOL(sdf), 
                                   ~tryCatch(t.test(.[år == sisteår], 
                                                    .[år == forrigeår],
                                                    var.equal = TRUE)$p.value, error = function(e) {return(NA)}, 
                                             silent = TRUE)))
  df_p <- df_p %>% t
  colnames(df_p) <- "p"
  return(df_p)
}

# Dupliser og namngi per år for å kunne skilje
# Select variables helper
datapakke_slim_variables22 <- function(sdf) {
  # Studentenes tilfredshet med undervisning (indeks Studiebarometeret)
  # indx_underv4
  # Studentenes tilfredshet med veiledning (indeks Studiebarometeret)
  # indx_tilbveil4
  # Faglig ansattes forventninger til studentene (indeks Studiebarometeret)
  # indx_forvent4
  # Bruk av digitale verktøy (indeks Studiebarometeret)
  # indx_digit4
  # Studentenes tilfredshet med vurderingsformene (indeks Studiebarometeret)
  # indx_vurd5
  
  sdf %>% select(fakultet, 
                 Studieprogramkode, 
                 StudiumID,
                 # studieprogram_instkode, 
                 år,
                 indx_underv4,
                 indx_tilbveil4,
                 indx_forvent4,
                 indx_digit4,
                 indx_vurd5
                 # organ_fagligsam_17,
                 # vurd_fagutv_17,
                 # egeteng_motivert_14,
                 # egeteng_orgakt_14,
                 # egeteng_forberedt_14,
                 # overord_garpahelst_13,
                 # egeteng_innsats_14,
                 # tid_orgstudier, 
                 # tid_egenstudier,
                 # sum_tid_studier,
                 # indx_laerutb10,
                 # laerutb_teori_13,
                 # laerutb_metforsk_13,
                 # laerutb_egenerf_13,
                 # laerutb_fagspes_13,
                 # laerutb_refleks_13,
                 # laerutb_samarb_13,
                 # laerutb_muntkom_13,
                 # laerutb_skriftkom_13,
                 # laerutb_tenke_13,
                 # laerutb_selvst_13,
                 # tidsbruk_laerakt_14,
                 # tidsbruk_egeninns_14
  )
}

# Dupliser og namngi per år for å kunne skilje
# Må matche variabellista over
datapakke_indikatorliste22 <- (c(
  "Studentenes tilfredshet med undervisning (indeks Studiebarometeret)",
  "Studentenes tilfredshet med veiledning (indeks Studiebarometeret)",
  "Faglig ansattes forventninger til studentene (indeks Studiebarometeret)",
  "Bruk av digitale verktøy (indeks Studiebarometeret)",
  "Studentenes tilfredshet med vurderingsformene (indeks Studiebarometeret)"
  # "Den faglige sammenhengen mellom emnene i studieprogrammet",
  # "Om eksamener, innleveringer og andre vurderingsformer hittil i studieprogrammet ditt har bidratt til faglig utvikling",
  # "Jeg er motivert for studieinnsats",
  # "Jeg benytter meg av de organiserte læringsaktivitetene som tilbys",
  # "Jeg møter godt forberedt til undervisningen",
  # "Jeg går på det studieprogrammet jeg helst vil gå på",
  # "Jeg opplever at studieinnsatsen min er høy",
  # "Timetall - Læringsaktiviteter organisert av institusjonen (undervisning, veiledning og praksis",
  # "Timetall - Egenstudier (inkl. frivillig studiearbeid med andre studenter)",
  # "Timetall - Tidsbruk studier totalt",
  # "Eget læringsutbytte (indeks)",
  # "LUB - Teoretisk kunnskap",
  # "LUB - Kunnskap om vitenskapelig arbeidsmetode og forskning",
  # "LUB - Egen erfaring med forsknings- og utviklingsarbeid",
  # "LUB - Yrkes- og fagspesifikke ferdigheter",
  # "LUB - Evne til refleksjon og kritisk tenking",
  # "LUB - Samarbeidsevne",
  # "LUB - Muntlig kommunikasjonsevne",
  # "LUB - Skriftlig kommunikasjonsevne",
  # "LUB - Evne til å tenke nytt",
  # "LUB - Evne til å arbeide selvstendig",
  # "Timetall - Læringsaktiviteter organisert av institusjonen (med ekstremverdier)",
  # "Timetall - Egenstudier (med ekstremverdier)"
))

# TODO: etterlign oppsett på programnivå
# Eksportere datapakke til fakultetsrapport - OsloMet-tal og per fakultet
# tidsserie - liste med datasett Studiebarometeret per siste tre år
# SA - datasett for siste tre år i Sisteårsstudenten
# nivå - 0 for bachelor, 1 for master
datapakke_print_aggregert_2022 <- function(tidsserie, SA, SB_fil = "", part = "", nivå = "") {
  # Del opp datasett, eldst til nyast, sett inn årstal
  # bruk tidsserie[[x]] for å velje
  # OBS oppdatere årstal
  Y1 <- tidsserie[[1]] %>% mutate(år = "2019")
  Y2 <- tidsserie[[2]] %>% mutate(år = "2020")
  Y3 <- tidsserie[[3]] %>% mutate(år = "2021")
  
  if (nivå != "") {
    Y1 <- Y1 %>% filter(master == nivå)
    Y2 <- Y2 %>% filter(master == nivå)
    Y3 <- Y3 %>% filter(master == nivå)
    
    if (nivå == 0) {
      titlepart <- "bachelor"
      part <- paste(" bachelor", part)
      SA <- SA %>% filter(master == "BA" | master == "4-årig studium" | master == "HK")
    } else if (nivå == 1) {
      titlepart <- "master"
      part <- paste(" master", part)
      SA <- SA %>% filter(master == "MA")
    }
  }
  
  # ved å sette vektoren indikatorliste og funksjonen slim_variables til år-spesifikke versjonar,
  # kan resten av koden vere lik frå år til år
  indikatorliste <- indikatorliste22
  slim_variables <- slim_variables22
  
  Y1 <- slim_variables(Y1)
  Y2 <- slim_variables(Y2)
  Y3 <- slim_variables(Y3)
  # # Tidsvariablar må justerast, slik at deltid ikkje blir tatt med
  # Y1$tid_orgstudier[Y1$heltid < 1] <- NA
  # Y2$tid_orgstudier[Y2$heltid < 1] <- NA
  # Y3$tid_orgstudier[Y3$heltid < 1] <- NA
  # 
  # Y1$tid_egenstudier[Y1$heltid < 1] <- NA
  # Y2$tid_egenstudier[Y2$heltid < 1] <- NA
  # Y3$tid_egenstudier[Y3$heltid < 1] <- NA
  # 
  # Y1$sum_tid_studier[Y1$heltid < 1] <- NA
  # Y2$sum_tid_studier[Y2$heltid < 1] <- NA
  # Y3$sum_tid_studier[Y3$heltid < 1] <- NA
  # 
  # Y1$tidsbruk_laerakt_14[Y1$heltid < 1] <- NA
  # Y2$tidsbruk_laerakt_14[Y2$heltid < 1] <- NA
  # Y3$tidsbruk_laerakt_14[Y3$heltid < 1] <- NA
  # 
  # Y1$tidsbruk_egeninns_14[Y1$heltid < 1] <- NA
  # Y2$tidsbruk_egeninns_14[Y2$heltid < 1] <- NA
  # Y3$tidsbruk_egeninns_14[Y3$heltid < 1] <- NA
  
  # Slå saman datasett
  ys <- rbind(Y1, Y2, Y3)
  
  # Grupper på fakultet
  ys_fak <- ys %>% group_by(fakultet) %>% group_split()
  
  # Berre ei fil, med fane for OsloMet + fakulteta
  sn <- 1
  wb <- createWorkbook()
  
  # Filnamn
  path <- "C:\\Users\\kyrremat\\OneDrive - OsloMet\\Dokumenter\\statistikk\\Kvalitetsrapport datapakke R\\rapportfiler\\"
  nameroot <- "OsloMet datapakke 2022 aggregert"
  # if (part == "") part <- " testrapport"
  suf <- ".xlsx"
  # print(fak_n)
  SB_fil <- paste(path, nameroot, part, suf, sep = "")
  
  # Forklaringstekst
  forklaring <- paste('Indikatorene er gjennomsnitt på en femdelt skala der høy verdi er positivt.\n',
                      'Når svaralternativene er Ja/Nei, er indikatoren andel som har svart "Ja".\n',
                      'De som har svart "Vet ikke" eller "Usikker" er ikke med i beregningene.')
  forklaring_n <- paste("Til høyre for gjennomsnittene står antall respondenter (N) per indikator.\n",
                        "Dersom færre enn fire har svart, har vi ikke tatt med resultatet.\n",
                        # "Vi har bare tatt med tall for programkoder der vi har data fra Studiebarometeret 2021.\n",
                        "Det er viktig å huske at tallene i Sisteårsundersøkelsen og Studiebarometeret ikke gjelder samme studentkull.\n",
                        "En kan derfor ikke forvente å observere sammenfallende trender i de to undersøkelsene.\n",
                        "Årstall viser til når undersøkelsen ble sendt ut.")
  forklaring_tid <- paste("Når NOKUT/Rambøll presenterer tidsbruk, tar de med alle svar. OsloMet har hatt en praksis med å filtrere bort svar",
                          "der summen av tid til organisert læring, egenstudier og arbeid blir under 11 eller over 60 timer per uke.",
                          "Vi tar her med verdier for begge varianter, og har markert versjonene der vi tar med også svar som gir samlet tidsbruk under 11 og over 60 timer.")
  forklaring_variabler <- paste("Indeksene er satt sammen av flere spørsmål, man må ha svart på de fleste av spørsmålene om et tema for at svarene skal bli inkludert i indeksen.")
  # paste("Praksisindeksen er satt sammen av disse variablene, man må ha svart på minst tre av dem for at svarene skal bli inkludert i indeksen:\n",
  #                             "Hvor enig er du i disse påstandene?  [Jeg var godt forberedt til praksisperioden(e)];\n",
  #                             "Hvor enig er du i disse påstandene?  [Det var godt samarbeid mellom praksisstedet og OsloMet];\n",
  #                             "Hvor enig er du i disse påstandene?  [Teoriopplæringen var relevant for praksisutøvelsen];\n",
  #                             "Hvor enig er du i disse påstandene?  [Praksisutøvelsen ble brukt som grunnlag for diskusjon/refleksjon i undervisningen]\n")
  # OsloMet-fane
  addWorksheet(wb, "OsloMet")
  # Snitt
  # utdata <- ys %>% group_by(år) %>% summarise(across(4:26, ~mean(., na.rm = T))) %>%
  utdata <- ys %>% group_by(år) %>% summarise(across(4:(ncol(ys)-1), ~mean(., na.rm = T))) %>%
    data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
  # N
  # utdata_n <- ys %>% group_by(år) %>% summarise(across(4:26, ~sum(!is.na(.) )))%>% 
  utdata_n <- ys %>% group_by(år) %>% summarise(across(4:(ncol(ys)-1), ~sum(!is.na(.) )))%>% 
    data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
  
  # Legg til spørsmålstekst
  utdata <- utdata %>% mutate(Indikator = indikatorliste)
  utdata <- relocate(utdata, Indikator, .after = Variabel)
  utdata <- utdata %>% subset(select = -Variabel)
  
  utdata_n <- utdata_n %>% subset(select = -Variabel)
  utdata_n <- utdata_n %>% rename_with(~paste("N", .x))
  
  # Lag tabell for spørsmål frå Sisteårsstudenten
  # Snitt
  sa_utdata <- SA %>% group_by(år) %>% 
    summarise(
      mean(flerkulturell_kompetanse, na.rm = T), 
      mean(nettbasert_internasjonalt, na.rm = T), 
      mean(indx_praksis4, na.rm = T)) %>%
    # mean(nyttigeemner, na.rm = T), 
    # mean(godtlub, na.rm = T)) %>% 
    data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
  
  # Gjer om til NA, for å rydde bort #NUM! i excelfila
  sa_utdata[sa_utdata == "NaN"] <- NA
  
  # N
  sa_utdata_n <- SA %>% group_by(år) %>% 
    summarise(
      sum(!is.na(flerkulturell_kompetanse)), 
      sum(!is.na(nettbasert_internasjonalt)), 
      sum(!is.na(indx_praksis4))) %>%
    # sum(!is.na(nyttigeemner)), 
    # sum(!is.na(godtlub)) ) %>% 
    data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
  # TODO sjekk om desse kan oppdaterast til same kode som for fakultetsutskrift, 
  # for å gjere dei meir robuste. Omnamninga er kanskje gjort betre her.
  # søk etter utdata_år$år <- paste("N", utdata_år$år)
  
  
  # Legg til spørsmålstekst
  # print(sa_utd[1,]$programkode)
  sa_utdata <- 
    sa_utdata %>% mutate(Indikator = 
                           c("Studentenes opplevelse av læring som gir internasjonal og flerkulturell kompetanse (Andel svart ja)",
                             # OBS: FÅ INN DENNE NÅR 2022-data kjem
                             "Studentenes deltagelse i nettbaserte grupper med studenter fra andre land (Sisteårsstudenten, kun stilt i 2022)",
                             "Studentenes tilfredshet med praksisstudiene (indeks Sisteårsstudenten)")) 
  # c("Har alle emnene i studieprogrammet ditt vært nyttige (andel Ja)",
  #   "Jeg er godt fornøyd med læringsutbyttet jeg har hatt på studieprogrammet"))
  
  sa_utdata <- relocate(sa_utdata, Indikator, .after = Variabel)
  sa_utdata <- sa_utdata %>% subset(select = -Variabel)
  
  # Gjer om til NA, for å rydde bort #NUM! i excelfila
  sa_utdata[sa_utdata == "NaN"] <- NA
  
  sa_utdata_n <- sa_utdata_n %>% subset(select = -Variabel)
  sa_utdata_n <- sa_utdata_n %>% rename_with(~paste("N", .x))
  
  # Studiebarometeret
  writeData(wb, sn, paste("OsloMet", titlepart, sep = " - "))
  
  writeData(wb, sn, forklaring, startRow = 2)
  writeData(wb, sn, forklaring_n, startRow = 3)
  addStyle(wb, sn, cols = 1, rows = 2:3, style = SB_style_wrap, gridExpand = T, stack = T)
  
  utrad <- 5
  
  # Slår saman snitt og N og fjernar resultat der det er færre enn fire som har svart
  sa_utdata_full <- cbind(sa_utdata, sa_utdata_n)
  kolonner <- ncol(sa_utdata)
  for (x in 2:kolonner) {
    sa_utdata_full[x][sa_utdata_full[(kolonner - 1) + x] < 4] <- NA
  }
  
  # Sisteårsstudenten
  writeData(wb, sn, "Sisteårsstudenten - OsloMet", startRow = utrad)
  utrad <- utrad + 1
  # Slår saman til éin tabell og skriv ut
  writeDataTable(wb, sn, sa_utdata_full, startRow = utrad, keepNA = T, na.string = "-")
  utrad <- utrad + 1
  addStyle(wb, sn, cols = 2:NCOL(sa_utdata), rows = utrad:(utrad + 1), style = pct_stil, gridExpand = T, stack = T)
  utrad <- utrad + 2
  snitt_sluttkol <- NCOL(sa_utdata)
  addStyle(wb, sn, cols = 2:snitt_sluttkol, rows = utrad, style = desimal_stil, gridExpand = T, stack = T)
  # writeDataTable(wb, sn, sa_utdata, startRow = utrad)
  # addStyle(wb, sn, cols = 2:NCOL(sa_utdata), rows = utrad + 1, style = pct_stil, gridExpand = T, stack = T)
  # 
  # nkol <- NCOL(sa_utdata) + 1
  # writeDataTable(wb, sn, sa_utdata_n, startRow = utrad, startCol = nkol)
  
  utrad <- utrad + 2
  
  # Slår saman snitt og N og fjernar resultat der det er færre enn fire som har svart
  sb_utdata_full <- cbind(utdata, utdata_n)
  kolonner <- ncol(utdata)
  for (x in 2:kolonner) {
    sb_utdata_full[x][sb_utdata_full[(kolonner - 1) + x] < 4] <- NA
  }
  
  writeData(wb, sn, "Studiebarometeret - OsloMet", startRow = utrad)
  utrad <- utrad + 1
  # Skriv ut
  writeDataTable(wb, sn, sb_utdata_full, startRow = utrad, keepNA = T, na.string = "-")
  snitt_startrad <- utrad
  snitt_sluttrad <- NROW(utdata) + utrad
  snitt_sluttkol <- NCOL(utdata)
  addStyle(wb, sn, cols = 2:snitt_sluttkol, rows = snitt_startrad:snitt_sluttrad, style = desimal_stil, gridExpand = T, stack = T)
  # # Snitt
  # writeDataTable(wb, sn, utdata, startRow = utrad)
  # # N
  # nkol <- NCOL(utdata) + 1
  # writeDataTable(wb, sn, utdata_n, startRow = utrad, startCol = nkol)
  # 
  utrad <- snitt_sluttrad + 2
  writeData(wb, sn, forklaring_variabler, startRow = utrad)
  
  
  # Stil
  setColWidths(wb, sn, cols = 1, widths = c(100))
  addStyle(wb, sn, cols = 1, rows = 1, style = SB_style_h1, gridExpand = T, stack = T)
  addStyle(wb, sn, cols = 1, rows = utrad, style = SB_style_wrap, gridExpand = T, stack = T)
  
  sn <- sn + 1
  # slutt OsloMet-fane
  
  for (fak in ys_fak) {
    fak_n <- fak[1,]$fakultet
    # fak_lang <- fak[1,]$FAKNAVN
    
    # Snitt
    # utdata <- fak %>% group_by(år) %>% summarise(across(4:26, ~mean(., na.rm = T))) %>% 
    utdata <- fak %>% group_by(år) %>% summarise(across(4:(ncol(fak)-1), ~mean(., na.rm = T))) %>% 
      data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
    
    # N
    # utdata_n <- fak %>% group_by(år) %>% summarise(across(4:26, ~sum(!is.na(.) )) ) %>% 
    utdata_n <- fak %>% group_by(år) %>% summarise(across(4:(ncol(fak)-1), ~sum(!is.na(.) )) ) %>% 
      data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
    
    # Legg til spørsmålstekst
    utdata <- utdata %>% mutate(Indikator = indikatorliste)
    utdata <- relocate(utdata, Indikator, .after = Variabel)
    utdata <- utdata %>% subset(select = -Variabel)
    
    utdata_n <- utdata_n %>% subset(select = -Variabel)
    utdata_n <- utdata_n %>% rename_with(~paste("N", .x))
    # print(utdata %>% names)
    
    # Lag tabell for spørsmål frå Sisteårsstudenten
    sa_f <- SA %>% filter(fakultet == fak_n)
    
    # Snitt
    sa_utdata <- sa_f %>% group_by(år) %>% 
      summarise(
        mean(flerkulturell_kompetanse, na.rm = T), 
        mean(nettbasert_internasjonalt, na.rm = T), 
        mean(indx_praksis4, na.rm = T)) %>%
      # mean(nyttigeemner, na.rm = T), 
      # mean(godtlub, na.rm = T)) %>% 
      data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
    
    # N
    sa_utdata_n <- sa_f %>% group_by(år) %>% 
      summarise(
        sum(!is.na(flerkulturell_kompetanse)), 
        sum(!is.na(nettbasert_internasjonalt)), 
        sum(!is.na(indx_praksis4))) %>%
      # sum(!is.na(nyttigeemner)), 
      # sum(!is.na(godtlub)) ) %>% 
      data.frame(., row.names = 1) %>% t %>% as.data.frame() %>% rownames_to_column(., var="Variabel")
    
    # Legg til spørsmålstekst
    sa_utdata <- 
      sa_utdata %>% mutate(Indikator = 
                             c("Studentenes opplevelse av læring som gir internasjonal og flerkulturell kompetanse (Andel svart ja)",
                               "Studentenes deltagelse i nettbaserte grupper med studenter fra andre land (Sisteårsstudenten, kun stilt i 2022)",
                               "Studentenes tilfredshet med praksisstudiene (indeks Sisteårsstudenten, ev. praksisevaluering)")) 
    # c("Har alle emnene i studieprogrammet ditt vært nyttige (andel Ja)",
    #   "Jeg er godt fornøyd med læringsutbyttet jeg har hatt på studieprogrammet"))
    
    # Legg til spørsmålstekst, tar bort variabelnamn
    sa_utdata <- relocate(sa_utdata, Indikator, .after = Variabel)
    sa_utdata <- sa_utdata %>% subset(select = -Variabel)
    # Gjer om til NA, for å rydde bort #NUM! i excelfila
    sa_utdata[sa_utdata == "NaN"] <- NA
    
    sa_utdata_n <- sa_utdata_n %>% subset(select = -Variabel)
    sa_utdata_n <- sa_utdata_n %>% rename_with(~paste("N", .x))
    # print(sa_utdata)
    
    addWorksheet(wb, fak_n)
    # Studiebarometeret
    writeData(wb, sn, paste(fak_n, titlepart, sep = " - "))
    
    writeData(wb, sn, forklaring, startRow = 2)
    writeData(wb, sn, forklaring_n, startRow = 3)
    addStyle(wb, sn, cols = 1, rows = 2:3, style = SB_style_wrap, gridExpand = T, stack = T)
    
    utrad <- 5
    
    # Slår saman snitt og N og fjernar resultat der det er færre enn fire som har svart
    sa_utdata_full <- cbind(sa_utdata, sa_utdata_n)
    kolonner <- ncol(sa_utdata)
    for (x in 2:kolonner) {
      sa_utdata_full[x][sa_utdata_full[(kolonner - 1) + x] < 4] <- NA
    }
    
    # Sisteårsstudenten
    writeData(wb, sn, paste("Sisteårsstudenten", fak_n), startRow = utrad)
    utrad <- utrad + 1
    
    # Slår saman til éin tabell og skriv ut
    writeDataTable(wb, sn, sa_utdata_full, startRow = utrad, keepNA = T, na.string = "-")
    # snitt_startrad <- utrad
    snitt_sluttrad <- NROW(utdata) + utrad
    snitt_sluttkol <- NCOL(utdata)
    
    utrad <- utrad + 1
    addStyle(wb, sn, cols = 2:NCOL(sa_utdata), rows = utrad:(utrad + 1), style = pct_stil, gridExpand = T, stack = T)
    utrad <- utrad + 2
    addStyle(wb, sn, cols = 2:snitt_sluttkol, rows = utrad:snitt_sluttrad, style = desimal_stil, gridExpand = T, stack = T)
    
    # writeDataTable(wb, sn, sa_utdata, startRow = utrad)
    # addStyle(wb, sn, cols = 2:NCOL(sa_utdata), rows = utrad + 1, style = pct_stil, gridExpand = T, stack = T)
    # 
    # nkol <- NCOL(sa_utdata) + 1
    # writeDataTable(wb, sn, sa_utdata_n, startRow = utrad, startCol = nkol)#, startCol = 6)
    utrad <- utrad + 2
    
    # Slår saman snitt og N og fjernar resultat der det er færre enn fire som har svart
    sb_utdata_full <- cbind(utdata, utdata_n)
    kolonner <- ncol(utdata)
    for (x in 2:kolonner) {
      sb_utdata_full[x][sb_utdata_full[(kolonner - 1) + x] < 4] <- NA
    }
    
    writeData(wb, sn, paste("Studiebarometeret", fak_n), startRow = utrad)
    utrad <- utrad + 1
    
    # Slår saman til éin tabell og skriv ut
    writeDataTable(wb, sn, sb_utdata_full, startRow = utrad, keepNA = T, na.string = "-")
    snitt_startrad <- utrad
    snitt_sluttrad <- NROW(utdata) + utrad
    snitt_sluttkol <- NCOL(utdata)
    addStyle(wb, sn, cols = 2:snitt_sluttkol, rows = snitt_startrad:snitt_sluttrad, style = desimal_stil, gridExpand = T, stack = T)
    
    # # Snitt
    # writeDataTable(wb, sn, utdata, startRow = utrad)
    # 
    # # N
    # nkol <- NCOL(utdata) + 1
    # writeDataTable(wb, sn, utdata_n, startRow = utrad, startCol = nkol)
    
    utrad <- snitt_sluttrad + 2
    writeData(wb, sn, forklaring_variabler, startRow = utrad)
    
    # Stil
    setColWidths(wb, sn, cols = 1, widths = c(100))
    addStyle(wb, sn, cols = 1, rows = 1, style = SB_style_h1, gridExpand = T, stack = T)
    addStyle(wb, sn, cols = 1, rows = utrad, style = SB_style_wrap, gridExpand = T, stack = T)
    
    sn <- sn + 1
    
    # *****************
  } # END print loop
  # *****************
  
  # Lagre fil
  saveWorkbook(wb, SB_fil, overwrite = TRUE)
  
} # END datapakke_print_aggregert_2022


# Fritekst ------------------------------------------------------------------------------------

# deler fritekstdata i filer gruppert på institutt
SA_fritekst_xlsx_2022 <- function(sdf, rapportmappe, variabler) {
  tabell_stil <- createStyle(wrapText = T, valign = "top")
  svarkol_stil <- createStyle(numFmt = "TEXT")
  # rapportmappe <- "C:\\Users\\kyrremat\\OneDrive - OsloMet\\Dokumenter\\statistikk\\sisteårs\\SA Instituttfiler fritekst 2022\\"
  for (inst in sdf) {
    instituttnamn <- inst$institutt[1]
    # Velge og sortere kolonner
    inst <- inst %>% select(Programkode, Studieprogramnavn, "Nivå" = grad, 12, 15, 40, 34) %>% arrange(Programkode)
    
    # TODO ikkje ta med kolonner som er heilt tomme
    not_all_na <- function(x) any(!is.na(x))
    inst <- inst %>% select_if(not_all_na)
    inst <- inst %>% mutate(across(where(is.character), str_trim))
    
    wb <- createWorkbook()
    addWorksheet(wb, "Sisteårsstudenten fritekst 2022")
    writeDataTable(wb, 1, inst)
    
    ## stil
    datalengde <- NROW(inst) + 1
    setColWidths(wb, 1, cols = 1:7, widths = c(10, 40, 6.5, 50, 50, 50, 50))
    # addStyle(wb, 1, cols = 5, rows = 2:datalengde, svarkol_stil, gridExpand = T, stack = T)
    addStyle(wb, 1, cols = 1:7, rows = 1:datalengde, tabell_stil, gridExpand = T, stack = T)
    
    ## Save workbook
    saveWorkbook(wb,
                 paste0(rapportmappe, instituttnamn, ".xlsx"),
                 overwrite = TRUE)
  }
} #end SA_fritekst_xlsx_2022


# Skriv ut filer med fritekst per institutt
SB_fritekst_xlsx <- function(source_df) {
  tabell_stil <- createStyle(wrapText = T, valign = "top")
  svarkol_stil <- createStyle(numFmt = "TEXT")
  for (inst in source_df$institutt %>% unique %>% sort) {
    utdata <- source_df %>% filter(institutt == inst)
    utdata <- utdata %>% select("studiepgm_navn",	
                                "studprog_kod",
                                "fritekst_studprog",
                                "fritekst_vurd",
                                # "oppr_utvalg",
                                "end_utvalg",
                                "andel_svart"
    )
    colnames(utdata) <- c("Studieprogram", 
                          "Programkode", 
                          "Kommentarer til studieprogrammet", 
                          "Kommentarer til vurderingsformer", 
                          "Antall svarende",
                          "Svarprosent")
    wb <- createWorkbook()
    addWorksheet(wb, "Studiebarometeret fritekst 2021")
    writeDataTable(wb, 1, utdata)
    
    ## stil
    datalengde <- NROW(utdata) + 1
    setColWidths(wb, 1, cols = 1:6, widths = c(50, "auto", 70, 70, "auto", "auto"))
    addStyle(wb, 1, cols = 5, rows = 2:datalengde, svarkol_stil, gridExpand = T, stack = T)
    addStyle(wb, 1, cols = 1:6, rows = 2:datalengde, tabell_stil, gridExpand = T, stack = T)
    
    ## Save workbook
    saveWorkbook(wb, 
                 paste0("SB Instituttfiler fritekst\\", inst, ".xlsx"), 
                 overwrite = TRUE)
  }
} # End instituttfiler fritekst

# Skriver ut en fil med fritekstsvar kategorisert tematisk, og med statistikk over andel som kommenterer på tema.
SB_fritekst_analyse_xlsx <- function(sdf) {
  tabell_stil <- createStyle(wrapText = T, valign = "top")
  svarkol_stil <- createStyle(numFmt = "TEXT")
  
  wb <- createWorkbook()
  ark <- 1
  
  # Per tema: N+snitt for institutt og fakultet på svar, pluss tabell med fritekst - ei fane per tema
  temaliste <- sdf %>% select(starts_with("tema_")) %>% names
  
  for (tema in temaliste) {
    # print(tema)
    
    # Statistikk over andel svar som rører tema
    # institusjonssnitt
    prop_studprog_OM <- sdf %>% filter(!is.na(fritekst_studprog)) %>%
      summarise({{tema}} := mean(.data[[tema]], na.rm=T)) %>% 
      mutate(Nivå = "OsloMet") %>% relocate(Nivå, .before = 1)
    
    n_OM <- sdf %>% filter(!is.na(fritekst_studprog)) %>% summarise(N = n()) %>% mutate(Nivå = "OsloMet") %>% 
      relocate(Nivå, .before = 1)
    
    # per fakultet
    prop_studprog_fak <- sdf %>% filter(!is.na(fritekst_studprog)) %>% group_by(FAKNAVN) %>% 
      summarise({{tema}} := mean(.data[[tema]], na.rm=T)) %>% rename(Nivå = FAKNAVN)
    
    n_fak <- sdf %>% filter(!is.na(fritekst_studprog)) %>% group_by(FAKNAVN) %>% summarise(N = n()) %>% 
      rename(Nivå = FAKNAVN)
    
    n <- rbind(n_OM, n_fak)
    prop_tema_studprog <- rbind(prop_studprog_OM, prop_studprog_fak)
    prop_tema_studprog <- cbind(prop_tema_studprog, n["N"]) %>% relocate(N, .after = 1)
    # print(prop_tema_studprog)
    
    
    ## Utskrift til ark
    # Eitt ark per tema
    addWorksheet(wb, tema)
    radnr <- 1
    kolnr <- 1
    
    writeData(wb, ark, paste("Tematisk utdrag av fritekstsvar i Studiebarometeret 2021:", tema))
    addStyle(wb, ark, cols = 1, rows = radnr, SB_style_header, gridExpand = T, stack = T)
    addStyle(wb, ark, cols = 1, rows = radnr, SB_style_bold, gridExpand = T, stack = T)
    
    radnr <- radnr + 2
    writeData(wb, ark, "Tabellen viser antall som har gitt fritekstsvar, og andelen av fritekstsvarene som inneholder ord vi knytter til temaet. Under følger alle svar som inneholder relaterte ord.",
              startRow = radnr)
    
    radnr <- radnr + 2
    writeData(wb, ark, prop_tema_studprog, startRow = radnr)
    
    radnr <- radnr + NROW(prop_tema_studprog) + 2
    
    # Velje ut det vi skal ha med
    fritekst_tema <- sdf %>% filter(.data[[tema]] == 1) %>% select(FAKNAVN, studprog_kod, studiepgm_navn, studiested, fritekst_studprog) %>% 
      arrange(FAKNAVN, studprog_kod)
    writeDataTable(wb, ark, fritekst_tema, startRow = radnr)
    
    sisterad <- NROW(fritekst_tema) + radnr
    
    ## stil
    tabell_stil <- createStyle(wrapText = T, valign = "top")
    setColWidths(wb, ark, cols = 1:5, widths = c(50, 14, 30, "auto", 85))
    addStyle(wb, ark, cols = 1:5, rows = radnr:sisterad, tabell_stil, gridExpand = T, stack = T)
    
    ark <- ark + 1
  } 
  
  ## Save workbook
  saveWorkbook(wb, 
               paste0("fritekst analyse\\", "friteksttest", ".xlsx"), 
               overwrite = TRUE)
} #end SB_fritekst_analyse_xlsx


# Stildefinisjonar ----------------------------------------------------------------------------

SB_style_default <- createStyle(
  border = "TopBottomLeftRight",
  borderStyle = "none"
)

SB_style_header <- createStyle(
  fontSize = 14
)

SB_style_normal <- createStyle(
  textDecoration = NULL
)

SB_style_bold <- createStyle(
  textDecoration = "bold",
)

SB_style_wrap <- createStyle(
  valign = "top",
  wrapText = T
)

SB_style_headerbg <- createStyle(
  fgFill = "#FFE757" # "#FFDC00"
)

SB_style_batteryborder <- createStyle(
  border = "right",
  borderColour = "#DCDCDC",
  borderStyle = "thin"
)

SB_style_spm <- createStyle(
  wrapText = T,
  valign = "top"
)

SB_style_lgrey <- createStyle(
  fgFill = "#DCDCDC"
)

SB_style_skille <- createStyle(
  fgFill = "#EBEBEB"
)

SB_style_num <- createStyle(
  numFmt = "#,#0.0"
)

SB_style_perc <- createStyle(
  numFmt = "0 %"
)

SB_style_orange <- createStyle(
  fgFill = "#ffa64d"
)

# Fargepaletten som har semantisk funksjon, er utforma ved hjelp av denne sida:
# https://davidmathlogic.com/colorblind/#%2305DE4B-%23EA6A00-%231C8A41-%23EA1A00
SB_style_pos_res_fyll <- createStyle(
  bgFill = "#98DCAE" #"#05DE4B"
)

SB_style_sus_res_fyll <- createStyle(
  bgFill = "#EFE23F" #"#EA6A00"
)

SB_style_svakt_res_fyll <- createStyle(
  bgFill = "#FFA65C" #"#EA6A00"
)

SB_style_sig_pos_ramme <- createStyle(
  border = "TopBottomLeftRight",
  borderStyle = "thin",
  borderColour = "#000000" #"#1C8A41"
)

SB_style_sig_neg_ramme <- createStyle(
  border = "TopBottomLeftRight",
  borderStyle = "dashed",
  borderColour = "#000000" #"#EA1A00"
)

SB_style_differanseblokk <- createStyle(
  fgFill = "#E7F6FF",
  border = "TopBottomLeftRight",
  borderColour = "#DCDCDC",
  borderStyle = "thin"
)


# Hjelpefunksjonar for utskrift ---------------------------------------------------------------

##** START Utskriftskode 2022 
##*
# 2022 - bytta studieprogram_instkode med StudiumID
# 2023 - bytta StudiumID med studieprogram_instkode
# 
# Denne skriv ut første rad med stikktitlar
# TODO: rydd denne, kan bli mykje enklare, berre programnamn, fakultet, OsloMet totalt
# Kommenterer ut alt som har med _forrige å gjere - det trengst ikkje her? 
SB_print_fc <- function(utd_df, fak_df, source_df, sn, sc, sr, ab) { 
  tmp_utd_df <- utd_df %>% summarise(N = sum(!is.na(instnr)))
  tmp_fak_df <- fak_df %>% summarise(N = sum(!is.na(instnr)))
  
  # TODO kommenter betre
  # TODO unødig komplisert - her skal vi berre ha namn på program, fakultet og institusjon
  # lag ein loop, skriv ut namn for første rad
  writeData(ab, sn, tmp_utd_df[1], sc, sr + 1, colNames = FALSE)
  writeData(ab, sn, "", sc, sr)
  
  prog_n <- utd_df %>% select(Studieprogram_instnamn) %>% unique %>% NROW
  writeData(ab, sheet = sn, x = tmp_fak_df[1], startCol = sc, startRow = sr + 2 + prog_n, colNames = FALSE)
  writeData(ab, sheet = sn, x = "OsloMet totalt", startCol = sc, startRow = sr + 4 + prog_n, colNames = FALSE)
  
  srbup <- sr
  sr <- sr + 6 + prog_n
  
  writeData(ab, sn, "Antall respondenter", sc, sr)
  sr <- sr + 1
  
  # Skriv ut Studieprogram_instnamn, fakultetsnamn og OsloMet totalt i første rad
  writeData(ab, sn, tmp_utd_df[1], sc, sr + 1, colNames = FALSE)
  writeData(ab, sheet = sn, x = tmp_fak_df[1], startCol = sc, startRow = sr + 2 + prog_n, colNames = FALSE)
  writeData(ab, sheet = sn, x = "OsloMet totalt", startCol = sc, startRow = sr + 4 + prog_n, colNames = FALSE)
  sr <- sr + 6 + prog_n
  
  # TODO kommenter betre
  writeData(ab, sn, "Differanse fra året før", sc, sr)
  sr <- sr + 1
  
  # Skriv ut Studieprogram_instnamn, fakultetsnamn og OsloMet totalt i første rad
  writeData(ab, sn, tmp_utd_df[1], sc, sr + 1, colNames = FALSE)
  writeData(ab, sheet = sn, x = tmp_fak_df[1], startCol = sc, startRow = sr + 2 + prog_n, colNames = FALSE)
  writeData(ab, sheet = sn, x = "OsloMet totalt", startCol = sc, startRow = sr + 4 + prog_n, colNames = FALSE)
  
  sr <- sr + 6 + prog_n
  writeData(ab, sn, "p-verdi", sc, sr)
  sr <- sr + 1
  
  # Skriv ut Studieprogram_instnamn, fakultetsnamn og OsloMet totalt i første rad
  writeData(ab, sn, tmp_utd_df[1], sc, sr + 1, colNames = FALSE)
  writeData(ab, sheet = sn, x = tmp_fak_df[1], startCol = sc, startRow = sr + 2 + prog_n, colNames = FALSE)
  writeData(ab, sheet = sn, x = "OsloMet totalt", startCol = sc, startRow = sr + 4 + prog_n, colNames = FALSE)
  
  sr <- srbup
}

# Skriv ut Blokktittel og spørsmålsdefinisjon
SB_print_batteryheader <- function(htittel, hspørsmål, ab, sn, sc) {
  writeData(ab, sheet = sn, x = {{htittel}}, startCol = sc, startRow = 1, colNames = FALSE)
  writeData(ab, sheet = sn, x = {{hspørsmål}}, startCol = sc, startRow = 2, colNames = FALSE)
}

##** Resultat for program, fakultet og OsloMet
##* TODO 
##* prøve å flytte bind_rows til OM_print_2022 - hadde vore bra å ikkje gjenta denne for kvar variabel
##* - det vil krevje anten å gruppere data i printfunksjon, eller å sende tre samanslåtte df-ar
##* - om bruke tre df - truleg bra å då putte alle df-ane i ei liste og sende dei over, for å forenkle kallet
SB_sub_print <- function(utd_df, fak_df, source_df, 
                         utd_df_forrige, fak_df_forrige, source_df_forrige, 
                         ab, sn, sc, sr, prog_n, spørsmål, varnamn, tid = FALSE) {
  tmp_utd_df <- utd_df
  tmp_fak_df <- fak_df
  tmp_OM_df <- source_df 
  # FORTSETT HER - må få inn grupperte versjonar av førre års data
  tmp_utd_df_prev <- utd_df_forrige
  tmp_fak_df_prev <- fak_df_forrige
  tmp_OM_df_prev <- source_df_forrige
  # Dersom det er tidsvariablar, må det handterast spesielt
  if (tid) {
    tmp_utd_df <- tmp_utd_df %>% filter(!is.na(tid_brutto))
    tmp_fak_df <- tmp_fak_df %>% filter(progresjon == 1, !is.na(tid_brutto))
    tmp_OM_df <- tmp_OM_df %>% filter(progresjon == 1, !is.na(tid_brutto))
  }
  
  # 2022 SB_utrad_snitt gir no fleire kolonner - bør få nytt namn, og alle bør bli bytta frå SB_ til OM_
  tmp_utd_df_resultat <- SB_utrad_snitt(tmp_utd_df, tmp_utd_df_prev, spørsmål, varnamn)
  tmp_fak_df_resultat <- SB_utrad_snitt(tmp_fak_df, tmp_fak_df_prev, spørsmål, varnamn)
  tmp_OM_df_resultat <- SB_utrad_snitt(tmp_OM_df, tmp_OM_df_prev, spørsmål, varnamn)
  
  writeData(ab, sn, tmp_utd_df_resultat[spørsmål], sc, sr, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_fak_df_resultat[spørsmål], sc, sr + 2 + prog_n, colNames = FALSE, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_OM_df_resultat[spørsmål], sc, sr + 4 + prog_n, colNames = FALSE, keepNA = T, na.string = "-")  
  
  # mellomlagrar utskriftsradnummer for å kunne skrive ut N, diff og p-verdi og så starte på nytt igjen
  srbup <- sr
  sr <- sr + prog_n + 7
  
  # Skriv ut N
  writeData(ab, sn, tmp_utd_df_resultat["N"], sc, sr, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_fak_df_resultat["N"], sc, sr + 2 + prog_n, colNames = FALSE, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_OM_df_resultat["N"], sc, sr + 4 + prog_n, colNames = FALSE)  
  
  # Skriv ut endring frå forrige år
  srbup <- sr
  sr <- sr + prog_n + 7  
  writeData(ab, sn, tmp_utd_df_resultat["Endring"], sc, sr, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_fak_df_resultat["Endring"], sc, sr + 2 + prog_n, colNames = FALSE, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_OM_df_resultat["Endring"], sc, sr + 4 + prog_n, colNames = FALSE)  
  
  # Skriv ut p-verdi for endring
  srbup <- sr
  sr <- sr + prog_n + 7 
  
  writeData(ab, sn, tmp_utd_df_resultat["p"], sc, sr, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_fak_df_resultat["p"], sc, sr + 2 + prog_n, colNames = FALSE, keepNA = T, na.string = "-")
  writeData(ab, sn, tmp_OM_df_resultat["p"], sc, sr + 4 + prog_n, colNames = FALSE)  
  
  # set utskriftsradnummer tilbake til utgangspunkt, går ei kolonne til høgre
  # TODO sjekk om desse to eigentleg påverkar noko etter at eg bytta frå macro til funksjon
  sr <- srbup
  sc <- sc + 1
}

##** Funksjon for å skrive ut kolonne med snitt
##* jf. https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
SB_utrad_snitt <- function(sdf, sdf_previous, spørsmål, varnamn) {
  df_ut <- sdf %>% summarise(snitt = mean(.data[[varnamn]], na.rm = T),
                             N = sum(!is.na(.data[[varnamn]]))
  ) %>% as.data.frame()
  
  # TODO få inn trycatch her - eller kanskje bind_rows løyser problemet med manglande variablar?
  # - vurder om det blir betre å bruke bind_rows datasett
  df_ut_previous <- sdf_previous %>%
    summarise(snitt_prev = mean(.data[[varnamn]], na.rm = T))
  df_ut <- suppressMessages(left_join(df_ut, df_ut_previous))
  df_ut <- df_ut %>% mutate(Endring = snitt - snitt_prev)
  
  # Mellomlagrar samanslått df for å kunne køyre t.test
  # TODO prøve å flytte denne til OM_print_2022 - hadde vore bra å ikkje gjenta denne for kvar variabel
  
  # dersom samanslått df blir sendt hit, må gruppering gjerast ved å sjekke kva nivå vi er på, eller i kall
  # print(sdf %>% group_vars())
  # print(sdf_previous %>% group_vars())
  # p_tmp_group <- p_tmp %>% group_by(sdf %>% group_vars())
  
  sisteår <- sdf$undersøkelse_år %>% unique
  forrigeår <- sdf_previous$undersøkelse_år %>% unique
  df_p_tmp <- bind_rows(sdf, sdf_previous)
  
  df_p <- df_p_tmp %>% summarise(
    p = tryCatch(t.test(.data[[varnamn]][undersøkelse_år == sisteår],
                        .data[[varnamn]][undersøkelse_år == forrigeår],
                        var.equal = TRUE)$p.value, error = function(e) {return(NA)},
                 silent = TRUE))
  
  df_ut <- suppressMessages(left_join(df_ut, df_p))
  
  names(df_ut)[2] <- spørsmål
  
  # unngå #NUM! i excel
  df_ut[df_ut == "NaN"] <- NA
  return(df_ut)
}
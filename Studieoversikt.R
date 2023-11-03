options(dplyr.summarise.inform = FALSE)
# options(OutDec = ",")
# options("openxlsx.numFmt" = "#,#0.00")

##** 
##* Lagar tabellar med oversikt over studietilbod for siste tre år
##* Kan skrive til Word, Excel eller returnere datasett
##* Eitt ark/tabell per studiestad – fakultet – nivå, viser registrerte studentar og fullførte per program
##* 
##* TODO: lage statistikk (til diagram) som viser 
##* * totalt omfang utdanningar og studentar, fordelt på studiestad/fakultet/institutt/nivå
##* * fordeling av andel av heltid, andel praksis fordelt på nivå og studiestad
##* 
##* # TODO - lag ein kommentar til utskrift om kva som er filtrert
##* TODO: Vurdere om vi skal vise fram retningar i paraply på noko vis
##* 
SP_studietilbod_OM <- function(mal_fil = "Rapportfiler/Studieoversikt/Studieoversikt_mal.docx", ut_fil = NULL) {
  år_avgrensing_diagram <- 2018
  år_avgrensing_tabellar <- 2020
  år_no <- 2023
  
  #**
  #* Bygger tabellar til utskrift
  #*
  # Hentar alle studietilbod for å få variablar for studieprogram
  studietilbod_dbh <- dbh_hent_programdata() %>% #filter(Årstall == år_no) %>% 
    # filter(Studieprogramkode != "SPH", Studieprogramkode != "SYPLGR") %>% 
    select(-Årstall, -Avdelingskode_SSB, -Avdelingskode, -Fakultetskode)
  
  # Legg til "(D)" i namn på deltidsstudium
  studietilbod_dbh <- studietilbod_dbh %>% 
    mutate(Studieprogramnavn = case_when(`Andel av heltid` < 1 ~ paste(Studieprogramnavn, "(D)"), 
                                         T ~ Studieprogramnavn))
  
  # Legg til studiepoeng i namn på studium som ikkje er bachelor, master eller ph.d.
  studietilbod_dbh <- studietilbod_dbh %>% 
    mutate(Studieprogramnavn = case_when(
      grepl("LN|AR|HN|VS|YU|HK", Nivåkode) ~ paste0(Studieprogramnavn, " (", Studiepoeng, " stp)"),
      T ~ Studieprogramnavn))
  
  # Lagrar til ny variabel for for å bygge tabellar
  studietilbod <- studietilbod_dbh
  
  # Hentar stadnavn i 1. og 2. syklus
  studiestad_OM <- dbh_data(124, filters=c("Institusjonskode"="1175"), 
                             group_by=c("Stednavn campus", "Studieprogramkode", "Årstall")) %>%
    filter(Årstall == år_no) %>% select(Studieprogramkode, Studiestad = `Stednavn campus`) %>% unique()
  
  # Legg til informasjon om studiestad
  studietilbod <- left_join(studietilbod, studiestad_OM, "Studieprogramkode")
  # Fjern utdanningar med studiestad Sandvika
  studietilbod <- studietilbod %>% filter(Studiestad != "Sandvika" | is.na(Studiestad))
  
  # Hentar tal på møtte studentar i 1. og 2. syklus
  nye_studentar_OM <- dbh_data(379, filters=c("Institusjonskode"="1175"), 
                               group_by=c("Studieprogramkode", "Årstall", "Semester")) %>% 
    # Tar bort desse: Søknadsalternativer, `Tilbud om opptak`, `Akseptert tilbud`
    select(Studieprogramkode, Årstall, Semester, Antall = `Møtt til studiestart` )

  # Hentar registrerte personar i 3. syklus
  registrerte_OM_phd <- dbh_data(115, filters=c("Institusjonskode"="1175"), 
                                 group_by=c("Studieprogramkode", "Årstall", "Semester")) %>%
    rename("Antall" = "Antall totalt") %>%
    select(Studieprogramkode, Årstall, Antall, Semester)
  
  # Slår saman til tabell med alle møtte personar på studieprogram (alle registrerte på ph.d.)
  studentar_OM <- bind_rows(nye_studentar_OM, registrerte_OM_phd)

  # Slår saman koder som har endra seg men som viser til same program
  # Filterer bort eldre studium
  studentar_OM <- studentar_OM %>%
    kople_studieprogramkode(vis_samla_kode = F) %>%
    # rename(Studiestad = `Stednavn campus`) %>%
    filter(Årstall > år_avgrensing_tabellar, Semester == 3) %>% #, Studiestad != "Sandvika" | is.na(Studiestad)) %>%
    select(-Semester)
  
  # Lagar tabell med registrerte fordelt på år
  studentar_OM_pivot <- studentar_OM  %>% 
    pivot_wider(names_from = Årstall, values_from = Antall, names_prefix = "Møtt ", 
                values_fn = {sum}, names_sort = T)

  # Slår saman med studieprogramvariablane
  studietilbod <- left_join(studietilbod, studentar_OM_pivot, "Studieprogramkode")
  
  # Hentar data om fullførte studieprogram i 1. og 2. syklus
  fullfort_OM <- dbh_data(118, filters=c("Institusjonskode"="1175"), 
                            group_by=c("Studieprogramkode", "Årstall", "Andel av heltid")) %>%
    rename("Antall" = "Antall totalt") 
  
  # Hentar personar i 3. syklus med avlagt avhandling
  fullfort_OM_phd <- dbh_data(101, filters=c("Institusjonskode"="1175"), 
                                 group_by=c("Studieprogramkode", "Årstall")) %>%
    rename("Antall" = "Antall totalt") %>%
    select(Studieprogramkode, Årstall, Antall)
  
  # Slår saman til tabell med alle fullførte personar på studieprogram
  fullfort_OM <- bind_rows(fullfort_OM, fullfort_OM_phd) 
  
  # Slår saman koder som har endra seg men som viser til same program
  # Filterer bort eldre studium
  fullfort_OM <- fullfort_OM %>% 
    kople_studieprogramkode(vis_samla_kode = F) %>%
    filter(Årstall > år_avgrensing_tabellar) %>% select(-`Andel av heltid`, -`Antall kvinner`, -`Antall menn`) 
  
  # Lagar tabell med fullførte fordelt på år
  fullfort_OM_pivot <- fullfort_OM %>%
    pivot_wider(names_from = Årstall, values_from = Antall, names_prefix = "Fullførte ", 
                values_fn = {sum}, names_sort = T)
  
  # Slår saman med studieprogramvariablane
  studietilbod <- left_join(studietilbod, fullfort_OM_pivot, "Studieprogramkode")
  
  # Endrar namn på kolonne frå "Studieprogramnavn" til "Studietilbud"
  studietilbod <- studietilbod %>% rename("Studietilbud" = "Studieprogramnavn")
  
  # TODO - skriv om til å bruke år_no og år_no - 1
  # Filtrerer bort linjer med lite nye data
  studietilbod[studietilbod == "0"] <- NA
  studietilbod <- studietilbod %>% filter(Studieprogramkode != "UPLASSERT", 
           !(is.na(`Møtt 2022`) & is.na(`Møtt 2023`) & is.na(`Fullførte 2023`)))
  
  # Ryddar i manglande studiestad
  studietilbod <- studietilbod %>% mutate(Studiestad = case_when(
    Nivåkode == "FU" & is.na(Studiestad) ~ "OsloMet",
    is.na(Studiestad) ~ "Uplassert",
    TRUE ~ Studiestad))  

  studietilbod <- studietilbod %>% relocate(Fakultetsnavn, Studiestad, Institutt)
  studietilbod <- studietilbod %>% OM_set_syklus(Nivåkode)
  
  #**
  #* Bygger tabellar til å lage overordna diagram
  
  #** 
  #*Hentar registrerte studentar i 1. og 2. syklus
  registrerte_OM <- dbh_data(124, filters=c("Institusjonskode"="1175"),
                             group_by=c("Stednavn campus", "Studieprogramkode", "Årstall", "Semester"))#,
                                        # "Andel av heltid", "Andel praksis"))

  registrerte_OM <- bind_rows(registrerte_OM, registrerte_OM_phd)
  registrerte_OM <- registrerte_OM %>% filter(Semester == 3)
  
  # Slår saman med studieprogramvariablane
  registrerte_OM <- left_join(studietilbod_dbh, registrerte_OM, "Studieprogramkode")

  # Slår saman koder som har endra seg men som viser til same program
  # Filterer bort eldre studium
  registrerte_OM <- registrerte_OM %>%
    kople_studieprogramkode(vis_samla_kode = F) %>%
    rename(Studiestad = `Stednavn campus`) %>%
    filter(Årstall > år_avgrensing_diagram, Semester == 3) %>% #, Studiestad != "Sandvika" | is.na(Studiestad)) %>%
    select(-Semester)
  
  registrerte_OM <- registrerte_OM %>% OM_set_syklus(Nivåkode)

  #** 
  #* Søknadsdata 
  søknadsdata <- dbh_data(379, filters = c("Institusjonskode"="1175"), 
                          group_by = c("Studieprogramkode", "Årstall", "Kvalifisert", "Prioritet", "Semester", "Opptakstype"))
  
  sp_1pri_søkarar <- søknadsdata %>% filter(Prioritet == 1, Kvalifisert == 1)
  
  # Slår saman med studieprogramvariablane
  sp_1pri_søkarar <- left_join(studietilbod_dbh, sp_1pri_søkarar, "Studieprogramkode")
  
  # Slår saman koder som har endra seg men som viser til same program
  # Filterer bort eldre studium
  # TODO - vurder å sjekke med Martin om både haust og vår bør med
  sp_1pri_søkarar_N <- sp_1pri_søkarar %>%
    filter(Årstall > år_avgrensing_diagram, Opptakstype == "N") %>%
    kople_studieprogramkode(vis_samla_kode = F) %>%
    select(-Semester)
  
  sp_1pri_søkarar_N <- sp_1pri_søkarar_N %>% OM_set_syklus(Nivåkode)
  
  sp_1pri_søkarar_L <- sp_1pri_søkarar %>%
    filter(Årstall > år_avgrensing_diagram, Opptakstype == "L") %>%
    kople_studieprogramkode(vis_samla_kode = F) %>%
    select(-Semester)
  
  sp_1pri_søkarar_L <- sp_1pri_søkarar_L %>% OM_set_syklus(Nivåkode)
  
  # Endre til faktor, for å kunne sortere betre
  studiestad_sortering <- c("Kjeller", "Pilestredet", "OsloMet", "Uplassert")
  fakultet_sortering <- c("HV", "LUI", "SAM", "TKD", "SPS", "OsloMet")
  syklus_sortering <- c("Bachelor", "Master", "Forskarutdanning", "Andre", "Vidaregåande skule-nivå")
  
  # Studiestad
  studietilbod <- studietilbod %>% 
    OM_lag_faktor(Studiestad, nivå = studiestad_sortering, sortert = TRUE)
  registrerte_OM <- registrerte_OM %>% 
    OM_lag_faktor(Studiestad, nivå = studiestad_sortering, sortert = TRUE)
  
  # Tilhøyrigheit
  studietilbod <- studietilbod %>%
    OM_lag_faktor(Fakultetsnavn, nivå = fakultet_sortering, sortert = TRUE)
  registrerte_OM <- registrerte_OM %>%
    OM_lag_faktor(Fakultetsnavn, nivå = fakultet_sortering, sortert = TRUE)
  sp_1pri_søkarar_N <- sp_1pri_søkarar_N %>%
    OM_lag_faktor(Fakultetsnavn, nivå = fakultet_sortering, sortert = TRUE)
  sp_1pri_søkarar_L <- sp_1pri_søkarar_L %>%
    OM_lag_faktor(Fakultetsnavn, nivå = fakultet_sortering, sortert = TRUE)
  
  # Nivåkode
  studietilbod <- studietilbod %>%
    OM_lag_faktor(Syklus, nivå = syklus_sortering, sortert = TRUE)
  registrerte_OM <- registrerte_OM %>%
    OM_lag_faktor(Syklus, nivå = syklus_sortering, sortert = TRUE)
  sp_1pri_søkarar_N <- sp_1pri_søkarar_N %>%
    OM_lag_faktor(Syklus, nivå = syklus_sortering, sortert = TRUE)
  sp_1pri_søkarar_L <- sp_1pri_søkarar_L %>%
    OM_lag_faktor(Syklus, nivå = syklus_sortering, sortert = TRUE)
  
  # return(list(registrerte_OM, studietilbod))
  
  # Om det ikkje er oppgitt ei utfil, returner datasett
  if (is.null(ut_fil)) {
    return(studietilbod)
  }
  
  # Om utfila ser ut som docx, skriv til Word
  if (!is.null(ut_fil) & grepl(".docx$", mal_fil)) {
    # Skriv ut til fil
    arbeidsbok <- read_docx(mal_fil)
    # Flyttar peikar til starten
    arbeidsbok <- cursor_begin(arbeidsbok)
    
    styles_info(arbeidsbok, type = "paragraph") %>% select(style_name) %>% print
    styles_info(arbeidsbok, type = "table") %>% select(style_name) %>% print
    
    # Skriv ut tittel og innhaldsforteikning(?)
    dokumenttittel <- paste("Studieoversikt OsloMet", år_no) 
    arbeidsbok <- body_add_par(arbeidsbok, "Studieoversikt OsloMet 2023", pos = "before", style = "heading 1")
    # arbeidsbok <- body_add_toc(arbeidsbok, level = 2, pos = "after", style = NULL)
    
    # Flyttar peikar til etter maltekst
    arbeidsbok <- cursor_end(arbeidsbok)
    arbeidsbok <- body_add_break(arbeidsbok)
    
    # Lagar nokre overordna statistikkar
    # OBS beskrive tydeleg kva tala er - ta utgangspunkt i nyaste år, eller serie?
        
    #**
    #* Søylediagram registrerte studentar per syklus, utan vidaregåande nivå
    registrerte_syklus_tidsserie <- registrerte_OM %>%  filter(Nivåkode != "VS") %>%
      group_by(Syklus, Årstall) %>% 
      summarise(Antall = sum(Antall, na.rm = T))

    sp_bar_studentar_syklus <- ms_barchart(registrerte_syklus_tidsserie,
                                           x = "Syklus", y = "Antall",
                                           group = "Årstall")
    sp_bar_studentar_syklus <- sp_bar_studentar_syklus %>% sp_bar_serie_format
    
    # print(sp_bar_studentar_syklus, preview = T)
    # return()
    
    #**
    #* Søylediagram andel heiltidsstudium per syklus
    sp_andel_heltid <- registrerte_OM  %>% filter(Nivåkode != "VS", Nivåkode != "FU") %>%
      mutate(Heltid = `Andel av heltid` == 1) %>%
      group_by(Syklus, Årstall) %>% summarise("Andel heiltidsutdanning" = mean(Heltid, na.rm=T))
    sp_bar_andel_heltid <- ms_barchart(sp_andel_heltid, x = "Syklus", y = "Andel heiltidsutdanning",
                                       group = "Årstall")
    sp_bar_andel_heltid <- sp_bar_andel_heltid %>% sp_bar_serie_format
    sp_bar_andel_heltid <- sp_bar_andel_heltid %>% sp_label_prosent
    sp_bar_andel_heltid <- sp_bar_andel_heltid %>% sp_ax_y_prosent
  
    #**
    #* Søylediagram andel studium med prakis per syklus
    sp_andel_praksis <- registrerte_OM  %>% filter(Nivåkode != "VS", Nivåkode != "FU") %>%
      mutate(Praksis = `Andel praksis` > 0) %>%
      group_by(Syklus, Årstall) %>% summarise("Andel utdanningar med praksis" = mean(Praksis, na.rm=T))
    sp_bar_andel_prakis <- ms_barchart(sp_andel_praksis, x = "Syklus", y = "Andel utdanningar med praksis",
                                       group = "Årstall")
    sp_bar_andel_prakis <- sp_bar_andel_prakis %>% sp_bar_serie_format
    sp_bar_andel_prakis <- sp_bar_andel_prakis %>% sp_label_prosent
    sp_bar_andel_prakis <- sp_bar_andel_prakis %>% sp_ax_y_prosent

    #**
    #* Søylediagram 1.-prioritetssøkarar NOM-opptak
    sp_1pri_søkarar_syklus_tidsserie_N <- sp_1pri_søkarar_N %>% group_by(Syklus, Årstall) %>% 
      summarise("Kvalifiserte 1.-prioritetssøkarar Samordna opptak" = sum(Søknadsalternativer, na.rm = T))
    
    sp_bar_1pri_søkarar_syklus_N <- ms_barchart(sp_1pri_søkarar_syklus_tidsserie_N,
                                           x = "Syklus", y = "Kvalifiserte 1.-prioritetssøkarar Samordna opptak",
                                           group = "Årstall")
    sp_bar_1pri_søkarar_syklus_N <- sp_bar_1pri_søkarar_syklus_N %>% sp_bar_serie_format
    
    #**
    #* Søylediagram 1.-prioritetssøkarar LOK-opptak
    sp_1pri_søkarar_syklus_tidsserie_L <- sp_1pri_søkarar_L %>% group_by(Syklus, Årstall) %>% 
      summarise("Kvalifiserte 1.-prioritetssøkarar lokalt opptak" = sum(Søknadsalternativer, na.rm = T))
    
    sp_bar_1pri_søkarar_syklus_L <- ms_barchart(sp_1pri_søkarar_syklus_tidsserie_L,
                                              x = "Syklus", y = "Kvalifiserte 1.-prioritetssøkarar lokalt opptak",
                                              group = "Årstall")
    sp_bar_1pri_søkarar_syklus_L <- sp_bar_1pri_søkarar_syklus_L %>% sp_bar_serie_format
        
    # Definere diagramstorleik
    diagram_w = 6.29
    diagram_h = 3.75
    
    ##** Legg til diagram i dokument
    # caption - mal
    # caption_registrerte_syklus <- block_caption(utrekningsgrunnlag, "Normal")
    # arbeidsbok <- body_add_caption(arbeidsbok, caption_registrerte_syklus, pos = "after")    
    
    #** Side 1
    
    # Registrerte per syklus
    arbeidsbok <- body_add_par(arbeidsbok, "OsloMets utdanningar i fugleperspektiv", style = "heading 2")
    arbeidsbok <- body_add_par(arbeidsbok, "Registrerte studentar på de ulike utdanningsnivåa", style = "heading 3")
    arbeidsbok <- body_add_chart(arbeidsbok, sp_bar_studentar_syklus, width = diagram_w, height = diagram_h, style = "Normal")
    arbeidsbok <- body_add_par(arbeidsbok, "", style = "Normal")
    
    # Andel heiltidsutdanningar
    # arbeidsbok <- body_add_par(arbeidsbok, "Heiltid og deltid på dei ulike utdanningsnivåa", style = "heading 3")
    # arbeidsbok <- body_add_chart(arbeidsbok, sp_bar_andel_heltid, width = diagram_w, height = diagram_h, style = "Normal")
    arbeidsbok <- body_add_break(arbeidsbok)
    
    #** Side 2
    
    # Andel heiltidsutdanningar
    arbeidsbok <- body_add_par(arbeidsbok, "Heiltid og deltid på dei ulike utdanningsnivåa", style = "heading 3")
    arbeidsbok <- body_add_chart(arbeidsbok, sp_bar_andel_heltid, width = diagram_w, height = diagram_h, style = "Normal")
    utrekningsgrunnlag <-"Tala er rekna ut frå utdanningar med registrerte studentar."
    caption_heiltid_syklus <- block_caption(utrekningsgrunnlag, "Normal")
    arbeidsbok <- body_add_caption(arbeidsbok, caption_heiltid_syklus, pos = "after")
    arbeidsbok <- body_add_par(arbeidsbok, "", style = "Normal")
    
    # Andel utdanningar med praksis
    arbeidsbok <- body_add_par(arbeidsbok, "Praksis på dei ulike utdanningsnivåa", style = "heading 3")
    arbeidsbok <- body_add_chart(arbeidsbok, sp_bar_andel_prakis, width = diagram_w, height = diagram_h, style = "Normal")
    caption_praksis_syklus <- block_caption(utrekningsgrunnlag, "Normal")
    arbeidsbok <- body_add_caption(arbeidsbok, caption_praksis_syklus, pos = "after")
    
    arbeidsbok <- body_add_break(arbeidsbok)
    
    #** Side 3
    
    # 1.-prioritetssøkarar NOM-opptak
    arbeidsbok <- body_add_par(arbeidsbok, "Kvalifiserte 1.-prioritetssøkarar på dei ulike utdanningsnivåa (Samordna opptak)", style = "heading 3")
    arbeidsbok <- body_add_chart(arbeidsbok, sp_bar_1pri_søkarar_syklus_N, width = diagram_w, height = diagram_h, style = "Normal")
    arbeidsbok <- body_add_par(arbeidsbok, "", style = "Normal")

    # 1.-prioritetssøkarar LOK-opptak
    arbeidsbok <- body_add_par(arbeidsbok, "Kvalifiserte 1.-prioritetssøkarar på dei ulike utdanningsnivåa (Lokalt opptak)", style = "heading 3")
    arbeidsbok <- body_add_chart(arbeidsbok, sp_bar_1pri_søkarar_syklus_L, width = diagram_w, height = diagram_h, style = "Normal")

    arbeidsbok <- body_add_break(arbeidsbok)
    
    ##** Slutt diagramdel
    
    # Dele opp for å skrive ut kvar for seg, fordelt på syklus
    # Lagar først liste fordelt på syklus
    # deler så listeelementa inn i nye element, gruppert etter studiestad og tilhøyrigheit
    sp_OM_split <- studietilbod %>% split(f = as.factor(.$Syklus)) %>% 
      lapply(function(x) {x %>% group_by(Studiestad, Fakultetsnavn) %>% group_split})
    
    # Skriv ut overskrift og ingress for tabelldel
    tabelldel_overskrift <- "Studentar møtt til studiestart og fullførte studietilbod"
    arbeidsbok <- body_add_par(arbeidsbok, tabelldel_overskrift, style = "heading 2")
    tabelldel_ingress_1 <- paste("Tabellane viser studietilbod med tal på studentar som har", 
                               "møtt til studiestart i haustsemesteret", 
                               "og tal på studentar som har fullført studietilbod.",
                               "For forskarutdanning, blir tal på registrerte studentar vist.")
    arbeidsbok <- body_add_par(arbeidsbok, tabelldel_ingress_1, style = "Normal")    
    tabelldel_ingress_2 <- paste('Utdanningar som går på deltid er markert med "(D)" etter namnet på studietilbodet.', 
                               "Utdanningar som ikkje er bachelor, master eller ph.d., er markert med tal på studiepoeng etter namnet på studietilbodet." 
                               # TODO - fortsett her
                               # "og tal på studentar som har fullført studietilbod.",
                               # "For forskarutdanning, blir tal på registrerte studentar vist."
                               )
    arbeidsbok <- body_add_par(arbeidsbok, tabelldel_ingress_2, style = "Normal")    
    tabelldel_ingress_3 <- paste("I tabellane er ikkje tal under 3 tatt med, dette samsvarer med val DBH har gjort for å trygge personvern.",
                                 "Tabellane inneheld ikkje program der det ikkje finst møtt/registrerte studentar frå dei siste to åra,",
                                 "og det heller ikkje er nokon som har fullført siste år." 
                               # TODO - fortsett her
                               # "og tal på studentar som har fullført studietilbod.",
                               # "For forskarutdanning, blir tal på registrerte studentar vist."
                               )
    
    # Slutt vertikal sideretning
    arbeidsbok <- body_add_par(arbeidsbok, tabelldel_ingress_3, style = "Normal")
    
    # Lagar inndelingsskift for å få landskapsorientering for tabellane
    arbeidsbok <- body_end_section_portrait(arbeidsbok)
    
    # Gå gjennom dei ulike delane og skriv til Word-dokument
    for (s in 1:length(sp_OM_split)) {
      # Skriv ut overskrift per syklus
      syklusnamn <- sp_OM_split[s] %>% names
      arbeidsbok <- body_add_par(arbeidsbok, syklusnamn, style = "heading 3")
      
      if (syklusnamn == "Forskarutdanning") {
        phd_atterhald <- "På grunn av rapporteringstidspunkt blir tal for hausten først tilgjengeleg 15. oktober året etterpå."
        arbeidsbok <- body_add_par(arbeidsbok, phd_atterhald, style = "Normal")
        phd_atterhald_volum <- paste("For ph.d.-utdanningar finst det krav som går på volum over tid.", 
                                      "I desse tabellane er det derfor oppgitt samla tal på registrerte personar,",
                                     "i staden for berre nye studentar.")
        arbeidsbok <- body_add_par(arbeidsbok, phd_atterhald_volum, style = "Normal")
      }
      
      # Skriv ut tabellar 
      for (utsnitt in sp_OM_split[[s]]) {
        if (syklusnamn == "Forskarutdanning") {
          colnames(utsnitt) <- gsub(pattern = "Møtt ", replacement = "Registrerte ", x = colnames(utsnitt))
        }
        utsnittID <- paste(utsnitt$Studiestad[1], utsnitt$Fakultetsnavn[1], utsnitt$Syklus[1], sep = " – ")
        print(utsnittID)
        
        utsnitt <- utsnitt %>% select(-Fakultetsnavn, -Institutt, -Studieprogramkode, -Nivåkode, 
                                      -"Tilbys til", -Studiestad, -`Andel av heltid`, 
                                      -`Andel praksis`, -Syklus, - Studiepoeng)
        
        # Fjernar NA i tabellar
        utsnitt <- utsnitt %>% mutate(across(where(is.numeric), as.character))
        utsnitt[is.na(utsnitt)] <- "–"
        arbeidsbok <- body_add_par(arbeidsbok, utsnittID, style = "heading 4")
        arbeidsbok <- body_add_table(arbeidsbok, utsnitt, style = "Greentable") #"roller")
        arbeidsbok <- body_add_par(arbeidsbok, "", style = "Normal")
      }
      arbeidsbok <- body_add_break(arbeidsbok)
    }
    arbeidsbok <- body_end_section_landscape(arbeidsbok)
    print(arbeidsbok, ut_fil)
  }
  
  # Om utfila ser ut som xlsx, skriv til Excel
  if (!is.null(ut_fil) & grepl(".xlsx$", ut_fil)) {
    # Skriv ut til fil
    arbeidsbok <- createWorkbook()
    
    # Lagar nokre overordna statistikkar
    
    
    # Dele opp for å skrive ut per ark
    studietilbod <- studietilbod %>% group_by(Studiestad, Fakultetsnavn, Syklus) %>% 
      arrange(Institutt, Nivåkode, Studieprogramkode)
    
    studietilbod <- studietilbod %>% group_split()
    
    # Gå gjennom dei ulike delane og skriv til ark i arbeidsbok
    for (utsnitt in studietilbod) {
      utsnittID <- paste(utsnitt$Studiestad[1], utsnitt$Fakultetsnavn[1], utsnitt$Syklus[1])
      arktittel <- paste(utsnitt$Fakultetsnavn[1], utsnitt$Studiestad[1], utsnitt$Syklus[1], sep = " – ")
      print(utsnittID)
      addWorksheet(arbeidsbok, utsnittID)
      utsnitt <- utsnitt %>% select(-Fakultetsnavn, -Institutt, -Studieprogramkode, -Nivåkode, -"Tilbys til",
                                    -Studiestad, -`Andel av heltid`, -`Andel praksis`, -Syklus)
      
      writeData(arbeidsbok, utsnittID, arktittel)
      writeDataTable(arbeidsbok, utsnittID, utsnitt, startRow = 3,
                     tableStyle = "TableStyleMedium2")
      setColWidths(arbeidsbok, utsnittID, cols = 1, widths = c(75))
      addStyle(arbeidsbok, utsnittID, cols = 1:7, rows = 3, style = SB_style_wrap, gridExpand = T, stack = T)
    }
    saveWorkbook(arbeidsbok, ut_fil, overwrite = TRUE)
  }
}

##**
##* Formateringsfunksjonar til mschart/officer for Word
##*
 
#**
#* Formatering for søylediagram tidsserie
sp_bar_serie_format <- function(diagram) {
  diagram <- chart_settings(
    {{diagram}},
    grouping = "clustered",
    overlap = -75,
    gap_width = 150
  )
  
  diagram <- chart_data_labels(
    {{diagram}},
    show_val = TRUE,
    position = "outEnd"
  )
  
  diagram <- chart_labels_text(
    {{diagram}},
    values = fp_text(font.family = "Calibri", font.size = 9)
  )
  diagram <- chart_theme(
    {{diagram}},
    legend_text = fp_text(font.family = "Calibri", font.size = 11)
  )
  return(diagram)
}

#**
#* Formatering for søylediagram
sp_bar_format <- function(diagram) {
  diagram <- chart_data_labels(
    {{diagram}},
    num_fmt = "0 %",
    show_val = TRUE,
    position = "outEnd"
  )
  diagram <- chart_labels_text(
    {{diagram}},
    values = fp_text(font.family = "Calibri", font.size = 11)
  )
  diagram <- chart_theme(
    {{diagram}},
    legend_text = fp_text(font.size = 11)
  )
  return(diagram)
}

#**
#* Formaterer label som prosent med ein desimal
sp_label_prosent <- function(diagram) {
  diagram <- chart_data_labels(
    {{diagram}},
    num_fmt = "0 %",
    show_val = TRUE,
    position = "outEnd"
  )
}

#**
#* Formaterer y-akse som prosent
sp_ax_y_prosent <- function(diagram) {
  diagram <- chart_ax_y(
    {{diagram}},
    limit_max = 1,
    minor_tick_mark = "none",
    num_fmt = "0 %"
  )
}

##**
##* Hentar studiepoengproduksjon, filtrert slik DBH gjer det
##* 
SP_studiepoengproduksjon <- function(eldsteår) {
  sp_prod <- dbh_hent_studiepoengdata(1175)
  sp_prod <- sp_prod %>% filter(Årstall >= eldsteår)
  
  sp_prod <- sp_prod %>% rename(Studieprogramkode = `Progkode emne`)
  
  sp_prod <- left_join(sp_prod, select(dbh_programdata, Studieprogramkode, Nivåkode), by="Studieprogramkode")
  
  sp_prod_filtrert <- sp_prod %>% filter(Studentkategori == "S", !grepl("FU", Nivåkode))
  return(sp_prod_filtrert)
}

SP_studieoversikt_2023 <- function() {
  ##** 
  ##* INNHALD
  ##*
  ##* Studieplassar (dbh_370)
  ##* Kvalifiserte 1.pri per plass (dbh_379)
  ##* Registrert per plass (dbh_379) (dbh_124 gir alle aktive, ikkje berre per opptak)
  ##* 
  ##* Fullført normert (bachelor/master) (dbh_707) + sp_gjennomfort/sp_planlagt (dbh_335)
  ##* Kandidatar/fullførte (dbh_118)
  ##* 
  ##* Alt i alt tilfreds Sisteårs
  ##* Andel relevant arbeid (bachelor), Grad relevant arbeid (master)
  
  ##** 
  ##* Søkartal
  ##* 
  dbh_379 <- dbh_data(379, filters = c("Institusjonskode"="1175"), 
                      group_by = c("Studieprogramkode", "Årstall", "Kvalifisert", "Prioritet", "Semester", "Opptakstype"))
  opptakstal_SO <- dbh_379 %>%  filter(Prioritet == 1, Kvalifisert == 1, Opptakstype == "N",  Årstall >= 2021)
  opptakstal_LOK <- dbh_379 %>%  filter(Prioritet == 1, Kvalifisert == 1, Opptakstype == "L",  Årstall >= 2021)
  
  # kval_tidsserie <- ferdige_kvalifikasjonar()
  # kvalifikasjonar_B3 <- kval_tidsserie %>% filter(progresjon == 1)
  # kvalifikasjonar_B3 <- kvalifikasjonar_B3 %>% mutate(Z_kval_23 = scale(k2023)[,1])
  # print(paste("rader kvalifikasjonar", kvalifikasjonar_B3 %>% names))
  
  # Bruk dbh_hent_orgdata() + dbh_add_programdata() for å få inn namn på studieprogram, institutt og fakultet
  # Slå saman med denne og filtrer på "Tilbys til" == 99999 for å beholde aktive program
  dbh_347 <- dbh_data(347, filter = c("Institusjonskode" = "1175"), group_by=c("Studieprogramkode", "Tilbys til"), variables =c("Studieprogramkode", "Tilbys til"))
  
  # dbh_707 har berre data for fulltidsstudium
  dbh_707 <- dbh_data(707, filters = list("Institusjonskode"=c("1175", "257")))
  # Tar med alle år
  # normert_tid <- dbh_707 %>% group_by(Studieprogramkode, Årstall, Nivåkode) %>% 
  #   summarise(Startkull = sum(Startkull), Normert = sum(`Fullført normert`)) %>% 
  #   arrange(Nivåkode, Studieprogramkode, Årstall) %>% mutate(Andel_fullført = Normert/Startkull) 
  
  # Tar berre med nyaste registrerte år per program 
  # normert_tid <- dbh_707 %>% group_by(Studieprogramkode, Årstall, Nivåkode) %>% 
  #   summarise(Startkull = sum(Startkull), Normert = sum(`Fullført normert`)) %>% group_by(Studieprogramkode) %>% top_n(1, Årstall) %>% 
  #   arrange(Nivåkode, Studieprogramkode, Årstall) %>% mutate(Andel_fullført = Normert/Startkull)
  
  # Utveksling
  # OBS - denne blir feil om den grupperer på studieprogramkode
  # TODO - kan sikkert gjere denne om til meir generisk kode for å legge til ekstra grupperingsvariablar i tillegg til dei DBH har valt ut
  dbh_142_vars <- dbh_metadata(142) %>% filter(`Group by (forslag)` == "J") %>% select(`Variabel navn`)
  dbh_142_vars <- dbh_142_vars[["Variabel navn"]]
  dbh_142_vars <- append(dbh_142_vars, "Studieprogramkode")
  dbh_142_vars <- append(dbh_142_vars, "Type")
  dbh_142_vars <- append(dbh_142_vars, "Landkode")
  dbh_142_vars <- append(dbh_142_vars, "Utvekslingsavtale")
  dbh_142_vars <- append(dbh_142_vars, "Semester")
  dbh_142 <- dbh_data(142, filters = c("Institusjonskode"="1175"), group_by = dbh_142_vars)
  
  # Lagar tabell med gjennomføringsandel per år
  dbh_707 <- dbh_707 %>% filter(Årstall > 2017)
  normert_tid <- dbh_707 %>% select(Studieprogramkode, Årstall, Nivå = Nivåkode, Startkull, 
                                    Årstall_normert = `Årstall normert tid`, Normert_faktisk = `Fullført normert`) %>%
    group_by(Studieprogramkode, Årstall_normert) %>% 
    summarise(Startkull = sum(Startkull), Normert_faktisk = sum(Normert_faktisk)) %>% 
    mutate(Andel_fullført = Normert_faktisk/Startkull) %>% select(-Startkull, -Normert_faktisk) %>% 
    pivot_wider(names_from = Årstall_normert, 
                values_from = c(Andel_fullført), names_prefix = "f", 
                values_fn = {sum}, names_sort = T)
  
  # Alternativ, iallfall for det som ikkje er BA/MA:
  # Bruke gjennomføring etter utdanningsplan - studiepoeng produsert
  # Pluss: Ser ut til å dekke alle typar utdanning.
  # Kan samanliknast på tvers, sidan det er ein proporsjon
  # dbh_335 <- dbh_data(335, 
  # filters = c("Institusjonskode"="1175"), 
  # group_by=c("Avdelingskode", "Nivåkode", "Studiepoeng", "Studieprogramkode", "Årstall"))
  # dbh_335 %>% filter(Årstall == 2022) %>% group_by(Nivåkode, Studieprogramkode) %>% 
  #   mutate(prosent_gj = sp_gjennomfort/sp_planlagt*100) %>% 
  #   arrange(Avdelingskode, Nivåkode, Studieprogramkode)
  
  dbh_104 <- dbh_data(104, filters = list("Institusjonskode"=c("1175", "257")), 
                      group_by = c("Andel av heltid", "Andel praksis", "Avdelingskode", 
                                   "Institusjonskode", "Nivåkode", "Organisering_kode", 
                                   "Studiepoeng", "Studieprogramkode", "Årstall"))
  
  # Lagar tabell med kandidatar fordelt på år
  # TODO mutate med snitt for siste tre år
  dbh_104 <- dbh_104 %>% filter(Årstall > 2018) %>% group_by(Studieprogramkode, Årstall) %>% 
    summarise(Total = sum(`Antall totalt`)) %>% 
    group_by(Studieprogramkode) %>% 
    pivot_wider(names_from = Årstall, values_from = Total, names_prefix = "k") %>% 
    mutate(ksnitt = rowMeans(across(2:4), na.rm=T))
  
  # Kan slå saman tre år og rekne snitt/Z på det
  # SB20_22 <- bind_rows(SB22 %>% select(studprog_kod, indx_laerutb10) %>% mutate(år = 2022), SB21 %>% select(studprog_kod, indx_laerutb10) %>% mutate(år = 2021))
  # SB20_22 <- bind_rows(SB20_22, SB20 %>% select(studprog_kod, indx_laerutb10) %>% mutate(år = 2020))
  
  SB22 <- SB_prepare_2023("../datafiler/studiebarometeret/SB2022_Rådata.xlsx", 2022, 1175, kopleprogramdata = F)
  SB22 <- SB22 %>% rename(any_of(c(progresjon == "Andel av heltid", Nivå = "Nivåkode"))) %>% 
    filter(progresjon == 1, Nivå == "B3")
  SB_LUB22 <- SB22 %>% rename("Studieprogramkode" = "studprog_kod")
  SB_LUB22 <- SB_LUB22 %>% group_by(Studieprogramkode) %>% summarise(indx_LUB_22 = mean(indx_laerutb10, na.rm = T))
  SB_LUB22 <- SB_LUB22 %>% mutate(Z_LUB_22 = scale(indx_LUB_22)[,1])
  
  # SB_LUB22 <- left_join(SB_LUB22,
  #                       SB22 %>% select(Studieprogramkode, fakultet, Nivå, Institutt) %>% unique, 
  #                       by = "Studieprogramkode")
  # print(paste("rader SB_LUB", SB_LUB22 %>% names))
  
  SA23 <- SA_prepare_2023("../datafiler/sisteårs/Sisteårs2023.xlsx", 2023, 1175)
  SA23 <- SA23 %>% filter(progresjon == 1, Nivå == "Bachelor")
  SA_sosial23 <- SA23 %>% group_by(Studieprogramkode) %>% 
    summarise(indx_sos_23 = mean(hvor_fornoyd_er_du_med_folgende_forhold_i_studieprogrammet_ditt_det_sosiale_miljoet_blant_studentene, na.rm = T))
  SA_sosial23 <- SA_sosial23 %>% mutate(Z_sosial_23 = scale(indx_sos_23)[,1])
  # print(paste("rader SA_sos", SA_sosial23 %>% names))
  
  KUserie <- OM_compile_kandidat_data_2022("../datafiler/kandidat/2022_nyenavn/", "2022")
  # KU22 <- KUserie %>% filter(progresjon == 1, Nivå == "Bachelor")
  KU_relevantarbeid22 <- KU22 %>% group_by(Studieprogramkode) %>% 
    summarise(indx_relevantarbeid_22 = mean(arbeider_utdannet_til, na.rm = T))
  KU_relevantarbeid22 <- KU_relevantarbeid22 %>% mutate(Z_relevantarbeid_23 = scale(indx_relevantarbeid_22)[,1])
  # print(paste("rader KU_relevantarbeid", KU_relevantarbeid22 %>% names))

  poenggrense_B3_23 <- read_excel("../../Studieportefølje/Poenggrenser opptak SO 2020–2023.xlsx", skip = 2)
  poenggrense_B3_23 <- poenggrense_B3_23 %>% rename(Studieprogramkode = STUDIEPROGRAMKODE)
  poenggrense_B3_23 <- poenggrense_B3_23 %>% mutate(`2023H_ORD` = case_when(
    `2023H_ORD` == -2 ~ NA,
    `2023H_ORD` == -1 ~ NA,
    T ~ `2023H_ORD`
  ))
  poenggrense_B3_23 <- poenggrense_B3_23 %>% mutate(Z_poeng_23 = scale(`2023H_ORD`)[,1])
  poenggrense_B3_23 <- poenggrense_B3_23 %>% select(-FAKULTET, -STUDIEPROGNAVN)
  
  indikator23_bachelor <- full_join(SB_LUB22, kvalifikasjonar_B3, "Studieprogramkode") 
  print(paste("rader 1. join", indikator23_bachelor %>% nrow))
  print(indikator23_bachelor %>% names)
  indikator23_bachelor <- full_join(indikator23_bachelor, SA_sosial23, "Studieprogramkode")
  print(paste("rader 2. join", indikator23_bachelor %>% nrow))
  print(indikator23_bachelor %>% names)
  indikator23_bachelor <- full_join(indikator23_bachelor, KU_relevantarbeid22, "Studieprogramkode")
  print(paste("rader 3. join", indikator23_bachelor %>% nrow))
  print(indikator23_bachelor %>% names)
  indikator23_bachelor <- full_join(indikator23_bachelor, poenggrense_B3_23, "Studieprogramkode")
  print(paste("rader 4. join", indikator23_bachelor %>% nrow))
  print(indikator23_bachelor %>% names)
  
  # Snitt Indikator A
  indikator23_bachelor <- indikator23_bachelor %>% 
    mutate(Indikator_A = rowMeans(across(c(Z_kval_23, Z_LUB_22, Z_sosial_23)), na.rm = T))
  
  # Snitt Indikator B
  indikator23_bachelor <- indikator23_bachelor %>% 
    mutate(Indikator_B = rowMeans(across(c(Z_relevantarbeid_23, Z_poeng_23)), na.rm = T))
  
  # Legg til programvariablar
  # indikator23_bachelor <- dbh_add_programdata(indikator23_bachelor, "Studieprogramkode", "1175")
  # indikator23_bachelor <- OM_add_programdata(indikator23_bachelor, "Studieprogramkode")
  # # indikator23_bachelor <- SB_add_nivå(indikator23_bachelor)
  indikator23_bachelor <- indikator23_bachelor %>% filter(progresjon == 1, Nivåkode == "B3")
  
  # Bolk A vil være basert på 4 indikatorer som vektes likt:
  #   -	Kandidattall 
  # -	Gjennomføring 
  # -	Et mål for faglig utbytte (indeks, men usikker på hvilken, helst ikke generell tilfredshet, studiebarometeret og sisteårsstudenten) 
  # -	Et mål for sosial trivsel (se over, studiebarometeret og sisteårsstudenten)
  
  # Bolk B som vi gjerne vil at skal ses i sammenheng med del A har to indikatorer som vektes likt:
  #   -	Andel i relevant jobb
  # -	Inntakssnitt (mål på inntakskvalitet) Poenggrense? 
  # 
  return(indikator23_bachelor)
}

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

##** 
##* Uteksaminerte, data henta frå justert Tableau-tabell GSP1
##* 

prep_uteksaminerte <- function(datafil = "../../Studieportefølje/Uteksaminerte 2018–2022.xlsx") {
  uteksaminerte <- read_excel(datafil, sheet = "Fra Tableau", skip = 1)
  uteksaminerte <- uteksaminerte %>% fill(Fakultet, `NSD nivå`)
  uteksaminerte <- uteksaminerte %>% mutate(Studieprogramkode = word(Studieprogram, 1))
  uteksaminerte <- uteksaminerte %>% mutate(Nivå = word(`NSD nivå`, 1))
  uteksaminerte <- uteksaminerte %>% filter(Fakultet != "Grand Total")
  uteksaminerte <- uteksaminerte %>% filter(Nivå %in% c("AR", "HK", "B3", "M2", "M5", "ME"))
  # uteksaminerte <- uteksaminerte %>% mutate(across(starts_with("20"),  ~replace_na(., 0)))
  uteksaminerte <- uteksaminerte %>% mutate(gjennomsnitt_uteks = rowMeans(select(., "2020", "2021", "2022"), na.rm = T))
  return(uteksaminerte)
}

##** 
##* Førebu datasett for fullførte kvalifikasjonar
##* 
##* Lagar alle delsetta som trengst og slår dei saman
ferdige_kvalifikasjonar <- function() {
  # årsstudium
  ar_fulltid_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/årsstudium_høgskulekandidat/Årsstudium oppstart 2020 - ferdig 2022.xlsx", ferdig_ar = "k2022", inn_progresjon = 1)
  ar_fulltid_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/årsstudium_høgskulekandidat/Årsstudium oppstart 2019 - ferdig 2021.xlsx", ferdig_ar = "k2021", inn_progresjon = 1)
  ar_fulltid_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/årsstudium_høgskulekandidat/Årsstudium oppstart 2018 - ferdig 2020.xlsx", ferdig_ar = "k2020", inn_progresjon = 1)
  
  # høgskulekandidat
  hk_fulltid_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/årsstudium_høgskulekandidat/Høgskulekandidat oppstart 2019 - ferdig 2022.xlsx", ferdig_ar = "k2022", inn_progresjon = 1)
  hk_fulltid_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/årsstudium_høgskulekandidat/Høgskulekandidat oppstart 2018 - ferdig 2021.xlsx", ferdig_ar = "k2021", inn_progresjon = 1)
  hk_fulltid_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/årsstudium_høgskulekandidat/Høgskulekandidat oppstart 2017 - ferdig 2020.xlsx", ferdig_ar = "k2020", inn_progresjon = 1)
  
  # bachelor
  # fulltid år ferdig
  ba_fulltid_2023 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2019 - ferdig 2023.xlsx", ferdig_ar = "k2023", inn_progresjon = 1)
  ba_fulltid_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2018 - ferdig 2022.xlsx", ferdig_ar = "k2022", inn_progresjon = 1)
  ba_fulltid_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2017 - ferdig 2021 - ferdig 2022 (trekvart).xlsx", ferdig_ar = "k2021", inn_progresjon = 1)
  ba_fulltid_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2016 - ferdig 2020 (fulltid) - ferdig 2021 (trekvart).xlsx", ferdig_ar = "k2020", inn_progresjon = 1)
    
  # ba trekvart år ferdig
  ba_trekvart_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2017 - ferdig 2021 - ferdig 2022 (trekvart).xlsx", ferdig_ar = "k2022", inn_progresjon = 0.75)
  ba_trekvart_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2016 - ferdig 2020 (fulltid) - ferdig 2021 (trekvart).xlsx", ferdig_ar = "k2021", inn_progresjon = 0.75)
  ba_trekvart_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2015 - ferdig 2020 (trekvart) - ferdig 2022 (halvfart).xlsx", ferdig_ar = "k2020", inn_progresjon = 0.75)
    
  # ba halvfart år ferdig
  ba_halvfart_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2015 - ferdig 2020 (trekvart) - ferdig 2022 (halvfart).xlsx", ferdig_ar = "k2022", inn_progresjon = 0.5)
  ba_halvfart_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2014 - ferdig 2021 (halvfart).xlsx",  ferdig_ar = "k2021", inn_progresjon = 0.5)
  ba_halvfart_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/bachelor/Bachelor oppstart 2013 - ferdig 2020 (halvfart).xlsx", ferdig_ar = "k2020", inn_progresjon = 0.5)
  
  # [1] "Bachelor oppstart 2013 - ferdig 2020 (halvfart).xlsx"                         
  # [2] "Bachelor oppstart 2014 - ferdig 2021 (halvfart).xlsx"                         
  # [3] "Bachelor oppstart 2015 - ferdig 2020 (trekvart) - ferdig 2022 (halvfart).xlsx"
  # [4] "Bachelor oppstart 2016 - ferdig 2020 (fulltid) - ferdig 2021 (trekvart).xlsx" 
  # [5] "Bachelor oppstart 2017 - ferdig 2021 - ferdig 2022 (trekvart).xlsx"           
  # [6] "Bachelor oppstart 2018 - ferdig 2022.xlsx" 
  
  # master
  ma_fulltid_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2019 - ferdig 2022.xlsx", ferdig_ar = "k2022", inn_progresjon = 1)
  ma_fulltid_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2018 - ferdig 2021 (fulltid) - ferdig 2022 (totredel).xlsx", ferdig_ar = "k2021", inn_progresjon = 1)
  ma_fulltid_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2017 - ferdig 2020 (fulltid) - ferdig 2021 (totredel) - ferdig 2022 (halvfart).xlsx", ferdig_ar = "k2020", inn_progresjon = 1)
  
  # ma totredel år ferdig
  ma_totredel_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2018 - ferdig 2021 (fulltid) - ferdig 2022 (totredel).xlsx", ferdig_ar = "k2022", inn_progresjon = 0.67)
  ma_totredel_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2017 - ferdig 2020 (fulltid) - ferdig 2021 (totredel) - ferdig 2022 (halvfart).xlsx", ferdig_ar = "k2021", inn_progresjon = 0.67)
  ma_totredel_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2016 - ferdig 2020 (totredel) - ferdig 2021 (halvfart).xlsx", ferdig_ar = "k2020", inn_progresjon = 0.67)
  
  # ma halvfart år ferdig
  ma_halvfart_2022 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2017 - ferdig 2020 (fulltid) - ferdig 2021 (totredel) - ferdig 2022 (halvfart).xlsx", ferdig_ar = "k2022", inn_progresjon = 0.5)
  ma_halvfart_2021 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2016 - ferdig 2020 (totredel) - ferdig 2021 (halvfart).xlsx", ferdig_ar = "k2021", inn_progresjon = 0.5)
  ma_halvfart_2020 <- ferdig_kvalifikasjon("../../Studieportefølje/Gjennomføring/master/Master oppstart 2015 - ferdig 2020 (halvfart).xlsx", ferdig_ar = "k2020", inn_progresjon = 0.5)
  
  # [1] "Master oppstart 2015 - ferdig 2020 (halvfart).xlsx"                                                 
  # [2] "Master oppstart 2016 - ferdig 2020 (totredel) - ferdig 2021 (halvfart).xlsx"                        
  # [3] "Master oppstart 2017 - ferdig 2020 (fulltid) - ferdig 2021 (totredel) - ferdig 2022 (halvfart).xlsx"
  # [4] "Master oppstart 2018 - ferdig 2021 (fulltid) - ferdig 2022 (totredel).xlsx"                         
  # [5] "Master oppstart 2019 - ferdig 2022.xlsx"
  
  # Slår saman alle dei tre åra som rader og så som kolonner
  kvalifikasjoner_2023 <- bind_rows(ba_fulltid_2023) %>%
    select(Studieprogramkode, Studieprogram, progresjon, Nivåkode, "k2023")
  kvalifikasjoner_2022 <- bind_rows(ar_fulltid_2022, hk_fulltid_2022,
                                    ba_fulltid_2022, ba_trekvart_2022, ba_halvfart_2022, 
                                    ma_fulltid_2022, ma_totredel_2022, ma_halvfart_2022) %>%
    select(Studieprogramkode, Studieprogram, progresjon, Nivåkode, "k2022")
  kvalifikasjoner_2021 <- bind_rows(ar_fulltid_2021, hk_fulltid_2021,
                                    ba_fulltid_2021, ba_trekvart_2021, ba_halvfart_2021, 
                                    ma_fulltid_2021, ma_totredel_2021, ma_halvfart_2021) %>%
    select(Studieprogramkode, Studieprogram, progresjon, Nivåkode, "k2021")
  kvalifikasjoner_2020 <- bind_rows(ar_fulltid_2020, hk_fulltid_2020,
                                    ba_fulltid_2020, ba_trekvart_2020, ba_halvfart_2020, 
                                    ma_fulltid_2020, ma_totredel_2020, ma_halvfart_2020) %>% 
    select(Studieprogramkode, Studieprogram, progresjon, Nivåkode, "k2020")
  
  kvalifikasjoner_full <- full_join(kvalifikasjoner_2020, kvalifikasjoner_2021)
  kvalifikasjoner_full <- full_join(kvalifikasjoner_full, kvalifikasjoner_2022)
  kvalifikasjoner_full <- full_join(kvalifikasjoner_full, kvalifikasjoner_2023)
  
  kvalifikasjoner_full <- kvalifikasjoner_full %>% 
    mutate(across(starts_with("k20"), ~as.numeric(gsub(",",".", .))))
  
  return(kvalifikasjoner_full)
}

##* Lage ein per progresjon / fullført år seinast 2 semester over normert
##* 
ferdig_kvalifikasjon <- function(filnamn, ferdig_ar, inn_progresjon, skriv_test = F) {
  # les inn excelutdrag frå Tableau, dropp første to rader
  inndata <- read_excel(filnamn, skip = 1)
  
  # transponer for å få programnamn som rader
  inndata <- inndata %>% t %>% as.data.frame

  # Gjer om første rad til kolonnenamn
  inndata <- inndata %>% row_to_names(row_number = 1) %>% 
    clean_names()
  
  # Gjer om radnamn til kolonne
  inndata <- inndata %>% rownames_to_column(var = "Studieprogram")
  # print(inndata %>% names)
  
  inndata <- inndata %>% as_tibble
  
  # Legg til radene pluss_1_sem og pluss_2_sem om dei ikkje finst
  inndata <- tryCatch(inndata %>% add_column(pluss_1_sem = as.character(NA)), 
           error = function(e) {
             return(inndata)},
           silent = TRUE)
  
  inndata <- tryCatch(inndata %>% add_column(pluss_2_sem = as.character(NA)), 
                      error = function(e) {
                        return(inndata)},
                      silent = TRUE)

  # Legg til progresjonsdata
  instnr <- 1175
  dbh_vars <- c("Studieprogramkode",
                "Årstall",
                "Andel av heltid",
                "Nivåkode")
  dbh_progresjon <- dbh_data(347, 
                             filters = c("Institusjonskode" = instnr), 
                             variables = dbh_vars, 
                             group_by = dbh_vars) %>%
    arrange(desc(Årstall)) %>% distinct(Studieprogramkode, .keep_all = T) %>%
    rename("dbh_ar" = "Årstall", "progresjon" = "Andel av heltid")
  inndata <- inndata %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% 
    left_join(dbh_progresjon, by = "Studieprogramkode")
  
  # Filtrer ut program med anna progresjon
  inndata <- inndata %>% filter(progresjon == inn_progresjon)
  
  # skriv ut kor mange typar progresjon datasettet har - for å kunne kontrollere tap av informasjon
  print(inndata %>% select(progresjon) %>% unique)
  
  # Fyll ut tomme celler
  inndata <- inndata %>% mutate(pluss_1_sem = coalesce(pluss_1_sem, normert))
  inndata <- inndata %>% mutate(pluss_2_sem = coalesce(pluss_2_sem, pluss_1_sem))
  
  feil_linjer <- inndata %>% filter(is.na(pluss_2_sem) | pluss_2_sem < pluss_1_sem)
  if (feil_linjer %>% nrow > 0) print(feil_linjer)
  
  # Bytt ut med pluss_1_sem
  inndata <- inndata %>% rename(!!ferdig_ar := pluss_2_sem)  
  
  if (skriv_test) inndata %>% select(-1) %>% print
  
  return(inndata)
  # View(inndata %>% head)
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
  # soknadsdata_namn <- left_join(soknadsdata, OM_programvar, "Studieprogramkode") %>% filter(!is.na(Studieprogram_instnamn))
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

# Hent gjennomføringsdata
portefolje_prep_normert_deprecated <- function() {
  
  instnr <- 1175
  dbh_vars <- c("Studieprogramkode",
                "Årstall",
                "Andel av heltid")
  dbh_progresjon <- dbh_data(347, 
                             filters = c("Institusjonskode" = instnr), 
                             variables = dbh_vars, 
                             group_by = dbh_vars) %>%
    arrange(desc(Årstall)) %>% distinct(Studieprogramkode, .keep_all = T) %>%
    rename("dbh_ar" = "Årstall", "progresjon" = "Andel av heltid")
  
  periode_1 <- "2020 HØST"
  periode_2 <- "2021 HØST"
  periode_3 <- "2022 HØST"
  
  ##** 
  ##* Les inn filer, fyller opp tomme celler med Studieprogramnamn, 
  ##* filtrerer bort GLU bortsett frå i eitt sett
  ##* TODO left_join på Studieprogram for å få tabell for MA, GLU, BA med fullførtprosent 2020, 2021, 2022
  ##* TODO bind_rows på dei tre datasetta, for å få ein samla tabell å bruke XLOOKUP på
  
  ##* Fulltid
  BA2016 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2016 BA mm Tallene - studieprogram.xlsx")
  BA2016 <- BA2016 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  BA2016_fulltid <- BA2016 %>% filter(`År termin` == periode_1) %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  BA2017 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2017 BA mm Tallene - studieprogram.xlsx")
  BA2017 <- BA2017 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  BA2017_fulltid <- BA2017 %>% filter(`År termin` == periode_2) %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  BA2018 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2018 BA mm Tallene - studieprogram.xlsx")
  BA2018 <- BA2018 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  BA2018_fulltid <- BA2018 %>% filter(`År termin` == periode_3) %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  BA2016_2018_fulltid <- list(BA2016_fulltid, BA2017_fulltid, BA2018_fulltid) %>% reduce(full_join, by = "Studieprogram")
  
  GLU2016 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2016 BA mm Tallene - studieprogram.xlsx")
  GLU2016 <- GLU2016 %>% fill(Studieprogram) %>% filter(grepl("GLU", Studieprogram))
  GLU2016 <- GLU2016 %>% filter(`År termin` == periode_3) %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  MA2017 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2017 MA Tallene - studieprogram.xlsx")
  MA2017 <- MA2017 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  MA2017_fulltid <- MA2017 %>% filter(`År termin` == periode_1) %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  MA2018 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2018 MA Tallene - studieprogram.xlsx")
  MA2018 <- MA2018 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  MA2018_fulltid <- MA2018 %>% filter(`År termin` == periode_2) %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  MA2019 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2019 MA Tallene - studieprogram.xlsx")
  MA2019 <- MA2019 %>% fill(Studieprogram) %>% filter(!grepl("GLU", Studieprogram))
  MA2019_fulltid <- MA2019 %>% filter(`År termin` == periode_3) %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`)
  
  MA2017_2019_fulltid <- list(MA2017_fulltid, MA2018_fulltid, MA2019_fulltid) %>% reduce(full_join, by = "Studieprogram")
  
  ##* Deltid BA
  BA2013 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2013 BA for deltid Tallene - studieprogram.xlsx")
  BA2013 <- BA2013 %>% fill(Studieprogram) %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  BA2014 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2014 BA for deltid Tallene - studieprogram.xlsx")
  BA2014 <- BA2014 %>% fill(Studieprogram) %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  BA2015 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2015 BA for deltid Tallene - studieprogram.xlsx")
  BA2015 <- BA2015 %>% fill(Studieprogram) %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  
  BA2016 <- BA2016 %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  BA2017 <- BA2017 %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  
  BA2013_deltid_halv <- BA2013 %>% filter(progresjon == 0.5, `År termin` == periode_1) %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  BA2014_deltid_halv <- BA2014 %>% filter(progresjon == 0.5, `År termin` == periode_2) %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)  
  BA2015_deltid_halv <- BA2015 %>% filter(progresjon == 0.5, `År termin` == periode_3) %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  
  BA2015_deltid_trekvart <- BA2015 %>% filter(progresjon == 0.75, `År termin` == periode_1) %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  BA2016_deltid_trekvart <- BA2016 %>% filter(progresjon == 0.75, `År termin` == periode_2) %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)  
  BA2017_deltid_trekvart <- BA2017 %>% filter(progresjon == 0.75, `År termin` == periode_3) %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  
  BA2013_2015_halvtid <- list(BA2013_deltid_halv, BA2014_deltid_halv, BA2015_deltid_halv) %>% reduce(full_join, by = "Studieprogram")
  BA2015_2017_kvarttid <- list(BA2015_deltid_trekvart, BA2016_deltid_trekvart, BA2017_deltid_trekvart) %>% reduce(full_join, by = "Studieprogram")
  
  # Deltid MA
  MA2015 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2015 MA for deltid Tallene - studieprogram.xlsx")
  MA2015 <- MA2015 %>% fill(Studieprogram) %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  MA2016 <- read_excel("c:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/Studieportefølje/Gjennomføring/2016 MA for deltid Tallene - studieprogram.xlsx")
  MA2016 <- MA2016 %>% fill(Studieprogram) %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  
  MA2017 <- MA2017 %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  MA2018 <- MA2018 %>% mutate(Studieprogramkode = word(Studieprogram, 1)) %>% left_join(dbh_progresjon, by = "Studieprogramkode")
  
  MA2015_deltid_halv <- MA2015 %>% filter(progresjon == 0.5, `År termin` == periode_1) %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  MA2016_deltid_halv <- MA2016 %>% filter(progresjon == 0.5, `År termin` == periode_2) %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)  
  MA2017_deltid_halv <- MA2017 %>% filter(progresjon == 0.5, `År termin` == periode_3) %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  
  MA2016_deltid_totredeler <- MA2016 %>% filter(progresjon == 0.67, `År termin` == periode_1) %>% 
    select(Studieprogram, Kvalifikasjoner_2020H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  MA2017_deltid_totredeler <- MA2017 %>% filter(progresjon == 0.67, `År termin` == periode_2) %>% 
    select(Studieprogram, Kvalifikasjoner_2021H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)  
  MA2018_deltid_totredeler <- MA2018 %>% filter(progresjon == 0.67, `År termin` == periode_3) %>% 
    select(Studieprogram, Kvalifikasjoner_2022H =`Andel_kvalifikasjoner 2 along Studieprogram, År termin`, -dbh_ar, -progresjon)
  
  MA2015_2017_halvtid <- list(MA2015_deltid_halv, MA2016_deltid_halv, MA2017_deltid_halv) %>% reduce(full_join, by = "Studieprogram")
  MA2016_2018_totredelstid <- list(MA2016_deltid_totredeler, MA2017_deltid_totredeler, MA2018_deltid_totredeler) %>% reduce(full_join, by = "Studieprogram")
  
  kvalifikasjoner_sett_fulltid <- bind_rows(BA2016_2018_fulltid, GLU2016, MA2017_2019_fulltid)
  kvalifikasjoner_sett_fulltid <- kvalifikasjoner_sett_fulltid %>% mutate(Studieprogramkode = word(Studieprogram, 1))
  kvalifikasjoner_sett_fulltid <- left_join(kvalifikasjoner_sett_fulltid, dbh_progresjon, by = "Studieprogramkode")
  kvalifikasjoner_sett_fulltid <- kvalifikasjoner_sett_fulltid %>% filter(progresjon == 1)
  
  
  kvalifikasjoner_sett_deltid <- bind_rows(BA2013_2015_halvtid, BA2015_2017_kvarttid,
                                           MA2015_2017_halvtid, MA2016_2018_totredelstid)
  kvalifikasjoner_sett_deltid <- kvalifikasjoner_sett_deltid %>% mutate(Studieprogramkode = word(Studieprogram, 1))
  kvalifikasjoner_sett_deltid <- left_join(kvalifikasjoner_sett_deltid, dbh_progresjon, by = "Studieprogramkode")
  
  kvalifikasjoner_sett <- bind_rows(kvalifikasjoner_sett_fulltid, kvalifikasjoner_sett_deltid)
  
  return(kvalifikasjoner_sett)
}

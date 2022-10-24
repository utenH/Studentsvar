##**
##* Funksjonar for førebuing av dataframes
##* Ville helst hatt berre ein, men datasetta er ikkje heilt like nok, 
##* og det er viktigare å ha same utskriftsfunksjonar
##* 
OM_prepare_2022 <- function(innfil, dataår, instnr, programkode) {
  OM <- read_excel(innfil)
  OM <- OM %>% mutate(undersøkelse_år = dataår)
  # TODO - har brukt "fakultet" som gruppering - om andre skal kunne bruke den, 
  # må eg bruke annan variabel, og Fakultetsnavn er for lang til å brukast i arknamn
  # OM <- SB_name_fak_kort(OM)
  # Må sikre at variabelen i datasettet heiter programkode, eller spørje kva variabel som skal brukast
  OM <- dbh_add_programdata(OM, programkode, instnr)
  OM <- SB_add_sum_tid(OM)
  OM <- SB_prep_indeks(OM)
  return(OM)
} 

##* Studiebarometeret
SB_prepare_2022 <- function(innfil, dataår, instnr) {
  OM <- read_excel(innfil)
  # OM <- read.xlsx(innfil)
  OM <- OM %>% mutate(undersøkelse_år = dataår)
  OM[OM==9999] <- NaN # Var NA, bytta 2020 for å skilje mellom ikkje-svar (NA) og Vet ikke (NaN)
  OM <- SB_name_fak_kort(OM)
  OM <- dbh_add_programdata(OM, "studprog_kod", instnr)
  OM <- SB_add_sum_tid(OM)
  OM <- SB_prep_indeks(OM)
  return(OM)
} 

##** Funksjon som tar spesialtilfelle i Studiebarometeret
##*  Andre datasett må vere vaska først
SB_unntak <- function(sdf) {
  sdf$Fakultetsnavn[grepl("YFLH", sdf$Studieprogramkode)] <- "Fakultet for lærerutdanning og internasjonale studier (LUI)"
  sdf <- sdf %>% mutate(Studieprogramnavn = coalesce(Studieprogramnavn, studiepgm_navn))
  sdf <- sdf %>% mutate(Nivåkode = coalesce(Nivåkode, STUDIENIVAKODE))
  return(sdf)
}

##**
##* Funksjon for førebuing av dataframes, Sisteårs 
##* Skulle gjerne hatt felles, men viktigare med felles utskriftsfunksjon
##* 
SA_prepare_2022 <- function(innfil, dataår, instnr) {
  OM <- read_excel(innfil, .name_repair = janitor::make_clean_names)
  colnames(OM) <- gsub(pattern = "-", replacement = "", x = colnames(OM))
  OM <- OM %>% mutate(undersøkelse_år = dataår)
  OM[OM==9999] <- NaN # skiljer mellom ikkje-svar (NA) og Vet ikke (NaN)
  OM <- dbh_add_programdata(OM, "fs_kode", instnr)
  OM <- SA_tilpassdata(OM)
  OM <- SA_prep_indeks(OM)
  # TODO dette er lat hack, finn betre måte, f.eks. ta med instnr frå DBH
  OM <- OM %>% mutate(instnr = 1175) 
  return(OM)
} 

## Koder og lagar df for data frå eitt år
# Fritekst analyse - lagt til 2022
SA_prepare_fritekst <- function(datafil) {
  SA_raw <- read_excel(datafil)
  SA <- as.data.frame(SA_raw)
  SA <- SA %>% rename("studprog_kod" = `fs-kode`)
  # SA <- SB_name_institute(SA)
  SA <- SA %>% rename("Programkode" = "studprog_kod")
  # SA <- dbh_add_programnavn(SA, "Programkode")
  SA <- dbh_add_programdata(SA, "Programkode", 1175)
  print(SA %>% names)
  SA <- SA %>% group_by(Institutt) %>% group_split()
  return(SA)
}

# Hjelpefunksjon for omkoding - sjekk kode for kandidatundersøking
SA_tilpassdata <- function(sdf) {
  #Svaralternativer: "1 = Ikke enig" - 5 = "Helt enig"	
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_jeg_kjenner_godt_til_laeringsutbyttebeskrivelsene_for_studieprogrammet_mitt)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_jeg_er_godt_fornoyd_med_laeringsutbyttet_jeg_har_hatt_pa_studieprogrammet)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_alt_i_alt_tror_jeg_ikke_at_kronapandemien_har_svekket_laeringsutbyttet_mitt)
  
  #Svaralternativer: 1="Ikke fornøyd" - 5="Svært fornøyd"	
  sdf <- SA_text_to_numerallevels(sdf, hvor_fornoyd_er_du_med_folgende_forhold_i_studieprogrammet_ditt_de_faglig_ansattes_evne_til_a_formidle_laerestoffet_pensum_pa_en_forstaelig_mate)
  sdf <- SA_text_to_numerallevels(sdf, hvor_fornoyd_er_du_med_folgende_forhold_i_studieprogrammet_ditt_faglig_veiledning_og_diskusjoner_med_faglig_ansatte)
  sdf <- SA_text_to_numerallevels(sdf, hvor_fornoyd_er_du_med_folgende_forhold_i_studieprogrammet_ditt_miljoet_mellom_undervisere_og_studenter)
  sdf <- SA_text_to_numerallevels(sdf, hvor_fornoyd_er_du_med_folgende_forhold_i_studieprogrammet_ditt_det_faglige_miljo_blant_studentene)
  sdf <- SA_text_to_numerallevels(sdf, hvor_fornoyd_er_du_med_folgende_forhold_i_studieprogrammet_ditt_det_sosiale_miljoet_blant_studentene)
  
  #Svaralternativer: "1 = Ikke enig" - 5 = "Helt enig"	
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_jeg_er_fornoyd_med_maten_emneevalueringer_gjennomfores_pa_i_mitt_studieprogram)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_jeg_tror_laererne_pa_mitt_studieprogram_bruker_emneevalueringene_til_a_forbedre_utdanningen)
  
  #"Ja, alle emnene har vært nyttige" versus  "Nei, minst ett emne har vært mindre nyttig"	
  sdf <- OM_janei_bin(sdf, har_alle_emnene_i_studieprogrammet_ditt_vaert_nyttige)
  
  #Svaralternativer: "1 = Ikke enig" - 5 = "Helt enig"	
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_det_er_lett_a_finne_informasjonen_jeg_trenger_i_canvas)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_det_er_lett_a_finne_informasjonen_jeg_trenger_pa_student_oslomet_no)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_det_er_godt_samsvar_mellom_informasjon_fra_undervisere_og_administrasjon)
  
  #Svaralternativer: "1 = Ikke enig" - 5 = "Helt enig"	
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_jeg_var_godt_forberedt_til_praksisperioden_e)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_det_var_godt_samarbeid_mellom_praksisstedet_og_oslo_met)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_teoriopplaeringen_var_relevant_for_praksisutovelsen)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_praksisutovelsen_ble_brukt_som_grunnlag_for_diskusjon_refleksjon_i_undervisningen)
  
  #Tellende svaralternativer: "Ja", "Nei". "Usikker" er ikke tatt med	
  sdf <- OM_janei_bin(sdf, har_du_i_lopet_av_tiden_pa_dette_studieprogrammet_hatt_gjesteforeleser_laerer_fra_en_utenlandsk_undervisningsinstitusjon_i_noen_av_dine_emner)
  sdf <- OM_janei_bin(sdf, har_du_i_lopet_av_tiden_pa_dette_studieprogrammet_fatt_undervisning_pa_engelsk_i_noen_av_dine_emner_ved_oslo_met)
  sdf <- OM_janei_bin(sdf, har_du_i_lopet_av_tiden_pa_dette_studieprogrammet_vaert_pa_utvekslings_studie_praksis_eller_prosjektopphold_i_utlandet)
  
  #"Svaralternativer:  1=""Ikke fornøyd"" - 5=""Svært fornøyd""
  #NB! De som opplyste at studieoppholdet ble sterkt berørt av koronasituasjonen har ikke fått dette spørsmålet."
  sdf <- SA_text_to_numerallevels(sdf, hvor_fornoyd_er_du_med_utbyttet_av_utenlandsoppholdet)
  
  #Tellende svaralternativer: "Ja", "Nei". "Usikker" er ikke tatt med	
  sdf <- OM_janei_bin(sdf, har_du_i_lopet_av_tiden_pa_dette_studieprogrammet_fatt_undervisning_som_har_gitt_deg_internasjonal_og_flerkulturell_kompetanse)
  sdf <- OM_janei_bin(sdf, har_du_i_lopet_av_tiden_pa_dette_studieprogrammet_deltatt_i_nettbaserte_grupper_med_studenter_fra_andre_land)
  #Skala: 1="I liten grad" - 5="I stor grad"	
  sdf <- SA_text_to_numerallevels(sdf, i_hvilken_grad_mener_du_at_eksamener_innleveringer_og_andre_vurderingsformer_i_studieprogrammet_ditt_har_handlet_om_sentrale_deler_av_laerestoffet_pensum)
  sdf <- SA_text_to_numerallevels(sdf, i_hvilken_grad_mener_du_at_eksamener_innleveringer_og_andre_vurderingsformer_i_studieprogrammet_ditt_har_krevd_forstaelse_og_resonnement)
  sdf <- SA_text_to_numerallevels(sdf, i_hvilken_grad_mener_du_at_eksamener_innleveringer_og_andre_vurderingsformer_i_studieprogrammet_ditt_har_hatt_tydelige_kriterier_for_vurdering)
  sdf <- SA_text_to_numerallevels(sdf, i_hvilken_grad_mener_du_at_eksamener_innleveringer_og_andre_vurderingsformer_i_studieprogrammet_ditt_har_bidratt_til_din_faglige_utvikling)
  #Skala: 1="Ikke fornøyd" - 5="Svært fornøyd"	
  sdf <- SA_text_to_numerallevels(sdf, alt_i_alt_hvor_fornoyd_er_du_med_studieprogrammet_du_gar_pa)
  
  return(sdf)
}

##**
##* DBH-funksjonar 
library(rdbhapi)

# Slår saman på programkode, for å legge til programdata frå DBH
# sdf - dataframe å slå saman med
# varnamn - variabel i sdf for programkode
# natjoin - set denne til T dersom to tabellar frå DBH skal slås saman
dbh_add_programdata <- function(sdf, varnamn, instnr, natjoin = F) {
  OM_data <- dbh_hent_programdata(instnr)
  if (!natjoin) {
  sdf <- left_join(sdf, OM_data,
                   by = setNames("Studieprogramkode", varnamn))
  } else {
    # Dersom det er behov for å slå saman tabellar frå DBH, 
    # ønsker vi å slå saman på alle kolonner som er like
    sdf <- left_join(sdf, OM_data)
  }
  sdf <- sdf %>% rename("Studieprogramkode" = {{varnamn}})
  # Om det er Studiebarometer, må vi handtere nokre greier, sjekke her for å kunne kode StudiumID
  if (instnr == 1175 & "STUDIENIVAKODE" %in% colnames(sdf)) {
    sdf <- SB_unntak(sdf)
  }
  
  sdf <- dbh_add_nivå(sdf)
  sdf <- dbh_add_utdtype(sdf)
  sdf <- dbh_add_progresjon(sdf, `Andel av heltid`)
  # TODO - vurder søk og fjern av
  # "Master i", Masterstudium i ", Master Programme in ", "Master's Degree Programme in "
  # "Bachelorstudium i ", "Bachelorstudium - " 
  # (vurder å passe på at det blir stor første bokstav
  sdf <- sdf %>% mutate(StudiumID = paste(Studieprogramkode, Studieprogramnavn))
  return(sdf)
}

# Slår saman på programkode, for å legge til programnamn
# TODO lag funksjon dbh_hent_programnavn, for å ha meir spissa spørring
# sdf - dataframe å slå saman med
# varnamn - variabel i sdf for programkode
dbh_add_programnavn <- function(sdf, varnamn) {
  OM_data <- dbh_hent_programdata() %>% 
    select(Studieprogramkode, Studieprogramnavn)
  sdf <- left_join(sdf, OM_data,
                   by = setNames("Studieprogramkode", varnamn))
  return(sdf)
}

##**
##* Hentar data frå DBH, til å slå saman med andre datasett 
#* TODO - denne må skrivast om for bruk på rshiny - slik at institusjonskode blir valt frå utfyllinga
dbh_hent_programdata <- function(instnr = "1175") {
  dbh_vars <- c("Studieprogramkode", 
                "Studieprogramnavn",
                "Avdelingskode",
                "Nivåkode", 
                "Årstall", 
                "Andel av heltid", 
                "Andel praksis")
  dbh_programdata <- dbh_data(347, 
                              filters = c("Institusjonskode" = instnr), 
                              variables = dbh_vars, 
                              group_by = dbh_vars) %>%
    arrange(desc(Årstall)) %>% distinct(Studieprogramkode, .keep_all = T) 
  dbh_programdata <- left_join(dbh_programdata, dbh_hent_orgdata(instnr), "Avdelingskode")
  return(dbh_programdata)
}

##** Hentar ein tabell med instituttnamn, kan koplast på kolonna Avdelingskode 
dbh_hent_orgdata <- function(instnr) {
  sdf <- dbh_data(457, filters = c("Institusjonskode" = instnr, "Årstall" = "2022"), 
                  variables = c("Avdelingskode", "Avdelingskode_SSB", "Avdelingsnavn", "Årstall"), 
                  group_by = c("Avdelingskode", "Avdelingskode_SSB", "Avdelingsnavn", "Årstall")) %>% 
    # Alternativ måte: sjekke om Avdelingskode_SSB sluttar med 00, det er instituttnivå, 000 og opp må også bort?
    # filter(as.numeric(Avdelingskode) %% 100 == 0 & as.numeric(Avdelingskode) %% 1000 != 0 & as.numeric(Avdelingskode) %% 10000 != 0) %>%
    filter(grepl("^Institutt for|^Handelshøyskolen", Avdelingsnavn)) %>%
    mutate(Fakultetskode = strtrim(Avdelingskode, 3)) %>% 
    arrange(Avdelingsnavn) %>% select(Avdelingskode, Avdelingskode_SSB, Fakultetskode, Institutt = Avdelingsnavn)
  
  if (instnr == 1175) {
    sdf <- sdf %>% mutate(Institutt = 
                            case_when(Institutt == "Institutt for fysioterapi" | 
                                        Institutt == "Institutt for ergoterapi og ortopediingeniørfag" ~ 
                                        "Institutt for rehabiliteringsvitenskap og helseteknologi",
                                      TRUE ~ Institutt))
  }
  
  # Hentar tabell med fakultetskode og -namn
  # TODO - dei programma som er tilknytt fakultet på høgste nivå, fell utanom her - forsøk å fikse
  dbh_210 <- dbh_data(210) %>% filter(Institusjonskode == instnr) %>%
    select(Fakultetskode, Fakultetsnavn) %>% unique() %>% arrange(Fakultetskode)
  
  # Forkorte fakultetsnamn
  if (instnr == 1175) {
    dbh_210 <- dbh_210 %>% mutate(Fakultetsnavn = case_when(
      grepl("HV", Fakultetsnavn) ~ "Fakultet HV",
      grepl("LUI", Fakultetsnavn) ~ "Fakultet LUI",
      grepl("SAM", Fakultetsnavn) ~ "Fakultet SAM",
      grepl("TKD", Fakultetsnavn) ~ "Fakultet TKD"
    ))
  }
  
  # Slår saman tabellane for å få med fakultetsnamn
  sdf <- left_join(sdf, dbh_210, by = "Fakultetskode")
  return(sdf)
}

##**
##* Legg til koding for Bachelor, Master, Annet
dbh_add_nivå <- function(sdf) {
  # Om det er Studiebarometer, må vi handtere nokre greier, sjekke her for å kunne kode StudiumID
  if ("STUDIEAR" %in% colnames(sdf)) {
    sdf <- sdf %>% mutate(Nivå = case_when(
      Nivåkode == "M2" |
        (Nivåkode == "M5" & STUDIEAR == 5) |
        Nivåkode == "ME" ~ "Master",
      Nivåkode == "B3" | 
        (Nivåkode == "M5" & STUDIEAR == 2) |
        # Takk, Studiebarometeret, i 2020 mangla denne for MxGLU
        (Nivåkode == "M5" & is.na(STUDIEAR)) ~ "Bachelor", 
      TRUE ~ "Annet"
    ))
  } else {
    sdf <- sdf %>% mutate(Nivå = case_when(
      Nivåkode == "B3" ~ "Bachelor",
      Nivåkode == "M2" | Nivåkode == "ME" | Nivåkode == "M5" ~ "Master",
      TRUE ~ "Annet"
    ))
  }
  return(sdf)
}

##**
##* Legg til koding for nivåforklaring, meir detaljert inndeling
dbh_add_utdtype <- function(sdf) {
  sdf <- sdf %>% mutate(Utdanningstype = case_when(
    Nivåkode == "B3" ~ "Bachelor",
    Nivåkode == "M2" ~ "Master 2-årig",
    Nivåkode == "ME" ~ "Master erfaringsbasert",
    Nivåkode == "M5" ~ "Master 5-årig",
    Nivåkode == "AR" ~ "Årsenhet",
    Nivåkode == "LN" ~ "Lavere nivå (øvrige)",
    Nivåkode == "HN" ~ "Høyere nivå (øvrige)",
    Nivåkode == "HK" ~ "Høyskolekandidat",
    Nivåkode == "VS" ~ "Videregående skoles nivå",
    TRUE ~ "Annet"
  ))
  return(sdf)
}

##**
##* Legg til variabel for progresjon, inndelt i fire kategoriar 
##* sdf - dataframe
##* tidsvar - kolonne med progresjonsvariabel som desimal
dbh_add_progresjon <- function(sdf, tidsvar) {
  # sdf <- sdf %>% mutate(Progresjon = cut(.$`Andel av heltid`, 
  sdf <- sdf %>% mutate(Progresjon = cut({{tidsvar}}, 
                                         breaks = c(0, 0.49, 0.5, 0.8, 1), 
                                         labels = c("<50 %", "50 %", "60-80 %", "100 %"), 
                                         include.lowest = T, ordered_result = T))
  return(sdf)
}

dbh_hent_studieplassdata <- function(institusjonsnummer) {
  sdf <- dbh_data(370, filters = c("institusjonskode"=institusjonsnummer))
  return(sdf)
}

dbh_hent_studiepoengdata <- function(institusjonsnummer) {
  sdf <- dbh_data(900, filters = c("institusjonskode"=institusjonsnummer))
  return(sdf)
}

##**
##* Funksjon for å hente inn programdata frå excelfil
##* sdf - dataframe som skal utvidast
##* varnamn - variabel i sdf som matchar OM_programkode, med sitatteikn rundt
# OM_programkode
# OM_programnavn
# OM_master
# OM_heltid
# OM_institutt
# OM_fakultetsnavn
# OM_fakultetskode
OM_add_programdata <- function(sdf, varnamn) {
  # TODO: Start å bruke OM_programkode i staden for studieprogramkode/studprog_kod?
  # mappesti <- "C:/Users/kyrremat/OneDrive - OsloMet/Dokumenter/statistikk/R felles/"
  OM_data <- read.xlsx("../R felles/OM_programdata.xlsx")
  sdf <- left_join(sdf, OM_data,
                   by = setNames("OM_programkode", varnamn))
  return(sdf)
  # sdf <- sdf %>% rename("OM_programkode" = {{varnamn}})
  # sdf <- left_join(sdf, OM_data, by="OM_programkode")
  # sdf <- sdf %>% rename({{varnamn}} := "OM_programkode")
}

## Taldata  ##

# Legg til alle indeksar, set alle indx med for mange missing til NA
# TODO vurder å bruke {{}}-notasjon for å komprimere koden
SA_prep_indeks <- function(sdf) {
  sdf$indx_praksis4 <- SB_add_indx_m(sdf, SAvar_praksis)
  sdf$mis_praksis4 <- SB_add_countmiss(sdf, SAvar_praksis)
  sdf$indx_praksis4[which(sdf$mis_praksis4 > 1)] <- NA
  return(sdf)
}

SAvar_praksis <- c("hvor_enig_er_du_i_disse_pastandene_jeg_var_godt_forberedt_til_praksisperioden_e",
                   "hvor_enig_er_du_i_disse_pastandene_det_var_godt_samarbeid_mellom_praksisstedet_og_oslo_met",
                   "hvor_enig_er_du_i_disse_pastandene_teoriopplaeringen_var_relevant_for_praksisutovelsen",
                   "hvor_enig_er_du_i_disse_pastandene_praksisutovelsen_ble_brukt_som_grunnlag_for_diskusjon_refleksjon_i_undervisningen")

# Filtrerer bort ytterverdiar: tid_brutto < 61, tid_brutto > 11 er med
# For fakultet og institusjonsnivå tar vi berre med heiltidsstudentar
SB_add_sum_tid <- function(sdf) {
  sdf$tid_brutto <- sdf %>% 
    select(tidsbruk_egeninns_14, tidsbruk_laerakt_14, tidsbruk_arbeid_14) %>%
    rowSums() 
  
  # Legg til variablar, tar bort ekstreme verdiar (deltid må filterast bort for institusjonsnivå og fakultet)
  sdf$sum_tid_studier <- sdf %>% select(tidsbruk_egeninns_14, tidsbruk_laerakt_14) %>% 
    rowSums()
  sdf$sum_tid_studier[sdf$tid_brutto <= 11 | sdf$tid_brutto >= 61] <- NA
  sdf$sum_tid_studier[is.na(sdf$tid_brutto)] <- NA
  
  sdf$tid_egenstudier <- sdf$tidsbruk_egeninns_14
  sdf$tid_egenstudier[sdf$tid_brutto <= 11 | sdf$tid_brutto >= 61] <- NA
  sdf$tid_egenstudier[is.na(sdf$tid_brutto)] <- NA
  
  sdf$tid_orgstudier <- sdf$tidsbruk_laerakt_14
  sdf$tid_orgstudier[sdf$tid_brutto <= 11 | sdf$tid_brutto >= 61] <- NA
  sdf$tid_orgstudier[is.na(sdf$tid_brutto)] <- NA
  
  sdf$tid_arbeid <- sdf$tidsbruk_arbeid_14
  sdf$tid_arbeid[sdf$tid_brutto <= 11 | sdf$tid_brutto >= 61] <- NA
  sdf$tid_arbeid[is.na(sdf$tid_brutto)] <- NA
  
  sdf$tid_brutto[sdf$tid_brutto <= 11 | sdf$tid_brutto >= 61] <- NA
  
  return(sdf)
}

# Lagar indeks per respondent, 
# tar også med der det vil vere for mange missing til å inkluderast i snitt
# kall på med df$indx_namn <- SB_add_indx_m
SB_add_indx_m <- function(sdf, var_liste) {
  sdf %>% select(all_of(var_liste)) %>% rowMeans(na.rm = TRUE) 
}

SB_add_countmiss <- function(sdf, var_liste) {
  sdf %>% select(all_of(var_liste)) %>% is.na() %>% rowSums()
}

# Legg til alle indeksar, set alle indx med for mange missing til NA
SB_prep_indeks <- function(sdf) {
  sdf$indx_underv4 <- SB_add_indx_m(sdf, var_underv)
  sdf$mis_underv4 <- SB_add_countmiss(sdf, var_underv)
  sdf$indx_underv4[which(sdf$mis_underv4 > 1)] <- NA
  
  sdf$indx_tilbveil4 <- SB_add_indx_m(sdf, var_tilbveil)
  sdf$mis_tilbveil4 <- SB_add_countmiss(sdf, var_tilbveil)
  sdf$indx_tilbveil4[which(sdf$mis_tilbveil4 > 1)] <- NA
  
  sdf$indx_psymiljo3 <- SB_add_indx_m(sdf, var_psymiljo)
  sdf$mis_psymiljo3 <- SB_add_countmiss(sdf, var_psymiljo)
  sdf$indx_psymiljo3[which(sdf$mis_psymiljo3 > 0)] <- NA  
  
  try(expr = {
    sdf$indx_fysmiljo4 <- SB_add_indx_m(sdf, var_fysmiljo)
    sdf$mis_fysmiljo4 <- SB_add_countmiss(sdf, var_fysmiljo)
    sdf$indx_fysmiljo4[which(sdf$mis_fysmiljo4 > 1)] <- NA  
  }, silent = TRUE)
  
  sdf$indx_organ4 <- SB_add_indx_m(sdf, var_organ)
  sdf$mis_organ4 <- SB_add_countmiss(sdf, var_organ)
  sdf$indx_organ4[which(sdf$mis_organ4 > 1)] <- NA  
  
  sdf$indx_vurd5 <- SB_add_indx_m(sdf, var_vurd)
  sdf$mis_vurd5 <- SB_add_countmiss(sdf, var_vurd)
  sdf$indx_vurd5[which(sdf$mis_vurd5 > 1)] <- NA  
  
  sdf$indx_laerutb10 <- SB_add_indx_m(sdf, var_laerutb)
  sdf$mis_laerutb10 <- SB_add_countmiss(sdf, var_laerutb)
  sdf$indx_laerutb10[which(sdf$mis_laerutb10 > 3)] <- NA  
  
  sdf$indx_medv3 <- SB_add_indx_m(sdf, var_medv)
  sdf$mis_medv3 <- SB_add_countmiss(sdf, var_medv)
  sdf$indx_medv3[which(sdf$mis_medv3 > 0)] <- NA  
  
  sdf$indx_insp3 <- SB_add_indx_m(sdf, var_insp)
  sdf$mis_insp3 <- SB_add_countmiss(sdf, var_insp)
  sdf$indx_insp3[which(sdf$mis_insp3 > 0)] <- NA  
  
  # Endra i 2020
  try(expr = {
    sdf$indx_praksis6 <- SB_add_indx_m(sdf, var_praksis)
    sdf$mis_praksis6 <- SB_add_countmiss(sdf, var_praksis)
    sdf$indx_praksis6[which(sdf$mis_praksis6 > 2)] <- NA
  }, silent = TRUE)
  
  try(expr = {
    sdf$indx_yrkrel6 <- SB_add_indx_m(sdf, var_yrkrel)
    sdf$mis_yrkrel6 <- SB_add_countmiss(sdf, var_yrkrel)
    sdf$indx_yrkrel6[which(sdf$mis_yrkrel6 > 2)] <- NA 
  }, silent = TRUE)
  
  sdf$indx_eget4 <- SB_add_indx_m(sdf, var_egeteng)
  sdf$mis_eget4 <- SB_add_countmiss(sdf, var_egeteng)
  sdf$indx_eget4[which(sdf$mis_eget4 > 1)] <- NA
  
  sdf$indx_forvent4 <- SB_add_indx_m(sdf, var_forvent)
  sdf$mis_forvent4 <- SB_add_countmiss(sdf, var_forvent)
  sdf$indx_forvent4[which(sdf$mis_forvent4 > 1)] <- NA
  
  sdf$indx_digit4 <- SB_add_indx_m(sdf, var_digitale)
  sdf$mis_digit4 <- SB_add_countmiss(sdf, var_digitale)
  sdf$indx_digit4[which(sdf$mis_digit4 > 1)] <- NA
  
  return(sdf)
}

##** Kandidatundersøkelsen - hjelpefunksjonar for å rydde og slå saman data til tidsserie
##*
##*

# Hjelpefunksjon for å lett kunne sjå enkeltvariablar
skrivvar <- function(sdf, variabel) {
  sdf %>% select({{variabel}}) %>% table(useNA = "ifany") %>% addmargins()
}

##** Kandidatundersøkinga 2022 - forbered data
OM_kandidat_setup <- function(sdf) {
  sdf <- OM_set_faktor(sdf, studerer_niva)
  sdf <- set_a5_hovedaktivitet(sdf)
  sdf <- OM_janei_bin(sdf, arbeider_utdannet_til)
  sdf <- set_grunn_annet_arbeid(sdf, grunn_annet_arbeid)
  sdf <- set_forventning(sdf, forventning_arbeidsmarked)
  sdf <- set_sektor(sdf, sektor)
  sdf <- set_fylke(sdf, arbeidssted)

  sdf <- set_heltidsstilling(sdf, stillingsprosent)
  sdf <- set_ufrivilligdeltid(sdf, grunn_redusert_stilling)  
  sdf <- set_grunn_redusert_stilling(sdf, grunn_redusert_stilling)
  
  sdf <- OM_janei_bin(sdf, flere_arbeidsgivere)
  
  sdf <- set_tidtiljobbdager(sdf, tid_til_relevant_arbeid)
  sdf <- set_jobbunderveis(sdf, tid_til_relevant_arbeid)
  
  sdf <- set_fornoydmedoppgaver(sdf, fornoyd_oppgaver)
  sdf <- set_forberedtforoppgaver(sdf, forberedt_oppgaver)
  
  sdf <- set_kompetanse_tverrprofesjonelt(sdf, kompetanse_tverrprofesjonelt)
  
  sdf <- set_sammeutdanning(sdf, valgt_samme_utdanning)
  sdf <- set_sammeinstitusjon(sdf, valgt_samme_institusjon)
  
  sdf <- OM_janei_bin(sdf, aktiv_organisasjon)
  sdf <- OM_janei_bin(sdf, aktiv_idrettslag)
  sdf <- OM_janei_bin(sdf, aktiv_studentforening)
  sdf <- OM_janei_bin(sdf, aktiv_ideell)
  sdf <- OM_janei_bin(sdf, aktiv_politisk)
  sdf <- OM_janei_bin(sdf, aktiv_ikke_svar)
  sdf <- OM_janei_bin(sdf, verv_klassetillitsvalgt)
  sdf <- OM_janei_bin(sdf, verv_studentforening)
  sdf <- OM_janei_bin(sdf, verv_studentrepresentant)
  sdf <- OM_janei_bin(sdf, verv_studentparlamentet)
  sdf <- OM_janei_bin(sdf, verv_ingen)
  
  sdf <- OM_janei_bin(sdf, arbeidsgiver_tilrettelegger_videreutdanning)
  sdf <- OM_janei_bin(sdf, arbeidsgiver_finansierer_videreutdanning)
  
  sdf <- set_kjonn(sdf) 
  sdf <- set_dinalder(sdf)
  
  sdf <- sdf %>% mutate(brutto_arslonn_vasket = brutto_arslonn)
  sdf$brutto_arslonn_vasket[sdf$arbeider_utdannet_til != 1] <- NA 
  sdf$brutto_arslonn_vasket[is.na(sdf$arbeider_utdannet_til)] <- NA 
  sdf$brutto_arslonn_vasket[sdf$brutto_arslonn < 300000] <- NA 
  sdf$brutto_arslonn_vasket[sdf$brutto_arslonn > 1000000] <- NA 
  
  return(sdf)
}

##** Slår saman excel-filer i gitt mappe, lagrar ny excelfil
OM_compile_xlsx <- function(path, filename, surveyyear) {
  filer <- dir(path, full.names = T)
  
  sdf <- lapply(filer, read_excel, col_types = "text")
  sdf <- suppressWarnings(lapply(sdf, OM_set_undersokelse_ar, year = surveyyear))
  
  sdf <- bind_rows(sdf)
  
  write.xlsx(sdf, filename)
  return(sdf)
}

# TODO lag eiga fil med programdata for å slå saman
# Slår saman datafiler og gjer ein del omforming
OM_compile_kandidat_data_2022 <- function(path, surveyyear, print = F, filename = NULL) {
  filer <- dir(path, full.names = T)
  
  sdf <- lapply(filer, read_excel, col_types = "text")
  sdf <- suppressWarnings(lapply(sdf, OM_set_undersokelse_ar, year = surveyyear))
  sdf <- lapply(sdf, OM_clean_kandidat_2022)
  
  sdf <- bind_rows(sdf)
  sdf <- OM_clean_kandidat_serie(sdf)
  
  # bytt ut denne med join på excelfil med programkode, program-, institutt- og fakultetsnamn
  # Hentar programnamn frå dbh
  sdf <- sdf %>% dbh_add_programdata("fs_kode", 1175)
  # TODO pass på programma som ikkje blir fanga opp med rett fakultet (2019)
  # Gruppere år i to
  sdf <- sdf %>% mutate(gruppe_ar = ifelse(undersokelse_ar < 2022, 2019, undersokelse_ar))
  
  # legg til programnamn med instituttilhøyrigheit
  OM_programvar <- read_excel("base/OsloMet_programvariabler.xlsx")
  sdf <- left_join(sdf, OM_programvar, "Studieprogramkode")
  
  # Lagar variabel for Studium_ar, Fakultet_ar og OM_ar
  # for å kunne gruppere før utskrift, slik at data kjem på ei linje per år
  sdf <- sdf %>% mutate(Studium_ar = paste(Studieprogramnavn, gruppe_ar))
  sdf <- sdf %>% mutate(StudiumID_ar = paste(StudiumID, gruppe_ar))
  sdf <- sdf %>% mutate(Studieprogram_instnr_ar = paste(Studieprogram_instnr, gruppe_ar))
  sdf <- sdf %>% mutate(Fakultet_ar = paste(Fakultetsnavn, gruppe_ar))
  sdf <- sdf %>% mutate(OM_ar = paste("OsloMet", gruppe_ar))
  sdf <- sdf %>% mutate(Institusjon = "OsloMet")
  
  missing_fakultet <- sdf %>% filter(is.na(Fakultetsnavn)) %>% select(Studieprogramkode, undersokelse_ar)
  if (nrow(missing_fakultet) > 0) {
    print("Program som ikkje er kopla til fakultet:")
    print(missing_fakultet)
  }

  sdf <- OM_kandidat_setup(sdf)
  
  if (print) {
    write.xlsx(sdf, filename)
  }
  return(sdf)
}

OM_clean_kandidat_serie <- function(sdf) {
  sdf[sdf == "N/A"] <- NA
  sdf$siste_side <- sdf$siste_side %>% type.convert("number", as.is = T)
  sdf$fullfort_ar <- sdf$fullfort_ar %>% type.convert("number", as.is = T)
  sdf$undersokelse_ar <- sdf$undersokelse_ar %>% type.convert("number", as.is = T)
  sdf$brutto_arslonn <- sdf$brutto_arslonn %>% type.convert("number", as.is = T)

  # Handterer at det i 2019 vart lagra som fullførtdato i dd.mm.åååå
  sdf <- sdf %>% mutate(fullfort_dato = str_sub(fullfort_dato, -4))
  sdf <- sdf %>% mutate(fullfort_ar = coalesce(fullfort_ar, as.numeric(fullfort_dato)))
  
  # Fjernar svar frå kandidatar der det er over tre år sidan dei fullførte, 
  # for å gi betre samanlikning mellom dei to gruppene
  sdf <- sdf %>% mutate(ar_fra_ferdig = undersokelse_ar - fullfort_ar)
  sdf <- sdf %>% filter(ar_fra_ferdig < 4)
  
  return(sdf)
}

OM_set_undersokelse_ar <- function(sdf, year) {
  if (is.null(sdf$undersokelse_ar)) {
    sdf <- sdf %>% mutate(undersokelse_ar = year)
  }
  # print(sdf$undersokelse_ar %>% unique())
  return(sdf)
}

OM_clean_kandidat_2022 <- function(sdf) {
  if (!is.null(sdf$etablert_selvstendig_ar)){
    sdf$etablert_selvstendig_ar <- sdf$etablert_selvstendig_ar %>% as.character()
  }  
  
  if (suppressWarnings(!is.null(sdf$`Hvilket år etablerte du deg som selvstendig næringsdrivende?`))) {
    sdf$`Hvilket år etablerte du deg som selvstendig næringsdrivende?` <- sdf$`Hvilket år etablerte du deg som selvstendig næringsdrivende?` %>% as.character()
  }
  
  if (!(2018 %in% sdf$undersokelse_ar %>% unique() | 
        2019 %in% sdf$undersokelse_ar %>% unique()) ) {
    return(sdf)
  }  
  
  if (2018 %in% sdf$undersokelse_ar %>% unique()) {
    sdf <- OM_sektor_kandidat2018(sdf)
    sdf$dato_svar <- sdf$dato_svar %>% as.character()
  }
  
  sdf <- sdf %>% OM_fakultetsnummer_til_fork()
  sdf$stillingsprosent[sdf$stillingsprosent == "1"] <- "100%"
  sdf$stillingsprosent[sdf$stillingsprosent == "0.5"] <- "50%"
  sdf$sammenlagt_stilling[sdf$sammenlagt_stilling == "0.5"] <- "50%"
  sdf$sammenlagt_stilling[sdf$sammenlagt_stilling == "0.6"] <- "60%"
  sdf$sammenlagt_stilling[sdf$sammenlagt_stilling == "0.8"] <- "80%"
  sdf$sammenlagt_stilling[sdf$sammenlagt_stilling == "1"] <- "100%"
  return(sdf)
}

OM_fakultetsnummer_til_fork <- function(sdf) {
  sdf <- sdf %>% mutate(fakultet = case_when(
    fakultet == 13 ~ "HV",
    fakultet == 14 ~ "LUI",
    fakultet == 15 ~ "SAM",
    fakultet == 16 ~ "TKD"
  ))
  
  return(sdf)
} 

OM_sektor_kandidat2018 <- function(sdf) {
  #   NA  "Fylkeskommunal sektor" 
  sdf$sektor[sdf$pre_sektor_privat == "Ja"] <- "Privat sektor"
  sdf$sektor[sdf$pre_sektor_statlig == "Ja"] <- "Statlig sektor"
  sdf$sektor[sdf$pre_sektor_kommunal == "Ja"] <- "Kommunal sektor"
  sdf$sektor[sdf$pre_sektor_frivillig == "Ja"] <- "Frivillig sektor"
  sdf$sektor[sdf$pre_sektor_vet_ikke == "Ja"] <- "Vet ikke"
  
  return(sdf)
}

# lagar ny variabel tidfrafullfort, må lagrast til dataframe med <-
set_tid_fra_fullf <- function(sdf) {
  sdf %>% mutate(tidfrafullfort = case_when(
    (undersøkelse_år == 2016 & årskull == 2015) ~ 1,
    (undersøkelse_år == 2020 & årskull == 2019) ~ 1,
    (undersøkelse_år == 2016 & årskull == 2014) ~ 2,
    (undersøkelse_år == 2020 & årskull == 2018) ~ 2,
    (undersøkelse_år == 2016 & årskull == 2013) ~ 3,
    (undersøkelse_år == 2020 & årskull == 2017) ~ 3,
    (undersøkelse_år == 2016 & årskull == 2012) ~ 4,
    (undersøkelse_år == 2020 & årskull == 2016) ~ 4,
    (undersøkelse_år == 2016 & årskull == 2011) ~ 5
  )
  )
}

set_sektor <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Stat" | {{variabel}} == "Statlig sektor" ~ "Statlig",               
    {{variabel}} == "Fylkeskommune" | 
      {{variabel}} == "Fylkeskommunal sektor" ~ "Fylkeskommunal",
    {{variabel}} == "Kommune" | 
      {{variabel}} == "Kommunal sektor" ~ "Kommunal",
    {{variabel}} == "Privat sektor" | {{variabel}} == "Privat virksomhet" ~ "Privat",
    {{variabel}} == "Frivillig (ideell) organisasjon" | 
      {{variabel}} == "Frivillig sektor" ~ "Frivillig"
  )) 
  sdf$sektor <- sdf$sektor %>% as.factor
  return(sdf)
}

# Lagar variabel covid
set_covid <- function(sdf) {
  sdf <- sdf %>% mutate(covid = case_when(
    Innledningsvis.ønsker.vi.å.høre.om.stillingen.din.på.arbeidsmarkedet.har.blitt.endret.som.følge.av.koronapandemien. == 
      "Ja, stillingen min på arbeidsmarkedet har blitt endret som følge av pandemien" ~ 1,
    Innledningsvis.ønsker.vi.å.høre.om.stillingen.din.på.arbeidsmarkedet.har.blitt.endret.som.følge.av.koronapandemien. == 
      "Nei, stillingen min på arbeidsmarkedet har ikke blitt endret som følge av pandemien" ~ 0
  ))
  return(sdf)
}

# Lagar variablar for fylke
set_fylke <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(oslo = case_when(
    {{variabel}} == "Oslo" ~ 1,
    {{variabel}} != "" ~ 0
  ))
  
  sdf <- sdf %>% mutate(viken = case_when(
    {{variabel}} == "Viken" ~ 1,
    {{variabel}} != "" ~ 0
  ))
  
  sdf <- sdf %>% mutate(osloviken = case_when(
    {{variabel}} == "Oslo" |
      {{variabel}} == "Viken" ~ 1,
    {{variabel}} != "" ~ 0
  ))
  return(sdf)
}

# OBS - Stor variasjon i svar mellom år, har oppdatert a5_hovedaktivitet til å få med alle alternativ i 2018+2019+2022
# # Endrar variabel for hovedaktivitet
# set_hovedaktivitet <- function(sdf) {
#   sdf <- sdf %>% mutate(hovedaktivitet = case_when(
#     hovedaktivitet == "Ansatt i fast stilling (inkl. prøvetid)" | hovedaktivitet == "Ansatt i fast stilling" ~ "Fast stilling",
#     hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet mer enn 6 måneder" |
#       hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet 6 måneder eller mer" ~ "Midlertidig, lang",
#     hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet mindre enn 6 måneder" | 
#       hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med kortere varighet enn 6 måneder" ~ "Midlertidig, kort",
#     hovedaktivitet == "Selvstendig næringsdrivende"  ~ "Selvstendig næringsdrivende",
#     hovedaktivitet == "Arbeidssøkende" ~ "Arbeidssøkende",
#     hovedaktivitet == "Annet" ~ "Annet"
#   )) 
#   return(sdf)
# }

# OBS - Stor variasjon i svar mellom år, har oppdatert a5_hovedaktivitet til å få med alle alternativ i 2018+2019+2022
# # Lagar variabel for hovedaktivitet, variant
# set_a6_hovedaktivitet <- function(sdf) {
#   sdf <- sdf %>% mutate(a6_hovedaktivitet = case_when(
#     Hva.er.din.hovedbeskjeftigelse. == "Ansatt i fast stilling (inkl. prøvetid)" | Hva.er.din.hovedbeskjeftigelse. == "Ansatt i fast stilling" ~ "1-Fast stilling",
#     Hva.er.din.hovedbeskjeftigelse. == "Ansatt i vikariat eller annen midlertidig stilling med varighet mer enn 6 måneder" | 
#       Hva.er.din.hovedbeskjeftigelse. == "Ansatt i vikariat eller annen midlertidig stilling med varighet 6 måneder eller mer" ~ "2-Midlertidig stilling, lang",
#     Hva.er.din.hovedbeskjeftigelse. == "Ansatt i vikariat eller annen midlertidig stilling med varighet mindre enn 6 måneder" | 
#       Hva.er.din.hovedbeskjeftigelse. == "Ansatt i vikariat eller annen midlertidig stilling med kortere varighet enn 6 måneder" ~ "3-Midlertidig stilling, kort",
#     Hva.er.din.hovedbeskjeftigelse. == "Selvstendig næringsdrivende" ~ "4-Selvstendig næringsdrivende",
#     Hva.er.din.hovedbeskjeftigelse. == "Arbeidssøkende" ~ "5-Arbeidssøkende",
#     Hva.er.din.hovedbeskjeftigelse. == "Annet" ~ "6-Annet" 						
#   )) 
#   return(sdf)
# }

# Lagar variabel for hovedaktivitet, variant
# TODO vurder å gjere om til integer med label
set_a5_hovedaktivitet <- function(sdf) {
  sdf <- sdf %>% mutate(a5_hovedaktivitet = case_when(
    hovedaktivitet == "Ansatt i fast stilling (inkl. prøvetid)" | hovedaktivitet == "Ansatt i fast stilling" ~ "1-Fast stilling",
    hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet mer enn 6 måneder" | 
      hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet 6 måneder eller mer" |
      hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet mindre enn 6 måneder" | 
      hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med kortere varighet enn 6 måneder" 
    | 
      hovedaktivitet == "Midlertidig" |
      hovedaktivitet == "Ansatt i vikariat / midlertidig stilling med varighet 6 måneder eller mer" |
      hovedaktivitet == "Ansatt i vikariat / midlertidig stilling med varighet mindre enn 6 måneder" | 
      hovedaktivitet == "Ansatt i vikariat / midlertidig stilling med kortere varighet enn 6 måneder" ~ "2-Midlertidig stilling",
    hovedaktivitet == "Selvstendig næringsdrivende" ~ "3-Selvstendig næringsdrivende",
    hovedaktivitet == "Arbeidssøkende"	~ "4-Arbeidssøkende",
    hovedaktivitet == "Annet" ~ "5-Annet"
  )) 
  return(sdf)
}

# Lagar variabel mann = 0 om kvinne, lik 1 om mann
set_kjonn <- function(sdf) {
  sdf <- sdf %>% mutate(mann = case_when(
    kjonn == "K"	|	kjonn == "Kvinne" ~ 0,
    kjonn == "M"	|	kjonn == "Mann" ~ 1,
    kjonn == "Annet" ~ NaN
  ))
  return(sdf)
}

# Lagar variabel dinalder
set_dinalder <- function(sdf) {
  sdf <- sdf %>% mutate(alder_int = case_when(
    # 2022-variant
    alder == "Yngre enn 25 år" ~ 24,
    alder == "25-27 år" ~ 26,
    alder == "28-30 år" ~ 29,
    alder == "31-33 år" ~ 32,
    alder == "34-36 år" ~ 35,
    alder == "37-39 år" ~ 38,
    alder == "40-42 år" ~ 41,
    alder == "43-45 år" ~ 44,
    alder == "Eldre enn 45 år" ~ 46,
    alder == "Ønsker ikke å oppgi alder" ~ NaN,
    
    # eldre variantar
    alder == "Yngre enn 26 år" ~ 25,
    alder == "26-28 år" ~ 27,
    alder == "29-31 år" ~ 30,
    alder == "32-34 år" ~ 33,
    alder == "35-37 år" ~ 36,
    alder == "38-40 år" ~ 39,
    alder == "Eldre enn 40 år" ~ 41,
    alder == "1" ~ 22,
    alder == "2" ~ 24,
    alder == "3" ~ 27,
    alder == "4" ~ 30,
    alder == "5" ~ 33,
    alder == "6" ~ 36,
    alder == "7" ~ 39,
    alder == "8" ~ 42
  ))
  return(sdf)
}

# Lagar variabel relevantforoppgaver - ny skala
set_relevantforoppgaver <- function(sdf) {
  sdf <- sdf %>% mutate(relevantarbeid = case_when(
    Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "Ikke relevant" | 
      Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "1 - Ikke relevant" ~ "1 - Ikke relevant",
    Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "2" ~ "2",
    Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "Delvis relevant" ~ "3",
    Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "3" ~ "3",
    Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "Relevant" ~ "4",
    Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "4" ~ "4",
    Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "Svært relevant" | 
      Hvor.relevant.er.masterutdanningen.for.oppgavene.du.utfører.i.stillingen.din. == "5 - Svært relevant" ~ "5 - Svært relevant"  
  ))
  return(sdf)
}

# Lagar variabel stillingsbrøk
set_stillingsbrøk <- function(sdf) {
  sdf <- sdf %>% mutate(stillingsbrøk = case_when(
    Hvilket.alternativ.passer.best.til.stillingsbrøken.i.din.arbeidskontrakt...Legg.sammen.hvis.du.har.flere.arbeidsgivere. == "100 %" ~ 1,
    Hvilket.alternativ.passer.best.til.stillingsbrøken.i.din.arbeidskontrakt...Legg.sammen.hvis.du.har.flere.arbeidsgivere. == "90 %" ~ 0.9,
    Hvilket.alternativ.passer.best.til.stillingsbrøken.i.din.arbeidskontrakt...Legg.sammen.hvis.du.har.flere.arbeidsgivere. == "80 %" ~ 0.8,
    Hvilket.alternativ.passer.best.til.stillingsbrøken.i.din.arbeidskontrakt...Legg.sammen.hvis.du.har.flere.arbeidsgivere. == "70 %" ~ 0.7,
    Hvilket.alternativ.passer.best.til.stillingsbrøken.i.din.arbeidskontrakt...Legg.sammen.hvis.du.har.flere.arbeidsgivere. == "60 %" ~ 0.6,
    Hvilket.alternativ.passer.best.til.stillingsbrøken.i.din.arbeidskontrakt...Legg.sammen.hvis.du.har.flere.arbeidsgivere. == "Mindre enn 50%" ~ 0.45
  ))
  return(sdf)
}

# Lagar variabel heltidsstilling
set_heltidsstilling <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(heltidsstilling = case_when(
    {{variabel}} == "100%" ~ 1,
    (!is.na({{variabel}}) & {{variabel}} != "Vet ikke") ~ 0,
    {{variabel}} == "Vet ikke" ~ NaN
  ))
  return(sdf)
}

# Lagar variabel ufrivilligdeltid
set_ufrivilligdeltid <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(ufrivilligdeltid = case_when(
    {{variabel}} == "Jeg ønsker å jobbe i full stilling, men min arbeidsgiver kan/vil ikke tilby meg det" ~ 1,
    {{variabel}} == "Jeg er fornøyd med den stillingsbrøken jeg har i dag" | 
      {{variabel}} == "Jeg ønsker å jobbe mer enn i dag, men ikke i full stilling" ~ 0,
    {{variabel}} == "Vet ikke/ønsker ikke å svare" |
      {{variabel}} == "Vet ikke / ønsker ikke å svare" ~ NaN
  ))
  return(sdf)
}

# Kodar om oppfølgingsspørsmål til grunn_redusert_stilling
# TODO vurder å gjere om til integer med label
set_grunn_redusert_stilling <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Jeg er fornøyd med den stillingsbrøken jeg har i dag" ~ "Fornøyd med stillingsbrøken", 
    {{variabel}} == "Jeg ønsker å jobbe i full stilling, men min arbeidsgiver kan/vil ikke tilby meg det"  ~ "Arbeidsgiver kan/vil ikke tilby mer",
    {{variabel}} == "Jeg ønsker å jobbe mer enn i dag, men ikke i full stilling" ~ "Ønsker større, men ikke full, stilling",
    {{variabel}} == "Vet ikke / ønsker ikke å svare" | 
      {{variabel}} == "Vet ikke/ønsker ikke å svare" ~ "Vet ikke/ønsker ikke å svare" 
  ))
  return(sdf)
}

# Lagar variabel tidjobbtilbdager
set_tidtiljobbdager <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(dager_til_jobb = case_when(
    {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå var masterstudent" |
      {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte" |
      {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte/var i turnustjeneste" ~ 0,
    {{variabel}} == "Mindre enn en måned etter fullført utdanning" |
    {{variabel}} == "Mindre enn en måned etter fullført utdanning/turnustjeneste" |
    {{variabel}} == "Fikk tilbud om relevant jobb mindre enn en måned etter fullført masterutdanning" ~ 15,
    {{variabel}} == "1-2 måneder" ~ 46,
    {{variabel}} == "3-4 måneder" ~ 107,
    {{variabel}} == "5-6 måneder" ~ 168,
    {{variabel}} == "7-12 måneder" ~ 290,
    {{variabel}} == "13-18 måneder" ~ 473,
    {{variabel}} == "19-24 måneder" ~ 656,
    {{variabel}} == "Mer enn to år" ~ 765,
    {{variabel}} == "Husker ikke" ~ NaN
  ))
}

# Lagar variabel haddejobbunderveis
set_jobbunderveis <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(hadde_jobb_underveis = case_when(
    {{variabel}} == "Hadde en relevant jobb allerede før jeg ble masterstudent" | 
    {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte" | 
      {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte/var i turnustjeneste" ~ 1, 
    {{variabel}} != "" ~ 0
  ))
  return(sdf)
}

# Lagar variabel forventning
set_forventning <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Det har vært færre relevante stillinger å søke på / mer konkurranse om stillingene enn jeg forventet" ~ -1,
    {{variabel}} == "Det har vært omtrent så mange relevante stillinger / så mye konkurranse om stillingene som jeg forventet" ~ 0,
    {{variabel}} == "Det har vært flere relevante stillinger å søke på / mindre konkurranse om stillingene enn jeg forventet" ~ 1,
    {{variabel}} == "Jeg hadde ingen klare forventninger på forhånd" | 
      {{variabel}} == "Vet ikke" ~ NaN
  ))
  return(sdf)
}

# Lagar variabel skiftetstilling
set_skiftetstilling <- function(sdf) {
  sdf <- rename(sdf, harduskiftetstillingitidenettera = 
                        Har.du.skiftet.stilling.i.tiden.etter.at.du.fullførte.masterutdanningen.)
  sdf <- sdf %>% mutate(skiftetstilling = case_when(
    harduskiftetstillingitidenettera=="Ja" ~ 1,
    harduskiftetstillingitidenettera=="Nei" ~ 0
  ))
  return(sdf)
}

# Lagar variabel nyeoppgaver
set_nyeoppgaver <- function(sdf) {
  sdf <- sdf %>% mutate(nyeoppgaver = case_when(
    Har.du.fått.nye.arbeidsoppgaver.eller.mer.ansvar.som.følge.av.at.du.fikk.mastergraden. == "Ja" ~ 1,
    Har.du.fått.nye.arbeidsoppgaver.eller.mer.ansvar.som.følge.av.at.du.fikk.mastergraden. == "Nei" ~ 0
    
  ))
  return(sdf)
}


# Lagar variabel nødvendigmaster
set_nødvendigmaster <- function(sdf) {
  sdf <- sdf %>% mutate(nødvendigmaster = case_when(
    Var.utdanning.på.masternivå.en.forutsetning.for.at.du.fikk.jobben.du.har.nå. == 
      "Nei, masterutdanningen var uten betydning." ~ 1,
    Var.utdanning.på.masternivå.en.forutsetning.for.at.du.fikk.jobben.du.har.nå. ==
      "Masterutdanningen gjorde meg bedre kvalifisert enn søkere med kun bachelorutdanning." ~ 2,
    Var.utdanning.på.masternivå.en.forutsetning.for.at.du.fikk.jobben.du.har.nå. ==
      "Ja, masterutdanningen var en forutsetning." ~ 3,
  ))
  sdf$nødvendigmaster <- label_nødvendigmaster(sdf$nødvendigmaster)
  return(sdf)
}

# Set labels på nødvendigmaster
label_nødvendigmaster <- function(sdf) {
  factor(sdf,
         levels = c(1, 2, 3),
         labels = c("Nei, masterutdanningen var uten betydning", 
                    "Masterutdanningen gjorde meg bedre kvalifisert enn søkere med kun bachelorutdanning", 
                    "Ja, masterutdanningen var en forutsetning"))
}

# Kodar om oppfølgingsspørsmål til arbeider_utdannet_til
# TODO vurder å gjere om til integer med label
set_grunn_annet_arbeid <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Har søkt på jobber som krever masterutdanning, men foreløpig uten hell" |
      {{variabel}} == "Har søkt på jobber jeg er utdannet for, men foreløpig uten hell" |
      {{variabel}} == "Har søkt på jobber jeg er utdannet til, men foreløpig uten hell" ~ "Har søkt uten hell", 
    {{variabel}} == "Har en jobb som passer meg like godt eller bedre." | 
      {{variabel}} == "Har fått en jobb som passer meg like godt eller bedre enn slike jobber som jeg er utdannet for" | 
      {{variabel}} == "Har fått en jobb som passer meg like godt eller bedre enn slike jobber som jeg er utdannet til"  ~ "Har fått jobb som passer meg like godt",
    {{variabel}} == "Prøvde meg en tid i en jobb som krevde masterutdanning, men fant ut at jeg ikke trivdes." |
      {{variabel}} == "Prøvde meg en tid i en jobb jeg er utdannet til, men fant ut at jeg ikke trivdes med det" |
      {{variabel}} == "Prøvde meg en tid i en slik jobb jeg er utdannet for, men fant ut at jeg ikke trivdes med det" |
      {{variabel}} == "Prøvde meg en tid i en slik jobb jeg er utdannet til, men fant ut at jeg ikke trivdes med det" ~ "Har prøvd, men trivdes ikke",
    {{variabel}} == "Annen grunn" | 
      {{variabel}} == "Annet" ~ "Annen grunn" 
  ))
  return(sdf)
}

# Lagar variabel hvorforikkeutdannetfor
# Eldre versjon, spørsmålstekst endra
set_hvorforikkeutdannetfor <- function(sdf) {
  sdf <- sdf %>% mutate(hvorforikkeutdannetfor = case_when(
    Hva.er.den.viktigste.grunnen.til.at.du.har.en.jobb.der.masterutdanning.ikke.har.betydning. == "Har søkt på jobber som krever masterutdanning, men foreløpig uten hell" ~ "Har søkt uten hell", 
    Hva.er.den.viktigste.grunnen.til.at.du.har.en.jobb.der.masterutdanning.ikke.har.betydning. == "Har en jobb som passer meg like godt eller bedre." ~ "Har fått jobb som passer meg like godt",
    Hva.er.den.viktigste.grunnen.til.at.du.har.en.jobb.der.masterutdanning.ikke.har.betydning. == "Prøvde meg en tid i en jobb som krevde masterutdanning, men fant ut at jeg ikke trivdes." ~ "Har prøvd, men trivdes ikke",
    Hva.er.den.viktigste.grunnen.til.at.du.har.en.jobb.der.masterutdanning.ikke.har.betydning. == "Annen grunn" ~ "Annen grunn"    
  ))
  return(sdf)
}

# TODO - desse kan generaliserast ein del, med unntakk av dei som må samanlikne med firedelt skala

# Lagar variabel fornøydmedoppgaver
set_fornoydmedoppgaver <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Ikke fornøyd" |
      {{variabel}} == "1 - Ikke fornøyd" |
      {{variabel}} == "1 - Helt uenig" ~ 1,
    {{variabel}} == "Delvis fornøyd" | 
      {{variabel}} == "2" ~ 2,
    {{variabel}} == "3" ~ 3,
    {{variabel}} == "Fornøyd" | 
      {{variabel}} == "4" ~ 4,
    {{variabel}} == "Svært fornøyd" | 
      {{variabel}} == "5 - Svært fornøyd" ~ 5,
      {{variabel}} == "5 - Helt enig" ~ 5,
      {{variabel}} == "Vet ikke" ~ NaN
  ))
  # Gjer om variabel til tekst for å kunne gjere om til factor med labels
  var_string <- deparse(substitute(variabel))
  sdf[[var_string]] <- label_fornoydmedoppgaver(sdf[[var_string]])
  return(sdf)
}

# Set labels på fornøydmedoppgaver
label_fornoydmedoppgaver <- function(sdf) {
  factor(sdf,
         levels = c(1, 2, 3, 4, 5),
         labels = c("1 - Helt uenig", "2", "3", "4", "5 - Helt enig"))
}

# Lagar variabel forberedtforoppgaver
set_forberedtforoppgaver <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Ikke godt" | 
      {{variabel}} == "1 - Ikke godt" |
      {{variabel}} == "1 - Helt uenig" ~ 1,
    {{variabel}} == "Delvis godt" | 
      {{variabel}} == "2" ~ 2,
    {{variabel}} == "3" ~ 3,
    {{variabel}} == "Godt" | 
      {{variabel}} == "4" ~ 4,
    {{variabel}} == "Svært godt" | 
      {{variabel}} == "5 - Svært godt" |
      {{variabel}} == "5 - Helt enig" ~ 5,
      {{variabel}} == "Vet ikke" ~ NaN
  ))
  # Gjer om variabel til tekst for å kunne gjere om til factor med labels
  var_string <- deparse(substitute(variabel))
  sdf[[var_string]] <- label_forberedtforoppgaver(sdf[[var_string]])
  
  # sdf$forberedtforoppgaver <- label_forberedtforoppgaver(sdf$forberedtforoppgaver)
  return(sdf)
}

# Set labels på forberedtforoppgaver
label_forberedtforoppgaver <- function(sdf) {
  factor(sdf,
         levels = c(1, 2, 3, 4, 5),
         labels = c("1 - Helt uenig", "2", "3", "4", "5 - Helt enig"))
}

# Lagar variabel kompetanse_tverrprofesjonelt
set_kompetanse_tverrprofesjonelt <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
      {{variabel}} == "1 - Helt uenig" ~ 1,
      {{variabel}} == "2" ~ 2,
    {{variabel}} == "3" ~ 3,
      {{variabel}} == "4" ~ 4,
      {{variabel}} == "5 - Helt enig" ~ 5,
    {{variabel}} == "Vet ikke" ~ NaN
  ))
  # Gjer om variabel til tekst for å kunne gjere om til factor med labels
  var_string <- deparse(substitute(variabel))
  sdf[[var_string]] <- label_kompetanse_tverrprofesjonelt(sdf[[var_string]])
  
  return(sdf)
}

# Set labels på kompetanse_tverrprofesjonelt
label_kompetanse_tverrprofesjonelt <- function(sdf) {
  factor(sdf,
         levels = c(1, 2, 3, 4, 5),
         labels = c("1 - Helt uenig", "2", "3", "4", "5 - Helt enig"))
}

# Lagar variabel sammeutdanning
set_sammeutdanning <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Svært lite sannsynlig" ~ 1,
    {{variabel}} == "Lite sannsynlig"       ~ 2,
    {{variabel}} == "Usikker"               ~ 3,
    {{variabel}} == "Sannsynlig"            ~ 4,
    {{variabel}} == "Svært sannsynlig"      ~ 5,
    {{variabel}} == "Vet ikke"      ~ NaN
  ))
  
  # Gjer om variabel til tekst for å kunne gjere om til factor med labels
  var_string <- deparse(substitute(variabel))
  sdf[[var_string]] <- label_sammeutdanning(sdf[[var_string]])
  return(sdf)
}

# Set labels på sammeutdanning
label_sammeutdanning <- function(sdf) {
  factor(sdf,
         levels = c(1, 2, 3, 4, 5),
         labels = c("1 - Svært lite sannsynlig", "2", "3", "4", "5 - Svært sannsynlig"))
}

# Lagar variabel sammelærested
# TODO endre valgt_samme_lærested til valgt_samme_institusjon i excel-filene
set_sammeinstitusjon <- function(sdf, variabel) {
  sdf <- sdf %>% mutate({{variabel}} := case_when(
    {{variabel}} == "Svært lite sannsynlig"     ~ 1,
    {{variabel}} == "Lite sannsynlig"           ~ 2,
    {{variabel}} == "Usikker"                   ~ 3,
    {{variabel}} == "Sannsynlig"                ~ 4,
    {{variabel}} == "Svært sannsynlig"          ~ 5,
    {{variabel}} == "Vet ikke"          ~ NaN     
  ))
  # Gjer om variabel til tekst for å kunne gjere om til factor med labels
  # TODO kan vere funksjonar for ulike skalaar, slik at alle med sannsynleg / einig-skala går same stad
  var_string <- deparse(substitute(variabel))
  sdf[[var_string]] <- label_sammeinstitusjon(sdf[[var_string]])
  return(sdf)
}

# Set labels på sammeutdanning
label_sammeinstitusjon <- function(sdf) {
  factor(sdf,
         levels = c(1, 2, 3, 4, 5),
         labels = c("1 - Svært lite sannsynlig", "2", "3", "4", "5 - Svært sannsynlig"))
}

# Set labels på fakultetsnummer
# Usikker på kva nytte denne har no
label_fakultet <- function(sdf) {
  factor(sdf,
         levels = c(13, 14, 15, 16),
         labels = c("HV", "LUI", "SAM", "TKD"))
}

##** Slutt Kandidatundersøkinga

# TODO - ikkje bruke denne - den finst berre hos oss. 
# DBH-variabel har forkorting i parantes, prøv å hent ut den
SB_name_fak_kort <- function(sdf) {
  sdf %>% mutate(fakultet = case_when(
    FAKNAVN == "Fakultet for samfunnsvitenskap" ~ "SAM",
    FAKNAVN == "Fakultet for teknologi, kunst og design" ~ "TKD",
    FAKNAVN == "Fakultet for lærerutdanning og internasjonale studier" ~ "LUI",
    FAKNAVN == "Fakultet for helsefag" ~ "HV",
    FAKNAVN == "Fakultet for helsevitenskap" ~ "HV",
  ))
}

##** 
##* Omkoding Sisteårs
##* 

# Kodar om til faktor for å sikre at alle nivå blir med i utskrift
OM_set_faktor <- function(sdf, svar) {
  sdf <- sdf %>% mutate({{svar}} := as.factor({{svar}}))
  return(sdf)
}

##** Kodar om ja/nei/usikker til binærvariabel
##*  Ja = 1
##*  Nei = 0
##*  Usikker = NaN
##*  sdf = dataframe
##*  svar = variabelen som skal tolkast
##*  nyvar = variabelen som blir laga
OM_janei_bin <- function(sdf, svar) {
  if (!(deparse(substitute(svar)) %in% colnames(sdf))) {
    sdf <- sdf %>% mutate({{svar}} := NaN)
    return(sdf)
  }
  sdf <- sdf %>% mutate({{svar}} := case_when(
    grepl("Ja", {{svar}}) ~ 1,
    grepl("Yes", {{svar}}) ~ 1,
    grepl("Nei", {{svar}}) ~ 0,
    grepl("No", {{svar}}) ~ 0,
    grepl("Vet ikke", {{svar}}) ~ NaN,
    grepl("Do not know", {{svar}}) ~ NaN,
    grepl("Usikker", {{svar}}) ~ NaN,
    grepl("Uncertain", {{svar}}) ~ NaN
  ))
}

##** Kodar om ja/nei/usikker til binærvariabel, lagar ny variabel, bra for testing
##*  Ja = 1
##*  Nei = 0
##*  Usikker = NaN
##*  sdf = dataframe
##*  svar = variabelen som skal tolkast
##*  nyvar = variabelen som blir laga
OM_janei_bin_nyvar <- function(sdf, nyvar, svar) {
  sdf %>% mutate({{nyvar}} := case_when(
    grepl("Ja", {{svar}}) ~ 1,
    grepl("Yes", {{svar}}) ~ 1,
    grepl("Nei", {{svar}}) ~ 0,
    grepl("No", {{svar}}) ~ 0,
    grepl("Vet ikke", {{svar}}) ~ NaN,
    grepl("Do not know", {{svar}}) ~ NaN,
    grepl("Usikker", {{svar}}) ~ NaN,
    grepl("Uncertain", {{svar}}) ~ NaN
  ))
}

# Kodar om frå tekst-"ordinal" til tal-nivå
SA_text_to_numerallevels <- function(sdf, originalvar) {
  if (!(deparse(substitute(originalvar)) %in% colnames(sdf))) {
    sdf <- sdf %>% mutate({{originalvar}} := NaN)
    return(sdf)
  }
  sdf %>% mutate({{originalvar}} := case_when(
    {{originalvar}} == "1- Ikke enig" ~ 1,
    {{originalvar}} == "1 - Ikke enig" ~ 1,
    {{originalvar}} == "1 - Disagree" ~ 1,
    {{originalvar}} == "1 - Ikke fornøyd" ~ 1,
    {{originalvar}} == "1 - Not satisfied" ~ 1,
    {{originalvar}} == "1 - I liten grad" ~ 1,
    {{originalvar}} == "1 - Ikke relevant" ~ 1,
    {{originalvar}} == "1 - Ikke godt" ~ 1,
    {{originalvar}} == "2" ~ 2,
    {{originalvar}} == "3" ~ 3,
    {{originalvar}} == "4" ~ 4,
    {{originalvar}} == "5 - Helt enig" ~ 5,
    {{originalvar}} == "5 - Completely agree" ~ 5,
    {{originalvar}} == "5 - Totally agree" ~ 5,
    {{originalvar}} == "5 - Svært fornøyd" ~ 5,
    {{originalvar}} == "5 - Very satisfied" ~ 5,
    {{originalvar}} == "5 - I stor grad" ~ 5,
    {{originalvar}} == "5 - Svært relevant" ~ 5,
    {{originalvar}} == "5 - Svært godt" ~ 5,
    {{originalvar}} == "Vet ikke" ~ NaN,
    {{originalvar}} == "Har ikke hatt ekstern praksis" ~ NaN,
    {{originalvar}} == "I have not carried out any external practical training" ~ NaN,
    {{originalvar}} == "I do not know" ~ NaN
  ))
}

# Kodar om frå tekst-"ordinal" til tal-nivå, lagar ny variabel, bra for testing
SA_text_to_numerallevels_nyvar <- function(sdf, nyvar, originalvar) {
  if (!(deparse(substitute(originalvar)) %in% colnames(sdf))) {
    sdf <- sdf %>% mutate({{nyvar}} := NaN)
    return(sdf)
  }
  sdf %>% mutate({{nyvar}} := case_when(
    {{originalvar}} == "1- Ikke enig" ~ 1,
    {{originalvar}} == "1 - Ikke enig" ~ 1,
    {{originalvar}} == "1 - Disagree" ~ 1,
    {{originalvar}} == "1 - Ikke fornøyd" ~ 1,
    {{originalvar}} == "1 - Not satisfied" ~ 1,
    {{originalvar}} == "1 - I liten grad" ~ 1,
    {{originalvar}} == "1 - Ikke relevant" ~ 1,
    {{originalvar}} == "1 - Ikke godt" ~ 1,
    {{originalvar}} == "2" ~ 2,
    {{originalvar}} == "3" ~ 3,
    {{originalvar}} == "4" ~ 4,
    {{originalvar}} == "5 - Helt enig" ~ 5,
    {{originalvar}} == "5 - Completely agree" ~ 5,
    {{originalvar}} == "5 - Totally agree" ~ 5,
    {{originalvar}} == "5 - Svært fornøyd" ~ 5,
    {{originalvar}} == "5 - Very satisfied" ~ 5,
    {{originalvar}} == "5 - I stor grad" ~ 5,
    {{originalvar}} == "5 - Svært relevant" ~ 5,
    {{originalvar}} == "5 - Svært godt" ~ 5,
    {{originalvar}} == "Vet ikke" ~ NaN,
    {{originalvar}} == "Har ikke hatt ekstern praksis" ~ NaN,
    {{originalvar}} == "I have not carried out any external practical training" ~ NaN,
    {{originalvar}} == "I do not know" ~ NaN
  ))
}

##**
##* Variabellister brukt til å bygge indeksar i Studiebarometeret
##* 

# Undervisning
var_underv <- c(
  "underv_engasj_18",
  "underv_formidl_18",
  "underv_pensum_18",
  "underv_aktiv_18"
  # "indeks_underv_18"
)
indx_underv <- c(
  "indx_underv4"
)

# Tilbakemelding og veiledning
var_tilbveil <- c(
  "tilbveil_antall_16",
  "tilbveil_konstru_13",
  "tilbveil_student_18",
  "tilbveil_fagdisk_18"
  # "indeks_tilbveil_16"
)
indx_tilbveil <- c(
  "indx_tilbveil4"
)

# Faglig og sosialt læringsmiljø
var_psymiljo <- c(
  "miljo_sosial_13",
  "miljo_fag_13",
  "miljo_studans_15"
  # "indeks_psymiljo_15"
)
indx_psymiljo <- c(
  "indx_psymiljo3"
)

# indeks Fysisk læringsmiljø
var_fysmiljo <- c(
  "miljo_lokaler_13",
  "miljo_utstyr_13",
  "miljo_biblio_13",
  "miljo_ikt_13"
  # "indeks_fysmiljo_13"
)
indx_fysmiljo <- c(
  "indx_fysmiljo4"
)

# Organisering
var_organ <- c(
  "organ_tilgjinfo_17",
  "organ_kvalinfo_17",
  "organ_admtilr_17",
  "organ_fagligsam_17"
  # "indeks_organ_17"
)
indx_organ <- c(
  "indx_organ4"
)

#indeks Vurderingsformer
var_vurd <- c(
  "vurd_pensum_13",
  "vurd_forstaelse_13",
  # "vurd_laert_17", # OBS MANGLAR I 2020
  "vurd_kriterier_17",
  "vurd_fagutv_17"
  # "indeks_vurd_17"
)
indx_vurd <- c(
  "indx_vurd5"
)

# medvirkning
var_medv <- c(
  "medvirk_innspill_18"
  # "medvirk_oppfolg_18",
  # "medvirk_tillitsv_18"
  # "indeks_medvirk_18"
)
indx_medv <- c(
  "indx_medv3"
)

#indeks Inspirerende program
var_insp <- c(
  "insp_stimul_13",
  "insp_utfordr_13",
  "insp_motivasjon_14"
  # "indeks_insp_14"
)
indx_insp <- c(
  "indx_insp3"
)

# Læringsutbytte
var_laerutb <- c(
  "laerutb_teori_13",
  "laerutb_metforsk_13",
  "laerutb_egenerf_13",
  "laerutb_fagspes_13",
  "laerutb_refleks_13",
  "laerutb_samarb_13",
  "laerutb_muntkom_13",
  "laerutb_skriftkom_13",
  "laerutb_tenke_13",
  "laerutb_selvst_13"
)
indx_laerutb <- c(
  "indx_laerutb10"
)

# Eget engasjement
var_egeteng <- c(
  "egeteng_motivert_14",
  "egeteng_orgakt_14",
  "egeteng_forberedt_14",
  "egeteng_innsats_14"
  # "indeks_egeteng_14"
)
indx_egeteng <- c(
  "indx_eget4"
)

# Faglig ansattes forventninger
var_forvent <- c(
  "forvent_klare_16",
  "forvent_forberedt_16",
  "forvent_deltar_16",
  "forvent_fagamb_16"
  # "indeks_forvent_16"
)
indx_forvent <- c(
  "indx_forvent4"
)

# Digitale verktøy
var_digitale <- c(
  "digitale_aktivt_18",
  "digitale_kompet_18",
  "digitale_opplaer_18",
  "digitale_laerplatt_18"
)
indx_digitale <- c(
  "indx_digit4"
)

# Overordnet tilfredshet
var_overord <- c(
  "overord_altialt_13"
)

# Tidsbruk
var_tidsbruk <- c(
  "tidsbruk_laerakt_14",
  "tidsbruk_egeninns_14",
  "tidsbruk_arbeid_14"
)
indx_sum_tid <- c(
  "sum_tid"
)
# Praksis - endra i 2020
var_praksis <- c(
  "praksis_forber_14",
  "praksis_passetinn_19",
  "praksis_veil_20",
  "praksis_laerutb_20",
  "praksis_relevant_19",
  "praksis_grunnlagdisk_19"
)
# ikkje bruk
# indx_praksis <- c(
#   "indx_praksis8"
# )

# Yrkesrelevans
var_yrkrel <- c(
  "yrkrel_bruk_18",
  "yrkrel_bransje_18",
  "yrkrel_formidle_19",
  "yrkrel_motearb_19",
  "yrkrel_bidrar_19",
  "yrkrel_prosjekt_19"
)
indx_yrkrel <- c(
  "indx_yrkrel6"
)

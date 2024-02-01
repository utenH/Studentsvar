##**
##* Hjelpefunksjonar
##* 
##* Hjelpefunksjon for å lett kunne sjå enkeltvariablar
skrivvar <- function(sdf, variabel) {
  sdf %>% select({{variabel}}) %>% table(useNA = "ifany") %>% addmargins()
}

##* For å skrive ut tabell til utklippsbrett
til_utklipp <- function(sdf, storleik = "", radnamn = F) {
  sdf %>% write.table(paste("clipboard", storleik, sep = "-"), sep = "\t", row.names = radnamn, dec = ",")
  return(sdf)
}

# Gjer det mogleg å til dømes filtrere på programkode som ikkje finst i studiebarometerfila
`%!in%` <- Negate(`%in%`)

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

##** 
##* Studiestart
##* 

OM_prepare_studiestart_2023 <- function(innfil = "../datafiler/studiestart/Studiestart 2023.xlsx", dataår = 2023) {
  OM <- read_excel(innfil)
  OM <- OM %>% clean_names
  OM %>% names %>% print
  OM <- OM %>% mutate(undersokelse_år = dataår)
  OM <- OM %>% mutate(gruppe_ar = undersokelse_år)
  OM <- OM %>% rename(Fakultetsnavn = fakultet)
  OM <- OM %>% rename(Institutt = institutt)
  OM <- OM %>% rename(Studieprogramkode = programkode)
  OM <- OM %>% rename(Studietilbud = programnavn)
  OM <- OM %>% mutate(Studieprogram_instnamn = paste(Institutt, Studieprogramkode, Studietilbud))
  OM <- OM %>% mutate(Institusjon = "OsloMet")
  
  # Omkoding
  svar_nivå <- c("1 – i liten grad", "2", "3", "4", "5 – i stor grad")
  OM <- OM %>% OM_text_to_factor(i_hvilken_grad_er_du_enig_i_folgende_pastander_jeg_opplever_a_vaere_en_del_av_et_studentmiljo_pa_studiet_mitt, svar_nivå)
  OM <- OM %>% OM_text_to_factor(i_hvilken_grad_er_du_enig_i_folgende_pastander_studiet_mitt_har_lagt_til_rette_for_ulike_gruppeaktiviteter, svar_nivå)
  OM <- OM %>% OM_text_to_factor(i_hvilken_grad_er_du_enig_i_folgende_pastander_de_faglige_og_sosiale_aktivitetene_pa_studiet_har_bidratt_til_at_jeg_har_blitt_kjent_med_medstudenter, svar_nivå)
  OM <- OM %>% OM_text_to_factor(i_hvilken_grad_er_du_enig_i_folgende_pastander_jeg_har_noen_a_diskutere_og_samarbeide_med_utenom_det_faglige_opplegget_pa_studiet_mitt, svar_nivå)
  OM <- OM %>% OM_text_to_factor(i_hvilken_grad_er_du_enig_i_folgende_pastander_jeg_har_noen_pa_studiet_mitt_jeg_kan_snakke_med_om_utfordringer_jeg_opplever_som_student, svar_nivå)
  OM <- OM %>% OM_text_to_factor(i_hvilken_grad_er_du_enig_i_folgende_pastander_jeg_opplever_at_jeg_henger_med_faglig_i_studiet_mitt, svar_nivå)
  OM <- OM %>% OM_text_to_factor(i_hvilken_grad_er_du_enig_i_folgende_pastander_det_faglige_innholdet_pa_studiet_svarer_sa_langt_til_mine_forventinger, svar_nivå)
  
  OM <- OM %>% OM_janei_bin_nyvar(svar = ser_du_for_deg_at_du_kommer_til_a_fullfore_dette_studiet,
                                  nyvar = ser_du_for_deg_at_du_kommer_til_a_fullfore_dette_studiet_bin)
  
  OM <- OM %>% mutate(ser_du_for_deg_at_du_kommer_til_a_fullfore_dette_studiet = 
                        factor(ser_du_for_deg_at_du_kommer_til_a_fullfore_dette_studiet,
                                        levels = c("Ja", "Vet ikke", "Nei"),
                                        labels = c("Ja", "Vet ikke", "Nei"),
                               ordered = T))

  return(OM) 
}

##** 
##* Studiestart
##* bruk EXC_tabell_meirinfo for å lage tabell om informasjon studentar sakna

OM_prepare_exchangestudents_2023 <- function(innfil = "../datafiler/exchangestudents/exchangestudents_h2023_data.xlsx", dataår = 2023) {
  OM <- read_excel(innfil)
  OM <- OM %>% clean_names
  OM %>% names %>% print
  OM <- OM %>% mutate(undersokelse_år = dataår)
  OM <- OM %>% mutate(gruppe_ar = undersokelse_år)
  OM <- OM %>% rename(Fakultetsnavn = fakultet)
  OM <- OM %>% rename(Institutt = institutt)
  OM <- OM %>% rename(Studieprogramkode = programkode)
  OM <- OM %>% rename(Studietilbud = programnavn)
  OM <- OM %>% mutate(Studieprogram_instnamn = paste(Institutt, Studieprogramkode, Studietilbud))
  OM <- OM %>% mutate(Institusjon = "OsloMet")
  
  # Omkoding
  svar_nivå <- c("1 – i liten grad", "2", "3", "4", "5 – i stor grad")
  OM <- OM %>% OM_text_to_factor(to_what_extent_are_you_satisfied_with_the_admission_process_to_oslo_met_application_admission_and_acceptance)
  OM <- OM %>% OM_text_to_factor(to_what_extent_are_you_satisfied_with_the_information_you_received_before_you_started)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_i_consider_myself_a_part_of_a_student_environment_in_my_studies)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_my_course_courses_has_facilitated_various_group_activities)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_the_academic_and_social_activities_in_my_course_courses_have_helped_me_get_to_know_fellow_students)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_i_have_someone_to_discuss_and_collaborate_with_outside_of_my_course_courses)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_i_have_someone_in_my_course_courses_i_can_talk_with_about_challenges_i_experience_as_a_student)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_i_feel_treated_equally_to_local_students)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_i_feel_that_i_am_able_to_keep_up_with_the_academic_requirements_in_course_courses_so_far)
  OM <- OM %>% OM_text_to_factor(to_what_extent_do_you_agree_with_the_following_statements_the_academic_content_of_my_course_courses_has_met_my_expectations_so_far)

  # Ja/Nei/Veit ikkje
  # did_you_use_the_course_catalogue_on_oslomet_no_before_deciding_what_courses_you_wanted_to_apply_for
  # are_you_satisfied_with_the_academic_support_you_have_received_so_far
  # do_you_have_any_inclusion_needs
  # gender
  
  # OM <- OM %>% mutate(used_course_catalogue =
  #                       factor(did_you_use_the_course_catalogue_on_oslomet_no_before_deciding_what_courses_you_wanted_to_apply_for,
  #                              levels = c("Ja", "Nei", "Vet ikke"),
  #                              labels = c("Ja", "Nei", "Vet ikke"),
  #                              ordered = T))
  janeiv <- c("Yes", "No", "Do not know")
  OM <- OM_lag_faktor(OM, did_you_use_the_course_catalogue_on_oslomet_no_before_deciding_what_courses_you_wanted_to_apply_for, janeiv)
  OM <- OM_lag_faktor(OM, are_you_satisfied_with_the_academic_support_you_have_received_so_far, janeiv)
  OM <- OM_lag_faktor(OM, do_you_have_any_inclusion_needs, janeiv)
  OM <- OM_lag_faktor(OM, gender, c("Female", "Male", "Other", "Do not wish to answer"))
  OM <- OM_lag_faktor(OM, what_is_your_age, c("Under 21 years", "21-24 years","25-30 years", "31-40 years", "Do not wish to answer"))
  
  # OM <- OM %>% OM_janei_bin_nyvar(svar = did_you_use_the_course_catalogue_on_oslomet_no_before_deciding_what_courses_you_wanted_to_apply_for,
  #                                 nyvar = used_course_catalogue)
  # 
  # OM <- OM %>% OM_janei_bin_nyvar(svar = are_you_satisfied_with_the_academic_support_you_have_received_so_far,
  #                                 nyvar = satisfied_academic_support)
  # 
  # OM <- OM %>% OM_janei_bin_nyvar(svar = do_you_have_any_inclusion_needs,
  #                                 nyvar = inclusion_needs)
  
  # OM <- OM %>% OM_janei_bin_nyvar(svar = gender,
  #                                 nyvar = gender_num)
  

  
  # Fleirval
  # what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_admission_nomination_application_declaration
  # what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_international_coordinator
  # what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_buddy_programme
  # what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_pre_arrival_guide_residence_permit_finance_udi_insurance_housing_accommodation_etc
  
  # Fjerne linjeskift, dei lagar krøll i utskrift til xlsx
  OM <- OM %>% mutate(across(where(is.character), ~gsub("\r\n", " ", .)))
  
  return(OM) 
}

# Gjer dette utanom, for enkelheits skuld
EXC_tabell_meirinfo <- function(sdf) {
  sdf %>% group_by(fakultet) %>% summarise(across(starts_with("what_do_you_wish_you_had"), ~sum(. == "Yes"))) %>% 
    set_names(c("Fakultet", "Admission", "International coordinator", "Buddy", "Pre-arrival"))
  return(sdf)
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

##* Studiebarometeret
SB_prepare_2024 <- function(innfil, dataår, instnr, kopleprogramdata = T) {
  OM <- read_excel(innfil)
  OM <- OM %>% mutate(undersøkelse_år = dataår)
  # I 2022 inneheldt datasettet nokre observasjonar utan studprog_kod/studiepgm_navn
  # Filtrerer bort slike
  OM <- OM %>% filter(!is.na(studprog_kod))
  OM[OM==9999] <- NaN # Var NA, bytta 2020 for å skilje mellom ikkje-svar (NA) og Vet ikke (NaN)
  OM <- SB_name_fak_kort(OM)
  if (kopleprogramdata) {
    if (instnr == 1175) {
      # legg til programnamn med instituttilhøyrigheit
      OM <- dbh_add_programdata(OM, "studprog_kod", instnr)
      OM <- OM_add_programdata(OM, "Studieprogramkode")
      OM <- SB_add_nivå(OM)
    } else {
      OM <- dbh_add_programdata(OM, "studprog_kod", instnr)
    }
  }
  OM <- SB_add_sum_tid(OM)
  OM <- SB_prep_indeks(OM)
  return(OM)
} 

SB_prepare_2023 <- function(innfil, dataår, instnr, kopleprogramdata = T) {
  OM <- read_excel(innfil)
  OM <- OM %>% mutate(undersøkelse_år = dataår)
  # I 2022 inneheldt datasettet nokre observasjonar utan studprog_kod/studiepgm_navn
  # Filtrerer bort slike
  OM <- OM %>% filter(!is.na(studprog_kod))
  OM[OM==9999] <- NaN # Var NA, bytta 2020 for å skilje mellom ikkje-svar (NA) og Vet ikke (NaN)
  OM <- SB_name_fak_kort(OM)
  if (kopleprogramdata) {
    if (instnr == 1175) {
      # legg til programnamn med instituttilhøyrigheit
      OM <- dbh_add_programdata(OM, "studprog_kod", instnr)
      OM <- OM_add_programdata(OM, "Studieprogramkode")
      OM <- SB_add_nivå(OM)
    } else {
      OM <- dbh_add_programdata(OM, "studprog_kod", instnr)
    }
  }
  OM <- SB_add_sum_tid(OM)
  OM <- SB_prep_indeks(OM)
  return(OM)
} 

##** Funksjon som tar spesialtilfelle i Studiebarometeret
##*  Andre datasett må vere vaska først
SB_unntak <- function(sdf) {
  sdf$Fakultetsnavn[grepl("YFLH", sdf$Studieprogramkode)] <- "Fakultet for lærerutdanning og internasjonale studier (LUI)"
  sdf$Institutt[grepl("YFLH", sdf$Studieprogramkode)] <- "Institutt for yrkesfaglærerutdanning"
  sdf <- sdf %>% mutate(Studieprogramnavn = coalesce(Studieprogramnavn, studiepgm_navn))
  sdf <- sdf %>% mutate(Nivåkode = coalesce(Nivåkode, STUDIENIVAKODE))
  return(sdf)
}

##**
##* Funksjon for førebuing av dataframes, Sisteårs 
##* Skulle gjerne hatt felles, men viktigare med felles utskriftsfunksjon
##* 
SA_prepare_2023 <- function(innfil, dataår, instnr) {
  OM <- read_excel(innfil, .name_repair = janitor::make_clean_names)
  colnames(OM) <- gsub(pattern = "-", replacement = "", x = colnames(OM))
  OM <- OM %>% mutate(undersøkelse_år = dataår)
  OM[OM==9999] <- NaN # skiljer mellom ikkje-svar (NA) og Vet ikke (NaN)
  OM <- dbh_add_programdata(OM, "fs_kode", instnr)
  OM <- OM_add_programdata(OM, "Studieprogramkode")
  OM <- SA_tilpassdata(OM)
  OM <- SA_prep_indeks(OM)
  # TODO dette er lat hack, finn betre måte, f.eks. ta med instnr frå DBH
  OM <- OM %>% mutate(instnr = instnr) 
  return(OM)
} 

## Koder og lagar df for data frå eitt år
SA_prepare_fritekst <- function(datafil) {
  SA_raw <- read_excel(datafil)
  SA <- as.data.frame(SA_raw)
  # SA <- SA %>% rename("studprog_kod" = `fs-kode`)
  # SA <- SB_name_institute(SA)
  # SA <- SA %>% rename("Programkode" = "studprog_kod")
  # SA <- dbh_add_programnavn(SA, "Programkode")
  SA <- dbh_add_programdata(SA, "progkode", 1175)
  print(SA %>% names)
  manglar_institutt <- SA %>% filter(is.na(Institutt)) %>% select(StudiumID) %>% unique
  if (manglar_institutt %>% nrow > 0){
    print("Program som ikkje er plassert på institutt:")
    print(manglar_institutt)
    }
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
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_det_er_god_luft_og_temperatur_i_undervisningsrommene)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_jeg_og_mine_medstudenter_finner_et_egnet_sted_a_vaere_nar_vi_skal_jobbe_sammen_i_gruppe)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_jeg_finner_lesesalsplass_nar_jeg_trenger_det)
  sdf <- SA_text_to_numerallevels(sdf, hvor_enig_er_du_i_disse_pastandene_bruken_av_digitalt_utstyr_fungerer_godt_i_undervisningen)
  
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

##**
##* Hjelpefunksjon for å skrive ut metadata om tabell
##* 
dbh_skriv_meta <- function(tabellnummer) {
  dbh_metadata(tabellnummer) %>% select(`Variabel navn`, `Group by (forslag)`, Kommentar) %>% print(n=Inf)
} 

##*
##* Hjelpefunksjon for å få med alle anbefalte grupperingsvariablar + dei ein vel sjølv
##* tablenum tabellnummer
##* vars_to_add string vector med variablar å legge til
##* institusjonskode string (vector) med institusjonsnummer å hente (bruk enkelt-sitatteikn)
##* år kan til dømes filtrere på større enn: år = c('greaterthan', '2019')
dbh_custom_data <- function(tablenum, vars_to_add, institusjonskode = "1175", år = c('top', '3')) {
  dbh_vars <- dbh_metadata(tablenum) %>% filter(`Group by (forslag)` == "J") %>% select(`Variabel navn`)
  dbh_vars <- dbh_vars[["Variabel navn"]]
  dbh_vars <- append(dbh_vars, vars_to_add)
  dbh_table <- dbh_data(tablenum, 
                        filters = list("Institusjonskode" = institusjonskode, "Årstall" = år), 
                        group_by = dbh_vars)
  return(dbh_table)
}

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
dbh_hent_programdata <- function(instnr = 1175) {
  dbh_vars <- c("Studieprogramkode", 
                "Studieprogramnavn",
                "Avdelingskode",
                "Avdelingskode_SSB", # TODO heller bruke denne, trunkert til to siffer for fakultet, heile talet for institutt. Kan godt lagre til eiga fil som programvariablane
                "Nivåkode", 
                "Årstall", 
                "Andel av heltid", 
                "Andel praksis",
                "Tilbys til",
                "Studiepoeng")
  dbh_programdata <- dbh_data(347, 
                              filters = c("Institusjonskode" = instnr), 
                              variables = dbh_vars, 
                              group_by = dbh_vars) %>%
    arrange(desc(Årstall)) %>% distinct(Studieprogramkode, .keep_all = T)
  
  # For å handtere samanslåing av institutt
  if (instnr == 1175) {
    dbh_programdata <- dbh_programdata %>% mutate(Avdelingskode = 
                                                    case_when(Avdelingskode == "510330" | 
                                                                Avdelingskode == "510340" ~ "510350",
                                                              TRUE ~ Avdelingskode))
  }  
  dbh_orgdata <- dbh_hent_orgdata(instnr) %>% filter(!grepl("studieprogram på instituttnivå", Institutt))
  dbh_programdata <- left_join(dbh_programdata, dbh_orgdata, "Avdelingskode")
  
  # For å handtere samanslåing av institutt og særtilfelle
  if (instnr == 1175) {
    # Handtere program som ikkje er knytt til institutt
    dbh_programdata <- dbh_programdata %>% 
      mutate(Fakultetskode = coalesce(Fakultetskode, strtrim(Avdelingskode, 3)))
    
    dbh_programdata <- dbh_programdata %>% mutate(Avdelingskode = 
                            case_when(Avdelingskode == "510330" | 
                                        Avdelingskode == "510340" ~ "510350",
                                      TRUE ~ Avdelingskode))
    
    dbh_programdata <- dbh_programdata %>% mutate(Institutt = 
                            case_when(Institutt == "Institutt for fysioterapi" | 
                                        Institutt == "Institutt for ergoterapi og ortopediingeniørfag" ~ 
                                        "Institutt for rehabiliteringsvitenskap og helseteknologi",
                                      Avdelingskode == "530360" ~ "School of Management",
                                      Avdelingskode == "540360" ~ "Makerspace",
                                      is.na(Institutt) ~ "Uplassert",
                                      TRUE ~ Institutt))
    
    dbh_programdata <- dbh_programdata %>% mutate(Fakultetsnavn = case_when(
      grepl("510", Fakultetskode) ~ "HV",
      grepl("520", Fakultetskode) ~ "LUI",
      grepl("530", Fakultetskode) ~ "SAM",
      grepl("540", Fakultetskode) ~ "TKD",
      grepl("620", Fakultetskode) ~ "SPS",
      grepl("000", Fakultetskode) ~ "OsloMet"
    ))
    
    dbh_programdata <- dbh_programdata %>% mutate(Avdelingskode = 
                                                    case_when(Avdelingskode == "510330" | 
                                                                Avdelingskode == "510340" ~ "510350",
                                                              TRUE ~ Avdelingskode))
  }
  
  return(dbh_programdata)
}

##** Hentar ein tabell med instituttnamn, kan koplast på kolonna Avdelingskode 
dbh_hent_orgdata <- function(instnr = 1175) {
  sdf <- dbh_data(457, filters = c("Institusjonskode" = instnr, "Årstall" = "2023"), 
                  variables = c("Avdelingskode", "Avdelingskode_SSB", "Avdelingsnavn", "Årstall"), 
                  group_by = c("Avdelingskode", "Avdelingskode_SSB", "Avdelingsnavn", "Årstall")) %>% 
    # Alternativ måte: sjekke om Avdelingskode_SSB sluttar med 00, det er instituttnivå, 000 og opp må også bort?
    # filter(as.numeric(Avdelingskode) %% 100 == 0 & as.numeric(Avdelingskode) %% 1000 != 0 & as.numeric(Avdelingskode) %% 10000 != 0) %>%
    filter(grepl("^Institutt for|^Handelshøyskolen", Avdelingsnavn)) %>%
    mutate(Fakultetskode = strtrim(Avdelingskode, 3)) %>% 
    arrange(Avdelingsnavn) %>% select(Avdelingskode, Avdelingskode_SSB, Fakultetskode, Institutt = Avdelingsnavn)
  
  # For å handtere samanslåing av institutt
  if (instnr == 1175) {
    sdf <- sdf %>% mutate(Avdelingskode = 
                            case_when(Avdelingskode == "510330" | 
                                        Avdelingskode == "510340" ~ "510350",
                                      TRUE ~ Avdelingskode))
  }
  
  # Hentar tabell med fakultetskode og -namn
  dbh_210 <- dbh_data(210) %>% filter(Institusjonskode == instnr) %>%
    select(Fakultetskode, Fakultetsnavn) %>% unique() %>% arrange(Fakultetskode)
  
  # Forkorte fakultetsnamn - flytta til dbh_hent_programdata()
  # if (instnr == 1175) {
  #   dbh_210 <- dbh_210 %>% mutate(Fakultetsnavn = case_when(
  #     grepl("HV", Fakultetsnavn) ~ "HV",
  #     grepl("LUI", Fakultetsnavn) ~ "LUI",
  #     grepl("SAM", Fakultetsnavn) ~ "SAM",
  #     grepl("TKD", Fakultetsnavn) ~ "TKD"
  #   ))
  # }
  
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
        # I 2020 mangla denne for MxGLU
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
  sdf <- dbh_data(370, filters = list("institusjonskode" = institusjonsnummer,
                                      "Årstall" = "*"), 
                  group_by = list("Avdelingskode", "Institusjonskode", "Årstall"))
  return(sdf)
}

dbh_hent_soknadsdata <- function(institusjonsnummer) {
  sdf <- dbh_data(379, filters = c("institusjonskode"=institusjonsnummer))
  return(sdf)
}

dbh_hent_gjennomforingsdata <- function(institusjonsnummer) {
  sdf <- dbh_data(707, filters = c("institusjonskode"=institusjonsnummer))
  return(sdf)
}

dbh_hent_studiepoengdata <- function(institusjonsnummer) {
  sdf <- dbh_data(900, filters = c("institusjonskode"=institusjonsnummer))
  return(sdf)
}

##** 
##* Kodar om Studieprogramkode, for å kunne vise eldre data saman med nytt program
##* vis_samla_kode gir moglegheit til å velje å vise berre nyaste studieprogramkode, eller samanslått
##* 
kople_studieprogramkode <- function(sdf, vis_samla_kode = T) {
  if (vis_samla_kode) {
    sdf <- sdf %>% 
      mutate(Studieprogramkode = case_when(
        Studieprogramkode == "SYKK" ~ "SYKK+SPH", 
        Studieprogramkode == "SPH" ~ "SYKK+SPH",
        Studieprogramkode == "SYKP" ~ "SYKP+SYPLGR", 
        Studieprogramkode == "SYPLGR" ~ "SYKP+SYPLGR",
        Studieprogramkode == "GVH" ~ "VERB+GVH",
        Studieprogramkode == "VERB" ~ "VERB+GVH",
        TRUE ~ Studieprogramkode))
    # TODO legg til dei siste som variantar for samanslått kode 
  }
  
  if (!vis_samla_kode) {
    sdf <- sdf %>% 
      mutate(Studieprogramkode = case_when(
        Studieprogramkode == "SPH" ~ "SYKK",
        Studieprogramkode == "SYPLGR" ~ "SYKP",
        Studieprogramkode == "GVH" ~ "VERB",
        grepl("YLBAH|YLDHH|YLEFH|YLHSH|YLRMH|YLSSH|YLTIH", Studieprogramkode) ~ "YFLH",
        grepl("YLSSN|YLBAN|YLEFN|YLTIN", Studieprogramkode) ~ "YFLN",
        Studieprogramkode == "MJOUR" ~ "MEDUT",
        Studieprogramkode == "MAERG" ~ "MERG",
        Studieprogramkode == "MAFYS" ~ "MAMUS",
        Studieprogramkode == "MAREHAB" ~ "MAHAB",
        Studieprogramkode == "MAPO" ~ "MAEMP",
        Studieprogramkode == "MASE" ~ "MAPHN",
        Studieprogramkode == "MAPSYKHD4" ~ "MAPSY",
        grepl("MASYKV|MASYKVD4", Studieprogramkode) ~ "MAKLI",
        TRUE ~ Studieprogramkode))
  }
  return(sdf)
}

##**
##* Legg til studiestadnamn for 1. og 2. syklus
##* 
##* TODO – nokre studietilbod er registrert både på Kjeller og Pilestredet, desse blir plassert på ein av dei
OM_studiestad <- function(sdf = NULL, eldste = 0) {
  # Hentar stadnavn i 1. og 2. syklus
  # print("Hentar DBH-data om stadnavn i 1. og 2. syklus")
  studiestad_OM <- dbh_data(124, filters=c("Institusjonskode"="1175"), 
                            group_by=c("Institusjonskode", "Stednavn campus", "Studieprogramkode", "Årstall")) %>%
    filter(Årstall >= eldste) %>%
    select(Studieprogramkode, Studiestad = `Stednavn campus`) %>% 
    filter(!grepl("UPLASSERT|HVXS|INTER", Studieprogramkode), !grepl("Sandvika", Studiestad),
           !(Studieprogramkode == "BIGB" & Studiestad == "Kjeller"),
           !(Studieprogramkode == "MAPHN" & Studiestad == "Kjeller"),
           !(Studieprogramkode == "MAVITE" & Studiestad == "Kjeller"), 
           !(Studieprogramkode == "PDB" & Studiestad == "Pilestredet"),
           !(Studieprogramkode == "VEIPR" & Studiestad == "Kjeller")) %>%
    mutate(Studiestad = case_when(Studiestad == "Uplassert" ~ "Pilestredet",
                                  T ~ Studiestad)) %>% unique()
  
  if (!is.null(sdf)) {
  # Legg til informasjon om studiestad
  sdf <- left_join(sdf, studiestad_OM, "Studieprogramkode")
  return(sdf)
  }
  return(studiestad_OM)
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
  OM_programvar <- read_excel("base/OsloMet_programvariabler.xlsx")
  sdf <- left_join(sdf, OM_programvar,
                   by = setNames("Studieprogramkode", varnamn))
  sdf <- sdf %>% rename("Studieprogramkode" = !!varnamn)
  
  # Om det er Studiebarometerdata, kan vi skilje mellom 2. og 5. år på grunnskulelærarutdanninga
  if ("STUDIEAR" %in% (sdf %>% names)) {
    sdf <- sdf %>% mutate(Studieprogram_instnamn = case_when(
      grepl("GLU", Studieprogramkode) ~ paste0(Studieprogram_instnamn, " (", STUDIEAR, ". studieår)"),
      T ~ Studieprogram_instnamn
    ))
  }
  return(sdf)
}

##**
#* Kodar Nivå-variabel til bruk i filtrering
SB_add_nivå <- function(sdf) {
    sdf <- sdf %>% mutate(Nivå = case_when(
      STUDIENIVAKODE == "M2" |
        (STUDIENIVAKODE == "M5" & STUDIEAR == 5) |
        STUDIENIVAKODE == "ME" ~ "Master",
      STUDIENIVAKODE == "B3" | 
        (STUDIENIVAKODE == "M5" & STUDIEAR == 2) |
        # I 2020 mangla denne for MxGLU
        (STUDIENIVAKODE == "M5" & is.na(STUDIEAR)) ~ "Bachelor", 
      TRUE ~ "Annet"
    ))
  return(sdf)
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
  
  try(expr = {
  sdf$indx_medv3 <- SB_add_indx_m(sdf, var_medv)
  sdf$mis_medv3 <- SB_add_countmiss(sdf, var_medv)
  sdf$indx_medv3[which(sdf$mis_medv3 > 0)] <- NA  
  }, silent = TRUE)
  
  # Endra i 2022
  try(expr = {
  sdf$indx_insp3 <- SB_add_indx_m(sdf, var_insp)
  sdf$mis_insp3 <- SB_add_countmiss(sdf, var_insp)
  sdf$indx_insp3[which(sdf$mis_insp3 > 0)] <- NA  
  }, silent = TRUE)
  
  # # Endra i 2020, erstatta i 2024
  # try(expr = {
  #   sdf$indx_praksis6 <- SB_add_indx_m(sdf, var_praksis)
  #   sdf$mis_praksis6 <- SB_add_countmiss(sdf, var_praksis)
  #   sdf$indx_praksis6[which(sdf$mis_praksis6 > 2)] <- NA
  # }, silent = TRUE)
  
  # Endra i 2024
  try(expr = {
    sdf$indx_praksis7 <- SB_add_indx_m(sdf, var_praksis)
    sdf$mis_praksis7 <- SB_add_countmiss(sdf, var_praksis)
    sdf$indx_praksis7[which(sdf$mis_praksis7 > 3)] <- NA
  }, silent = TRUE)
  
  try(expr = {
    sdf$indx_yrkrel6 <- SB_add_indx_m(sdf, var_yrkrel)
    sdf$mis_yrkrel6 <- SB_add_countmiss(sdf, var_yrkrel)
    sdf$indx_yrkrel6[which(sdf$mis_yrkrel6 > 2)] <- NA 
  }, silent = TRUE)
  
  sdf$indx_eget4 <- SB_add_indx_m(sdf, var_egeteng)
  sdf$mis_eget4 <- SB_add_countmiss(sdf, var_egeteng)
  sdf$indx_eget4[which(sdf$mis_eget4 > 1)] <- NA
  
  # Endra i 2022
  try(expr = {
  sdf$indx_forvent4 <- SB_add_indx_m(sdf, var_forvent)
  sdf$mis_forvent4 <- SB_add_countmiss(sdf, var_forvent)
  sdf$indx_forvent4[which(sdf$mis_forvent4 > 1)] <- NA
  }, silent = TRUE)
  
  # Endra i 2022
  try(expr = {
  sdf$indx_digit4 <- SB_add_indx_m(sdf, var_digitale)
  sdf$mis_digit4 <- SB_add_countmiss(sdf, var_digitale)
  sdf$indx_digit4[which(sdf$mis_digit4 > 1)] <- NA
  }, silent = TRUE)
  
  return(sdf)
}

##** Kandidatundersøkelsen - hjelpefunksjonar for å rydde og slå saman data til tidsserie
##*
##*

##** Kandidatundersøkinga 2022 - forbered data
OM_kandidat_setup_2022 <- function(sdf) {
  # Nokre svar kan rekodast basert på fritekstsvar
  sdf <- OM_rekode_2022_hovedaktivitet(sdf)  
  sdf <- set_a5_hovedaktivitet(sdf)
  sdf <- OM_janei_bin(sdf, arbeider_utdannet_til)
  
  sdf <- set_grunn_annet_arbeid(sdf, grunn_annet_arbeid)
  
  sdf <- set_forventning(sdf, forventning_arbeidsmarked)

  sdf <- set_sektor(sdf, sektor)
  sdf <- set_fylke(sdf, arbeidssted)
  
  sdf <- set_stillingsandel(sdf, stillingsprosent, sammenlagt_stilling)
  # Nokre svar kan rekodast basert på fritekstsvar
  sdf <- OM_rekode_2022_stillingsandel(sdf)
  
  sdf <- set_heltidsstilling(sdf, stillingsandel)
  # Nokre svar kan rekodast basert på fritekstsvar
  sdf <- set_deltidsstilling(sdf, stillingsandel)
  
  sdf <- set_ufrivilligdeltid(sdf, grunn_redusert_stilling)  
  sdf <- set_grunn_redusert_stilling(sdf, grunn_redusert_stilling)
  
  # Nokre svar kan rekodast basert på fritekstsvar
  sdf <- OM_rekode_2022_studerer_niva(sdf)
  sdf <- OM_set_faktor_studerer_niva(sdf, studerer_niva)
  
  sdf <- OM_janei_bin(sdf, flere_arbeidsgivere)
  
  sdf <- set_tidtiljobbdager(sdf, tid_til_relevant_arbeid)
  sdf <- set_jobbunderveis(sdf, tid_til_relevant_arbeid)
  sdf <- set_lang_tid_til_relevant_arbeid(sdf, tid_til_relevant_arbeid)
  
  sdf$fornoyd_pre <- sdf$fornoyd_oppgaver
  sdf <- set_fornoydmedoppgaver(sdf, fornoyd_oppgaver)
  sdf$fornoyd_oppgaver[sdf$arbeider_utdannet_til == 0 | is.nan(sdf$arbeider_utdannet_til)] <- NA
  sdf$forberedt_pre <- sdf$forberedt_oppgaver  
  sdf <- set_forberedtforoppgaver(sdf, forberedt_oppgaver)
  sdf$forberedt_oppgaver[sdf$arbeider_utdannet_til == 0 | is.nan(sdf$arbeider_utdannet_til)] <- NA
  
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
  sdf$brutto_arslonn_vasket[sdf$brutto_arslonn < 300000] <- NA 
  sdf$brutto_arslonn_vasket[sdf$brutto_arslonn > 1000000] <- NA 
  sdf$brutto_arslonn_vasket[sdf$stillingsandel < 1] <- NA
  
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
  
  # Hentar programnamn frå dbh
  sdf <- sdf %>% dbh_add_programdata("fs_kode", 1175)
  
  # Gruppere år i to
  sdf <- sdf %>% mutate(gruppe_ar = ifelse(undersokelse_ar < 2022,
                                           "2018/2019",
                                           undersokelse_ar))
  
  # legg til programnamn med instituttilhøyrigheit
  OM_programvar <- read_excel("base/OsloMet_programvariabler.xlsx")
  sdf <- left_join(sdf, OM_programvar, "Studieprogramkode")
  
  # # Gir så store utslag at dei ikkje skal vere med i felles rapport
  sdf <- sdf %>% filter(`_fs_kode` != "SKUT-MA" | is.na(`_fs_kode` != "SKUT-MA"))

  # Desse gir så ulike resultat at dei må handterast som ulike program.
  sdf <- sdf %>% mutate(Studieprogram_instnamn = case_when(
    Studieprogramkode == "ESTDTK-BA" & `_fs_kode` == "PPUDTK" ~ paste(Studieprogram_instnamn, "m. PPU"),
    T ~ Studieprogram_instnamn
  ))
  
  ##** 
  ##* Rydding og omkoding av data
  # Fjernar svar frå kandidatar der det er over tre år sidan dei fullførte, 
  # for å gi betre samanlikning mellom dei to gruppene
  sdf <- sdf %>% mutate(ar_fra_ferdig = undersokelse_ar - fullfort_ar)
  # Må ta vare på dei to fysioterapi-utdanningane, dei har eit år turnus etter fullført
  sdf <- sdf %>% filter(grepl("fysio", Studieprogramnavn) |
                          ar_fra_ferdig < 4)
  # Fjernar program utan fakultet
  sdf <- sdf %>% filter(!is.na(Fakultetsnavn))
  
  # Lagar variabel for Studium_ar, Fakultet_ar og OM_ar
  # for å kunne gruppere før utskrift, slik at data kjem på ei linje per år
  sdf <- sdf %>% mutate(Studium_ar = paste(Studieprogramnavn, gruppe_ar))
  sdf <- sdf %>% mutate(StudiumID_ar = paste(StudiumID, gruppe_ar))
  sdf <- sdf %>% mutate(Studieprogram_instnamn_ar = paste(Studieprogram_instnamn, gruppe_ar))
  sdf <- sdf %>% mutate(Fakultet_ar = paste(Fakultetsnavn, gruppe_ar))
  sdf <- sdf %>% mutate(OM_ar = paste("OsloMet", gruppe_ar))
  sdf <- sdf %>% mutate(Institusjon = "OsloMet")
  
  missing_fakultet <- sdf %>% filter(is.na(Fakultetsnavn)) %>% select(Studieprogramkode, undersokelse_ar)
  if (nrow(missing_fakultet) > 0) {
    print("Program som ikkje er kopla til fakultet:")
    print(missing_fakultet)
  }
  
  sdf <- sdf %>% filter(Studieprogramkode != "FAMA", Studieprogramkode != "FMAN", 
                        Studieprogramkode != "ABY", Studieprogramkode != "YLMKH")
  
  sdf <- OM_kandidat_setup_2022(sdf)
  
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
  
  return(sdf)
}

OM_set_undersokelse_ar <- function(sdf, year) {
  if (is.null(sdf$undersokelse_ar)) {
    sdf <- sdf %>% mutate(undersokelse_ar = year)
  }
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
  # sdf$sektor <- sdf$sektor %>% as.factor
  # Endra for å sette rekkefølgja betre
  # sdf$sektor <- factor(sdf$sektor, levels = c("Kommunal", "Fylkeskommunal", "Statlig", "Privat", "Frivillig"))
  sdf <- sdf %>% mutate({{variabel}} := factor({{variabel}}, 
                                               levels = c("Kommunal", 
                                                          "Fylkeskommunal", 
                                                          "Statlig", "Privat", 
                                                          "Frivillig"))
  )
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
  
  sdf <- sdf %>% mutate(oslo_viken_annet = case_when(
    {{variabel}} == "Oslo" ~ "Oslo",
      {{variabel}} == "Akershus" |
      {{variabel}} == "Buskerud" |
      {{variabel}} == "Østfold" |
      {{variabel}} == "Viken" ~ "Viken",
    {{variabel}} != "" ~ "Annet"
  ),
  oslo_viken_annet = factor(oslo_viken_annet, levels=c("Oslo", "Viken","Annet"))
  )
  
  return(sdf)
}

# Lagar variabel for hovedaktivitet, variant
# TODO gjere om til integer med label
set_a5_hovedaktivitet <- function(sdf) {
  sdf <- sdf %>% mutate(a5_hovedaktivitet = case_when(
    hovedaktivitet == "Ansatt i fast stilling (inkl. prøvetid)" | hovedaktivitet == "Ansatt i fast stilling" ~ "1-Fast stilling",
    hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet mer enn 6 måneder" | 
      hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet 6 måneder eller mer" |
      hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med varighet mindre enn 6 måneder" | 
      hovedaktivitet == "Ansatt i vikariat eller annen midlertidig stilling med kortere varighet enn 6 måneder" | 
      hovedaktivitet == "Midlertidig" |
      hovedaktivitet == "Ansatt i vikariat / midlertidig stilling med varighet 6 måneder eller mer" |
      hovedaktivitet == "Ansatt i vikariat / midlertidig stilling med varighet mindre enn 6 måneder" | 
      hovedaktivitet == "Ansatt i vikariat / midlertidig stilling med kortere varighet enn 6 måneder" ~ "2-Midlertidig stilling",
    hovedaktivitet == "Selvstendig næringsdrivende" ~ "3-Selvstendig næringsdrivende",
    hovedaktivitet == "Arbeidssøkende"	~ "4-Arbeidssøkende",
    hovedaktivitet == "Student" ~ "5-Student"
    # hovedaktivitet == "Annet" ~ "5-Annet"
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
  
  # Endrar aldersvariabel for å få alder då dei fullførte utdanninga
  sdf <- sdf %>% mutate(alder_int = alder_int - ar_fra_ferdig)
  
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

# Lagar variabel stillingsandel
set_stillingsandel <- function(sdf, variabel, sammenlagtvariabel) {
  sdf <- sdf %>% mutate(stillingsandel = case_when(
    {{sammenlagtvariabel}} == "Mer enn 100% stilling" |
      {{sammenlagtvariabel}} == "Mer enn 100%" ~ 1.1,
    {{variabel}} == "100%" | 
      {{sammenlagtvariabel}} == "100%" ~ 1,
    {{variabel}} == "90%" ~ 0.9,
    {{variabel}} == "80% (tilsvarende 4 dager i uka)" |
      {{sammenlagtvariabel}} == "80% (tilsvarende 4 dager i uka)" |
      {{sammenlagtvariabel}} == "80%" ~ 0.8,
    {{variabel}} == "70 %" ~ 0.7,
    {{variabel}} == "60% (tilsvarende 3 dager i uka)" |
      {{sammenlagtvariabel}} == "60%" |
      {{sammenlagtvariabel}} == "60% (tilsvarende 3 dager i uka)" ~ 0.6,
    {{variabel}} == "50%" |
      {{sammenlagtvariabel}} == "50%" ~ 0.5,
    {{variabel}} == "Mindre enn 50%" |
      {{sammenlagtvariabel}} == "Mindre enn 50%" ~ 0.4,
    {{variabel}} == "Vet ikke" |
      {{sammenlagtvariabel}} == "Vet ikke" ~ NaN
  ))
  return(sdf)
}

# Lagar variabel heltidsstilling
set_heltidsstilling <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(heltidsstilling = case_when(
    {{variabel}} >= 1 ~ 1,
    {{variabel}} < 1 ~ 0,
    T ~ {{variabel}}
  ))
  return(sdf)
}

# Lagar variabel deltidsstilling
set_deltidsstilling <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(deltidsstilling = case_when(
    {{variabel}} >= 1 ~ 0,
    {{variabel}} < 1 ~ 1,
    T ~ {{variabel}}
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
  return(sdf)
}

# Lagar variabel haddejobbunderveis
set_jobbunderveis <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(hadde_jobb_underveis = case_when(
    {{variabel}} == "Hadde en relevant jobb allerede før jeg ble masterstudent" | 
      {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte" | 
      {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte/var i turnustjeneste" ~ 1,
    {{variabel}} == "Husker ikke" ~ NaN, 
    {{variabel}} != "" ~ 0
  ))
  return(sdf)
}

# Lagar variabel for meir enn 6 mnd til relevant arbeid
set_lang_tid_til_relevant_arbeid <- function(sdf, variabel) {
  sdf <- sdf %>% mutate(lang_tid_til_relevant_arbeid = case_when(
    {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå var masterstudent" |
      {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte" |
      {{variabel}} == "Fikk tilbud om relevant jobb mens jeg ennå studerte/var i turnustjeneste" |
    {{variabel}} == "Mindre enn en måned etter fullført utdanning" |
      {{variabel}} == "Mindre enn en måned etter fullført utdanning/turnustjeneste" |
      {{variabel}} == "Fikk tilbud om relevant jobb mindre enn en måned etter fullført masterutdanning" |
    {{variabel}} == "1-2 måneder" |
    {{variabel}} == "3-4 måneder" |
    {{variabel}} == "5-6 måneder" ~ 0,
    {{variabel}} == "7-12 måneder" |
    {{variabel}} == "13-18 måneder" |
    {{variabel}} == "19-24 måneder" |
    {{variabel}} == "Mer enn to år" ~ 1,
    {{variabel}} == "Husker ikke" ~ NaN
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
  sdf <- sdf %>% mutate({{variabel}} := factor({{variabel}},
                                               levels = c("Har fått jobb som passer meg like godt", 
                                                    "Har søkt uten hell", 
                                                    "Har prøvd, men trivdes ikke", 
                                                    "Annen grunn"))
  )
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

# TODO - desse kan generaliserast ein del, med grepl-varianten

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

# ##** 
# ##* Exchange students survey
# ##* Slår saman fleirval lagra i ulike variablar til ein factor  
# EXC_fleirval_til_faktor <- function(sdf) {
#   sdf$meirinfo[sdf$what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_admission_nomination_application_declaration == "Yes"] <- "Admission"
#   sdf$meirinfo[sdf$what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_international_coordinator == "Yes"] <- "International coordinator"
#   sdf$meirinfo[sdf$what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_buddy_programme == "Yes"] <- "Buddy programme"
#   sdf$meirinfo[sdf$what_do_you_wish_you_had_received_more_information_about_before_arrival_at_oslo_met_you_can_choose_multiple_options_pre_arrival_guide_residence_permit_finance_udi_insurance_housing_accommodation_etc == "Yes"] <- "Pre-Arrival guide"
#   nivå <- c("Admission", "International coordinator", "Buddy programme", "Pre-Arrival guide")
#   sdf <- OM_lag_faktor(sdf, meirinfo, nivå, sortert = T)
#   return(sdf)
# }

##** 
##* Generiske funksjonar for omkoding av variablar
##* 

# Kodar om til faktor, med gitte nivå
OM_lag_faktor <- function(sdf, variabel, nivå = NULL, sortert = F) {
  sdf <- sdf %>% mutate({{variabel}} := factor({{variabel}},
                                                 levels = nivå,
                                               ordered = sortert))
  return(sdf)
}

# Kodar om til faktor for å sikre at alle nivå blir med i utskrift
OM_set_faktor <- function(sdf, svar) {
  sdf <- sdf %>% mutate({{svar}} := as.factor({{svar}}))
  return(sdf)
}

# Kodar om til faktor
OM_set_faktor_studerer_niva <- function(sdf, variabel) {
  # sdf$studerer_niva <- factor(sdf$studerer_niva, levels = c("Bachelor", "Master", "Ph.d.", "Annet"))
  # sdf$studerer_niva <- factor(sdf$studerer_niva, levels = c("Bachelor", "Master", "Annet"))
  sdf <- sdf %>% mutate({{variabel}} := factor({{variabel}},
                                               levels = c("Bachelor", "Master", "Annet"))
  )
  return(sdf)
}

##** Kodar om ja/nei/usikker til binærvariabel
##*  Ja = 1
##*  Nei = 0
##*  Usikker = NaN
##*  sdf = dataframe
##*  svar = variabelen som skal tolkast
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
    grepl("Ønsker ikke å svare på dette spørsmålet", {{svar}}) ~ NaN,
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

##** 
##* Kodar om frå tekst til faktor basert på variablar som inneheld tal
##* 
OM_text_to_factor <- function(sdf, innvariabel, nivå = NULL) {
  # if (is.null(utvariabel)) {
  #   utvariabel <- {{innvariabel}}
  # }
  sdf <- sdf %>% mutate({{innvariabel}} := case_when(
    grepl("1", {{innvariabel}}) ~ 1,
    grepl("2", {{innvariabel}}) ~ 2,
    grepl("3", {{innvariabel}}) ~ 3,
    grepl("4", {{innvariabel}}) ~ 4,
    grepl("5", {{innvariabel}}) ~ 5,
    {{innvariabel}} != "" ~ NaN
  ))
  if (is.vector(nivå)) {
    sdf <- sdf %>% mutate({{innvariabel}} := factor({{innvariabel}},
                                                   levels = c(1, 2, 3, 4, 5),
                                                 labels = nivå))
  }
  return(sdf)
}

OM_text_to_factor_nyvar <- function(sdf, innvariabel, nivå = NULL, utvariabel = NULL) {
  sdf <- sdf %>% mutate({{utvariabel}} := case_when(
    grepl("1", {{innvariabel}}) ~ 1,
    grepl("2", {{innvariabel}}) ~ 2,
    grepl("3", {{innvariabel}}) ~ 3,
    grepl("4", {{innvariabel}}) ~ 4,
    grepl("5", {{innvariabel}}) ~ 5,
    {{innvariabel}} != "" ~ NaN
  ))
  if (is.vector(nivå)) {
    sdf <- sdf %>% mutate({{utvariabel}} := factor({{utvariabel}},
                                                   levels = c(1, 2, 3, 4, 5),
                                                   labels = nivå))
  }
  return(sdf)
}
##**
##* Til å lage binær variabel for dei to beste svaralternativa. 
##* Fungerer med across, t.d.:
##* mutate(across(contains("lub_"), OM_svart_fire_eller_fem_bin, .names = "{.col}_topp"))
##* 
OM_svart_fire_eller_fem_bin <- function(innvariabel) {
  case_when(
    grepl("1", {{innvariabel}}) ~ 0,
    grepl("2", {{innvariabel}}) ~ 0,
    grepl("3", {{innvariabel}}) ~ 0,
    grepl("4", {{innvariabel}}) ~ 1,
    grepl("5", {{innvariabel}}) ~ 1,
    {{innvariabel}} != "" ~ NaN
  )
}

##** 
##* Legg til variabel syklus, basert på nivåkode
##* 
OM_set_syklus <- function(sdf, nivåvariabel) {
  sdf <- sdf %>% mutate(Syklus = case_when(
    grepl("B4|HK|YU|AR|HN|LN", {{nivåvariabel}}) ~ "Andre",
    grepl("B3", {{nivåvariabel}}) ~ "Bachelor",
    grepl("M2|ME|M5", {{nivåvariabel}}) ~ "Master",
    grepl("FU", {{nivåvariabel}}) ~ "Forskarutdanning",
    grepl("VS", {{nivåvariabel}}) ~ "Vidaregåande skule-nivå",
    T ~ NA
  ))
  return(sdf)
}

##**
##* Til å lage binær variabel for dei to beste svaralternativa. 
##*
# OM_svart_fire_eller_fem_bin <- function(sdf, innvariabel, utvariabel) {
#   sdf <- sdf %>% mutate({{utvariabel}} := case_when(
#     grepl("1", {{innvariabel}}) ~ 0,
#     grepl("2", {{innvariabel}}) ~ 0,
#     grepl("3", {{innvariabel}}) ~ 0,
#     grepl("4", {{innvariabel}}) ~ 1,
#     grepl("5", {{innvariabel}}) ~ 1,
#     {{innvariabel}} != "" ~ NaN
#   ))
#   return(sdf)
# }

##** 
##* Omkoding Sisteårs
##* 

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

##** Rekodar nokre svar der fritekst viser at dei kunne svart innanfor kategoriane
##* 
##* hovedaktivitet
OM_rekode_2022_hovedaktivitet <- function(sdf) {
  sdf <- sdf %>% mutate(hovedaktivitet = case_when(
    hovedaktivitet_fritekst == "Ansatt fast 100% + student 50%" & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Ansatt i fast full jobb med deltidsstudier i et mastergradsprogramm" & undersokelse_ar == 2019 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Jeg har begynt på en master men har tatt permisjon og jobber nå som pedagogisk leder fast 100 %" & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "50% fast, men søker ny arbeidsplass" & undersokelse_ar == 2019 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Ansatt i fast deltid stilling" & undersokelse_ar == 2019 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Ansatt i fast stilling, men ikke 100%" & undersokelse_ar == 2022 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "er i et 50% vikariat nå, men tilbudt 100% fast stilling på samme avdeling, fra august 2018. har takket ja til denne" & undersokelse_ar == 2018 ~ "Midlertidig",
    hovedaktivitet_fritekst == "Fast ansatt i mindre stillinger" & undersokelse_ar == 2019 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Jobber i en deltidsfast stilling som ikke er relevant for ingeniør utdanningen min." & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Jobber en tilnærmet 80% stilling men har ikke noe fast da jeg ikke er sikker på hva jeg skal eller hvor jeg skal være." & undersokelse_ar == 2018 ~ "Midlertidig",
    hovedaktivitet_fritekst == "30%fast og 70% midlertidig" & undersokelse_ar == 2022 ~ "Midlertidig",
    hovedaktivitet_fritekst == "63% fast og 32% vikariat ( varighet 6mnd eller mer)" & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "70% fast stilling + 100% vikariat ved siden av med varighet over ett år." & undersokelse_ar == 2022 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "80 fast og 20 vikariat" & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Ansatt i fast stilling, men ikke som sykepleier" & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Ansatt i midlertidig stilling, men fast stilling fra høsten" & undersokelse_ar == 2018 ~ "Midlertidig",
    hovedaktivitet_fritekst == "Arbeidsledig" & undersokelse_ar == 2019 ~ "Arbeidssøkende",
    hovedaktivitet_fritekst == "Arbeidsløs" & undersokelse_ar == 2022 ~ "Arbeidssøkende",
    hovedaktivitet_fritekst == "Arbeidssøkende og tilkallingsvikar, timebasert" & undersokelse_ar == 2019 ~ "Arbeidssøkende",
    hovedaktivitet_fritekst == "Arbeidssøkende+selvstendig næringsdrivende, tilkallingsvikar x 2" & undersokelse_ar == 2022 ~ "Arbeidssøkende",
    hovedaktivitet_fritekst == "Fast ansatt  &  selvstendig næringsdrivende" & undersokelse_ar == 2019 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Fast ansatt 100% og fulltids masterstudent" & undersokelse_ar == 2022 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Fast ansatt i tillegg til ekstravakt stilling." & undersokelse_ar == 2022 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Fast ansatt og masterstudent." & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Har både fast stilling 50% og vikariat 25%" & undersokelse_ar == 2022 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Jeg går et masterstudium, som er ferdig nå i juni. Jobber deltid ved siden av." & undersokelse_ar == 2022 ~ "Student",
    hovedaktivitet_fritekst == "Masterstudent (heltid). Studentmedarbeider i kommunikasjon (deltid)." & undersokelse_ar == 2022 ~ "Student",
    hovedaktivitet_fritekst == "Masterstudent med deltidsjobb på sykehus" & undersokelse_ar == 2022 ~ "Student",
    hovedaktivitet_fritekst == "Masterstudent og deltidsansatt" & undersokelse_ar == 2019 ~ "Student",
    hovedaktivitet_fritekst == "Masterstudent ved OsloMet med vikarstilling som lærer ved siden av studiet." & undersokelse_ar == 2022 ~ "Student",
    hovedaktivitet_fritekst == "studerer også en videreutdanning" & undersokelse_ar == 2018 ~ "Student",
    hovedaktivitet_fritekst == "Studerer til å bli sykepleier" & undersokelse_ar == 2022 ~ "Student",
    hovedaktivitet_fritekst == "Sykemeldt fra fast stilling" & undersokelse_ar == 2018 ~ "Ansatt i fast stilling",
    hovedaktivitet_fritekst == "Vikariat og masterstudent" & undersokelse_ar == 2018 ~ "Student",
    hovedaktivitet_fritekst == "Vikariat, men inn i fast stilling høst 2018" & undersokelse_ar == 2018 ~ "Midlertidig",
    hovedaktivitet_fritekst == "Fast ansatt & selvstendig næringsdrivende" & undersokelse_ar == 2019 ~ "Ansatt i fast stilling",
    T ~ hovedaktivitet))
  
  return(sdf)
}

#* stillingsandel
OM_rekode_2022_stillingsandel <- function(sdf) {
  # print(paste("stillingsandel NA", sdf %>% filter(is.na(stillingsandel)) %>% count))
  sdf <- sdf %>% mutate(stillingsandel = case_when(
    hovedaktivitet_fritekst == "Ansatt fast 100% + student 50%" & undersokelse_ar == 2018 ~ 1,
    hovedaktivitet_fritekst == "Ansatt i fast full jobb med deltidsstudier i et mastergradsprogramm" & undersokelse_ar == 2019 ~ 1,
    hovedaktivitet_fritekst == "Jeg har begynt på en master men har tatt permisjon og jobber nå som pedagogisk leder fast 100 %" & undersokelse_ar == 2018 ~ 1,
    hovedaktivitet_fritekst == "50% fast, men søker ny arbeidsplass" & undersokelse_ar == 2019 ~ 0.5,
    hovedaktivitet_fritekst == "Ansatt i fast deltid stilling" & undersokelse_ar == 2019 ~ 0.5,
    hovedaktivitet_fritekst == "Ansatt i fast stilling, men ikke 100%" & undersokelse_ar == 2022 ~ 0.5,
    hovedaktivitet_fritekst == "er i et 50% vikariat nå, men tilbudt 100% fast stilling på samme avdeling, fra august 2018. har takket ja til denne" & undersokelse_ar == 2018 ~ 0.5,
    hovedaktivitet_fritekst == "Fast ansatt i mindre stillinger" & undersokelse_ar == 2019 ~ 0.5,
    hovedaktivitet_fritekst == "Jobber i en deltidsfast stilling som ikke er relevant for ingeniør utdanningen min." & undersokelse_ar == 2018 ~ 0.5,
    hovedaktivitet_fritekst == "Jobber en tilnærmet 80% stilling men har ikke noe fast da jeg ikke er sikker på hva jeg skal eller hvor jeg skal være." & undersokelse_ar == 2018 ~ 0.8,
    T ~ stillingsandel))
  
  return(sdf)
}

#* studerer_niva
OM_rekode_2022_studerer_niva <- function(sdf) {
  sdf <- sdf %>% mutate(studerer_niva = case_when(
    hovedaktivitet_fritekst == "Studerer til å bli sykepleier" & undersokelse_ar == 2022 ~ "Bachelor",
    hovedaktivitet_fritekst == "Jeg går et masterstudium, som er ferdig nå i juni. Jobber deltid ved siden av." & undersokelse_ar == 2022 ~ "Master",
    hovedaktivitet_fritekst == "Masterstudent (heltid). Studentmedarbeider i kommunikasjon (deltid)." & undersokelse_ar == 2022 ~ "Master",
    hovedaktivitet_fritekst == "Masterstudent med deltidsjobb på sykehus" & undersokelse_ar == 2022 ~ "Master",
    hovedaktivitet_fritekst == "Masterstudent og deltidsansatt" & undersokelse_ar == 2019 ~ "Master",
    hovedaktivitet_fritekst == "Masterstudent ved OsloMet med vikarstilling som lærer ved siden av studiet." & undersokelse_ar == 2022 ~ "Master",
    hovedaktivitet_fritekst == "Vikariat og masterstudent" & undersokelse_ar == 2018 ~ "Master",
    studerer_niva == "Ph.D." ~ "Annet",
    T ~ studerer_niva))
  
  return(sdf)
}
#* Ferdig med spesialisert omkoding basert på fritekst

##**
##* Variabellister brukt til å bygge indeksar i Studiebarometeret
##* 2023 - kommenterte ut indx_... desse er ikkje i bruk no
##* 

# Undervisning
# NOKUT-indeks: "indeks_underv_18"
var_underv <- c(
  "underv_engasj_18",
  "underv_formidl_18",
  "underv_pensum_18",
  "underv_aktiv_18"
)
# indx_underv <- c(
#   "indx_underv4"
# )

# Tilbakemelding og veiledning
# NOKUT-indeks: "indeks_tilbveil_16"
var_tilbveil <- c(
  "tilbveil_antall_16",
  "tilbveil_konstru_13",
  "tilbveil_student_18",
  "tilbveil_fagdisk_18"
)
# indx_tilbveil <- c(
#   "indx_tilbveil4"
# )

# Faglig og sosialt læringsmiljø
# NOKUT-indeks: "indeks_psymiljo_15"
var_psymiljo <- c(
  "miljo_sosial_13",
  "miljo_fag_13",
  "miljo_studans_15"
)
# indx_psymiljo <- c(
#   "indx_psymiljo3"
# )

# indeks Fysisk læringsmiljø
# NOKUT-indeks: "indeks_fysmiljo_13"
var_fysmiljo <- c(
  "miljo_lokaler_13",
  "miljo_utstyr_13",
  "miljo_biblio_13",
  "miljo_ikt_13"
)
# indx_fysmiljo <- c(
#   "indx_fysmiljo4"
# )

# Organisering
# NOKUT-indeks: "indeks_organ_17"
var_organ <- c(
  "organ_tilgjinfo_17",
  "organ_kvalinfo_17",
  "organ_admtilr_17",
  "organ_fagligsam_17"
)
# indx_organ <- c(
#   "indx_organ4"
# )

#indeks Vurderingsformer
# NOKUT-indeks: "indeks_vurd_17"
var_vurd <- c(
  "vurd_pensum_13",
  "vurd_forstaelse_13",
  # "vurd_laert_17", # OBS MANGLAR I 2020
  "vurd_kriterier_17",
  "vurd_fagutv_17"
)
# indx_vurd <- c(
#   "indx_vurd5"
# )

# medvirkning
# NOKUT-indeks: "indeks_medvirk_18"
var_medv <- c(
  "medvirk_innspill_18"
  # "medvirk_oppfolg_18",
  # "medvirk_tillitsv_18"
)
# indx_medv <- c(
#   "indx_medv3"
# )

#indeks Inspirerende program
# NOKUT-indeks: "indeks_insp_14"
var_insp <- c(
  "insp_stimul_13",
  "insp_utfordr_13",
  "insp_motivasjon_14"
)
# indx_insp <- c(
#   "indx_insp3"
# )

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
# indx_laerutb <- c(
#   "indx_laerutb10"
# )

# Eget engasjement
# NOKUT-indeks: "indeks_egeteng_14"
var_egeteng <- c(
  "egeteng_motivert_14",
  "egeteng_orgakt_14",
  "egeteng_forberedt_14",
  "egeteng_innsats_14"
)
# indx_egeteng <- c(
#   "indx_eget4"
# )

# Faglig ansattes forventninger
# NOKUT-indeks: "indeks_forvent_16"
var_forvent <- c(
  "forvent_klare_16",
  "forvent_forberedt_16",
  "forvent_deltar_16",
  "forvent_fagamb_16"
)
# indx_forvent <- c(
#   "indx_forvent4"
# )

# Digitale verktøy
var_digitale <- c(
  "digitale_aktivt_18",
  "digitale_kompet_18",
  "digitale_opplaer_18",
  "digitale_laerplatt_18"
)
# indx_digitale <- c(
#   "indx_digit4"
# )

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
# indx_sum_tid <- c(
#   "sum_tid"
# )
# Praksis - endra i 2020
var_praksis <- c(
  "praksis_inf_19", # lagt til 2024 for å levere tal til Styringsportalen
  "praksis_forber_14",
  "praksis_passetinn_19",
  "praksis_veil_20",
  "praksis_laerutb_20",
  "praksis_relevant_19",
  "praksis_grunnlagdisk_19"
)

# Yrkesrelevans
var_yrkrel <- c(
  "yrkrel_bruk_18",
  "yrkrel_bransje_18",
  "yrkrel_formidle_19",
  "yrkrel_motearb_19",
  "yrkrel_bidrar_19",
  "yrkrel_prosjekt_19"
)
# indx_yrkrel <- c(
#   "indx_yrkrel6"
# )

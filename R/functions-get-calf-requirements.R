#' Calculates the calf requirements.
#'
#' @param liq_diet A numeric vector with the liquid diet intake.
#' @param liq_diet_me A numeric vector with the liquid diet metabolizable energy.
#' @param starter_composition A list with the composition of the starter. The list
#' has to contain cs_ndf, cs_nfc, cs_cp, and cs_ee.
#' @param cs_intake_equation is the equation used to calculate the intake of the starter.
#' @param liq_diet_dm A numeric vector with the dry matter content of the liquid diet.
#' @param initial_bw A numeric vector with the initial body weight.
#' @param weaning_age A numeric vector with the weaning age.
#' @param average_temperature A numeric vector with the average temperature.
#' @param liq_diet_only A logical vector indicating if the calf is fed only liquid diet.
#' @param mature_weight A numeric vector with the mature weight.
#' @param max_size A numeric vector with the maximum size of the vectors.
#'
#' @return A dataframe with the calf requirements.
#' @export
#'
#' @examples
#' get_calf_requirements(liq_diet = rep(6, 70),
#'                      liq_diet_me = 4.6,
#'                      starter_composition = list(
#'                        cs_ndf = 12,
#'                        cs_nfc = 50,
#'                        cs_cp = 22,
#'                        cs_ee = 6,
#'                        form_of_starter = "pelleted"),
#'                      cs_intake_equation = "NASEM",
#'                      liq_diet_dm = 0.12,
#'                      initial_bw = 45,
#'                      weaning_age = 70,
#'                      average_temperature = 20,
#'                      liq_diet_only = FALSE,
#'                      mature_weight = 750,
#'                      max_size = 100)
get_calf_requirements <- function(liq_diet           = rep(6, 70),
                                  liq_diet_me        = 4.6,
                                  starter_composition = list(
                                    cs_ndf = 12,
                                    cs_nfc = 50,
                                    cs_cp = 22,
                                    cs_ee = 6,
                                    form_of_starter = "pelleted"),
                                  cs_intake_equation = "NASEM",
                                  liq_diet_dm        = 0.12,
                                  initial_bw           = 45,
                                  weaning_age       = 70,
                                  average_temperature         = 20,
                                  liq_diet_only    = FALSE,
                                  mature_weight    = 750,
                                  max_size         = 100) {

  init_vectors <- c("days_of_life",
                    "liquid_diet_only",
                    "weaned",
                    "temp",
                    "liq_diet_all",
                    "liq_diet_intake",
                    "BW",
                    "BW_cor",
                    "EBW",
                    "me_from_liq_diet",
                    "FPstarter",
                    "me_gap",
                    "starter_intake",
                    "me_gap",
                    "Quigley",
                    "NASEM",
                    "silva2019",
                    "nfc_intake_cum",
                    "me_calf_starter",
                    "me_from_starter_intake",
                    "total_dmi",
                    "total_me_intake",
                    "liq_diet_ratio",
                    "me_temperature",
                    "me_maintenance",
                    "me_balance",
                    "me_ef_starter",
                    "me_ef_liq_diet",
                    "average_me_ef",
                    "net_energy_gain",
                    "EBW_gain",
                    "ADG",
                    "fat_gain",
                    "protein_gain",
                    "scurf_cp",
                    "urinary_cp",
                    "fecal_cp",
                    "met_protein_maintenance",
                    "net_protein_gain",
                    "ef_met_protein_gain",
                    "met_protein_gain",
                    "total_met_protein_req",
                    "ef_met_protein_to_cp",
                    "total_cp_req",
                    "diet_cp_pct"
  )

  for (i in init_vectors) {

    assign(i, vector(mode = "logical", length = max_size))

  }

  days_of_life[1] <- 1

  liquid_diet_only[1] <- liq_diet_only[1]

  weaned[1] <- FALSE

  temp[1] <- average_temperature

  BW[1] <- initial_bw

  EBW[1] <- empty_body_weight(BW[1], liquid_diet_only = liquid_diet_only[1], weaned = weaned[1])

  liq_diet_all[1] <- liq_diet[1] # LD[1]

  liq_diet_intake[1] <- liq_diet_all[1] * liq_diet_dm

  me_from_liq_diet[1] <- liq_diet_intake[1] * liq_diet_me

  FPstarter[1] <- 1 / 7

  #starter_intake[1] <- starter_intake_nasem(BW = BW[1], MEiLD = me_from_liq_diet[1], FPstarter = FPstarter[1], temperature = average_temperature)

  #me_gap[1] <- me_gap(BW = BW[1], ADG = ADG[1], MEiLD = me_from_liq_diet[1])

  #Quigley[1] <- starter_intake_quigley(me_gap = me_gap[1], age = 1, temperature = average_temperature, NDFDM = 18.3, PctForage = 0.6)

  NASEM[1] <- starter_intake_nasem(BW = BW[1], MEiLD = me_from_liq_diet[1], FPstarter = FPstarter[1], temperature = average_temperature)

  silva2019[1] <- starter_intake_silva2019(milk_intake = liq_diet_all[1], age = 1)

  starter_intake[1] <- ifelse(liquid_diet_only[1] == TRUE & weaned[1] == FALSE, 0,
                             ifelse(cs_intake_equation == "NASEM", NASEM[1], silva2019[1]))

  nfc_intake_cum[1] <- starter_intake[1] * starter_composition$cs_nfc / 100


  #me_calf_starter[1] <- me_calf_starter(ccsNFCI = nfc_intake_cum[1], pelleted = TRUE, texturized = TRUE) #TO DO

  me_calf_starter[1] <- starter_met_energy(cs_ndf = starter_composition$cs_ndf,
                                cs_nfc = starter_composition$cs_nfc,
                                cs_cp = starter_composition$cs_cp,
                                cs_ee = starter_composition$cs_ee,
                                form_of_starter = starter_composition$form_of_starter,
                                nfc_intake = nfc_intake_cum[1])


  me_from_starter_intake[1] <- starter_intake[1] * me_calf_starter[1] #solDietME

  total_dmi[1] <- liq_diet_intake[1] + starter_intake[1]

  total_me_intake[1] <- me_from_liq_diet[1] + me_from_starter_intake[1]

  liq_diet_ratio[1] <- liq_diet_intake[1] / (liq_diet_intake[1] + starter_intake[1])

  # NEtemp

  me_temperature[1] <- NEtemperature(EBW = EBW[1], age = days_of_life[1], temperature = average_temperature)

  me_maintenance[1] <- NEm(EBW[1], F) / km(liquid_diet_only = liquid_diet_only[1]) / 1000 + (me_temperature[1] / km(liquid_diet_only = liquid_diet_only[1]))

  me_balance[1] <- total_me_intake[1] - me_maintenance[1]

  me_ef_starter[1] <- kg_solids(ME = me_calf_starter[1]) #solDietME)

  me_ef_liq_diet[1] <- 0.55

  average_me_ef[1] <- (me_ef_starter[1] * (1 - liq_diet_ratio[1]) + (me_ef_liq_diet[1] * liq_diet_ratio[1]))

  net_energy_gain[1] <- me_balance[1] * average_me_ef[1]

  EBW_gain[1] <- empty_body_weight_gain_from_re(retained_energy = net_energy_gain[1], EBW = EBW[1])

  ADG[1] <- ADG_from_empty_gain_weight(EGW = EBW_gain[1], liquid_diet_only = liquid_diet_only[1], weaned = weaned[1])

  BW_cor[1] <- BW[1] + ADG[1]

  fat_gain[1] <- gain_composition(retained_energy = net_energy_gain[1])[[1]]

  protein_gain[1] <- gain_composition(retained_energy = net_energy_gain[1])[[2]]

  # Protein requirements

  scurf_cp[1] <- scurf_cp(BW = BW[1])

  urinary_cp[1] <- endogenous_urinary_cp(BW = BW[1])

  fecal_cp[1] <- metabolic_fecal_cp(liq_diet_dmi = liq_diet_intake[1], solid_diet_dmi = starter_intake[1])

  met_protein_maintenance[1] <- MPmain(scurf_cp = scurf_cp[1], endogenous_urinary_cp = urinary_cp[1], metabolic_fecal_cp = fecal_cp[1])

  net_protein_gain[1] <- net_protein_gain(EBW_gain = EBW_gain[1], retained_energy = net_energy_gain[1])

  ef_met_protein_gain[1] <- ef_met_protein_gain(BW = BW[1], mature_weight = mature_weight)

  met_protein_gain[1] <- net_protein_gain[1] / ef_met_protein_gain[1]

  total_met_protein_req[1] <- met_protein_maintenance[1] + met_protein_gain[1]

  ef_met_protein_to_cp[1] <- liq_diet_ratio[1] * 0.95 + (1 - liq_diet_ratio[1]) * 0.75

  total_cp_req[1] <- total_met_protein_req[1] / ef_met_protein_to_cp[1]

  diet_cp_pct[1] <- total_cp_req[1] / 10 / total_dmi[1]

  for (i in 2:max_size) {

    days_of_life[i] <- i

    liquid_diet_only[i] <- liq_diet_only[1]

    weaned[i] <- ifelse(i <= weaning_age, FALSE, TRUE)

    temp[i] <- average_temperature

    BW[i] <- BW[i-1] + ADG[i-1]

    EBW[i] <- empty_body_weight(BW[i], liquid_diet_only = liquid_diet_only[i], weaned = weaned[i])

    liq_diet_all[i] <- liq_diet[i] #LD[i]

    liq_diet_intake[i] <- ifelse(i <= weaning_age, liq_diet_all[i] * liq_diet_dm, 0)

    me_from_liq_diet[i] <- liq_diet_intake[i] * liq_diet_me

    FPstarter[i] <- i / 7

    #starter_intake[i] <- starter_intake_nasem(BW = BW[i], MEiLD = me_from_liq_diet[i], FPstarter = FPstarter[i], temperature = average_temperature)

    #me_gap[i] <- me_gap(BW = BW[i], ADG = ADG[i], MEiLD = me_from_liq_diet[i])

    #Quigley[i] <- starter_intake_quigley(me_gap = me_gap[i], age = i, temperature = average_temperature, NDFDM = 18.3, PctForage = 0.6)

    NASEM[i] <- starter_intake_nasem(BW = BW[i], MEiLD = me_from_liq_diet[i], FPstarter = FPstarter[i], temperature = average_temperature)

    silva2019[i] <- starter_intake_silva2019(milk_intake = liq_diet_all[i], age = i)

    starter_intake[i] <- ifelse(liquid_diet_only[i] == TRUE & weaned[i] == FALSE, 0,
                               ifelse(cs_intake_equation == "NASEM", NASEM[i], silva2019[i]))

    #me_from_starter_intake[i] <- starter_intake[i] * solDietME

    nfc_intake_cum[i] <- sum(starter_intake[1:i]) * starter_composition$cs_nfc / 100


    #me_calf_starter[i] <- me_calf_starter(ccsNFCI = nfc_intake_cum[i], pelleted = TRUE, texturized = TRUE)

    me_calf_starter[i] <- starter_met_energy(cs_ndf = starter_composition$cs_ndf,
                                  cs_nfc = starter_composition$cs_nfc,
                                  cs_cp = starter_composition$cs_cp,
                                  cs_ee = starter_composition$cs_ee,
                                  form_of_starter = starter_composition$form_of_starter,
                                  nfc_intake = nfc_intake_cum[i])




    me_from_starter_intake[i] <- starter_intake[i] * me_calf_starter[i] #solDietME

    total_dmi[i] <- liq_diet_intake[i] + starter_intake[i]

    total_me_intake[i] <- me_from_liq_diet[i] + me_from_starter_intake[i]

    liq_diet_ratio[i] <- liq_diet_intake[i] / (liq_diet_intake[i] + starter_intake[i])

    # NEtemp

    me_temperature[i] <- NEtemperature(EBW = EBW[i], age = days_of_life[i], temperature = average_temperature)

    me_maintenance[i] <- NEm(EBW[i], F) / km(liquid_diet_only = liquid_diet_only[i]) / 1000 + (me_temperature[i] / km(liquid_diet_only = liquid_diet_only[i]))

    me_balance[i] <- total_me_intake[i] - me_maintenance[i]

    me_ef_starter[i] <- kg_solids(ME = me_calf_starter[i]) #solDietME)

    me_ef_liq_diet[i] <- 0.55

    average_me_ef[i] <- (me_ef_starter[i] * (1 - liq_diet_ratio[i]) + (me_ef_liq_diet[i] * liq_diet_ratio[i]))

    net_energy_gain[i] <- me_balance[i] * average_me_ef[i]

    EBW_gain[i] <- empty_body_weight_gain_from_re(retained_energy = net_energy_gain[i], EBW = EBW[i])

    ADG[i] <- ADG_from_empty_gain_weight(EGW = EBW_gain[i], liquid_diet_only = liquid_diet_only[i], weaned = weaned[i])

    BW_cor[i] <- BW[i] + ADG[i]

    fat_gain[i] <- gain_composition(retained_energy = net_energy_gain[i])[[1]]

    protein_gain[i] <- gain_composition(retained_energy = net_energy_gain[i])[[2]]

    # Protein requirements

    scurf_cp[i] <- scurf_cp(BW = BW[i])

    urinary_cp[i] <- endogenous_urinary_cp(BW = BW[i])

    fecal_cp[i] <- metabolic_fecal_cp(liq_diet_dmi = liq_diet_intake[i], solid_diet_dmi = starter_intake[i])

    met_protein_maintenance[i] <- MPmain(scurf_cp = scurf_cp[i], endogenous_urinary_cp = urinary_cp[i], metabolic_fecal_cp = fecal_cp[i])

    net_protein_gain[i] <- net_protein_gain(EBW_gain = EBW_gain[i], retained_energy = net_energy_gain[i])

    ef_met_protein_gain[i] <- ef_met_protein_gain(BW = BW[i], mature_weight = mature_weight)

    met_protein_gain[i] <- net_protein_gain[i] / ef_met_protein_gain[i]

    total_met_protein_req[i] <- met_protein_maintenance[i] + met_protein_gain[i]

    ef_met_protein_to_cp[i] <- liq_diet_ratio[i] * 0.95 + (1 - liq_diet_ratio[i]) * 0.75

    total_cp_req[i] <- total_met_protein_req[i] / ef_met_protein_to_cp[i]

    diet_cp_pct[i] <- total_cp_req[i] / 10 / total_dmi[i]

  }

  return(
    tibble::tibble(
      days_of_life,
      liquid_diet_only,
      weaned,
      temp,
      BW,
      BW_cor,
      EBW,
      liq_diet_all,
      liq_diet_intake,
      me_from_liq_diet,
      FPstarter,
      #Quigley,
      NASEM,
      silva2019,
      starter_intake,
      #me_gap,
      nfc_intake_cum,
      me_calf_starter,
      me_from_starter_intake,
      total_dmi,
      total_me_intake,
      liq_diet_ratio,
      me_temperature,
      me_maintenance,
      me_balance,
      me_ef_starter,
      me_ef_liq_diet,
      average_me_ef,
      net_energy_gain,
      EBW_gain,
      ADG,
      fat_gain,
      protein_gain,
      scurf_cp,
      urinary_cp,
      fecal_cp,
      met_protein_maintenance,
      net_protein_gain,
      ef_met_protein_gain,
      met_protein_gain,
      total_met_protein_req,
      ef_met_protein_to_cp,
      total_cp_req,
      diet_cp_pct
    )
  )

}

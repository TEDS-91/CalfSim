#' NASEM (2021) equation to predict the calf empty body weight (kg) - Table 10-1.
#'
#' @param BW Body weight (kg).
#' @param liquid_diet_only Argument to define whether the calves have only milk available or also forage.
#' @param weaned Argument to define whether the calves are weaned or not.
#'
#' @return The Empty body weight (kg).
#' @export
#'
#' @examples
#' empty_body_weight(BW = 45, liquid_diet_only = TRUE, weaned = FALSE)
empty_body_weight <- function(BW               = 45,
                              liquid_diet_only = TRUE,
                              weaned           = FALSE ) {

  if(weaned == FALSE & liquid_diet_only == TRUE) {

    EBW <- 0.94 * BW

  } else if (weaned == FALSE & liquid_diet_only == FALSE) {

    EBW <- 0.93 * BW

  } else {

    EBW <- 0.85 * BW

  }

  return(round(EBW, 3))

}

#' NASEM (2021) equation to predict the calf empty body gain.
#'
#' @param ADG Average daily gain (kg).
#' @param liquid_diet_only Argument to define whether the calves have only milk available or also forage.
#' @param weaned Argument to define whether the calves are weaned or not.
#'
#' @return The empty body gain (kg).
#' @export
#'
#' @examples
#' empty_weight_gain(ADG = 0.450, liquid_diet_only = TRUE, weaned = FALSE)
empty_weight_gain <- function(ADG              = 0.450,
                              liquid_diet_only = TRUE,
                              weaned           = FALSE ) {

  if(weaned == FALSE & liquid_diet_only == TRUE) {

    EGW <- ADG * 0.94

  } else if (weaned == FALSE & liquid_diet_only == FALSE) {

    EGW <- ADG * 0.91

  } else {

    EGW <- ADG * 0.85

  }

  return(round(EGW, 3))

}

#' Average daily gain calculated from the empty weight gain.
#'
#' @param EGW Empty weight gain (kg).
#' @param liquid_diet_only Argument to define whether the calves have only milk available or also forage.
#' @param weaned Argument to define whether the calves are weaned or not.
#'
#' @return The ADG from empty weight gain.
#' @export
#'
ADG_from_empty_gain_weight <- function(EGW              = 0.450,
                                       liquid_diet_only = TRUE,
                                       weaned           = FALSE ) {

  if(weaned == FALSE & liquid_diet_only == TRUE) {

    ADG <- EGW / 0.94

  } else if (weaned == FALSE & liquid_diet_only == FALSE) {

    ADG <- EGW / 0.91

  } else {

    ADG <- EGW / 0.85

  }

  return(round(ADG, 3))

}

#' NASEM (2021) equation to predict the efficiency of use of the metabolizable energy.
#'
#' @param ME Metabolizable energy (Mcal/kg DM).
#' @param liquid_diet_only Argument to define whether the calves have only milk available or also forage.
#' @param weaned Argument to define whether the calves are weaned or not.
#'
#' @return The efficiency of use of the ME.
#' @export
#'
#' @examples
#' km(ME = 3.5, liquid_diet_only = TRUE, weaned = FALSE)
km <- function(ME               = 3.5,
               liquid_diet_only = TRUE,
               weaned           = FALSE ) {

  if(weaned == FALSE & liquid_diet_only == TRUE) {

    km <- 0.72

  } else if (weaned == FALSE & liquid_diet_only == FALSE) {

    km <- 0.69

  } else {

    # efficiency of use of ME - Galyean et al. (2016) - NASEM (2021)

    km <- ( (1.1104 * ME) - (0.0946 * ME ^ 2) + (0.0065 * ME ^ 3) - 0.7783 ) / ME

  }

  return(round(km, 2))

}

#' NASEM (2021) equation to predict the energy requirements for maintenance (Mcal/day).
#'
#' @param EBW Empty body weight (kg).
#' @param weaned Argument to define whether the calves are weaned or not.
#'
#' @return The energy requirement for maintenance (Mcal/day).
#' @export
#'
#' @examples
#' NEm(EBW = 50, weaned = FALSE)
NEm <- function(EBW    = 50,
                weaned = FALSE) {

  if(weaned == FALSE) {

    NEm <- EBW^0.75 * 76.9

  } else {

    NEm <- EBW^0.75 * 97

  }

  return(round(NEm, 3))

}

#' NASEM (2021) equation to predict the additional maintenance energy necessary
#' for either heat or cold stress abatement (Mcal/day).
#'
#' @param EBW Empty body weight (kg).
#' @param age Age (days).
#' @param temperature Average temperature (°C).
#'
#' @return The additional maintenance energy for heat or clod stress abatement (Mcal/day).
#' @export
#'
#' @examples
#' NEtemperature(EBW = 41.85, age = 1, temperature = 10)
NEtemperature <- function(EBW         = 42.3,
                          age         = 15,
                          temperature = 12) {

  if(age <= 21 & temperature < 15) {

    NE <- (15 - temperature) * 2.01 * EBW^0.75

  } else if(age <= 21 & temperature > 25) {

    NE <- (temperature - 25) * 2.01 * EBW^0.75

  } else if(age > 21 & temperature <= 5) {

    NE <- (5 - temperature) * 2.01 * EBW^0.75

  } else if(age > 21 & temperature > 25) {

    NE <- (temperature - 25) * 2.01 * EBW^0.75

  } else {

    NE <- 0

  }

  return(round(NE / 1000, 3))

}

#' NASEM (2021) equation to predict the efficiency of use of the ME from the starter.
#'
#' @param ME Metabolizable energy (Mcal).
#'
#' @return Efficiency of use of the ME from starter.
#' @export
#'
#' @examples
#' kg_solids(ME = 3)
kg_solids <- function(ME = 3) {

  kg_solids <- ((1.1376 * ME) - (0.1198 * ME ^ 2) + (0.0076 * ME ^ 3) - 1.2979) / ME

  return(round(kg_solids, 3))

}

#' NASEM (2021) equation tom predict the empty body weight gain based on the retained energy available.
#'
#' @param retained_energy Retained energy (Mcal).
#' @param EBW Empty body weight (kg)
#'
#' @return The empty body weight gain based on the RE.
#' @export
#'
#' @examples
#' empty_body_weight_gain_from_re(retained_energy = 1, EBW = 45)
empty_body_weight_gain_from_re <- function(retained_energy = 0.96,
                                           EBW             = 42.3) {

  EBWG <- ifelse(retained_energy < 0, -( abs(retained_energy) / (EBW ^ 0.205) ) ^ (1 / 1.1), ( abs(retained_energy) / (EBW ^ 0.205) ) ^ (1 / 1.1))

  return(EBWG)

}

#' Title
#'
#' @param ccsNFCI
#' @param pelleted
#' @param texturized
#'
#' @return
#' @export
#'
#' @examples
MEcs <- function(ccsNFCI = 1.7,
                 pelleted = TRUE,
                 texturized = TRUE) {

  MEcs_exp <- 1.6230 + 1.7683 * (1 - exp(-(0.1677 * ccsNFCI)))

  texturized <- ifelse(texturized == TRUE, 1, 0)

  pelleted <- ifelse(pelleted == TRUE, 1, 0)

  # MEcs_lin <- 2.3186 + 0.2297 * log(ccsNFCI) - 0.6628 * pelleted - 0.2708 * texturized + 0.2960 * pelleted * log(ccsNFCI) + 0.2093 * texturized * log(ccsNFCI)
  #
  # MEcs <- ifelse(MEcs_lin <= 0, 0, MEcs_lin)

  return(MEcs_exp)

}


#' Estimates the calf calcium adequate intakes (AIs) - NASEM (2021) Eq. 10-19.
#'
#' @param EBW Empty body weight (kg)
#' @param EBG Empty body gain (day)
#'
#' @return Ca adequate intakes (g/day).
#' @export
#'
#' @examples
#' get_ca_req(EBW = 65, EBG = 0.45)
get_ca_req <- function(EBW = 65,
                       EBG = 0.45) {

  ca <- (0.0127 * EBW + (14.4 * EBW ^ -0.139 * EBG )) / 0.73

  return(ca)

}

#' Estimates the calf phosphorus adequate intakes (AI) - NASEM (2021) Eq. 10-20.
#'
#' @param EBW Empty body weight (kg).
#' @param EBG Empty body gain (day).
#'
#' @return P adequate intakes (g/day).
#' @export
#'
#' @examples
#' get_p_req(EBW = 65, EBG = 0.45)
get_p_req <- function(EBW = 65,
                      EBG = 0.45) {

  p <- (0.0118 * EBW + (5.85 * EBW ^ -0.027 * EBG )) / 0.65

  return(p)

}

#' Estimates the calf magnesium adequate intakes (AI) - NASEM (2021) Eq. 10-21.
#'
#' @param EBW Empty body weight (kg).
#' @param EBG Empty body gain (day).
#'
#' @return Mg adequate intakes (g/day).
#' @export
#'
#' @examples
#' get_mg_req(EBW = 65, EBG = 0.45)
get_mg_req <- function(EBW = 65,
                       EBG = 0.45) {

  mg <- (0.0035 * EBW + (0.60 * EBW ^ -0.036 * EBG )) / 0.30

  return(mg)

}

#' Estimates the calf potassium adequate intakes (AI) - NASEM (2021) Eq. 10-22.
#'
#' @param EBW Empty body weight (kg).
#' @param EBG Empty body gain (day).
#'
#' @return K adequate intakes (g/day).
#' @export
#'
#' @examples
#' get_k_req(EBW = 65, EBG = 0.45)
get_k_req <- function(EBW = 65,
                      EBG = 0.45) {

  k <- (0.0203 * EBW + (1.14 * EBW ^ -0.048 * EBG )) / 0.13

  return(k)

}

#' Estimates the calf sodium adequate intakes (AI) - NASEM (2021) Eq. 10-23.
#'
#' @param EBW Empty body weight (kg).
#' @param EBG Empty body gain (day).
#'
#' @return Na adequate intakes (g/day).
#' @export
#'
#' @examples
#' get_na_req(EBW = 65, EBG = 0.45)
get_na_req <- function(EBW = 65,
                       EBG = 0.45) {

  na <- (0.00637 * EBW + (1.508 * EBW ^ -0.045 * EBG )) / 0.24

  return(na)

}

#' Estimates the calf sulfur adequate intakes (AI) - NASEM (2021) Eq. 10-24.
#'
#' @param BW Body weight (kg).
#' @param ADG Average daily gain (kg).
#'
#' @return S adequate intakes (mg/day).
#' @export
#'
#' @examples
#' get_cu_req(BW = 65, ADG = 0.45)
get_cu_req <- function(BW = 65,
                       ADG = 0.45) {

  cu <- (0.0145 * BW + 2.5 * ADG) / 0.5

  return(cu)

}

#' Estimates the calf iron adequate intakes (AI) - NASEM (2021) Eq. 10-25.
#'
#' @param ADG Average daily gain (kg).
#'
#' @return Fe adequate intakes (mg/day).
#' @export
#'
#' @examples
#' get_fe_req(ADG = 0.5)
get_fe_req <- function(ADG = 0.5) {

  fe <- (34 * ADG) / 0.25

  return(fe)

}

#' Estimates the calf manganese adequate intakes (AI) - NASEM (2021) Eq. 10-26.
#'
#' @param BW Body weight (kg).
#' @param ADG Average daily gain (kg).
#'
#' @return Mn adequate intakes (mg/day).
#' @export
#'
#' @examples
#' get_mn_req(BW = 65, ADG = 0.45)
get_mn_req <- function(BW = 65,
                       ADG = 0.45) {

  mn <- (0.0026 * BW + 0.7 * ADG) / 0.01

  return(mn)

}

#' Estimates the calf zinc adequate intakes (AI) - NASEM (2021) Eq. 10-27.
#'
#' @param DMI Dry matter intake (kg/day).
#' @param ADG Average daily gain (kg).
#'
#' @return Zn adequate intakes (mg/day).
#' @export
#'
#' @examples
#' get_zn_req(DMI = 65, ADG = 0.45)
get_zn_req <- function(DMI = 65,
                       ADG = 0.45) {

  zn <- (2 * DMI + 24 * ADG) / 0.25

  return(zn)

}

#' NASEM (2021) equation to predict the allowance for scurf - Eq. 10-10.
#'
#' @param BW Body weight (kg).
#'
#' @return The allowance for scurf (hair, skin, secretions, etc.; g/day).
#' @export
#'
#' @examples
#' scurf_cp(BW = 45)
scurf_cp <- function(BW = 40) {

  scurf_cp <- 0.22 * BW ^ 0.6

  return(round(scurf_cp, 3))

}

#' NASEM (2021) equation to predict the endogenous urinary crude protein (CP) losses - Eq. 10-11.
#'
#' @param BW Body weight (kg).
#'
#' @return Endogenous urinary CP losses (g/d).
#' @export
#'
#' @examples
#' endogenous_urinary_cp(BW = 45)
endogenous_urinary_cp <- function(BW = 40) {

  endogenous_urinary_cp <- 2.75 * BW ^ 0.50

  return(round(endogenous_urinary_cp, 3))

}

#' NASEM (2021) equation to predict the metabolic fecal crude protein (CP) losses - Eq. 10-12.
#'
#' @param liq_diet_dmi Liquid feed dry matter intake (DMI; kg/d).
#' @param solid_diet_dmi Solid feed dry matter intake (DMI; kg/d).
#'
#' @return Metabolic fecal CP losses (g/d).
#' @export
#'
#' @examples
#' metabolic_fecal_cp(liq_diet_dmi   = 0.7, solid_diet_dmi = 0.4)
metabolic_fecal_cp <- function(liq_diet_dmi   = 0.7,
                               solid_diet_dmi = 0.4) {

  metabolic_fecal_cp <- 11.9 * liq_diet_dmi + 20.6 * solid_diet_dmi

  return(round(metabolic_fecal_cp, 3))

}

#' NASEM (2021) equation to predict the total maintenance net protein (NP) requirement - Eq. 10-13.
#'
#' @param scurf_cp Total scurf CP losses (g/d).
#' @param endogenous_urinary_cp Endogenous urinary CP losses (g/d).
#' @param metabolic_fecal_cp Metabolic fecal CP losses (g/d).
#'
#' @return The total maintenance net protein (NP) requirement (g/d)
#' @export
#'
#' @examples
#' MPmain(scurf_cp = 2, endogenous_urinary_cp = 17, metabolic_fecal_cp  = 17)
MPmain <- function(scurf_cp              = 2,
                   endogenous_urinary_cp = 17,
                   metabolic_fecal_cp    = 17) {

  MPmain <- endogenous_urinary_cp + ((scurf_cp + metabolic_fecal_cp) / 0.68)

  return(round(MPmain, 3))

}

#' NASEM (2021) equation to predict the net protein requirement for growth - Eq. 10-14.
#'
#' @param EBW_gain Empty body weight gain (kg/d).
#' @param retained_energy retained energy (Mcal/d).
#'
#' @return Net protein requirement for growth (g/d).
#' @export
#'
#' @examples
#' net_protein_gain(EBW_gain = 0.4, retained_energy = 0.9)
net_protein_gain <- function(EBW_gain         = 0.4,
                   retained_energy = 0.9) {

  net_protein_gain <- (166.2 * EBW_gain) + (6.1276 * (retained_energy / EBW_gain))

  return(round(net_protein_gain, 3))

}

#' NASEM (2021) equation to predict the efficiency of converting metabolizable protein for gain (MPg) to net protein (NPg) - 10-15.
#'
#' @param BW Body weight (kg).
#' @param mature_weight Mature weight (kg).
#'
#' @return The efficiency of converting MPg to NPg.
#' @export
#'
#' @examples
#' ef_met_protein_gain(BW = 0.16, mature_weight = 0.29)
ef_met_protein_gain <- function(BW            = 50,
                     mature_weight = 700) {

  ef_met_protein_gain <- 0.7 - 0.532 * BW / mature_weight

  return(round(ef_met_protein_gain, 3))

}

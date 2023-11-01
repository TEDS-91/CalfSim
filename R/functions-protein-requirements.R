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
#' @param liqDietDMI Liquid feed dry matter intake (DMI; kg/d).
#' @param solidDietDMI Solid feed dry matter intake (DMI; kg/d).
#'
#' @return Metabolic fecal CP losses (g/d).
#' @export
#'
#' @examples
#' metabolic_fecal_cp(liqDietDMI   = 0.7, solidDietDMI = 0.4)
metabolic_fecal_cp <- function(liqDietDMI   = 0.7,
                               solidDietDMI = 0.4) {

  metabolic_fecal_cp <- 11.9 * liqDietDMI + 20.6 * solidDietDMI

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
#' @param EBWgain Empty body weight gain (kg/d).
#' @param retained_energy retained energy (Mcal/d).
#'
#' @return Net protein requirement for growth (g/d).
#' @export
#'
#' @examples
#' NPgain(EBWgain = 0.4, retained_energy = 0.9)
NPgain <- function(EBWgain         = 0.4,
                   retained_energy = 0.9) {

  NPgain <- (166.2 * EBWgain) + (6.1276 * (retained_energy / EBWgain))

  return(round(NPgain, 3))

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
#' efMPgain(BW = 0.16, mature_weight = 0.29)
efMPgain <- function(BW            = 50,
                     mature_weight = 700) {

  efMPgain <- 0.7 - 0.532 * BW / mature_weight

  return(round(efMPgain, 3))

}

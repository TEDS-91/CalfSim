
#' The NASEM (2021) equation to predict the average starter intake of calves - Eq. 10-1 and 10-2.
#'
#' @param BW Body weight (kg).
#' @param MEiLD Metabolizable energy content of the liquid diet (Mcal/kgDM).
#' @param FPstarter Week relative the first week of starter offering (weeks).
#' @param temperature Average temperature (°C).
#'
#' @return The expected average starter intake of a calf (kg).
#' @export
#'
#' @examples
#' starter_intake_nasem(BW = 50, FPstarter = 5, temperature = 20, MEiLD = 2.76)
starter_intake_nasem <- function(BW          = 45,
                                 MEiLD       = 2.76,
                                 FPstarter   = 2,
                                 temperature = 30) {

  if(temperature <= 35) {

    CSintake <- (-652.525 + (BW * 14.734) + (MEiLD * 18.896) + (FPstarter * 73.303) + (FPstarter ^ 2 * 13.496) - (29.614 * FPstarter * MEiLD)) / 1000

  } else {

    CSintake <- (600.053 * (1 + 14863.651 * (exp(-1.553 * FPstarter)))^-1 + (9.951 * BW) - (130.434 * MEiLD)) / 1000

  }

  CSintake <- ifelse(CSintake < 0, 0, CSintake)

  return(round(CSintake, 3))

}

#' The Silva et al. (2019) equation to predict average starter intake of calves (kg).
#'
#' @param milk_intake Milk intake (l).
#' @param age Calf age (days).
#' @seealso The equation used here is also available in \href{https://www.sciencedirect.com/science/article/pii/S1751731118000666}{Silva et al. (2019)}.
#' @return The expected average starter intake of a calf (kg).
#' @export
#'
#' @examples
#' starter_intake_silva2019(milk_intake = 5, age = 30)
starter_intake_silva2019 <- function(milk_intake = 5,
                                     age = 30) {

  starter_intake <- ifelse(milk_intake <= 5,
                           0.1839 * milk_intake * exp((0.0333 - 0.004 * milk_intake) * (age - (0.8302 + 6.0332 * milk_intake))) - (0.12 * milk_intake),
                           0.1225 * milk_intake * exp((0.02117 - 0.0015 * milk_intake) * (age - (3.5382 + 1.9508 * milk_intake))) - (0.12 * milk_intake))

  starter_intake <- ifelse(starter_intake < 0, 0, starter_intake)

  return(starter_intake)

}


#' The NASEM (2021) equation to predict the average starter intake of calves.
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

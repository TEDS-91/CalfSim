#' NASEM (2021) equation to predict the milk metabolizable energy - Eq. 10-16.
#'
#' @param protein Milk protein content (\%; as is).
#' @param fat Milk fat content (\%; as is).
#' @param ashes Milk ashes content (\%; as is).
#' @param total_solids Milk total solids (\%).
#'
#' @return Milk metabolizable energy (Mcal/l).
#' @export
#'
#' @examples
#' milk_metabolizable_energy(protein = 3.18, fat = 3.85, ashes = 0.787, total_solids = 12.5)
milk_metabolizable_energy <- function(protein = 3.2,
                                      fat = 3.8,
                                      ashes = 1,
                                      total_solids = 12) {

  fatty_acids <- fat * 0.945 / (total_solids / 100) # NASEM (2021): factor to convert fat to fatty acids: 0.945

  protein <- protein / (total_solids / 100)

  ashes <- ashes / (total_solids / 100)

  metab_energy <- ((fatty_acids * 9.4) + (protein * 5.7) + ((100 - protein - fatty_acids - ashes) * 4)) / 100 * 0.93 # 0.93 = 0.97 * 0.96

  return(round(metab_energy, 2))

}

#' NASEM (2021) equation to predict the milk replacer metabolizable energy - Eq. 10-16.
#'
#' @param protein Milk protein content (\%; DM).
#' @param fat Milk fat content (\%; DM).
#' @param ashes Milk ashes content (\%; DM).
#' @param total_solids Milk total solids (\%; DM).
#'
#' @return Milk replacer metabolizable energy (Mcal/l).
#' @export
#'
#' @examples
#' milk_replacer_metabolizable_energy(protein = 21.7, fat = 18.6, ashes = 7.3, total_solids = 96.8)
milk_replacer_metabolizable_energy <- function(protein = 21.7,
                                               fat = 18.6,
                                               ashes = 7.3,
                                               total_solids = 96.8) {

  fatty_acids <- fat * 0.945 / (total_solids / 100) # NASEM (2021): factor to convert fat to fatty acids: 0.945

  protein <- protein / (total_solids / 100)

  ashes <- ashes / (total_solids / 100)

  metab_energy <- ((fatty_acids * 9.4) + (protein * 5.7) + ((100 - protein - fatty_acids - ashes) * 4)) / 100 * 0.91 # 0.91 = 0.95 * 0.96

  return(round(metab_energy, 2))

}

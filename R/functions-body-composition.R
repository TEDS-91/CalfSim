#' NASEM (2021) equation to predict the gain composition of dairy calves.
#'
#' @param retained_energy Retained energy (Mcal/d)
#'
#' @return A list with the predicted energy gained in the form of fat and protein in the gain.
#' @export
#'
#' @examples
#' gain_composition(retained_energy = 1.45)
gain_composition <- function(retained_energy = 1.456) {

  fat <- 0.0786 + 0.037 * retained_energy

  protein <- 0.191 - 0.0071 * retained_energy

  return(
    list(
      fat_energy = round(fat, 5),
      protein_energy = round(protein, 5)
    )
  )

}

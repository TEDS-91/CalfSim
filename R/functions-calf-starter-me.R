
#' Metabolizable energy of calf starter (Mcal/kg DM).
#'
#' @param cs_ndf Neutral detergent fiber of calf starter (\% DM).
#' @param cs_nfc Non-fiber carbohydrates of calf starter (\% DM).
#' @param cs_cp Crude protein of calf starter (\% DM).
#' @param cs_ee Ether extract of calf starter (\% DM).
#' @param NFCint Cumulative intake of non-fiber carbohydrates (\% DM).
#' @param form_of_starter Form of calf starter (pelleted or texturized).
#'
#' @return Metabolizable energy of calf starter (Mcal/kg DM).
#' @export
#'
#' @examples
#' starter_met_energy(cs_ndf = 12,
#'                   cs_nfc = 50,
#'                   cs_cp = 22,
#'                   cs_ee = 6,
#'                   NFCint = 15,
#'                   form_of_starter = "pelleted")
starter_met_energy <- function(cs_ndf = 12,
                               cs_nfc = 50,
                               cs_cp = 22,
                               cs_ee = 6,
                               NFCint = 15,
                               form_of_starter = "pelleted") {

  lnCSNFCI <- ifelse(NFCint <= 15.8, log(NFCint), log(15.8))

  pel <- ifelse(form_of_starter == "pelleted", 1, 0)

  tex <- ifelse(form_of_starter == "texturized", 1, 0)

  dCPcs <- 0.7074 + 0.0268 * lnCSNFCI - 0.3296 * pel - 0.0421 * tex + 0.1076 * pel * lnCSNFCI - 0.0130 * tex * lnCSNFCI

  dEEcs <- 0.8834 - 0.0218 * lnCSNFCI - 0.2123 * pel - 0.3808 * tex + 0.0916 * pel * lnCSNFCI + 0.1226 * tex * lnCSNFCI

  dNDFcs <- 0.3904 + 0.0262 * lnCSNFCI - 0.0909 * pel - 0.2630 * tex + 0.0789 * pel * lnCSNFCI + 0.0826 * tex * lnCSNFCI

  dNFCcs <- 0.9351 + 0.0047 * lnCSNFCI - 0.3486 * pel - 0.3226 * tex + 0.1348 * pel * lnCSNFCI + 0.1272 * tex * lnCSNFCI

  starter_dig_energy <- (dCPcs * cs_cp * 5.6 + dEEcs * cs_ee * 9.4 + dNDFcs * cs_ndf * 4.2 + dNFCcs * cs_nfc * 4.2) / 100

  starter_dig_energy <- ifelse(starter_dig_energy <= 0 | NFCint == 0, 0, starter_dig_energy)

  starter_met_energy <- (1.01 * starter_dig_energy - 0.45) + (0.0046 * (cs_ee - 3))

  starter_met_energy <- ifelse(starter_met_energy <= 0 | NFCint == 0, 0, starter_met_energy)

  return(starter_met_energy)

}

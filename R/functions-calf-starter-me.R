#' Metabolizable energy calcualtion based on .
#'
#' @param cs_ndf Calf starter NDF content (\%).
#' @param cs_nfc Calf starter NFC content (\%).
#' @param cs_cp Calf starter CP content (\%).
#' @param cs_ee Calf starter EE content (\%).
#' @param CSNFCI Calf starter cumulative NFC intake (kg).
#'
#' @return Metabolizable energy (ME) of calf starter (Mcal/kg DM).
#' @export
#'
#' @examples
#' starter_met_energy(cs_ndf = 12.9,
#'                   cs_nfc = 55.79,
#'                   cs_cp = 22,
#'                   cs_ee = 3.9,
#'                   CSNFCI = 15)
starter_met_energy <- function(cs_ndf = 12.9,
                               cs_nfc = 55.79,
                               cs_cp = 22,
                               cs_ee = 3.9,
                               CSNFCI = 15) {

  dCPcs <- 0.5547 + 0.2981 * (1 - exp(-(0.0692 * CSNFCI)))

  dEEcs <- 0.4892 + 0.3719 * (1 - exp(-(0.1316 * CSNFCI)))

  dNDFcs <- 0.1028 + 0.5687 * (1 - exp(-(0.0843 * CSNFCI)))

  dNFCcs <- 0.3792 + 0.5857 * (1 - exp(-(0.4154 * CSNFCI)))

  starter_dig_energy <- (dCPcs * cs_cp * 5.6 + dEEcs * cs_ee * 9.4 + dNDFcs * cs_ndf * 4.2 + dNFCcs * cs_nfc * 4.2) / 100

  starter_dig_energy <- ifelse(starter_dig_energy <= 0 | CSNFCI == 0, 0, starter_dig_energy)

  starter_met_energy <- (1.01 * starter_dig_energy - 0.45) + (0.0046 * (cs_ee - 3))

  starter_met_energy <- ifelse(starter_met_energy <= 0 | CSNFCI == 0, 0, starter_met_energy)

  return(starter_met_energy)

}
#
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

  return(starter_met_energy = starter_met_energy)

}

seq(0, 35) |>
  purrr::map(starter_met_energy,
             cs_ndf = 24.8,
             cs_nfc = 41.3,
             cs_cp = 18.8,
             cs_ee = 6.9) |>
  unlist() |>
  plot(ylab = "ME (Mcal/kg DM)", xlab = "NFC intake (kg)")

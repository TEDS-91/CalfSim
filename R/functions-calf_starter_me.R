
#
# cs_ndf <- 12
# cs_nfc <- 50
# cs_cp <- 22
# cs_ee <- 6
#
# #starter_met_energy
#
# dCPcs <- 0.7074 + 0.0268 * lnCSNFCI - 0.3296 * pel - 0.0421 * tex + 0.1076 * pel * lnCSNFCI - 0.0130 * tex * lnCSNFCI
# dEEcs <- 0.8834 - 0.0218 * lnCSNFCI - 0.2123 * pel - 0.3808 * tex + 0.0916 * pel * lnCSNFCI + 0.1226 * tex * lnCSNFCI
# dNDFcs <- 0.3904 + 0.0262 * lnCSNFCI - 0.0909 * pel - 0.2630 * tex + 0.0789 * pel * lnCSNFCI + 0.0826 * tex * lnCSNFCI
# dNFCcs <- 0.9351 + 0.0047 * lnCSNFCI - 0.3486 * pel - 0.3226 * tex + 0.1348 * pel * lnCSNFCI + 0.1272 * tex * lnCSNFCI
#

starter_met_energy <- function(cs_ndf = 12,
                               cs_nfc = 50,
                               cs_cp = 22,
                               cs_ee = 6,
                               NFCint = 15,
                               form_of_starter = "pelleted") {

  lnCSNFCI <- log(NFCint)

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

  return(list(starter_dig_energy = starter_dig_energy,
              starter_met_energy = starter_met_energy))

}
#
#
# sim <- expand.grid(
#   NFCint = seq(0, 50, 0.5),
#   cs_nfc = c(50, 53, 56),
#   cs_ee = c(3, 4, 5),
#   form_of_starter = c("pelleted", "texturized")
# ) |>
#   mutate(starter_met_energy = pmap(list(NFCint, cs_nfc, cs_ee, form_of_starter),
#                                    ~ starter_met_energy(cs_ndf = 12,
#                                                         cs_nfc = ..2,
#                                                         cs_cp = 22,
#                                                         cs_ee = ..3,
#                                                         NFCint = ..1,
#                                                         form_of_starter = ..4))) |>
#   unnest(everything()) |>
#   mutate("NFC (%)" = as.factor(cs_nfc),
#          "EE (%)" = as.factor(cs_ee))
#
# plotly::ggplotly(
# sim |>
#   ggplot2::ggplot(ggplot2::aes(x = NFCint, y = starter_met_energy, col = `NFC (%)`)) +
#   ggplot2::geom_line(aes(linetype = `EE (%)`)) +
#   ggplot2::facet_wrap(~ form_of_starter, ncol = 2) +
#   theme_bw() +
#   theme(legend.position = "bottom") +
#   labs(x = "Cumulative NFC intake (kg)", y = "CS ME (Mcal)")
# )



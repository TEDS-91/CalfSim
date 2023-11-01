#' Cards to be shown.
#'
#' @param card_title Card title.
#' @param card_value Card value.
#' @param card_icon Card icon.
#' @param card_color Card color.
#' @param card_bottom_message Card bottom message.
#' @param message_icon Message incon.
#'
#' @return Card.
#'
valueBoxCustom <- function(card_title = "I got",
                           card_value = "teste1",
                           card_icon  = "emoji-smile",
                           card_color = "#00891A",
                           card_bottom_message = "Tatata",
                           message_icon = "emoji-smile") {

  bslib::value_box(
    style = paste0('background-color: ', card_color,'!important;'),
    title = card_title,
    value = shiny::textOutput(card_value),
    showcase = bsicons::bs_icon(card_icon),
    p(card_bottom_message, bsicons::bs_icon(message_icon))
  )
}

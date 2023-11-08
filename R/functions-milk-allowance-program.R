#' Allows the user to simulate different nutritional plans for calves.
#'
#' @param weaning_age Weaning age (days).
#' @param nutritional_plans Number of nutritional plans.
#' @param milk_allowance Vector with the milk allowance according to the nutritional plan (kg/day).
#' @param ages_of_change Vector with the changes in the milk allowance (days).
#'
#' @return A vector with the milk allowance program.
#'
milk_allowance_program <- function(weaning_age = 70,
                                   nutritional_plans = 4,
                                   milk_allowance = c(6, 4, 2, 1),
                                   ages_of_change = c(25, 35, 45)) {

  if(length(milk_allowance) == 1) {

    milk_program <- rep(milk_allowance[1], weaning_age)

  } else {

    ages_of_change <- c(0, ages_of_change, weaning_age)

    milk_program <- vector("list", length = nutritional_plans)

    for (i in 1:nutritional_plans) {

      milk_program[[i]] <- rep(milk_allowance[i], (ages_of_change[i+1] - ages_of_change[i]))

    }

  }

  return(unlist(milk_program))

}


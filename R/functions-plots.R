#' Scenario plot.
#'
#' @param dataset A dataset with the variables to be plotted.
#' @param var_y A character string with the variable to be plotted.
#'
#' @return A plotly object.
#' @export
#'
scenario_plot <- function(dataset,
                          var_y) {

  plot <- dataset |>
    dplyr::group_by(scenario) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 3))) |>
    echarts4r::e_charts(days_of_life, renderer = "svg") |>
    echarts4r::e_line_(var_y, smooth = TRUE) |>
    echarts4r::e_tooltip(trigger = "axis",
                         axis_pointer = list(type = "cross")) |>
    echarts4r::e_title("Scenarios") |>
    echarts4r::e_theme("roma") |>
    echarts4r::e_y_axis(
      name = var_y,
      nameLocation = "center",
      nameTextStyle = list(
        color = "#666666",
        fontWeight = "bold"
      ),
      name_gap = 30
    ) |>
    echarts4r::e_x_axis(
      name = "Days of Life",
      nameLocation = "center",
      nameTextStyle = list(
        color = "#666666",
        fontWeight = "bold"
      ),
      name_gap = 30
    )

  return(plot)
}

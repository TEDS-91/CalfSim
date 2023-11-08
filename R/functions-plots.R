#' Line plot with two Y axis.
#'
#' @param data The data frame to be used.
#' @param x_axis The X axis to be plotted.
#' @param y_axis The first Y axis to be plotted.
#' @param y_axis_2 The second (right side) Y axis to be plotted.
#' @param x_label The X axis label.
#' @param y_label The first Y axis label.
#' @param y_axis_2_label The second (right side) label.
#' @param plot_title The title of the plot.
#'
#' @return A plot with two Y axis.
#' @export
#'
line_plotly <- function(data           = datasets::iris,
                        x_axis         = "Sepal.Length",
                        y_axis         = "Petal.Length",
                        y_axis_2       = "Petal.Width",
                        x_label        = "Sepal length",
                        y_label        = "Petal Length",
                        y_axis_2_label = "Petal.Width",
                        plot_title     = " ") {

  line <- plotly::plot_ly(data = data,
                        x = ~data[[ x_axis]],
                        y = ~round(data[[ y_axis ]], 3),
                        type = "scatter",
                        mode = "lines+markers",
                        line = list(width = 2),
                        marker = list(size = 10),
                        name = y_label,
                        text = ~paste("Variables plotted: <br>",
                                      paste0(y_label, ": "), data[[ y_axis ]],
                                      paste0( "<br>", x_label ,': '), data[[ x_axis ]],
                                      paste0( "<br>", y_axis_2_label ,': '), data[[ y_axis_2 ]]),
                        hovertemplate = paste(
                          "<b>%{text}</b>",
                          "<extra></extra>"
                        )) |>
    plotly::add_trace(x = ~data[[ x_axis ]], y = ~data[[ y_axis_2 ]], name = y_axis_2_label, mode = "lines+markers", yaxis = "y2") |>
    plotly::layout(yaxis2 = list(overlaying = "y", side = "right", title = y_axis_2_label, tickfont = list(size = 20), titlefont = list(size = 20))) |>
    plotly::layout(title = plot_title,
                   xaxis = list(title = x_label),
                   yaxis = list(title = y_label)) |>
    plotly::layout(title = " ",
                   xaxis = list(tickfont = list(size = 20), titlefont = list(size = 20)),
                   yaxis = list (tickfont = list(size = 20), titlefont = list(size = 20)),
                   showlegend = FALSE) |>
    plotly::layout(autosize = TRUE,
                   margin = list(
                     l = 80,
                     r = 105,
                     b = 100,
                     t = 0,
                     pad = 4)) |>
    plotly::config(displayModeBar = FALSE)

  return(line)

}

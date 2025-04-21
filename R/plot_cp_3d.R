#' Interactive 3D Plot of Bivariate Time Series and Change Points
#'
#' Generates an interactive 3D plot using plotly to visualize bivariate data
#' over time, with detected change points highlighted.
#'
#' @param x1 A numeric vector for the first variable (e.g., incidence).
#' @param x2 A numeric vector for the second variable (e.g., deaths).
#' @param change_points A numeric vector of indices indicating detected change points.
#'
#' @return A plotly 3D subplot object.
#' @export
#'
#' @import plotly
plot_cp_3d <- function(x1, x2, change_points) {
  n <- length(x1)
  timeline <- 1:n
  d1 <- data.frame(x1 = x1, x2 = x2)

  # Only keep valid change point indices
  change_points <- change_points[change_points > 0 & change_points <= n]
  d_cp <- data.frame(x = x1[change_points],
                     y = x2[change_points],
                     z = change_points)

  # Base data points
  p1 <- plot_ly(d1, x = ~x1, y = ~x2, z = ~timeline,
                type = "scatter3d", mode = "markers",
                marker = list(size = 4, color = '#00BFFF',
                              line = list(color = 'blue', width = 1)),
                name = "Data")

  # Change point markers
  p2 <- plot_ly(d_cp, x = ~x, y = ~y, z = ~z,
                type = "scatter3d", mode = "markers",
                marker = list(size = 5, color = 'red'),
                name = "Change Points", showlegend = FALSE)

  # Line connecting change points
  p3 <- plot_ly(d_cp, x = ~x, y = ~y, z = ~z,
                type = "scatter3d", mode = "lines",
                line = list(color = 'red', width = 2),
                showlegend = FALSE)

  # Combine
  final_plot <- subplot(p1, p2, p3) %>%
    layout(
      plot_bgcolor = "grey",
      scene = list(
        xaxis = list(title = "Variable 1"),
        yaxis = list(title = "Variable 2"),
        zaxis = list(title = "Time")
      )
    )

  return(final_plot)
}


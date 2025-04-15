#' Bivariate Normal Change Point Detection
#'
#' Detects change points in bivariate normal data streams.
#'
#' @param x1 Numeric vector for the first series.
#' @param x2 Numeric vector for the second series.
#' @param theta Prior probability for change point (default 0.95).
#' @param lambda Prior precision (default 0.01).
#' @param neu Degrees of freedom (default 4).
#' @param mu01 Prior mean for x1 (default 0.5).
#' @param mu02 Prior mean for x2 (default 0.5).
#' @param th_cp Threshold for detecting change point (default 0.5).
#' @param save_output Logical. If TRUE, saves the result to an Excel file (default is FALSE).
#' @param file_name Name of the Excel file to save the output (used only if save_output = TRUE).
#'
#' @return A list with original series and a matrix of posterior probabilities and detected change point locations.
#' @export
#'
#' @import cubature
#' @import openxlsx
#'
bivar_norm_cp <- function(x1, x2, theta = 0.95, lambda = 0.01, neu = 4, mu01 = 0.5, mu02 = 0.5, th_cp = 0.5,
                         save_output = FALSE, file_name = "bivnormcp_output.xlsx") {
  n <- length(x1)
  n2 <- length(x2)
  post_prob_t <- array(0, dim = c(n))
  post_prob_t1 <- array(0, dim = c(n))
  prob_max <- array(0, dim = c(n - 1, 2))
  t <- 1
  post_prob_t[t] <- 1
  j <- 0
  j_start_value <- 0
  t2_val <- 0

  if (n != n2) {
    stop("Error: Length of the two vectors are not equal!")
  } else {
    while (t < n) {
      j <- j_start_value
      weight_vec1 <- array(0, dim = c(t))

      while (j <= t) {
        sum_x1_N <- sum(x1[(j + 1):(t + 1)])
        sum_x1_D <- sum(x1[(j + 1):t])
        sum_x2_N <- sum(x2[(j + 1):(t + 1)])
        sum_x2_D <- sum(x2[(j + 1):t])

        sum_x1_sqr_N <- sum(x1[(j + 1):(t + 1)]^2)
        sum_x1_sqr_D <- sum(x1[(j + 1):t]^2)
        sum_x2_sqr_N <- sum(x2[(j + 1):(t + 1)]^2)
        sum_x2_sqr_D <- sum(x2[(j + 1):t]^2)

        sum_x1x2_N <- sum(x1[(j + 1):(t + 1)] * x2[(j + 1):(t + 1)])
        sum_x1x2_D <- sum(x1[(j + 1):t] * x2[(j + 1):t])

        integrand_numerator <- function(x) {
          ((1 - x[1]^2)^((-neu / 2) - 2 - ((t - j) / 2))) *
            exp((-1 / (2 * (1 - x[1]^2))) *
                  ((2 + sum_x1_sqr_N + sum_x2_sqr_N - 2 * x[1] * sum_x1x2_N +
                      lambda * mu01^2 - 2 * lambda * mu01 * mu02 * x[1] + lambda * mu02^2) -
                     ((lambda * mu01 - lambda * mu02 * x[1] + sum_x1_N - x * sum_x2_N)^2 / (t - j + lambda + 1)) -
                     (((lambda * mu02 + sum_x2_N)^2 * (1 - x[1]^2)) / (t - j + lambda + 1))))
        }

        integrand_denominator <- function(x) {
          ((1 - x[1]^2)^((-neu / 2) - 2 - ((t - j - 1) / 2))) *
            exp((-1 / (2 * (1 - x[1]^2))) *
                  ((2 + sum_x1_sqr_D + sum_x2_sqr_D - 2 * x[1] * sum_x1x2_D +
                      lambda * mu01^2 - 2 * lambda * mu01 * mu02 * x[1] + lambda * mu02^2) -
                     ((lambda * mu01 - lambda * mu02 * x[1] + sum_x1_D - x[1] * sum_x2_D)^2 / (t - j + lambda)) -
                     (((lambda * mu02 + sum_x2_D)^2 * (1 - x[1]^2)) / (t - j + lambda))))
        }

        integrand_j_eq_t <- function(x) {
          (((1 - x[1]^2)^((-neu / 2) - 2)) * lambda / (lambda + 1)) *
            exp((-1 / (2 * (1 - x[1]^2))) *
                  (2 + x1[t + 1]^2 + x2[t + 1]^2 - 2 * x[1] * x1[t + 1] * x2[t + 1] +
                     lambda * mu01^2 - 2 * lambda * mu01 * mu02 * x[1] + lambda * mu02^2 -
                     ((lambda * mu01 - lambda * mu02 * x[1] + x1[t + 1] - x * x2[t + 1])^2 / (lambda + 1)) -
                     (((lambda * mu02 + x2[t + 1])^2 * (1 - x[1]^2)) / (lambda + 1))))
        }

        if (j < t) {
          weight_vec1[j + 1] <- ((t - j + lambda) / (t - j + lambda + 1)) *
            (cubature::adaptIntegrate(integrand_numerator, c(-1), c(1))$integral /
               cubature::adaptIntegrate(integrand_denominator, c(-1), c(1))$integral)
          post_prob_t1[j + 1] <- weight_vec1[j + 1] * theta * post_prob_t[j + 1]
        } else {
          weight_vec2 <- cubature::adaptIntegrate(integrand_j_eq_t, c(-1), c(1))$integral
          post_prob_t1[j + 1] <- weight_vec2 * (1 - theta) * sum(post_prob_t[1:t])
        }
        j <- j + 1
      }

      post_prob_t <- post_prob_t1 / sum(post_prob_t1)

      prob_max[t, 1] <- max(post_prob_t)
      if (max(post_prob_t) > th_cp) {
        prob_max[t, 2] <- which.max(post_prob_t) - 1
        j_start_value <- prob_max[t, 2]
        post_prob_t1 <- array(0, dim = c(n))
        t2_val <- which.max(post_prob_t) - 1
      } else {
        prob_max[t, 2] <- t2_val
      }
      t <- t + 1
    }

    biv_bcp <- list(x1 = x1, x2 = x2, prob_max = prob_max, series_length = length(x1))

    if (save_output) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("Package 'openxlsx' is required to write Excel output. Please install it.")
      }

      wb <- openxlsx::createWorkbook()

      # Sheet 1: x1 and x2
      df_x <- data.frame(x1 = x1, x2 = x2)
      openxlsx::addWorksheet(wb, "Data")
      openxlsx::writeData(wb, sheet = "Data", df_x)

      # Sheet 2: prob_max
      df_prob <- as.data.frame(prob_max)
      colnames(df_prob) <- c("PosteriorMaxProb", "ChangePointLoc")
      openxlsx::addWorksheet(wb, "ChangePoints")
      openxlsx::writeData(wb, sheet = "ChangePoints", df_prob)

      # Save workbook
      openxlsx::saveWorkbook(wb, file = file_name, overwrite = TRUE)
    }


    return(biv_bcp)
  }
}

#' Concatenate time series
#'
#' Function that concatenates multiple time series.
#' @param ... Time series objects to be concatenated.
#' @details
#' It works in the same way \code{c()} does for vectors, and \code{rbind()} for
#' matrices. The function accepts both univariate and multivariate time serie.
#' They should all be the same type and, in the case of multivariate, have the
#' same columns.
#' @returns The joined time series.
#' @examples
#' x <- sample(98:102, 8, TRUE) |> ts(start = 2010, frequency = 4)
#' y <- sample(98:102, 4, TRUE) |> ts(start = 2012, frequency = 4)
#' concat(x,y)
#' @export
concat <- function(...) {
  series <- list(...)
  if (!methods::is(series[[1]], "mts")) {
    vectors <- lapply(series, as.numeric) |> unlist()
    result <- ts(vectors, start = start(series[[1]]),
            frequency = frequency(series[[1]]))
  } else {
    result <- rbind(...) |> ts(start = start(series[[1]]),
                               frequency = frequency(series[[1]]))
  }
  return(result)
}

#' Transform time series object to data.frame
#'
#' Represents an object of class "ts" as a data.frame. It accepts monthly,
#' quarterly, and annual series.
#'
#' @param x The time series of class "ts".
#' @returns A data.frame with the data from x in columns 2 onwards, and the
#' first column for the dates.
#' @details
#' The first column of the resulting data.frame is a column of dates. For example,
#' for quarterly series, quarters are represented by the first month of the
#' quarter, e.g. 2015 Q2 turns into 01/04/2015.
#' @examples
#' set.seed(23)
#' x <- sample(98:102, 10, TRUE) |> ts(start = 2010, frequency = 4)
#' as.data.frame(x)
#' set.seed(24)
#' x <- sample(98:102, 16, TRUE) |> ts(start = c(2010,3), frequency = 12)
#' as.data.frame(x)
#' @export
as.data.frame.ts <- function(x, ...) {
  if (!(frequency(x) %in% c(1,4,12))) {
    stop("Frequency must be one of 1, 4, 12.")
  }
  freq_string <- switch (as.character(frequency(x)),
                         "1" = "year",
                         "4" = "quarter",
                         "12" = "month"
  )
  if (frequency(x) == 1) {
    start <- paste0("01/01/",start(x)[1]) |>
      as.Date("%d/%m/%Y")
  } else {
    start <- paste0("01/", round((time(x)[1] - floor(time(x)[1]))*12 + 1,0), "/",start(x)[1]) |>
      as.Date("%d/%m/%Y")
  }
  dates <- seq.Date(from = start, by = freq_string, along.with = as.matrix(x)[,1])
  df <- data.frame(Fecha = dates, Series = as.matrix(x))
  if (!is.null(colnames(x))) {
    colnames(df)[-1] <- colnames(x)
  }
  return(df)
}
#' Compute the growth-rate series
#'
#' Function that computes the growth-rate series of a given time series.
#'
#' @param x A time series of class "ts"
#' @param s Lag at which the growth-rate is computed
#' @return Series of growth-rates
#' @examples
#' set.seed(23)
#' x <- sample(98:102, 10, TRUE) |> ts(start = 2010, frequency = 4)
#' compute_gr(x, 4)
#' compute_gr(x, 1)
#' @export
compute_gr <- function(x, s) {
  gr <- (x / lag(x, -s) - 1) *100
  return(gr)
}

#' Apply method to multivariate
#'
#' This function applies a function for univariate series ("ts") to a multivariate
#' ("mts").
#' @param x A multivariate time series of class "mts".
#' @param f A function that takes an univariate series as input.
#' @param ... Arguments for \code{f}.
apply_multivariate <- function(x, f, ...){
  n <- ncol(x)
  y <- lapply(1:n, \(i) {
    z <- f(x[,i], ...)
    w <- ts(z, start = start(z), frequency = frequency(z))
    return(w)
  })
  result <- do.call(cbind, y)
  colnames(result) <- colnames(x)
  return(result)
}

#' #' Correlation for time series
#' #'
#' #' Computes the Pearson correlation between two univariate time series in the
#' #' time interval they coincide.
#' #' @param x A time series "ts" object.
#' #' @param y A time series "ts" object.
#' cor.ts <- function(x,y) {
#'   u <- x
#'   v <- y
#'   w <- u*v
#'   u <- window(u, start = start(w), end = end(w)) |> scale()
#'   v <- window(v, start = start(w), end = end(w)) |> scale()
#'   w <- u*v
#'   return(sum(w)/(length(w)-1))
#' }
#'
#' #' Scaling and Centering of time series
#' #'
#' #'
#' scale.ts <- function(x, center = TRUE, scale = TRUE) {
#'   if (methods::is(x, "mts")) {
#'     y <- apply_multivariate(x, scale.ts_uni, center, scale)
#'   } else {
#'     y <- scale.ts_uni(x, center, scale)
#'   }
#'   return(y)
#' }
#'
#' scale.ts_uni <- function(x, center = TRUE, scale = TRUE) {
#'   u <- x |>
#'     scale.default(center = center, scale = scale) |>
#'     as.vector() |>
#'     ts(start = start(x), frequency = frequency(x))
#'   return(u)
#' }

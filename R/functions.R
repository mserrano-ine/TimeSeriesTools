#' time series to dataframe
#'
#' Converts a ts object to a dataframe whose first column are the dates.
#'
#' @param x A time series object ts
#' @param date_name Name of the date column. Defaults to "Date".
#'
#' @returns A dataframe with same column names (if mts) plus a first
#' column of dates
#' @examples
#' passengers <- datasets::AirPassengers
#' ts_to_df(passengers)
#' @importFrom zoo as.Date
#' @importFrom dplyr mutate
#' @importFrom stats time
#' @export
ts_to_df <- function(x, date_name = "Date") {
    dates <- zoo::as.Date(stats::time(x))
    df <- as.data.frame(x) |> dplyr::mutate(.before = 1, Date = dates)
    colnames(df)[1] <- date_name
    if (is.mts(x)) {
        colnames(df)[2:ncol(df)] <- colnames(x)
    }
    return(df)
}

#' dataframe to time series
#'
#' Converts a dataframe to a ts object. Only time frequencies between
#' 1 day and 1 year are supported.
#'
#' @param df A dataframe whose first column are dates.
#'
#' @returns A ts object.
#' @examples
#' passengers <- datasets::AirPassengers
#' passengers_df <- ts_to_df(passengers)
#' df_to_ts(passengers_df)
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom stats ts
#' @export
df_to_ts <- function(df) {
    dates <- df[, 1] <- as.Date(df[, 1])
    freq <- 12 / round(as.numeric(difftime(dates[2], dates[1], units = "days") / 31))
    first_date <- c(
        lubridate::year(dates[1]),
        ceiling(lubridate::month(dates[1]) * freq / 12)
    )
    s <- stats::ts(data = df[, -1], start = first_date, frequency = freq)
    return(s)
}

#' Read time series from an Excel file
#'
#' Reads time series from an Excel file. It is assumed that the first column
#' are dates and the rest are numeric.
#'
#' @details
#' This function uses `readxl::read_excel()` to read the file, specifying that the
#' first column be of type date and the rest numeric. It reads the file twice
#' behind the scenes, to count the number of columns.
#'
#' After the contents are loaded into a dataframe, `df_to_ts()` is applied.
#'
#' @param filepath (character) A path to an Excel file.
#' @param ...  Parameters passed to `read_excel()`
#'
#' @returns A ts object.
#' @examples
#' passengers_df <- ts_to_df(datasets::AirPassengers)
#' df_to_ts(passengers_df)
#' @importFrom readxl read_excel
#' @importFrom lubridate month
#' @export
ts_from_excel <- function(filepath, ...) {
    df <- readxl::read_excel(filepath, ...)
    n <- ncol(df)
    ctypes <- c("date", rep("numeric", n - 1))
    df <- read_excel(filepath, col_types = ctypes, ...) |>
        as.data.frame()
    s <- df_to_ts(df)
    return(s)
}

#' Compute the growth-rate series
#'
#' Function that computes the growth-rate series of a given time series.
#'
#' @param x (ts) A time series.
#' @param s (int) Lag at which the growth-rate is computed.
#' @return Series of growth-rates.
#' @examples
#' compute_gr(datasets::AirPassengers, 12)
#' @importFrom stats lag
#' @export
compute_gr <- function(x, s) {
    gr <- (x / stats::lag(x, -s) - 1) *100
    if (is.mts(x)) {
        colnames(gr) <- colnames(x)
    }
    return(gr)
}

#' Update (univariate) time series
#'
#' Takes a ts object and updates its data in place with a second ts object.
#'
#' @param x (ts/mts) Original time series.
#' @param y (ts/mts) New time series
#' @importFrom stats is.ts
#' @export
update_ts_uni <- function(x, y) {
    if (!is.ts(x) | !is.ts(y)) {
        stop("Arguments must be ts objects.")
    }
    z <- cbind(x,y)
    for (i in 1:nrow(z)) {
        if (!is.na(z[i,2])) {
            z[i,1] <- z[i,2]
        }
    }
    return(z[,1])
}

#' Update time series
#'
#' Takes a ts (mts) object and updates its data in place with a second ts (mts) object.
#'
#' @param x (ts/mts) Original time series.
#' @param y (ts/mts) New time series
#' @param update_shared (bool) If x and y don't have the same columns,
#' should the matching ones be updated? Default: FALSE.
#' @importFrom stats is.mts
#' @export
update_ts <- function(x, y, update_shared = FALSE) {
    if (is.mts(x) && is.mts(y)) {
        if (!update_shared &&
            !(length(colnames(x)) == length(colnames(y)) &&
              all(sort(colnames(x)) == sort(colnames(y))))) {
            stop("The columns in x and y are not the same. To update shared columns, choose: update_shared = TRUE")
        }
        z <- cbind(x,y[,1])
        z <- z[,-(ncol(x) + 1)]
        colnames(z) <- colnames(x)
        for (nm in colnames(x)) {
            if (nm %in% colnames(y)) {
                z[, nm] <- update_ts_uni(x[,nm], y[,nm])
            }
        }
    } else {
        z <- update_ts_uni(x,y)
    }
    return(z)
}

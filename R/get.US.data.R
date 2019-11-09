#' Get US data for estimation procedure
#'
#' Downloads US data from FRED website and saves as a dataframe
#' funtion takes 3 arguments - the output folder, filename, and an option to chose between output
#' specifications.
#' @param folder the csv to perform the HLW estimation on. Please see elp file for required column names
#' @param file a string for the country you are performing the estimates on - this is used in the output file name
#' @param pcGDP logical. (default) FALSE uses real GDP, TRUE uses real GDP per capita
#' @examples
#' folder <-  "data/inputData/"     # output folder
#' file <-  "rstar.data.us.csv"     # output filename
#' pcGDP <- FALSE                   # False uses real GDP, true uses real GDP per capita
#'
#' get.US.data(folder = folder, file = file)
#' @export
get.US.data <- function(folder = "data/input/" , file ="rstar.data.us.csv", pcGDP = FALSE) {
  # Import data using the function getFRED() in utilities.R
  # If the connection does not work, try the old URL:
  # "https://research.stlouisfed.org/fred2/data/NAME.txt"

  data.start <- c(1960,1)
  data.start.year <- 1960
  data.start.quarter <- 1

  # create alternate output measures
  # choice between GDP and GDP per capita
  if (pcGDP == FALSE) {
    gdp <- getFRED('https://fred.stlouisfed.org/data/GDPC1.txt')
  }
  else {
    gdp <- getFRED('https://fred.stlouisfed.org/data/A939RX0Q048SBEA.txt')
  }


  price.index     <- getFRED('https://fred.stlouisfed.org/data/PCEPILFE.txt')
  #ny.discount     <- getFRED('https://fred.stlouisfed.org/data/INTDSRUSM193N.txt')
  fed.funds       <- getFRED('https://fred.stlouisfed.org/data/FEDFUNDS.txt')

  #------------------------------------------------------------------------------#
  # Prepare Data
  #------------------------------------------------------------------------------#

  # Take log of real GDP
  gdp.log <- log(gdp)

  # Create an annualized inflation series using the price index
  inflation <- 400*log(price.index/Lag(price.index, k=1))

  # Inflation expectations measure: 4-quarter moving average of past inflation
  inflation.expectations <- (inflation + Lag(inflation, k=1) + Lag(inflation, k=2) + Lag(inflation, k=3))/4

  # Express interest rate data on a 365-day basis
  #ny.discount.eff <- 100*((1+ny.discount/36000)^365 -1)
  fed.funds.eff   <- 100*((1+fed.funds/36000)^365 -1)

  # NY Fed discount rate is used prior to 1965; thereafter, use the effective federal funds rate
  #interest.rate <- mergeSeries(window(ny.discount.eff, end = c(1964,4)),window(fed.funds.eff, start = c(1965,1)))
  interest.rate <- fed.funds.eff
  #------------------------------------------------------------------------------#
  # Output Data
  #------------------------------------------------------------------------------#
  #get the end of the data
  data_t <- window(cbind(gdp.log, inflation, inflation.expectations, interest.rate))
  quarters <- nrow(data_t)
  data.end.year <- data.start.year + quarters %/% 4
  data.end.quarter <- (data.start.quarter + quarters %% 4) - 1
  data.end <- c(data.end.year, data.end.quarter)

  message("Dateset spans from ", data.start.year, " Q", data.start.quarter,
               " to ", data.end.year, " Q", data.end.quarter)

  # create date column sequence
  date <- seq(from = (as.Date(ti(shiftQuarter(data.start,-1),'quarterly'))+1),
                      to = (as.Date(ti(shiftQuarter(data.end,-1),tif='quarterly'))+1),
                      by = 'quarter')
  # merge time series into a dataframe
  data.out <- (window(cbind(gdp.log, inflation, inflation.expectations, interest.rate), start = data.start, end = data.end))
  data.out <- cbind(date, as.data.frame(data.out))
  write.table(data.out, file = paste0(folder, file), sep = ',',
              col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)
}

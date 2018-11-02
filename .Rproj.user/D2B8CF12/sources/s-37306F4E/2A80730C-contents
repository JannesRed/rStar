#' Get South African data for estimation procedure
#'
#' Downloads SA data from FRED website and saves as a dataframe
#' funtion takes 2 arguments - the output folder and filename
#' @param folder the csv to perform the HLW estimation on. Please see elp file for required column names
#' @param file a string for the country you are performing the estimates on - this is used in the output file name
#' @examples
#' folder <-  "data/inputData/"         # output folder
#' file <-  "rstar.data.us.csv"     # output filename
#'
#' get.SA.data(folder = folder, file = file)
#' @export
get.SA.data <- function(folder = "data/input/" , file ="rstar.data.sa.csv") {
  # Import data using the function getFRED() in utilities.R
  # If the connection does not work, try the old URL:
  # "https://research.stlouisfed.org/fred2/data/NAME.txt"

  data.start <- c(1960,1)
  data.start.year <- 1960
  data.start.quarter <- 1


  gdp             <- getFRED('https://fred.stlouisfed.org/data/NAEXKP01ZAQ661S.txt')
  price.index     <- getFRED('https://fred.stlouisfed.org/data/ZAFCPIALLMINMEI.txt')
  interest.rate   <- getFRED('https://fred.stlouisfed.org/data/IRSTCB01ZAM156N.txt')
  #discount.rate   <- getFRED("https://fred.stlouisfed.org/data/IRSTCB01ZAM156N.txt")

  #------------------------------------------------------------------------------#
  # Prepare Data
  #------------------------------------------------------------------------------#

  # Take log of real GDP
  gdp.log <- log(gdp)

  # Create an annualized seasonally adjusted inflation series using the price index
  cpi.back.start <- c(1960,1)
  price.index <- final(seas(as.ts(naWindow(price.index),freq=4)))
  price.index  <- as.tis(price.index,start=cpi.back.start,tif='monthly')

  inflation <- 400*log(price.index/Lag(price.index, k=1))

  # Inflation expectations measure: 4-quarter moving average of past inflation
  inflation.expectations <- (inflation + lag(inflation, n=1) + lag(inflation, n=2) + lag(inflation, n=3))/4

  # express discount rate on 365 day basis
  interest.rate <- 100*((1+interest.rate/36000)^365 -1)
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
  date <- seq(from = (as.Date(ti(shiftQuarter(data.start,3),'quarterly'))+1),
              to = (as.Date(ti(shiftQuarter(data.end,-1),tif='quarterly'))+1),
              by = 'quarter')
  # merge time series into a dataframe
  data.out <- data_t
  data.out <- data.out[-1:-4,]
  data.out <- cbind(date, as.data.frame(data.out))
  write.table(data.out, file = paste0(folder, file), sep = ',',
              col.names = TRUE, quote = FALSE, na = '.', row.names = FALSE)
}

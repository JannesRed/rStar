#------------------------------------------------------------------------------#
# File:        run.hlw.R
#
# Description: This the main file for HLW, which does the following:
#              (1) Runs the three-stage HLW estimation
#              (2) Saves output.
#------------------------------------------------------------------------------#

#' Run estimation on specified file
#'
#' Takes csv with columns date, gdp.log, inflation, inflation.expectations, interest.rate as input.
#' The sample must be at least 98 quarters.
#' Saves two output files to specified folder and you should specify country name
#' @param input_file the csv to perform the HLW estimation on. Please see elp file for required column names
#' @param country_name a string for the country you are performing the estimates on - this is used in the output file name
#' @param output_folder String
#' @param niter Integer. Sets the number of iterations for Monte Carlo Simulation. default is set to 5000
#' @param run.se logical. default is TRUE, set to FALSE if you do not want to compute standard errors for the estimation
#'
#' @examples
#' input_file <-  "data/input/rstar.data.us.test.csv"  # location of input file
#' country_name <-  "US.test"                     #country name for output file
#' output_folder <-  "data/output/"                  # output folder for estimation
#' run.se <-  FALSE                                    # turn on standard errors
#' niter <-5000       #iterations for monte carlo simulation
#'
#' hlw_nri(input_file = input_file,
#'        country_name = country_name,
#'        output_folder = output_folder,
#'        run.se = run.se,
#'        niter = niter)
#' @export
hlw_nri <- function(input_file,
                    country_name,
                    output_folder,
                    niter = 5000,
                    run.se = TRUE) {


  # Read in input file for a country
  data <- read.table(input_file,
                     sep = ',',
                     na.strings = ".",
                     header=TRUE,
                     stringsAsFactors=FALSE) %>%
    mutate(date = as.Date(parse_date_time(date, c("Ymd", "mdY"))))

  # perform data quality checks
  #-------------------------------------------------------------------------
  # minimum quarters is 98 otherwise singular matrices occur
  message("Checking that data is at least 98 quarters...")
  if (nrow(data) < 98) {
    message("Your data is of length ", nrow(data),
                 " please make sure it spans at least 98 quarters for the estimation to run.")
    stop("You should get a longer sample period")
  } else {
    message("Data is of length ", nrow(data), " continuing to estimation precedure")
  }

  # zeros or NAs break the code
  message("Please check yourself that the data doesn't contain non-sensible zeros...")
  message("Checking that data does not contain illegal values...")
  for (data_column in c(2,3,4,5)) {
    if (sum(is.na(data[data_column]))>5) {
      stop(paste0(colnames(data[data_column])), " contains ", sum(data[data_column]), " NAs")
    }
    if (sum(data[data_column] == 0)>0) {
      stop(paste0(colnames(data[data_column])), " contains ", sum(data[data_column]), " zeros")
    }
  }

  # provide additional info
  if(run.se == TRUE) {message("Running with standard error calculations")}
  if(run.se == FALSE) {message("Running without standard error calculations")}
  message("Iterations for Monte Carlo simulation are set to: ", niter, ". Default is 5000.")

  # Automatically set the start and end dates of the estimation sample
  #------------------------------------------------------------------------------
  # (format is c(year,quarter))
  data.start.quarter <- as.numeric(quarter(data$date[1]))
  data.start.year <- as.numeric(year(data$date[1]))
  data.start <- c(data.start.year, data.start.quarter)

  # The estimation process uses data beginning 4 quarters prior to the sample start
  sample.start    <- shiftQuarter(data.start,+4)

  data.end.quarter <- as.numeric(quarter(data$date[length(data$date)]))
  data.end.year <- as.numeric(year(data$date[length(data$date)]))
  sample.end   <- c(data.end.year, data.end.quarter)

  message("Data starts in ", data.start.year, " quarter ", data.start.quarter)
  message("Sample starts 4 quarters after data start")
  message("Data ends in ", data.end.year, " quarter ", data.end.quarter )

  #------------------------------------------------------------------------------#
  # Define variables
  #------------------------------------------------------------------------------#
  # Upper bound on a_3 parameter (slope of the IS curve)
  a3.constraint <- -0.0025

  # Lower bound on b_2 parameter (slope of the Phillips curve)
  b2.constraint <- 0.025


  # Set start index for y
  g.pot.start.index <- 1 + ti(shiftQuarter(sample.start,-3),'quarterly')-ti(data.start,'quarterly')

  # Set column names for CSV output
  output.col.names <- c("Date","rstar","g","z","output gap","","All results are output from the Stage 3 model.",rep("",8),"Standard Errors","Date","y*","r*","g","","rrgap")

  # Set number of iterations for Monte Carlo standard error procedure
  # original is 5000
  niter <- niter

  # Because the MC standard error procedure is time consuming, we include a run switch
  # Set run.se to TRUE to run the procedure
  run.se <- run.se

  # create variable arrays from data
  #--------------------------------------------------------------------------
  #the source of my pain. Using an index. the matrix is singular when logged.
  # but estimates are wonky when output is logged,


  log.output            <- as.numeric(data$gdp.log)
  inflation              <- as.numeric(data$inflation)
  # Inflation expectations measure: 4-quarter moving average of past inflation
  inflation.expectations <- as.numeric(data$inflation.expectations)
  nominal.interest.rate  <- as.numeric(data$interest.rate)
  real.interest.rate     <- as.numeric(nominal.interest.rate - inflation.expectations)


  # Run HLW estimation
  estimation <- run.hlw.estimation(log.output, inflation, real.interest.rate, nominal.interest.rate,
                                      a3.constraint = a3.constraint, b2.constraint = b2.constraint,
                                   run.se = run.se, g.pot.start.index = g.pot.start.index,
                                   niter = niter)

  # One-sided (filtered) estimates
  one.sided.est <- cbind(estimation$out.stage3$rstar.filtered,
                            estimation$out.stage3$trend.filtered,
                            estimation$out.stage3$z.filtered,
                            estimation$out.stage3$output.gap.filtered)

  # Two-sided (smoothed) estimates
  two.sided.est <- cbind(estimation$out.stage3$rstar.smoothed,
                         estimation$out.stage3$trend.smoothed,
                         estimation$out.stage3$z.smoothed,
                         estimation$out.stage3$output.gap.smoothed)

  # Save one-sided estimates to CSV
  #------------------------ change url to dynamically change
  write.table(one.sided.est, paste0(output_folder, 'one.sided.est.', country_name, '.csv'), row.names = FALSE, col.names = c("rstar","g","z","output gap"), quote = FALSE, sep = ',', na = ".")

  # Save two-sided estimates to CSV
  #------------------------ change url to dynamically change
  write.table(two.sided.est, paste0(output_folder, 'two.sided.est.', country_name, '.csv'), row.names = FALSE, col.names = c("rstar","g","z","output gap"), quote = FALSE, sep = ',', na = ".")

  # Save output to CSV
  #------------------------ change url to dynamically change
  output <- format.output(estimation, one.sided.est, real.interest.rate, sample.start, sample.end, run.se = run.se, niter = niter)
  write.table(output, paste0(output_folder, 'output.', country_name, '.csv'), col.names = output.col.names, quote=FALSE, row.names=FALSE, sep = ',', na = '')
  message("Done. You can find the output in: ", output_folder)
}

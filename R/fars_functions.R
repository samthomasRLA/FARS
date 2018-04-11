if(getRversion() >= "2.15.1") utils::globalVariables(c("year", "MONTH", "n", "STATE"))

#' Read FARS data
#' 
#' This function reads data from the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System 
#' into R. It takes a filename as an argument. 
#' 
#' @note The function suppressess messages generated during the data read process and throws an error if the \code{filename} does not exist in the working directory.
#' 
#' @param filename A character string indicating the name of the file to be read
#' 
#' @return This function returns a dataframe of the FARS data.
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @examples
#' fars_read(system.file("extdata", "accident_2013.csv.bz2", package = "FARS"))
#' \dontrun{fars_read("does_not_exist.csv")}
#' 
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE, quote = "")
  })
  dplyr::tbl_df(data)
}

#' Makes a filename by concatenating "accident_\%d.csv.bz2" and a user-supplied year, where \%d becomes the year.
#' 
#' @param year An integer representing the year that will be inserted into the file name
#' 
#' @note If \code{year} cannot be coerced into an integer, the function will throw an error
#' 
#' @return returns the complete file name
#' 
#' @examples
#' make_filename(2013)
#' \dontrun{make_filename(not_a_number)}
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads FARS data for selected years and generates separate lists for each year with two columns each: MONTH and year.
#' 
#' @note Throws an error if the \code{year} is not a number or not a year found in the file(s)
#' 
#' @param years A numerical vector representing years to read in the dataset(s)
#' 
#' @return This function returns a list for each year containing two columns: MONTH and year
#' 
#' @importFrom dplyr mutate select %>%
#' 
#' @examples
#' \dontrun{fars_read_years(2013:2015)}
#' \dontrun{fars_read_years(not_a_number)}
#' 
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' This function summarizes the number of incidents by month and year in FARS data and returns a dataframe.
#' 
#' @note the function will throw an error if the \code{year} supplied is not found in the dataset(s) or is not a number
#' 
#' @inheritParams fars_read_years
#' 
#' @return This function returns a dataframe summarizing the number of FARS incidents by month and year.
#' The datframe includes a column for MONTH and a column for each year summarized.
#' 
#' @importFrom dplyr bind_rows %>% group_by summarize
#' @importFrom tidyr spread
#' 
#' @examples
#' \dontrun{fars_summarize_years(2013:2015)}
#' \dontrun{fars_summarize_years(not_a_number)}
#' 
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' This function generate a map of all FARS events in one or more US states, with each event displayed as a dot on the map.
#' User may specify the state(s) by unique ID# and year from which FARS event data should be drawn.
#' 
#' @note This function throws an error if an invalid \code{year} is selected (not a number or not a year in the available dataset(s))
#' @note It also throws an error if an invalid \code{state.num} is specified
#' @note More than one state may be selected at a time, generating a country-wide map with data plotted for selected states
#' @note If a valid subset of data is selected, but not FARS events are avaialble, the function will print a message indicating that
#' 
#' @inheritParams make_filename
#' @param state.num An integer vector that uniquely identifies one or more US states in the FARS dataset
#' 
#' @return Returns map plot of one or more US states with black dots representing FARS events in the selected year
#'  
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' 
#' @examples
#' \dontrun{fars_map_state(1, 2013)}
#' \dontrun{fars_map_state(1:50, 2013)}
#' \dontrun{fars_map_state(75, 2013)}
#' 
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
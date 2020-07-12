#' Read  (FARS) data
#'
#' The `fars_read` function reads FARAS data and converts it to a `dplyr` data table. The file name is checked in the working directory. If it does not exist, the function is halted and a message is displayed to the console.
#'
#' @param filename A character string with the full file name (and path, if needed).
#'
#' @return The function returns a `dplyr` data table with FARS data. If the file does not exist, the function returns an error message.
#'
#' @note Function execution is stopped and an error message is displayed if the file name does not exist.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'     data <- 2014 %>% make_filename %>% fars_read
#' }
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Creates a file name
#'
#' The `make_filename` function creates a file name `accident_[year].csv.bz2` given a valid year.
#'
#' @param year Year either as a character string or integer.
#'
#' @return Returns a file name as a character string.
#'
#' @note The `year` paramater should be valid.
#'
#' @examples
#' make_filename("2013")
#' make_filename(2014)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        system.file("extdata",
                    sprintf("accident_%d.csv.bz2", year),
                    package = "FARSfun",
                    mustWork = TRUE)
}

#' Reads FARS files and generates data tables
#'
#' The `fars_read_years` function reads file(s) and creates `dat` data tables for the year(s) needed -- provided the years argument is valid.
#'
#' @param years A single or vector of years either as an integer, character, or combination
#'
#' @return The function returns a `dat` data frame with the `MONTH` and `year` columns for each valid year in the argument. If an invalid year is provided, the functions returns a warning message.
#'
#' @note The function displays a warning message if an invalid year is entered and returns NULL.
#'
#' @importFrom dplyr  mutate
#' @importFrom dplyr  select
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_read_years(c("2013", 2014, "2015"))
#' fars_read_years(2013:2015)
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

#' Summarizes by year and month
#'
#' The `fars_summarize_years` function retrieves data tables and summarizes counts by `year` and `MONTH`.
#'
#' @param years A single or vector of years either as an integer, character, or combination
#'
#' @return The function returns a `dat_list` data frame with count summaries by `year` (columns) and `MONTH` (rows). If the paramaters passed are not valid errors from the feeder functions `fars_read_years`, `make_filename`, and `fars_read` displayed.
#'
#' @note Inerited function errors are displayed if the `years` argument is out of range.
#'
#' @importFrom dplyr  bind_rows
#' @importFrom dplyr  group_by
#' @importFrom dplyr  summarize
#' @importFrom tidyr  spread
#'
#' @examples
#' fars_summarize_years(c("2013", 2014, "2015"))
#' fars_summarize_years(2013:2015)
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot FARS data
#'
#' The `fars_map_state` function plots fatal data on a map for the state and year provided.
#'
#' @param state.num The state number either as an integer or character
#' @param year The year either as an integer or character string.
#'
#' @return The function plots a map of the US state and observations where fatal accidents happened for the selected valid state and year. An error message is displayed if the state number is invalid, and if there are no accidents for the state/year combination a message is displayed. Furthermore, if an invalid year is entered, error message from `fars_read` is displayed.
#'
#' @note If the state number is invalid an error message is displayed and execution is halted.
#' @note If there are no accidents/observations, a message is displayed and the function returns NULL.
#' @note If the `year` is out of range, inherited function error messages are displayed.
#'
#' @importFrom dplyr  filter
#' @importFrom maps  map
#' @importFrom graphics  points
#' @importFrom magrittr "%>%"
#'
#' @examples
#' fars_map_state(1, 2015)
#' fars_map_state("10", 2013)
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

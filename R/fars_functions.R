#' fars_read
#'
#' read the file into R by file name.
#'
#' @param filename the name of file that contains the data of interest. the file
#'   name con be contructed using another function make_filename in the same
#'   package
#'
#' @return data in the format of data_frame
#'
#' @note Where the working directory does not contain the furnished file name,
#'   an error meesage stating that file doesnot exist will appear. this function
#'   is for internal use only, not exported.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' filename <- make_filename(2015)
#' data_2015 <- fars_read(filename)
#' }
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}



#' make_filename
#'
#' construct a file name according to the year of interest
#'
#' @param year year as numeric or integer, even a string year would work
#'
#' @return file name of the data for the year of interest
#'
#' @export
#'
#' @note for internal use only, not exported
#'
#' @examples
#'
#' \dontrun{
#' filename <- make_filename(2015)
#' filename2 <- make_filename("2013")
#' }
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' fars_read_years
#'
#' Read data files according to the years of interest.  dplyr needs to be imported for this function.
#'
#' @param years years of interest, it could be either a single year of a vector
#'   of years
#'
#' @return a list of data, each element is a data_frame for the data for a given
#'   year
#'
#' @note If the data for specified years of interest are not present, a warning message will show
#' those years as invalid year.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' \dontrun{
#' data_2013 <- fars_read_years(2013)
#'
#' data_2013to2015 <- fars_read_years(c(2013, 2014, 2015))
#' }
#'
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



#' fars_summarize_years
#'
#' summarize the monthly total of the accidents for year(s) of interest. dplyr
#' and tidyr need to be imported for this function.
#'
#' @param years the years of interest. It could be a single year or a vector of
#'   years
#'
#' @return summary of monthly tally of the number of accidents for the years of
#'   interest
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' sum_2015 <- fars_summarize_years(2015)
#' sum_2013to2015 <- fars_summarize_years(c(2013, 2014, 2015))
#'
#' }
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}



#' fars_map_state
#'
#' plot the location of accidents on any state map. If the state code is
#' invalid, an error message will indicate that. No plot will be generated if
#' there is no accident for the state of interest during the year(s) of
#' interest. Instead, a message will show "no accident to plot". maps and dplyr
#' need to be imported for this function.
#'
#' @param state.num numeric code representing state
#' @param year year of interest
#'
#' @return a state map with the locations of accidents marked as dots
#' @export
#'
#' @examples
#'
#' \dontrun{
#' fars_map_state(12, 2015)
#' }
#'
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





#'#' Manually Correct Processed Dendrometer Data
#'
#' \code{corr_dendro_L2} corrects remaining errors in cleaned \code{L2}
#'   dendrometer data. The function can be used to manually correct remaining
#'   errors that cannot be removed by adjusting the input parameter values in
#'   \code{\link{proc_dendro_L2}}. The function can reverse erroneously
#'   introduced changes or force changes that were not automatically made.
#'
#' @param dendro_L1 time-aligned dendrometer data as produced by
#'   \code{\link{proc_L1}}. Optional, only needed for \code{reverse} and if
#'   \code{plot = TRUE}.
#' @param reverse numeric vector, specify ID numbers of the changes that should
#'   be reversed. ID numbers are reported in the plots produced by
#'   \code{\link{proc_dendro_L2}} or \code{\link{plot_proc_L2}} with the
#'   argument \code{plot_period = "monthly"}.
#' @param force character vector, specify the dates after which jumps/shifts
#'   should be corrected. The largest value difference occurring in a
#'   period of \code{n_days} after the specified dates in \code{force} is
#'   corrected. Dates need to be in a standard date or
#'   datetime format (e.g. \code{\%Y-\%m-\%d \%H:\%M:\%S}).
#' @param delete character vector, specify pairs of dates between which
#'   all dendrometer data will be deleted (i.e. 4 dates will result in two
#'   periods: 1-2 and 3-4 in which data is deleted). Dates need to be in the
#'   same standard date or datetime format
#'   (e.g. \code{\%Y-\%m-\%d \%H:\%M:\%S}).
#' @param force.now character vector: specify the specific date where a jump
#'   will be corrected. There's no period of following days where treenetproc
#'   will look for the largest difference, but the force will be done in the
#'   given date and time specified in \code{force.now}. Dates need to be in a
#'   standard date or datetime format (e.g. \code{\%Y-\%m-\%d \%H:\%M:\%S}).
#' @param series character, specify the name of a single dendrometer series
#'   for which changes should be made. Data of other series is left unchanged.
#'   Not needed if only a single series is provided.
#' @param plot logical, specify whether implemented changes should be plotted.
#' @param n_days numeric, length of the period (in days) following the dates
#'   specified in \code{force} in which a jump/shift is corrected. Increase if
#'   the gap in data is longer than the default (\code{n_days = 5}).
#' @inheritParams proc_dendro_L2
#' @inheritParams plot_proc_L2
#'
#' @return The function returns a \code{data.frame} with corrected \code{L2}
#'   dendrometer data. The corrections are documented in the column
#'   \code{flags}.
#'
#' @seealso \code{\link{corr_dendro_L1}} to correct \code{L1} data.
#'
#' @export
#'
#' @examples
#' recalc_treenetproc_output(dendro_L2 = dendro_data_L2)
#'
recalc_treenetproc_output <- function(dendro_L2, tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)

  check_data_L2(data_L2 = dendro_L2)

  df <- dendro_L2;

  # remove leading and trailing NA's
  na_list <- remove_lead_trail_na(df = df)
  df <- na_list[[1]]
  lead_trail_na <- na_list[[2]]

  df <- calcmax(df = df)
  df <- calctwdgro(df = df, tz = tz)

  df <- df %>%
    dplyr::mutate(gro_yr = ifelse(is.na(value), NA, gro_yr)) %>%
    dplyr::mutate(twd = ifelse(is.na(value), NA, twd)) %>%
    dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
    dplyr::mutate(
      version = utils::packageDescription("treenetproc",
                                          fields = "Version", drop = TRUE))
  # append leading and trailing NA's
  df <- append_lead_trail_na(df = df, na = lead_trail_na)

  return(df)
}

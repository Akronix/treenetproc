print_growth_statistics <- function(df, reso, tz) {

  data_plot <- df

  series_no <- first(data_plot$series)

  graphics::layout(mat = matrix(c(1, 2), nrow = 2), heights = c(2, 4),
                   widths = 1)

  # plot yearly growth curves
  graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))

  data_plot <- data_plot %>%
    dplyr::mutate(doy = as.numeric(strftime(ts, format = "%j", tz = tz)) - 1) %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::mutate(month = strftime(ts, format = "%m", tz = tz))

    graphics::plot(data = data_plot, gro_yr ~ doy, type = "n",
                 main = as.character(series_no), ylab = paste0("gro_yr (",
                                                               "\u00b5", "m)"),
                 xlab = "day of year", xlim = c(0, 365),
                 ylim = c(0, max(data_plot$gro_yr, na.rm = TRUE)), las = 1)

  years <- unique(data_plot$year)
  colors <- c("grey", grDevices::rainbow(length(years)))
  data_year <- data_plot %>%
    dplyr::group_by(year) %>%
    dplyr::group_split()

  for (y in 1:length(years)) {
    graphics::lines(data = data_year[[y]], gro_yr ~ doy, col = colors[y])
  }
  graphics::legend(x = "topleft", legend = years, col = colors, bty = "n",
                   lty = 1, seg.len = 0.8)

  # Put some separation space and set margins
  graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))

  graphics::plot(x = c(0, 1), y = c(0, 1), ann = FALSE, bty = "n",
                 type = "n", xaxt = "n", yaxt = "n")

  # print growth values for different periods
  gro_period <- calcgroperiods(df = data_plot, reso = reso, tz = tz)

  if (length(gro_period) > 0) {
    graphics::text(x = 0, y = 1, adj = c(0, 1), font = 2, cex = 0.8,
                   labels = paste0("growth statistics (", "\u00b5",
                                   "m): median (min / max)"))
    for (r in 1:nrow(gro_period)) {
      gro_period_single <- gro_period[r, ]
      graphics::text(x = 0, y = 0.97 - 0.03 * r, adj = c(0, 1), cex = 0.8,
                     labels = paste0(gro_period_single$period, ": ",
                                     gro_period_single$gro_med, " (",
                                     gro_period_single$gro_min, " / ",
                                     gro_period_single$gro_max, ")"))
    }
  }
}

#' Re-calculate the growth variables: max, twd and gro_yr
#'
#' \code{recalc_growth_variables} calculates the growth variables: max, twd and
#' gro_yr from the \code{value} column of L2 dendrometer data, and optionally
#' plot the gro_yr plot and the growth statistics. \cr
#' This function is useful, for instance, if you has changed the initial moment
#' of measure (for example, by filtering the data), and you want to recalculate
#' the growing stats or phase stats; or you you want to recalculate the twd, max
#' or gro_yr values.
#'
#' @param dendro_L2 input \code{data.frame} containing cleaned \code{L2}
#'   dendrometer data (obtained from functions \code{\link{proc_dendro_L2}} or
#'   \code{\link{corr_dendro_L2}}). Note that it should contain (at least) the
#'   following columns: series, ts, value, max, twd, gro_yr
#'
#' @inheritParams proc_dendro_L2
#'
#' @return The function returns a \code{data.frame} with the \code{treenetproc}
#'   growth output (max, twd and gro_yr) recalculated and the treenetproc
#'   version in use appended. It leaves the other columns unmodified.
#'
#'
#' @seealso \code{\link{grow_seas}} to calculate the growing season variables
#'    from the dendro_L2 data, which is updated with this function.
#'
#' @export
#'
#' @examples
#' recalc_growth_variables(dendro_L2 = dendro_data_L2, tz = 'Europe/Madrid')
#'
recalc_growth_variables <- function(dendro_L2,
                                    plot = FALSE,
                                    plot_export = FALSE,
                                    tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)

  check_data_L2(data_L2 = dendro_L2)

  df <- dendro_L2;

  reso <- reso_check_L1(df = df, tz = tz)
  # passenv$reso <- reso # <- in case we wanted to have reso in the environment

  # iterate over every series on the dataframe
  series_vec <- unique(df$series)
  list_series <- vector("list", length = length(series_vec))
  df_all <- df
  for (s in 1:length(series_vec)) {
    df <- df_all %>% dplyr::filter(series == series_vec[s])

    # remove leading and trailing NA's
    na_list <- remove_lead_trail_na(df = df)
    df <- na_list[[1]]
    lead_trail_na <- na_list[[2]]

    df.aux <- df # do a copy to prevent changin previous data by external functions
    df.aux <- calcmax(df = df.aux)
    df.aux <- calctwdgro(df = df.aux, tz = tz)

    df <- df %>%
      dplyr::mutate(gro_yr = ifelse(is.na(value), NA, df.aux$gro_yr)) %>%
      dplyr::mutate(twd = ifelse(is.na(value), NA, df.aux$twd)) %>%
      dplyr::mutate(max = ifelse(is.na(value), NA, df.aux$max)) %>%
      dplyr::mutate(
        version = utils::packageDescription("treenetproc",
                                            fields = "Version", drop = TRUE))
    # append leading and trailing NA's
    df <- append_lead_trail_na(df = df, na = lead_trail_na)

    # generate pdf report
    if (plot) {
      series <- first(df$series)
      print("plotting new growth statistics...")
      if (plot_export) {
        grDevices::pdf(paste0("recalc_growth_", series, ".pdf"),
                       width = 8.3, height = 11.7)
      }

      print_growth_statistics(df, reso, tz)

      if (plot_export) {
        grDevices::dev.off()
      }
    }

    list_series[[s]] <- df

  }

  df <- dplyr::bind_rows(list_series)

  return(df)
}

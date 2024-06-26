#' Helper Function for Reso
#'
#' \code{passobj} passes an object to a function call that is saved in a
#'   separate environment accessible for all functions. Environment is
#'   saved outside function call to be accessible for all functions.
#'
#' @param value name of the variable as character string.
#'
#' @keywords internal
#'
passenv <- new.env(parent = emptyenv())
passobj <- function(value) {
  val <- get(x = value, envir = passenv, inherits = FALSE)
  return(val)
}


#' Creates Flag for Gaps
#'
#' \code{creategapflag} adds a flag to gaps that are longer than
#'   \code{gaple}. Used to be called \code{cleanaftergaps}.
#'
#' @param df input \code{data.frame}.
#' @param gaple specify minimum length of gap for flagging.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
creategapflag <- function(df, reso, gaple = 12 * (60 / reso)) {

  if ("gapflag" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gapflag)
  }

  wnd <- gaple
  nc <- ncol(df)

  df <- df %>%
    dplyr::arrange(ts) %>%
    dplyr::mutate(isgap = is.na(value)) %>%
    dplyr::mutate(gaps = cumsum(isgap)) %>%
    dplyr::mutate(y = c(0, diff(gaps, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(gapnr = cumsum(z)) %>%
    dplyr::mutate(diff_ts = as.numeric(difftime(ts, dplyr::lag(ts, 1),
                                                units = "mins"))) %>%
    dplyr::mutate(diff_ts = c(0, diff_ts[2:dplyr::n()])) %>%
    dplyr::group_by(gapnr) %>%
    dplyr::mutate(gaple_mins = sum(diff_ts) + reso) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gapflag = ifelse(isgap & gaple_mins > wnd, 1, 0)) %>%
    dplyr::select(1:nc, gapflag)

  return(df)
}


#' Fills NA's With Last Non-NA Value
#'
#' \code{fillna} fills NA's with previous non-NA value (function
#'   adapted from \code{\link[zoo]{na.locf}} of the \code{zoo} package).
#'
#' @param x input \code{vector}.
#'
#' @keywords internal
#'
fill_na <- function(x, from_last = FALSE) {

  if (from_last) {
    x <- rev(x)
  }

  nonaid <- !is.na(x)
  val_nona <- c(NA, x[nonaid])
  fillid <- cumsum(nonaid) + 1
  x <- val_nona[fillid]

  if (from_last) {
    x <- rev(x)
  }

  return(x)
}


#' Fills Trailing NA's With First Non-NA Value
#'
#' \code{fill_na_lead} fills leading NA's with first non-NA value.
#'
#' @param x input \code{vector}.
#'
#' @keywords internal
#'
fill_na_lead <- function(x) {
  if (is.na(x[1])) {
    nonaid <- which(!is.na(x))[1]
    val_nona <- x[nonaid]
    fillid <- 1:(nonaid - 1)
    x[fillid] <- rep(val_nona)
  }

  return(x)
}


#' Calculates Hourly Change in Data
#'
#' \code{calcdiff} calculates an hourly change in \code{value}. In case there
#'   are gaps, the difference is also calculated at an hourly rate respective
#'   to the length of the preceding gap.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
calcdiff <- function(df, reso) {
  if ("diff_val" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-diff_val)
  }

  nc <- ncol(df)
  df <- df %>%
    # calculate gaplenght in minutes
    dplyr::arrange(ts) %>%
    dplyr::mutate(isgap = is.na(value)) %>%
    dplyr::mutate(gaps = cumsum(isgap)) %>%
    dplyr::mutate(y = c(0, diff(gaps, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(gapnr = cumsum(z)) %>%
    dplyr::group_by(gapnr) %>%
    dplyr::mutate(gaple = dplyr::n()) %>%
    dplyr::mutate(gaple_mins = (gaple + 1) * reso) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gaple_mins = dplyr::lag(gaple_mins, n = 1)) %>%
    dplyr::select(1:nc, gaple_mins) %>%
    # calculate hourly change over gaps
    dplyr::mutate(diff_val = c(NA, diff(value)) * (60 / reso)) %>%
    dplyr::mutate(val_gap = fill_na(value)) %>%
    dplyr::mutate(diff_gap = c(NA, diff(val_gap))) %>%
    dplyr::mutate(diff_gap = ifelse(is.na(diff_val) & diff_gap != 0,
                  diff_gap, NA)) %>%
    dplyr::mutate(diff_gap = diff_gap / (gaple_mins / 60)) %>%
    dplyr::mutate(diff_val = ifelse(!is.na(diff_gap),
                                    diff_gap, diff_val)) %>%
    dplyr::select(1:nc, diff_val)

  return(df)
}


#' Creates Flag for Potential Frost
#'
#' \code{createfrostflag} adds a flag for potential frost. Gaps of temperature
#'   data are filled with previous non-NA value.
#'
#' @param df input \code{data.frame}.
#' @param sample_temp logical, specifying whether sample temperature dataset
#'   is used.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
createfrostflag <- function(df, tem, lowtemp = 5, sample_temp) {
  temp_series <- df$temp_ref[1]

  df_frost <- tem %>%
    dplyr::filter(series == temp_series) %>%
    dplyr::mutate(frost = ifelse(value < lowtemp, TRUE, FALSE)) %>%
    dplyr::select(ts, frost) %>%
    dplyr::right_join(., df, by = "ts") %>%
    dplyr::arrange(ts)

  # print amount of missing temperature data (not if sample temperature
  # dataset is used)
  if (!passobj("sample_temp")) {
    na_temp <- sum(is.na(df_frost$frost))
    na_perc <- round(na_temp / nrow(df_frost) * 100, 2)
    if (na_perc < 0.1) {
      message("No temperature data is missing.")
    } else {
      message(paste0(na_perc, "% of temperature data is missing."))
    }
  }

  if (all(is.na(df_frost$frost))) {
    df <- df_frost %>%
      dplyr::mutate(frost = FALSE)

    return(df)
  }

  df <- df_frost %>%
    dplyr::mutate(frost = fill_na(frost)) %>%
    dplyr::mutate(frost = fill_na_lead(frost))

  return(df)
}


#' Define Outliers Based on Value Distribution
#'
#' \code{calcflagmad} is a helper function of \code{\link{createflagmad}}.
#'
#' @param df input \code{data.frame}.
#' @param frost logical, defines whether outliers should be calculated for
#'   frost periods or other periods.
#' @param save_thr logical, specifies whether the applied thresholds are
#'   saved for later plotting or not.
#' @inheritParams createflagmad
#'
#' @keywords internal
#'
calcflagmad <- function(df, reso, wnd = NULL, tol = 10, frost,
                        frost_thr, save_thr = FALSE, correction = NULL) {

  check_logical(var = frost, var_name = "frost")
  check_logical(var = save_thr, var_name = "save_thr")
  if (!(correction %in% c("outlier", "jump"))) {
    stop("correction needs to be either 'out' or 'jump'.")
  }

  if (frost) {
    df <- df %>%
      dplyr::filter(frost == TRUE)
  }
  if (!frost & correction == "outlier") {
    df <- df %>%
      dplyr::filter(frost == FALSE)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  if (length(wnd) == 0) {
    span <- trunc(nrow(df) / 2)
    } else {
      span <- 60 / reso * 24 * (wnd / 2)
      if (nrow(df) < 2 * span) {
        message("you don't have enough data for regular outlier detection! ",
                "Outlier detection may not work properly.")
        span <- trunc(nrow(df) / 2)
      }
    }

  st <- span + 1
  en <- nrow(df) - span + 1
  steps <- seq(st, en, by = span)

  flagqlow <- vector(length = nrow(df))
  flagqhigh <- vector(length = nrow(df))
  thr_min <- -100000
  thr_max <- 100000
  for (qq in steps) {
    b1 <- qq - span; b2 <- qq + span - 1; ran <- b1:b2
    q30 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.3,
                                      na.rm = TRUE))
    q70 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.7,
                                      na.rm = TRUE))
    df$diff_val[ran][df$diff_val[ran] > q30 &
                       df$diff_val[ran] < q70] <- NA

    q1 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.25,
                                     na.rm = TRUE))
    q3 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.75,
                                     na.rm = TRUE))

    mad <- stats::mad(df$diff_val[ran], na.rm = TRUE)

    if (mad == 0) { # MAD can be 0 when more of the 50% has the same value, we fallback to MAE in such case
      median <- stats::median(df$diff_val[ran], na.rm = TRUE)
      mae <- mean(abs(df$diff_val[ran] - median), na.rm = TRUE)
      if (mae != 0) {
        mad <- mae
      }
      else { # if, for any improbable reason, MAE is 0, change it to 0.001
        mad <- 0.001
      }
    }

    low <- q1 - tol * mad
    high <- q3 + tol * mad

    if (frost) {
      low <- low * frost_thr
      high <- high * frost_thr
    }

    if (save_thr) {
      if (!is.na(low) && low != 0) {
        if (low > thr_min) {
          thr_min <- round(low, 2)
        }
      }
      if (!is.na(high) && high != 0) {
        if (high < thr_max) {
          thr_max <- round(high, 2)
        }
      }
      if (correction == "outlier") {
        passenv$thr_out_plot <- c(thr_min, thr_max)
      }
      if (correction == "jump") {
        passenv$thr_jump_plot <- c(thr_min, thr_max)
      }
    }

    if (!is.na(low)) {
      flagqlow[ran][df$diff_val[ran] < low] <- TRUE
    }
    if (!is.na(high)) {
      flagqhigh[ran][df$diff_val[ran] > high] <- TRUE
    }
  }

  df <- df %>%
    dplyr::mutate(flagoutlow = flagqlow) %>%
    dplyr::mutate(flagouthigh = flagqhigh)

  # save applied thresholds for density_plot
  passenv$thr_low <- low
  passenv$thr_high <- high

  return(df)
}


#' Creates Flag for Outliers Based on Median Absolute Deviation
#'
#' \code{createflagmad} creates flag for outliers that are above or below a
#'   threshold distant from the first or third quantile. The threshold is
#'   specified by the the median absolute deviation
#'   (\code{\link[stats]{mad}}).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#' @inheritParams calcflagmad
#'
#' @keywords internal
#'
createflagmad <- function(df, reso, wnd, tol, save_thr, frost_thr,
                          correction) {

  nc <- ncol(df)
  df <- df %>%
    dplyr::mutate(diff_val = ifelse(diff_val < 0.001 & diff_val > -0.001,
                                    NA, diff_val))

  if (correction == "outlier") {
    df_frost <- calcflagmad(df = df, reso = reso, wnd = wnd, tol = tol,
                            frost = TRUE, frost_thr = frost_thr,
                            correction = correction)
  }
  if (correction == "jump") {
    df_frost <- df[0, ]
  }
  df <- calcflagmad(df = df, reso = reso, wnd = wnd, tol = tol,
                    frost = FALSE, frost_thr = frost_thr,
                    save_thr = save_thr, correction = correction)

  if (nrow(df_frost) > 0) {
    df <- dplyr::bind_rows(df, df_frost)
  }

  df <- df %>%
    dplyr::arrange(ts) %>%
    dplyr::select(1:nc, flagoutlow, flagouthigh)

  return(df)
}


#' Create Flag for Outliers
#'
#' \code{createflagout} creates flags for outlier values.
#'
#' @param df input \code{data.frame}.
#' @param len specifies the minimal number of consecutive times the value
#'   difference threshold has to be exceeded for flagging.
#'
#' @keywords internal
#'
createflagout <- function(df, out, len) {

  flagout <- df %>%
    dplyr::mutate(flag = out) %>%
    dplyr::mutate(flag_group = cumsum(flag)) %>%
    dplyr::mutate(y = c(0, diff(flag_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flag_nr = cumsum(z)) %>%
    dplyr::group_by(flag_nr) %>%
    dplyr::mutate(flag_len = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flagout = ifelse(flag_len >= len & flag, TRUE, FALSE)) %>%
    dplyr::select(flagout) %>%
    unlist(., use.names = FALSE)

  return(flagout)
}


#' Create Flag for Small Data Fragments
#'
#' \code{createflagfragment} flags short fragments of not flagged data that
#'   is in-between flagged data. Such cases occur often in spikes of outlier
#'   data.
#'
#' @param out logical, vector with outlier flags.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
createflagfragment <- function(df, frag_len = NULL) {

  if (length(frag_len) == 0) {
    frag_len <- 2.1
  }

  flagfrag1 <- df %>%
    dplyr::mutate(flagout = flagoutlow + flagouthigh) %>%
    dplyr::mutate(flagout = ifelse(flagout > 0, TRUE, FALSE)) %>%
    dplyr::mutate(flag_group = cumsum(flagout)) %>%
    dplyr::mutate(y = c(0, diff(flag_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flag_nr = cumsum(z)) %>%
    # remove NA's of value column to enable flagging of fragments in between
    # long periods of NA
    dplyr::filter(!(is.na(value) & !flagout)) %>%
    dplyr::group_by(flag_nr) %>%
    dplyr::mutate(flag_len = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flagfrag = ifelse(flag_len <= frag_len & !flagout,
                                TRUE, FALSE)) %>%
    # classsify first and last group as FALSE since they are not in-between
    # flagged groups
    dplyr::mutate(flagfrag = ifelse(flag_group == 0, FALSE, flagfrag)) %>%
    dplyr::mutate(flagfrag = ifelse(flag_group == max(flag_group),
                                FALSE, flagfrag)) %>%
    # add flagfrag to outliers
    dplyr::mutate(flagfrag = flagfrag + flagout) %>%
    dplyr::mutate(flagfrag = ifelse(flagfrag > 0, TRUE, FALSE)) %>%
    dplyr::select(ts, flagfrag)

  flagfrag2 <- df %>%
    dplyr::full_join(flagfrag1, by = "ts") %>%
    dplyr::arrange(ts) %>%
    dplyr::mutate(flagfrag = ifelse(is.na(flagfrag), FALSE, flagfrag)) %>%
    dplyr::select(flagfrag) %>%
    unlist(., use.names = FALSE)

  # save value of frag_len for later plotting
  passenv$frag_len_plot <- frag_len

  return(flagfrag2)
}


#' Remove Outliers
#'
#' \code{executeflagout} removes outlier values flagged by
#'   \code{\link{createflagout}}.
#'
#' @param df input \code{data.frame}.
#' @param plot_density logical, defines whether density plots should be drawn
#'   in the console. Can be used to check outlier thresholds and removal.
#' @inheritParams createflagout
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
executeflagout <- function(df, len, frag_len, plot_density = FALSE,
                           plot_export, frost_thr) {

  check_logical(var = plot_density, var_name = "plot_density")

  flagout_nr <- length(grep("^flagout[0-9]", colnames(df)))
  if (flagout_nr > 0) {
    passenv$flagout_nr <-  passenv$flagout_nr + 1
  } else {
    passenv$flagout_nr <- 1
  }

  # optional density plot before outlier removal
  if (sum(plot_export, plot_density) == 2) {
    series <- unique(df$series)[1]
    grDevices::pdf(paste0("density_plot_", series, ".pdf"),
                   width = 8.3, height = 5.8)
  }
  if (plot_density) {
    plot_density(df = df, low = passobj("thr_low"),
                 high = passobj("thr_high"), limit_val = 20,
                 frost_thr = frost_thr, reso = passobj("reso"))
  }

  # flag short fragments of not flagged data in between flagged data
  out <- createflagfragment(df = df, frag_len = frag_len)
  # only flag outliers that occur in groups of a certain length
  out <- createflagout(df = df, out = out, len = len)

  nc <- ncol(df)
  df <- df %>%
    dplyr::mutate(flagout = out) %>%
    dplyr::mutate(value = ifelse(flagout, NA, value)) %>%
    dplyr::mutate(!!paste0("flagout", passobj("flagout_nr")) := flagout) %>%
    dplyr::select(1:nc, grep("^flagout[0-9]", colnames(.)))

  # optional density plot after outlier removal
  if (plot_density) {
    plot_density(df = df, low = passobj("thr_low"),
                 high = passobj("thr_high"), limit_val = 20,
                 frost_thr = frost_thr, reso = passobj("reso"))
  }
  if (sum(plot_export, plot_density) == 2) {
    grDevices::dev.off()
  }

  return(df)
}


#' Creates Flag for Jumps in Data
#'
#' \code{createjumpflag} creates flag for jumps.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
createjumpflag <- function(df) {

  nc <- ncol(df)
  flagjump_nr <- length(grep("^flagjump[0-9]", colnames(df)))
  if (flagjump_nr > 0) {
    passenv$flagjump_nr <-  passenv$flagjump_nr + 1
  } else {
    passenv$flagjump_nr <- 1
  }

  df <- df %>%
    dplyr::mutate(flagjump = ifelse(flagoutlow | flagouthigh, TRUE, FALSE)) %>%
    dplyr::mutate(!!paste0("flagjump", passobj("flagjump_nr")) := flagjump) %>%
    dplyr::select(1:nc, grep("^(flagjump)(?!.*low)(?!.*high)",
                             names(.), perl = TRUE)) %>%
    as.data.frame()

  return(df)
}


#' Remove Jumps
#'
#' \code{executejump} removes jumps that occur after resetting the dendrometer
#'   needle.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
executejump <- function(df) {

  nc <- ncol(df)
  le <- nrow(df)
  ran <- which(df$flagjump)

  diff_jump <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(diff_jump = c(NA, diff(value))) %>%
    dplyr::select(ts, diff_jump)
  df <- dplyr::left_join(df, diff_jump, by = "ts")

  val <- as.vector(df$value, mode = "numeric")
  if (length(ran) > 0) {
    for (uu in 1:length(ran)) {
      zz <- ran[uu]
      val[zz:le] <- val[zz:le] - df$diff_jump[zz]
    }
  }

  df <- df %>%
    dplyr::mutate(value = val) %>%
    dplyr::select(1:nc)

  return(df)
}


#' Calculate Maximum
#'
#' \code{calcmax} calculates the maximum of measured values.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
calcmax <- function(df) {

  if ("max" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-max)
  }
  nc <- ncol(df)
  first_val <- df$value[which(!is.na(df$value))[1]]

  df <- df %>%
    dplyr::mutate(val_nona = fill_na_lead(value)) %>%
    dplyr::mutate(val_nona = fill_na(val_nona)) %>%
    dplyr::mutate(diff_nona = c(0, diff(val_nona, lag = 1))) %>%
    dplyr::mutate(diff_sum = cumsum(diff_nona)) %>%
    dplyr::mutate(diff_sum = fill_na(diff_sum))

  max_sum <- df$diff_sum
  for (i in 2:nrow(df)) {
    if (max_sum[i - 1] < max_sum[i]) {
      next
    } else {
      max_sum[i] <- max_sum[i - 1]
    }
  }

  df <- df %>%
    dplyr::mutate(max = max_sum + first_val) %>%
    dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
    dplyr::select(1:nc, max)

  return(df)
}


#' Calculate TWD and GRO
#'
#' \code{calctwdgro} calculates the tree water deficit (twd) and the growth
#'   since the beginning of the year (gro_yr).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
calctwdgro  <- function(df, tz) {

  if ("twd" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-twd)
  }
  if ("gro_yr" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_yr)
  }
  nc <- ncol(df)

  gro <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(gro = c(0, diff(max))) %>%
    dplyr::mutate(gro = ifelse(is.na(gro), 0, gro)) %>%
    dplyr::mutate(year = substr(ts, 1, 4)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(gro_yr = cumsum(gro)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-gro, -year)

  df <- df %>%
    dplyr::filter(is.na(value)) %>%
    dplyr::mutate(gro_yr = NA) %>%
    dplyr::bind_rows(., gro) %>%
    dplyr::arrange(series, ts) %>%
    dplyr::mutate(twd = abs(value - max)) %>%
    dplyr::select(1:nc, twd, gro_yr)

  return(df)
}


#' Calculates Percentages of Interpolated, Deleted and Missing Data
#'
#' \code{calcmissing} calculates the percentage of interpolated, deleted and
#'   missing data.
#'
#' @inheritParams plotting_proc_L2
#'
#' @return a list of length = 3 with percentages of interpolated, deleted,
#'   and missing data.
#'
#' @keywords internal
#'
calcmissing <- function(data_plot) {

  len <- nrow(data_plot)
  interpol <- data_plot %>%
    dplyr::slice(grep("fill", flags))
  interpol_perc <- round(nrow(interpol) / len * 100, 2)

  deleted <- data_plot %>%
    dplyr::slice(grep("out", flags))
  deleted_perc <- round(nrow(deleted) / len * 100, 2)

  missing <- data_plot %>%
    dplyr::mutate(missing = ifelse(is.na(value_L1) & is.na(value_L2),
                                   1, 0)) %>%
    dplyr::summarise(missing = sum(missing)) %>%
    unlist(., use.names = FALSE)
  missing_perc <- round(missing / len * 100, 2)

  list_missing <- list(interpol_perc, deleted_perc, missing_perc)

  return(list_missing)
}


#' Calculate Growth for Different Time Periods
#'
#' \code{calcgroperiods} calculates the minimum, median and maximum growth
#'   for different time periods. The periods are selected depending on the
#'   resolution. Growth values are calculated after removing periods without
#'   growth (i.e. growth = 0).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
calcgroperiods <- function(df, reso, tz) {

  # add grouping variables
  df <- df %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::mutate(month = strftime(ts, format = "%m", tz = tz)) %>%
    dplyr::mutate(week = strftime(ts, format = "%V", tz = tz)) %>%
    dplyr::mutate(day = strftime(ts, format = "%d", tz = tz)) %>%
    dplyr::mutate(hour = strftime(ts, format = "%H", tz = tz)) %>%
    dplyr::mutate(diff_gro = c(NA, diff(max)))

  # define grouping variables
  list_gro <- vector("list", length = 4)
  list_cond <- vector("list", length = 4)
  if (reso > 43800) {
    return(NULL)
  }
  if (reso <= 43800) {
    list_cond[[1]] <- c("year", "month")
  }
  if (reso <= 10080) {
    list_cond[[2]] <- c("year", "week")
  }
  if (reso <= 1440) {
    list_cond[[3]] <- c("year", "week", "day")
  }
  if (reso <= 60) {
    list_cond[[4]] <- c("year", "week", "day", "hour")
  }
  # remove empty list elements
  list_cond <- Filter(f = length, x = list_cond)

  # calculate growth for different periods
  options(warn = -1)
  for (l in 1:length(list_cond)) {
    gro_period <- df %>%
      dplyr::group_by_at(list_cond[[l]]) %>%
      dplyr::summarise(gro = sum(diff_gro, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(gro > 0) %>%
      dplyr::summarise(gro_max = round(max(gro, na.rm = TRUE), 2),
                       gro_med = round(stats::median(gro, na.rm = TRUE), 2),
                       gro_min = round(min(gro, na.rm = TRUE), 2)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(period = dplyr::last(list_cond[[l]]))

    list_gro[[l]] <- gro_period
  }
  options(warn = 0)

  gro_period <- dplyr::bind_rows(list_gro)

  return(gro_period)
}


#' Summarise Flags
#'
#' \code{summariseflags} summarises all previously created flags in one column.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
summariseflags <- function(df) {

  list_flags <- vector("list", length = passenv$flagout_nr * 2 + 1)

  n_flags <- 1
  for (out in n_flags:(passenv$flagout_nr + n_flags - 1)) {
    flagout_nr <- out - n_flags + 1
    flagout <- df[[paste0("flagout", flagout_nr)]]
    list_flags[[out]] <- ifelse(flagout, paste0("out", flagout_nr), NA)
  }

  n_flags <- n_flags + passenv$flagjump_nr
  for (jump in n_flags:(passenv$flagjump_nr + n_flags - 1)) {
    flagjump_nr <- jump - n_flags + 1
    flagjump <- df[[paste0("flagjump", flagjump_nr)]]
    list_flags[[jump]] <- ifelse(flagjump, paste0("jump", flagjump_nr), NA)
  }

  if ("flagfill" %in% colnames(df)) {
    n_flags <- n_flags + 1
    list_flags[[n_flags]] <- ifelse(df$flagfill, "fill", NA)
  }

  flags <- do.call("paste", c(list_flags, sep = ", "))
  flags <- gsub(", NA|NA, |, $", "", flags)
  flags <- ifelse(flags %in% c("NA", ""), NA, flags)

  df$flags <- flags

  return(df)
}


#' Save Applied Cleaning Thresholds for Plotting
#'
#' \code{saveplotthr} stores the applied threshold values for jump and
#'   outlier detection during data cleaning. The values are later used
#'   for plotting (i.e. presented below the plot with the yearly growth
#'   curves).
#'
#' @param thr_out vector, containing the values of the minimum and maximum
#'   outlier thresholds applied.
#' @param thr_jump vector, containing the values of the minimum and maximum
#'   jump thresholds applied.
#'
#' @keywords internal
#'
saveplotthr <- function(df, thr_out, thr_jump) {

  series <- df$series[1]
  thr_plot <- as.data.frame(matrix(ncol = 5, nrow = 1))
  colnames(thr_plot) <- c("thr_out_min", "thr_out_max", "thr_jump_min",
                          "thr_jump_max", "series")
  thr_plot[1, ] <- c(thr_out[1], thr_out[2], thr_jump[1], thr_jump[2], series)
  thr_plot[, 1:4] <- as.numeric(thr_plot[, 1:4])

  return(thr_plot)
}

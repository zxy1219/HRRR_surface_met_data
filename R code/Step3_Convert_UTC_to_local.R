# ---- Rebucket HRRR files by LOCAL year (date/hour already local) ----
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

# ========== USER SETTINGS ==========
root_dir  <- "C:/Users/u254415/Box/Pollen/RLINE/AERMET/HRRR/ATL"  # use / not \
out_root  <- file.path(root_dir, "local_time")
overwrite <- TRUE
make_hour_1_24 <- TRUE   # convert 0-23 -> 1-24 by +1 (no date shifting)
# ===================================

to_year4 <- function(y) {
  y <- as.integer(y)
  ifelse(!is.na(y) & y < 100, 2000L + y, y)
}

normalize_cols <- function(dt) {
  # force numeric/integer columns
  dt[, Year  := to_year4(Year)]
  dt[, Month := as.integer(Month)]
  dt[, Day   := as.integer(Day)]
  dt[, hour  := as.integer(hour)]
  invisible(dt)
}

process_one_file_rebucket <- function(file_prev_year = NA_character_,
                                      file_this_year,
                                      file_next_year = NA_character_,
                                      target_year,
                                      out_dir,
                                      overwrite = TRUE,
                                      make_hour_1_24 = TRUE) {
  
  req <- c("Year","Month","Day","hour")
  
  read_if_ok <- function(fp) {
    if (is.na(fp) || !file.exists(fp)) return(NULL)
    dt <- fread(fp, showProgress = FALSE)
    miss <- setdiff(req, names(dt))
    if (length(miss) > 0) {
      warning(sprintf("Ignoring %s (missing: %s)", fp, paste(miss, collapse=", ")))
      return(NULL)
    }
    normalize_cols(dt)
    dt
  }
  
  dt_prev <- read_if_ok(file_prev_year)
  dt_this <- read_if_ok(file_this_year)
  dt_next <- read_if_ok(file_next_year)
  
  if (is.null(dt_this)) {
    warning("Skipping (could not read main file): ", file_this_year)
    return(invisible(NULL))
  }
  
  # Combine prev + this + next so we capture spillover hours into target_year
  dt <- rbindlist(list(dt_prev, dt_this, dt_next), use.names = TRUE, fill = TRUE)
  
  # Convert 0-23 -> 1-24 by +1 (NO date shifting)
  if (make_hour_1_24) {
    if (!all(is.na(dt$hour)) && suppressWarnings(max(dt$hour, na.rm = TRUE)) <= 23 &&
        suppressWarnings(min(dt$hour, na.rm = TRUE)) >= 0) {
      dt[, hour := hour + 1L]
    }
  }
  
  # Keep only rows whose LOCAL year == target_year
  dt <- dt[Year == target_year]
  
  # Sort in local chronological order
  setorder(dt, Year, Month, Day, hour)
  
  # Write output with same filename as "this year" file
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  outfile <- file.path(out_dir, basename(file_this_year))
  
  if (!overwrite && file.exists(outfile)) {
    message("Exists, skipping: ", outfile)
    return(invisible(outfile))
  }
  
  fwrite(dt, outfile)
  
  # Sanity check: last day should have 24 hours (1-24)
  if (nrow(dt) > 0) {
    last_day <- dt[.N, .(Year, Month, Day)]
    hrs_last <- dt[last_day, on=.(Year, Month, Day), unique(hour)]
    missing <- setdiff(1:24, sort(hrs_last))
    if (length(missing) > 0) {
      warning(sprintf(
        "Output still missing hours on last day (%04d-%02d-%02d): %s",
        last_day$Year, last_day$Month, last_day$Day,
        paste(missing, collapse=", ")
      ))
    }
  } else {
    warning("Output has 0 rows after filtering to target year: ", target_year,
            " for file: ", basename(file_this_year))
  }
  
  message("Wrote: ", outfile)
  invisible(outfile)
}

# ---- Main loop over year folders ----
year_dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
year_dirs <- year_dirs[str_detect(basename(year_dirs), "^\\d{4}$")]
if (length(year_dirs) == 0) stop("No year folders like 2023/ found under: ", root_dir)

years <- sort(as.integer(basename(year_dirs)))
names(year_dirs) <- as.character(years)

get_year_dir <- function(y) {
  key <- as.character(y)
  if (key %in% names(year_dirs)) year_dirs[[key]] else NULL
}

for (y in years) {
  yd <- get_year_dir(y)
  in_files <- list.files(yd, pattern = "\\.csv$", full.names = TRUE)
  if (length(in_files) == 0) next
  
  out_dir  <- file.path(out_root, as.character(y))
  prev_dir <- get_year_dir(y - 1L)
  next_dir <- get_year_dir(y + 1L)
  
  message("\n--- Re-bucketing LOCAL year: ", y, " (", length(in_files), " files) ---")
  
  for (f in in_files) {
    base <- basename(f)
    
    f_prev <- NA_character_
    if (!is.null(prev_dir)) {
      cand <- file.path(prev_dir, base)
      if (file.exists(cand)) f_prev <- cand
    }
    
    f_next <- NA_character_
    if (!is.null(next_dir)) {
      cand <- file.path(next_dir, base)
      if (file.exists(cand)) f_next <- cand
    }
    
    process_one_file_rebucket(
      file_prev_year = f_prev,
      file_this_year = f,
      file_next_year = f_next,
      target_year    = y,
      out_dir        = out_dir,
      overwrite      = overwrite,
      make_hour_1_24 = make_hour_1_24
    )
  }
}

message("\nDone. Rebucketed LOCAL-year files are under: ", out_root)

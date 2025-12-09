# https://oris.ceskyorientak.cz/API

library(jsonlite)
library(httr)
library(tidyverse)
library(purrr)
library(glue)


`%||%` <- function(x, y = NA_character_) {
  if (is.null(x) || length(x) == 0) y else x
}

current_date <- today()

get_event_list <- function(date_from, date_to) {
  # date_from = "2025-11-21"; date_to = "2025-12-31"
  url <- glue("https://oris.ceskyorientak.cz/API/?format=json&method=getEventList&datefrom={date_from}&dateto={date_to}&myClubId=23&all=1")
  r <- GET(url)
  stop_for_status(r)
  j <- fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  
  df <- map_dfr(j$Data, \(ev) {
    tibble(
      event_id   = ev$ID %||% NA_character_,
      cst_entries = max(as.integer(ev$ClubStartCount), as.integer(ev$ClubEntryCount)) %||% NA_integer_,
      date       = as.Date(ev$Date %||% NA_character_),
      name       = ev$Name %||% NA_character_,
      place      = ev$Place %||% NA_character_,
      region     = ev$Region %||% NA_character_,
      sport      = ev$Sport$NameCZ %||% ev$Sport$NameEN %||% NA_character_,
      discipline = ev$Discipline$NameCZ %||% ev$Discipline$NameEN %||% NA_character_,
      level      = ev$Level$NameCZ %||% ev$Level$NameEN %||% NA_character_,
      org1_abbr  = ev$Org1$Abbr %||% NA_character_,
      org1_name  = ev$Org1$Name %||% NA_character_,
      org2_abbr  = ev$Org2$Abbr %||% NA_character_,
      org2_name  = ev$Org2$Name %||% NA_character_,
      gps_lat    = suppressWarnings(as.numeric(ev$GPSLat %||% NA_character_)),
      gps_lon    = suppressWarnings(as.numeric(ev$GPSLon %||% NA_character_)),
      status     = ev$Status %||% NA_character_
    )
  }) |>
    arrange(date, name)
  
  return(df)
}

get_our_entries <- function(event_id) {
  url <- glue("https://oris.ceskyorientak.cz/API/?format=json&method=getEventEntries&eventid={event_id}")
  r <- GET(url)
  stop_for_status(r)
  j <- fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)$Data %||%
    tibble(RegNo = NA, Name = NA, SI = NA, ClassDesc = NA, Fee = NA) %>%
    bind_rows() %>%
    filter(
      RegNo %in% c('CST6501', 'CST6550', 'CST8902', 'CST8351', 'TAP1751', 'CST1950', 'CST2100', 'CST8750', 'CST8700', 'CST8903') |
        str_detect(Name, "^Nepejch") |
        SI %in% c("2164948")
    ) %>%
    select(ClassDesc, Name, SI, Fee) %>%
    mutate(event_id)
  
  return(j)
}

get_our_start_list <- function(event_id) {
  url <- glue("https://oris.ceskyorientak.cz/API/?format=json&method=getEventStartLists&eventid={event_id}")
  r <- GET(url)
  stop_for_status(r)
  j <- fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)$Data %||%
    tibble(RegNo = NA_character_, Name = NA_character_, StartTime = NA_character_, ClassDesc = NA_character_) %>%
    bind_rows() %>%
    filter(
      RegNo %in% c('CST6501', 'CST6550', 'CST8902', 'CST8351', 'TAP1751', 'CST1950', 'CST2100', 'CST8750', 'CST8700', 'CST8903') |
        str_detect(Name, "^Nepejch")
    ) %>%
    select(ClassDesc, Name, StartTime) %>%
    arrange(StartTime) %>%
    mutate(event_id)
  
  return(j)
}

fn_script_dir <- function() {
  this_file <- grep("^--file=", commandArgs(), value = TRUE)
  this_file <- gsub("^--file=", "", this_file)
  if (length(this_file) == 0) {
    this_file <- Sys.getenv("GITHUB_WORKSPACE", unset = NA)
    if (!is.na(this_file)) {
      return(normalizePath(this_file))
    }
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      this_file <- rstudioapi::getSourceEditorContext()$path
    } else {
      stop("Can't determine script directory: not in RStudio and not running as script.")
    }
  }
  return(normalizePath(dirname(this_file)))
}

wd <- fn_script_dir() 

events_df <- get_event_list(date_from = current_date - days(7), date_to = current_date + days(30))

our_future_entries <- events_df %>%
  filter(cst_entries >= 1 & date >= current_date) %>%
  pull(event_id) %>%
  # head(2) %>%
  map_df(~ tryCatch(get_our_entries(.x), error = function(e) NULL)) %>% 
  group_by(event_id) %>%
  summarise(
    our_future_entries = list(pick(everything()))
  ) %>% 
  ungroup()

# startovka
our_future_start_lists <- events_df %>%
  filter(cst_entries >= 1 & date >= current_date) %>%
  pull(event_id) %>%
  map_df(~ tryCatch(get_our_start_list(.x), error = function(e) NULL)) %>%
  group_by(event_id) %>%
  summarise(our_future_start_lists = list(pick(everything()))) %>%
  ungroup()

out_df <- events_df %>%
  left_join(our_future_entries, by = join_by(event_id)) %>%
  left_join(our_future_start_lists, by = join_by(event_id))


# output html -------------------------------------------------------------

html_template <- fs::path(wd, "docs", "index_api_template.html") %>%
  read_file()

json_out <- out_df %>%
  # filter(event_id %in% c(9140, 9101)) %>%
  toJSON(auto_unbox = T)

html_out <- html_template %>%
  str_replace("__ORIS_DATA__", json_out)

html_out %>%
  write_file(file = fs::path(wd, "index_api.html"))
  

library(tidyverse)
library(fs)
library(rvest)
library(glue)
library(jsonlite)

fn_script_dir <- function() {
  # Works in both interactive RStudio and non-interactive Rscript (e.g., GitHub Actions)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg))))
  } else {
    # In Actions (Rscript), default to the current working directory (repo root)
    return(normalizePath(getwd()))
  }
}

wd <- fn_script_dir()

# na 90 dní dopředu
races_url <- glue(
  "https://oris.orientacnisporty.cz/?date_from={today()}&date_to={today()+90}"
) %>%
  read_html() %>%
  html_elements("#data_table a") %>%     # any <a> inside the table
  html_attr("href") %>%
  keep(~ str_detect(.x, "Zavod"))

race_scrape_fn <- function(url) {
  one_race_html_part <- url %>%
    read_html() %>%
    html_elements("div.container-fluid") %>%
    html_elements("div.pb-1")
  
  df <- tibble(
    nazev = one_race_html_part %>% html_elements("div.col-4") %>% html_text2(),
    hodnota = one_race_html_part %>% html_elements("div.col-8") %>% html_text2()
  ) %>%
    filter(str_detect(nazev, "ORIS ID|Název|Datum|Místo konání|Start 00|Sport|termín přihlášek|Souřadnice"))

  gps <- df %>%
    filter(str_detect(nazev, "Souřadnice")) %>%
    pull(hodnota) %>%
    str_extract("^[^:]+") %>%
    paste0("")
  
  date <- df %>%
    filter(str_detect(nazev, "Datum")) %>%
    pull(hodnota) %>%
    head(1)
  
  return(tibble("gps" = gps, "date" = date, "url" = url, "tbl" = list(df)))
  
  Sys.sleep(0.3)
  
}

df_all_races <- races_url %>%
  # head() %>%              # keep for debugging if needed
  map_dfr(race_scrape_fn)

json <- toJSON(df_all_races, auto_unbox = TRUE, dataframe = "rows")

html_template <- read_file(
  "https://raw.githubusercontent.com/novotny1akub/obcicd/refs/heads/main/docs/index.tmpl.html"
) %>%
  str_replace("__ORIS_DATA__", json)

html_template %>%
  write_file(file = fs::path(wd, "index.html"))




# scrape_oris.R
library(tidyverse)
library(rvest)

main <- function() {
  message("Starting scrape: ", Sys.time())
  zavody_html <- read_html("https://oris.orientacnisporty.cz/")
  
  zavody_url <- zavody_html %>%
    html_elements("#data_table a") %>%
    html_attr("href") %>%
    keep(~str_detect(.x, "Zavod")) %>%
    unique()
  
  scrape_zavod <- function(url) {
    tryCatch({
      html <- read_html(url)
      rows <- html %>% html_elements("div.row") %>% html_text2()
      tibble(
        url = url,
        datum = rows %>% str_extract("Datum:\\s*[^\r\n]+") %>% str_remove("^Datum:\\s*") %>% discard(is.na) %>% first(),
        zavod_nazev = rows %>% str_extract("Název:\\s*[^\r\n]+") %>% str_remove("^Název:\\s*") %>% discard(is.na) %>% first(),
        misto_konani = rows %>% str_extract("Místo konání:\\s*[^\r\n]+") %>% str_remove("^Místo konání:\\s*") %>% discard(is.na) %>% first(),
        gps_zavod = rows %>% str_extract("-?[0-9]+\\.[0-9]+,\\s*-?[0-9]+\\.[0-9]+") %>% discard(is.na) %>% first()
      )
    }, error = function(e) {
      warning("Failed: ", url, " -> ", conditionMessage(e))
      tibble(url=url, datum=NA, zavod_nazev=NA, misto_konani=NA, gps_zavod=NA)
    })
  }
  
  zavody_data <- map_dfr(zavody_url, ~{ Sys.sleep(1); scrape_zavod(.x) }) |>
    mutate(scraped_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  dir.create("data", showWarnings = FALSE)
  outfile <- file.path("data", paste0("oris_", format(Sys.Date(), "%Y-%m-%d"), ".csv"))
  readr::write_csv(zavody_data, outfile)
  message("Wrote: ", outfile)
}

if (identical(environment(), globalenv())) main()
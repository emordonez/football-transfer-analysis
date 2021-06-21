# SETUP =======================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "broom",
  "data.table",
  "dplyr",
  "readr",
  "ggplot2",
  "glue",
  "showtext",
  "tidyr",
  "xtable"
)

DATA_URL <- paste("https://drive.google.com/",
  "uc?export=download&id=1i_UREnRvRtPDKpADQRmE6_EMgHbzF0vP",
  sep="")

LEAGUES <- c(
  "England",
  "Germany",
  "Italy",
  "Spain",
  "France"
)
# Exchange rate is fixed to 16 Apr 2021, when I last web scraped the above data
EUR_TO_GBP <- 0.8635

OUTPUT_1 <- "output-1-spending-inequality.tex"
OUTPUT_2 <- "output-2-defender-attacker-values.tex"
OUTPUT_3 <- "output-3-english-player-premium.tex"

# DATA ========================================================================

get_league_df <- function(league) {
  dir("./data", pattern = "[12][0129]\\d\\d") %>%
    file.path("./data", ., paste0(sprintf("%s.csv", league))) %>%
    lapply(read_csv, col_types = cols()) %>%
    rbindlist(use.names = TRUE)
}

if (!dir.exists("./data")) {
  download.file(DATA_URL, destfile = "data.zip")
  system(paste(
    "/bin/bash -c",
    shQuote("mkdir data && unzip data.zip -d data && rm data.zip")
  ))
} else {
  print("Data directory already exists. Ensure it contains all needed data!")
}

data <- rbindlist(list(
  get_league_df("premier-league") %>% mutate(league = "England"),
  get_league_df("ligue-1") %>% mutate(league = "France"),
  get_league_df("1-bundesliga") %>% mutate(league = "Germany"),
  get_league_df("serie-a") %>% mutate(league = "Italy"),
  get_league_df("laliga") %>% mutate(league = "Spain")
), use.names = TRUE)

system(sprintf("echo '' > %s", OUTPUT_1))
system(sprintf("echo '' > %s", OUTPUT_2))
system(sprintf("echo '' > %s", OUTPUT_3))
print("Output files will be overwritten each time you run an analysis.")

rm(get_league_df)
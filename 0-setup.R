# SETUP =======================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  "broom",
  "data.table",
  "dplyr",
  "readr",
  "ggplot2",
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
OUTPUT_1 <- "output-spending-inequality.tex"
OUTPUT_2 <- "output-defender-attacker-values.tex"
OUTPUT_3 <- "output-english-player-premium.tex"

# UTILITY FUNCTIONS ===========================================================

get_league_df <- function(league) {
  dir("./data", pattern = "[12][0129]\\d\\d") %>%
    file.path("./data", ., paste0(sprintf("%s.csv", league))) %>%
    lapply(read_csv, col_types = cols()) %>%
    rbindlist(use.names = TRUE)
}

gini <- function(df, col = club) {
  df <- df %>%
    group_by({{ col }}) %>%
    summarize(spend = sum(fee)) %>%
    arrange(spend)
  x <- 0
  n <- nrow(df)
  total_spend <- sum(df$spend)
  for (i in 1:n) {
    x <- x + i * df$spend[i]
  }

  2 * x / (n * total_spend) - (n + 1) / n
}

save_plot <- function(filename, plot, width = 7, height = 7, svg = FALSE) {
  ggsave(
    sprintf("%s.png", filename),
    plot,
    width = width, height = height,
    units = "in", dpi = 96
  )
  if (svg) {
    system(paste("/bin/bash -c", shQuote("mkdir -p svg")))
    ggsave(
      sprintf("svg/%s.svg", filename),
      plot,
      width = width, height = height,
      units = "in", dpi = 96
    )
  }
  invisible(filename)
}

# DATA ========================================================================

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
  get_league_df("premier-league"),
  get_league_df("1-bundesliga") %>% mutate(league = "Bundesliga"),
  get_league_df("laliga") %>% mutate(league = "La Liga"),
  get_league_df("serie-a"),
  get_league_df("ligue-1")
), use.names = TRUE)
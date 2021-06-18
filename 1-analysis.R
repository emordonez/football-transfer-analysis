# INEQUALITIES IN LEAGUE TRANSFER SPENDING ====================================
#
# This analysis investigates whether total transfer spending within leagues and
# between leagues is becoming more unequal over time. We use a Gini coefficient
# calculation to measure these inequalities.

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

ineq_df <- data %>%
  # We look only at transfer purchases with a fee
  filter(movement == "in" & fee > 0) %>%
  mutate(fee = fee / 1e6, season = as.integer(season)) %>%
  select(c(club, fee, league, season))

league_spending <- ineq_df %>%
  group_by(season, league) %>%
  summarize(spend = sum(fee)) %>%
  merge(
    ineq_df %>%
      group_by(season) %>%
      summarize(total = sum(fee)) %>%
      ungroup(),
    by = "season"
  ) %>%
  # Data frames are spread wide for tables
  spread(league, spend) %>%
  select(c(season, England, France, Germany, Italy, Spain, total))

league_proportions <- ineq_df %>%
  group_by(season, league) %>%
  summarize(spend = sum(fee)) %>%
  mutate(proportion = spend / sum(spend)) %>%
  select(-spend) %>%
  spread(league, proportion)

# TODO: Visualization
gini_intraleague <- ineq_df %>%
  group_by(league, season) %>%
  do(data.frame(gini_coeff = gini(.))) %>%
  ungroup() %>%
  mutate(t = season - 1992)

# TODO: Visualization
gini_interleague <- ineq_df %>%
  group_by(season) %>%
  do(data.frame(gini_coeff = gini(., col = league))) %>%
  ungroup() %>%
  mutate(t = season - 1992)

# WRITE RESULTS ===============================================================

sink(OUTPUT_1)
writeLines(
  "% TOTAL SEASON SPENDING BY LEAGUE ============================================="
)
print(
  xtable(
    league_spending,
    caption = "Total transfer spending by league from 1992--2020 (million €).",
    label = "league-spending"
  ),
  include.rownames = FALSE
)
writeLines(
  "% PROPORTION OF TOTAL SPENDING BY LEAGUE ======================================"
)
print(
  xtable(
    league_proportions,
    caption = "Proportions of total transfer spending from 1992--2020.",
    label = "league-proportions"
  ),
  include.rownames = FALSE
)
writeLines(
  "% LINEAR FITS FOR GINI COEFFICIENTS OF LEAGUE TRANSFER SPENDING ==============="
)
for (league_name in LEAGUES) {
  df <- filter(gini_intraleague, league == league_name)
  fit <- lm(gini_coeff ~ t, data = df)
  print(
    xtable(
      summary(fit),
      caption = sprintf("Linear fit for Gini coefficients of %s.", league_name),
      display = rep("g", 5),
      auto = TRUE
    ),
    include.rownames = FALSE
  )
}
writeLines(
  "% LINEAR FIT FOR GINI COEFFICIENT OF SPENDING AMONG ALL LEAGUES ==============="
)
fit <- lm(gini_coeff ~ t, data = gini_interleague)
print(
  xtable(
    summary(fit),
    caption = "Linear fit for Gini coefficients of spending across leagues.",
    display = rep("g", 5),
    auto = TRUE
  ),
  include.rownames = FALSE
)
sink()

rm(fit, gini, ineq_df, league_proportions, league_spending)
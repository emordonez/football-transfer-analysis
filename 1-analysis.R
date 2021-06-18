system(sprintf("echo '' > %s", OUTPUT_1))
system(sprintf("echo '' > %s", OUTPUT_2))
system(sprintf("echo '' > %s", OUTPUT_3))

# INEQUALITIES IN LEAGUE TRANSFER SPENDING ====================================

ineq_df <- data %>%
  # We look only at transfer purchases with a fee
  filter(movement == "in" & fee > 0) %>%
  mutate(
    fee = fee / 1e6,
    season = as.integer(season),
    league = case_when(
      league == "Premier League" ~ "England",
      league == "Ligue 1" ~ "France",
      league == "Bundesliga" ~ "Germany",
      league == "Serie A" ~ "Italy",
      league == "La Liga" ~ "Spain"
    )
  ) %>%
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
  spread(league, spend)

league_proportions <- ineq_df %>%
  group_by(season, league) %>%
  summarize(spend = sum(fee)) %>%
  mutate(proportion = spend / sum(spend)) %>%
  select(-spend) %>%
  spread(league, proportion)

gini_intraleague <- ineq_df %>%
  group_by(league, season) %>%
  do(data.frame(gini_coeff = gini(.))) %>%
  ungroup() %>%
  mutate(t = season - 1992)

gini_interleague <- ineq_df %>%
  group_by(season) %>%
  do(data.frame(gini_coeff = gini(., col = league))) %>%
  ungroup() %>%
  mutate(t = season - 1992)

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
      display = c("g", "g", "g", "g", "g"),
      auto = TRUE
    ),
    include.rownames = FALSE
  )
}
writeLines(
  "% LINEAR FIT FOR GINI COEFFICIENT OF SPENDING AMONG ALL LEAGUES ==============="
)
print(
  xtable(
    summary(lm(gini_coeff ~ t, data = gini_interleague)),
    caption = "Linear fit for Gini coefficients of spending across leagues.",
    display = c("g", "g", "g", "g", "g"),
    auto = TRUE
  ),
  include.rownames = FALSE
)
sink()
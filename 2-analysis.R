# VALUE COMPARISON OF DEFENDERS VS ATTACKERS ==================================

# Fees are easily visually verified to be non-normal
# We use the Shapiro-Wilk test to determine if fees are log normal
sw_as_dataframe <- function(df, year) {
  tidy(shapiro.test(df$log_fee)) %>%
    mutate(season = year, sample.size = nrow(df)) %>%
    select(c(season, statistic, sample.size, p.value))
}

# We use the Kolmogorov-Smirnov test to determine if log fees for defenders and
# attackers are distributed the same way
ks_as_dataframe <- function(df1, df2, year) {
  tidy(ks.test(df1$log_fee, df2$log_fee)) %>%
    mutate(season = year) %>%
    select(c(season, statistic, p.value))
}

def_att_df <- data %>%
  filter(movement == "in" & fee > 0) %>%
  filter(
    short_pos %in% c("LW", "SS", "RW", "CF", "attack") |
    short_pos %in% c("LB", "SW", "RB", "CB", "defence")
  ) %>%
  mutate(
    fee = fee / 1e6,
    log_fee = log(fee),
    season = as.integer(season),
    is_attacker = ifelse(
      short_pos %in% c("LW", "SS", "RW", "CF", "attack"),
      TRUE, FALSE
    ),
    league = case_when(
      league == "Premier League" ~ "England",
      league == "Ligue 1" ~ "France",
      league == "Bundesliga" ~ "Germany",
      league == "Serie A" ~ "Italy",
      league == "La Liga" ~ "Spain"      
    )
  ) %>%
  select(c(name, fee, log_fee, league, season, is_attacker))

# TODO: Visualization
def_att_comparison <- def_att_df %>%
  group_by(season, league, is_attacker) %>%
  summarize(
    mean_fee = mean(fee),
    med_fee = median(fee),
    mean_log_fee = mean(log_fee),
    med_log_fee = median(log_fee)
  )

df <- filter(def_att_df, season == 1992)
df_att <- filter(df, is_attacker)
df_def <- filter(df, !is_attacker)

sw_att_results <- sw_as_dataframe(df_att, 1992)
sw_def_results <- sw_as_dataframe(df_def, 1992)
ks_results <- ks_as_dataframe(df_att, df_def, 1992)

for (year in 1993:2020) {
  df <- filter(def_att_df, season == year)
  df_att <- filter(df, is_attacker)
  df_def <- filter(df, !is_attacker)

  temp_sw_att <- sw_as_dataframe(df_att, year)
  temp_sw_def <- sw_as_dataframe(df_def, year)
  temp_ks <- ks_as_dataframe(df_att, df_def, year)

  sw_att_results <- rbind(sw_att_results, temp_sw_att)
  sw_def_results <- rbind(sw_def_results, temp_sw_def)
  ks_results <- rbind(ks_results, temp_ks)
}

sw_att_results <- sw_att_results %>%
  rename(att.stat = statistic, att.n = sample.size, att.p = p.value)
sw_def_results <- sw_def_results %>%
  rename(def.stat = statistic, def.n = sample.size, def.p = p.value)
sw_results <- merge(sw_att_results, sw_def_results) %>%
  mutate(season = as.integer(season))
ks_results <- ks_results %>% mutate(season = as.integer(season))

sink(OUTPUT_2)
writeLines(
  "% TESTS FOR IF FEES ARE LOG NORMAL ============================================"
)
print(
  xtable(
    sw_results,
    caption = "Shapiro--Wilk tests for normality.",
    label = "sw-test-log-fee",
    display = c("d", "d", "f", "d", "g", "f", "d", "g"),
    auto = TRUE
  ),
  include.rownames = FALSE
)
writeLines(
  "% TESTS FOR IF LOG FEES ARE IDENTICALLY DISTRIBUTED ==========================="
)
print(
  xtable(
    ks_results,
    caption = "Kolmogorov--Smirnov tests for if distributions are identical.",
    label = "ks-test-log-fee",
    display = c("d", "d", "f", "g"),
    auto = TRUE 
  ),
  include.rownames = FALSE
)
sink()

rm(
  df, df_att, df_def,
  ks_as_dataframe, ks_results,
  sw_as_dataframe, sw_att_results, sw_def_results, sw_results
)
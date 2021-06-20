# VALUE COMPARISON OF DEFENDERS VS ATTACKERS ==================================
#
# This analysis determines whether the top European leagues value defenders or
# attackers more and whether any such differences are changing over time.

# TEST UTILITY FUNCTIONS ======================================================

# Fees are easily visually verified to be non-normal
# We use the Shapiro-Wilk test to determine if fees are log normal
sw_as_dataframe <- function(df, year) {
  tidy(shapiro.test(df$log_fee)) %>%
    mutate(season = year, sample.size = nrow(df)) %>%
    select(c(season, statistic, sample.size, p.value))
}

# We use the Kolmogorov-Smirnov test to determine if fees for defenders and
# attackers are distributed the same way
ks_as_dataframe <- function(df1, df2, year) {
  tidy(ks.test(df1$fee, df2$fee)) %>%
    mutate(season = year) %>%
    select(c(season, statistic, p.value))
}

# We use the Mann-Whitney U test to compare mean ranks of the fees
mwu_as_dataframe <- function(df, year) {
  # Note that `is_attacker == FALSE` comes first, so we use a "less"
  # one-tailed test to test if defenders cost less than attackers
  tidy(wilcox.test(df$fee ~ df$is_attacker, alternative = "less")) %>%
    mutate(
      season = year,
      att.n = nrow(filter(df, is_attacker)),
      def.n = nrow(filter(df, !is_attacker))
    ) %>%
    select(c(season, att.n, def.n, statistic, p.value))
}

# We use a permutation test for significance of the median differences between
# defender and attacker populations
median_diff <- function(x, n_a, n_b) {
  index <- sample.int(n_a + n_b, n_b)
  a <- x[index]
  b <- x[-index]
  
  median(a) - median(b)
}

perm_test <- function(df, n_iter) {
  df_a <- filter(df, is_attacker)
  df_b <- filter(df, !is_attacker)
  n_a <- nrow(df_a)
  n_b <- nrow(df_b)

  w <- replicate(n_iter, median_diff(df$fee, n_a, n_b))
  w_star <- median(df_a$fee) - median(df_b$fee)

  mean(abs(w) > abs(w_star))
}

perm_as_dataframe <- function(df, year, n_iter = 1000) {
  df_att <- filter(df, is_attacker)
  df_def <- filter(df, !is_attacker)
  tibble(
    season = year,
    med.att = median(df_att$fee),
    med.def = median(df_def$fee),
    med.diff = med.att - med.def,
    p.value = perm_test(df, n_iter)
  )
}

# TODO: Visualization (density plots)
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
  group_by(season, is_attacker) %>%
  summarize(
    mean_fee = mean(fee),
    med_fee = median(fee),
    mean_log_fee = mean(log_fee),
    med_log_fee = median(log_fee)
  )

# TESTS FOR NORMALITY, DISTRIBUTION EQUALITY, AND MEAN/MEDIAN DIFFERENCES =====

df <- filter(def_att_df, season == 1992)
df_att <- filter(df, is_attacker)
df_def <- filter(df, !is_attacker)

sw_att_results <- sw_as_dataframe(df_att, 1992)
sw_def_results <- sw_as_dataframe(df_def, 1992)
ks_results <- ks_as_dataframe(df_att, df_def, 1992)
mwu_results <- mwu_as_dataframe(df, 1992)
perm_results <- perm_as_dataframe(df, 1992)

print("Now running 1000 permutations of fees for each of 29 seasons...")
for (year in 1993:2020) {
  df <- filter(def_att_df, season == year)
  df_att <- filter(df, is_attacker)
  df_def <- filter(df, !is_attacker)

  temp_sw_att <- sw_as_dataframe(df_att, year)
  temp_sw_def <- sw_as_dataframe(df_def, year)
  temp_ks <- ks_as_dataframe(df_att, df_def, year)
  temp_mwu <- mwu_as_dataframe(df, year)
  temp_perm <- perm_as_dataframe(df, year)

  sw_att_results <- rbind(sw_att_results, temp_sw_att)
  sw_def_results <- rbind(sw_def_results, temp_sw_def)
  ks_results <- rbind(ks_results, temp_ks)
  mwu_results <- rbind(mwu_results, temp_mwu)
  perm_results <- rbind(perm_results, temp_perm)
}

sw_att_results <- sw_att_results %>%
  rename(att.stat = statistic, att.n = sample.size, att.p = p.value)
sw_def_results <- sw_def_results %>%
  rename(def.stat = statistic, def.n = sample.size, def.p = p.value)
sw_results <- merge(sw_att_results, sw_def_results) %>%
  mutate(season = as.integer(season))
ks_results <- ks_results %>% mutate(season = as.integer(season))
mwu_results <- mwu_results %>% mutate(season = as.integer(season))
perm_results <- perm_results %>% mutate(season = as.integer(season))

# TODO: Visualization
median_diff_trend <- perm_results %>% mutate(significant = p.value < 0.1)

# WRITE RESULTS ===============================================================

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
  "% TESTS FOR IF FEES ARE IDENTICALLY DISTRIBUTED ==============================="
)
print(
  xtable(
    ks_results,
    caption = "Kolmogorov--Smirnov tests for if distributions are identical.",
    label = "ks-test-fee",
    display = c("d", "d", "f", "g"),
    auto = TRUE 
  ),
  include.rownames = FALSE
)
writeLines(
  "% TESTS FOR IF MEAN RANKS OF FEES ARE DIFFERENT ==============================="
)
print(
  xtable(
    mwu_results,
    caption = "Mann--Whitney U tests for if mean ranks differ.",
    label = "mwu-test-fee",
    display = c("d", "d", "d", "d", "f", "g"),
    auto = TRUE
  ),
  include.rownames = FALSE
)
writeLines(
  "% TESTS FOR IF MEDIANS OF FEES ARE DIFFERENT =================================="
)
print(
  xtable(
    perm_results,
    caption = "Permutation tests for if medians of fees are different.",
    label = "perm-test-fee",
    display = c("d", "d", "f", "f", "f", "g"),
    auto = TRUE
  ),
  include.rownames = FALSE
)
sink()

rm(
  df, df_att, df_def,
  ks_as_dataframe, ks_results,
  mwu_as_dataframe, mwu_results,
  perm_as_dataframe, perm_results,
  sw_as_dataframe, sw_att_results, sw_def_results, sw_results
)
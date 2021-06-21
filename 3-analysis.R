# HOMEGROWN PLAYER PREMIUM IN THE PREMIER LEAGUE ==============================
#
# This analysis looks at premiums paid for English players in the Premier
# League. We use a difference-in-differences estimate to measure the effect
# of the Homegrown Player Rule since in went into effect in advance of the
# 2010/11 season.

pl_hgp <- data %>%
  # Market values in the Premier League are available only since 2004
  filter(league == "England" & season >= 2004) %>%
  # We look only at transfer purchases with an estimated and actual fee
  filter(movement == "in" & market_value > 0 & fee > 0) %>%
  filter(!is.na(nationality) & !is.na(short_pos)) %>%
  mutate(
    fee = EUR_TO_GBP * fee / 1e6,
    market_value = EUR_TO_GBP * market_value / 1e6,
    premium = fee - market_value,
    is_english = nationality == "England",
    is_gk = short_pos == "GK",
    is_def = short_pos %in% c("LB", "CB", "RB", "SW", "defence"),
    is_mid = short_pos %in% c("DM", "LM", "CM", "RM", "AM", "midfield"),
    is_fw = short_pos %in% c("LW", "SS", "RW", "CF", "attack"),
    is_big_six = ifelse(
      club %in% c(
        "Arsenal FC", "Chelsea FC", "Liverpool FC",
        "Manchester City", "Manchester United", "Tottenham Hotspur"
      ),
      TRUE, FALSE
    ),
    t = season - 2004
  ) %>%
  select(
    -c(club, nationality, position, short_pos, dealing_club, league,
    dealing_country, movement, window, is_loan, loan_status)
  )

# MULTIPLE REGRESSION =========================================================

# We first determine if a domestic player premium has existed since 2010
hgp_estimate <- filter(pl_hgp, season >= 2010)
col_list <- list(
  "premium", "age", "is_english", "is_gk",
  "is_def", "is_mid", "is_fw", "is_big_six"
)

summary_2010 <- rbindlist(lapply(col_list, function(col) {
  col_var <- get(col, hgp_estimate)
  tibble(
    variable = col,
    mean = mean(col_var),
    std.dev = sd(col_var),
    min = min(col_var),
    max = max(col_var)
  )
}), use.names = TRUE)

cor_matrix <- cor(hgp_estimate %>%
  select(c(age, is_english, is_gk, is_def, is_mid, is_big_six))
)

fit <- lm(
  # Note that attackers are the omitted category
  premium ~
  age +
  is_english +
  is_gk +
  is_def +
  is_mid +
  is_big_six,
  data = hgp_estimate
)

new_fit <- lm(
  # We drop `is_mid` from the regression because it is the most insignificant
  premium ~
  age +
  is_english +
  is_gk +
  is_def +
  is_big_six,
  data = hgp_estimate
)

# We run a partial F-test for validity of the new fit
partial_f <- anova(fit, new_fit)

# DIFFERENCE-IN-DIFFERENCES ESTIMATE ==========================================

population_balance <- function(df, col) {
  if (col == "is_english") { return() }
  col_var <- get(col, df)
  tidy(t.test(col_var ~ df$is_english)) %>%
    rename(
      diff = estimate,
      control = estimate1,
      treatment = estimate2
    ) %>%
    mutate(
      variable = col,
      control.sd = sd(get(col, filter(df, !is_english))),
      treatment.sd = sd(get(col, filter(df, is_english))),
      sample = mean(col_var),
      sample.sd = sd(col_var)
    ) %>%
    select(
      variable, control, treatment, diff, p.value, control.sd, treatment.sd
    )
}

pre_post_differences <- function(df, col) {
  if (col == "is_english") { return() }
  col_var <- get(col, df)
  tidy(t.test(col_var ~ df$hgp_rule)) %>%
    rename(difference = estimate, pre = estimate1, post = estimate2) %>%
    mutate(
      variable = col,
      # We want the difference to be read as "post minus pre"
      difference = difference * -1,
      pre.sd = sd(get(col, filter(df, !hgp_rule))),
      post.sd = sd(get(col, filter(df, hgp_rule)))
    ) %>%
    select(variable, pre, post, difference, p.value, pre.sd, post.sd)
}

# We introduce indicators for years and for if the HG Player rule is in effect
hgp_did <- pl_hgp %>% mutate(hgp_rule = season >= 2010)
for (n in 0:max(hgp_did$t)) {
  hgp_did <- hgp_did %>% mutate("t{n}" := t == n)
}
time_indicators <- c()
for (i in 0:16) {
  time_indicators[i + 1] <- sprintf("t%d", i)
}

# We ensure the control and treatment groups are balanced for observables
balancing_table <- rbindlist(
  lapply(col_list, population_balance, df = hgp_did),
  use.names = TRUE
  ) %>%
  mutate(
    control.n = nrow(filter(hgp_did, !is_english)),
    treatment.n = nrow(filter(hgp_did, is_english)),
    sample.n = nrow(hgp_did)
  )
imbalances <- (balancing_table %>%
  filter(p.value < 0.1 & variable != "premium"))$variable %>%
  paste(collapse = " + ")

# The intended naive regression is:
# `premium ~ is_english + hgp_rule + is_english * hgp_rule`
outcome <- "premium"
regressors <- c("is_english", "hgp_rule", "is_english * hgp_rule")

naive_model <- lm(
  premium ~
  is_english +
  hgp_rule +
  is_english * hgp_rule,
  data = hgp_did
)

# We add the unbalanced variables as controls, which should be:
# `is_gk + is_fw + is_big_six`
variables <- paste(c(regressors, imbalances), collapse = " + ")
f <- paste(outcome, variables, sep = " ~ ")
imbalances_model <- lm(f, data = hgp_did)

# The final model includes year indicators and interaction terms for any still
# significant imbalances, which should be: `is_big_six`
interactions <- c(
  "is_english * is_big_six",
  "hgp_rule * is_big_six",
  "is_english * hgp_rule * is_big_six"
)
variables <- paste(
  c(regressors, imbalances, interactions, time_indicators),
  collapse = " + "
)
f <- paste(outcome, variables, sep = " ~ ")
interaction_model <- lm(f, data = hgp_did)

# We summarize the differential findings in these tables
treatment <- filter(hgp_did, is_english)
treatment_pre_post <- rbindlist(
  lapply(col_list, pre_post_differences, df = treatment),
  use.names = TRUE
)
control <- filter(hgp_did, !is_english)
control_pre_post <- rbindlist(
  lapply(col_list, pre_post_differences, df = control),
  use.names = TRUE
)

# WRITE RESULTS ===============================================================

sink(OUTPUT_3)
writeLines(
  "% SUMMARY STATISTICS FOR SAMPLE SINCE 2010 ===================================="
)
print(
  xtable(
    summary_2010,
    caption = sprintf("Summary stats for %d transfers.", nrow(hgp_estimate)),
    label = "summary-pl-2010"
  ),
  include.rownames = FALSE
)
writeLines(
  "% CORRELATION MATRIX FOR REGRESSORS ==========================================="
)
print(
  xtable(
    cor_matrix,
    caption = "Correlation matrix for independent regressors.",
    label = "cor-matrix"
  ),
  include.rownames = FALSE
)
writeLines(
  "% ORIGINAL MULTIPLE REGRESSION ================================================"
)
print(summary(fit))
writeLines(
  "% NEW FIT WITH DROPPED REGRESSORS- ============================================"
)
print(summary(new_fit))
writeLines(
  "% PARTIAL F-TEST FOR VALIDITY OF NEW FIT ======================================"
)
print(partial_f)
writeLines(
  "% POPULATION IMBALANCES BETWEEN TREATMENT AND CONTROL GROUPS =================="
)
print(
  xtable(
    balancing_table,
    caption = "Summary statistics of sample and treatment/control groups.",
    label = "treatment-control-balance"
  ),
  include.rownames = FALSE
)
writeLines(
  "% NAIVE MODEL ================================================================="
)
print(summary(naive_model))
writeLines(
  "% MODEL WITH UNBALANCED VARIABLES ============================================="
)
print(summary(imbalances_model))
writeLines(
  "% MODEL WITH INTERACTION TERMS ================================================"
)
print(summary(interaction_model))
writeLines(
  "% DIFFERENCE-IN-DIFFERENCES ESTIMATE OF TREATMENT EFFECT ======================"
)
print(
  xtable(
    treatment_pre_post,
    caption = "Difference-in-differences of the treatment group.",
    label = "diff-in-diff-treatment"
  ),
  include.rownames = FALSE
)
print(
  xtable(
    control_pre_post,
    caption = "Difference-in-differences of the control group.",
    label = "diff-in-diff-control"
  ),
  include.rownames = FALSE
)
sink()
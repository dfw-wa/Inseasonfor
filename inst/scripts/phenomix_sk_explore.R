## =============================================================================
## phenomix_sk_explore.R
##
## In-season sockeye run size prediction using the phenomix package.
## Fits parametric timing curves to daily Bonneville Dam sockeye counts
## (June 1 – July 31), compares candidate model forms, and predicts the
## remaining unobserved counts for 2026.
##
## =============================================================================

# ── 0. Setup ──────────────────────────────────────────────────────────────────

library(tidyverse)
library(lubridate)
library(here)
library(patchwork)


# Load Inseasonfor for bon_dat_fun()
pkgload::load_all(here::here(), quiet = TRUE)


# Load phenomix from local clone (adjust path if needed)
phenomix_path <- "C:/Users/sorelmhs/OneDrive - Washington State Executive Branch Agencies/Documents/Desktop/Other_docs/R packages/phenomix"
pkgload::load_all(phenomix_path, quiet = TRUE)


# Output directory
out_dir <- here::here("inst", "scripts", "output", "phenomix_sk")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ── 1. Pull & Prepare Data ────────────────────────────────────────────────────

pred_date  <- Sys.Date()-1        # (last observed date)
start_year <- 2000              #

message("Pulling Bonneville sockeye counts from FPC ...")

bon_cnts <- bon_dat_fun(
  sdate    = as.Date(paste0(start_year, "-01-01")),
  pred_date = pred_date
)

# Filter to June 1 – July 31 window
sk_window <- bon_cnts |>
  filter(
    (month %in% 6:7),
    # !(month==7&mday>15)
  ) |>
  mutate(doy = yday(CountDate)) |>
  select(year, CountDate, month, mday, doy, count = Sockeye) |>
  # For 2026: keep only observed dates (through pred_date)
  filter(!(year == year(pred_date) & CountDate > pred_date)) |>
  arrange(year, doy)

message(sprintf(
  "Data spans %d – %d; %d total obs; 2026 obs through %s",
  min(sk_window$year), max(sk_window$year),
  nrow(sk_window),
  format(pred_date, "%B %d")
))

# Quick peek at current year data
sk_2026 <- sk_window |> filter(year == year(pred_date))
message(sprintf(
  "2026: %d days observed, cumulative count = %s",
  nrow(sk_2026),
  format(sum(sk_2026$count), big.mark = ",")
))

# ── 2. Candidate Model Definitions ────────────────────────────────────────────
#
# 6 candidates: {asym/sym} × {student_t/gaussian} × {negbin/poisson/lognormal}

candidate_specs <- tribble(
  ~id,   ~asymmetric, ~tail_model,  ~family,
  # "M1",  TRUE,        "student_t",  "negbin",
  # "M2",  FALSE,       "student_t",  "negbin",
  # "M3",  TRUE,        "gaussian",   "negbin",
  # "M4",  FALSE,       "gaussian",   "negbin",
  # "M5",  TRUE,        "student_t",  "poisson",
  # "M6",  FALSE,       "student_t",  "poisson",
  "M7",  TRUE,        "student_t",  "lognormal"#,
  # "M8",  FALSE,       "student_t",  "lognormal",
  # "M9",  TRUE,        "gaussian",   "lognormal",
  # "M10", FALSE,       "gaussian",   "lognormal",
    # "M11",  TRUE,        "gnorm",  "lognormal"
)

# Prepare the data frame phenomix expects.
# Add 0.5 to counts so lognormal models can take log(y) on zero-count days;
# negligible effect on negbin/poisson since 0.5 << typical daily counts.
phenomix_dat <- sk_window |>
  select(year, doy, count) |>
  mutate(count = count + 0.5) |>
  as.data.frame()

# ── 3. Fit All Candidate Models ───────────────────────────────────────────────

message("Fitting candidate phenomix models ...")

fit_one <- function(spec) {
  message(sprintf(
    "  Fitting %s: asym=%s, tail=%s, family=%s ...",
    spec$id, spec$asymmetric, spec$tail_model, spec$family
  ))

  tryCatch({
    dl <- create_data(
      data             = phenomix_dat,
      variable         = "count",
      time             = "year",
      date             = "doy",
      min_number       = 0,
      asymmetric_model = spec$asymmetric,
      tail_model       = spec$tail_model,
      family           = spec$family,
      est_mu_re        = TRUE,
      est_sigma_re     = TRUE,
      max_theta        = 15     # allows totals up to exp(15) ≈ 3.3M; used when limits=TRUE
    )
    fit(dl, silent = TRUE,
        limits    = TRUE,
        control = list(eval.max = 5000, iter.max = 3000, rel.tol = 1e-12))
  }, error = function(e) {
    message("    FAILED: ", conditionMessage(e))
    NULL
  })
}

fits <- pmap(candidate_specs, function(id, asymmetric, tail_model, family) {
  fit_one(list(id = id, asymmetric = asymmetric,
               tail_model = tail_model, family = family))
}) |>
  set_names(candidate_specs$id)

# ── AIC Comparison ─────────────────────────────────────────────────────────

aic_table <- candidate_specs |>
  mutate(
    converged = map_lgl(fits[id], \(f) !is.null(f)),
    AIC       = map_dbl(fits[id], \(f) if (is.null(f)) NA_real_ else AIC(f))
  ) |>
  arrange(AIC)

message("\n── Model comparison (AIC) ──────────────────────────")
print(aic_table)

readr::write_csv(aic_table, file.path(out_dir, "aic_table.csv"))

# Best model
best_id  <- aic_table |> filter(!is.na(AIC)) |> slice(1) |> pull(id)
best_fit <- fits[[best_id]]
best_spec <- candidate_specs |> filter(id == best_id)

message(sprintf("\nBest model: %s (AIC = %.1f)", best_id, AIC(best_fit)))

# ── 4. 2026 In-Season Prediction ─────────────────────────────────────────────

message("\nExtracting 2026 prediction ...")

# Year index for 2026 in the fitted model
# ── 4. 2026 In-Season Prediction ─────────────────────────────────────────────
#
# Use predict.phenomix(newdata=) which evaluates the exact density formula
# (gaussian / student-t / gnorm, symmetric or asymmetric) from the fitted
# parameters — no hand-rolled approximations.

obs_to_date <- sum(sk_2026$count)

# Full Jun 1 – Jul 31 window
full_dates <- seq(as.Date(paste0(year(pred_date), "-06-01")),
                  as.Date(paste0(year(pred_date), "-07-31")), by = 1)
full_doys  <- yday(full_dates)

nd_full <- data.frame(year = year(pred_date), doy = full_doys)
preds_full <- predict(best_fit, newdata = nd_full, se.fit = TRUE)

daily_pred    <- exp(preds_full$pred)
daily_pred_lo <- exp(preds_full$pred - 1.96 * preds_full$se.fit)
daily_pred_hi <- exp(preds_full$pred + 1.96 * preds_full$se.fit)

# Future days only (pred_date+1 through Jul 31)
future_dates <- seq(pred_date + 1,
                    as.Date(paste0(year(pred_date), "-07-31")), by = 1)
future_idx   <- which(full_dates %in% future_dates)

pred_remaining    <- sum(daily_pred[future_idx])
pred_remaining_lo <- sum(daily_pred_lo[future_idx])
pred_remaining_hi <- sum(daily_pred_hi[future_idx])

pred_total_2026 <- obs_to_date + pred_remaining
pred_total_lo95 <- obs_to_date + pred_remaining_lo
pred_total_hi95 <- obs_to_date + pred_remaining_hi

cat(sprintf(
  "\n── 2026 In-Season Prediction (Jun 1 – Jul 31) ──────────────────────
  Observed to date (Jun 1 – %s):  %s
  Predicted remaining (%s – Jul 31):  %s
  Total run size prediction:           %s
  95%% CI (delta method):              [%s, %s]\n",
  format(pred_date, "%b %d"),
  format(round(obs_to_date),     big.mark = ","),
  format(pred_date + 1, "%b %d"),
  format(round(pred_remaining),  big.mark = ","),
  format(round(pred_total_2026), big.mark = ","),
  format(round(pred_total_lo95), big.mark = ","),
  format(round(pred_total_hi95), big.mark = ",")
))

# Write summary
writeLines(
  c(
    sprintf("Best model: %s (asym=%s, tail=%s, family=%s)",
            best_id, best_spec$asymmetric, best_spec$tail_model, best_spec$family),
    sprintf("Observed to date (%s): %s", format(pred_date, "%b %d"),
            format(round(obs_to_date), big.mark = ",")),
    sprintf("Predicted remaining: %s", format(round(pred_remaining), big.mark = ",")),
    sprintf("Total prediction: %s", format(round(pred_total_2026), big.mark = ",")),
    sprintf("95%% CI: [%s, %s]",
            format(round(pred_total_lo95), big.mark = ","),
            format(round(pred_total_hi95), big.mark = ","))
  ),
  con = file.path(out_dir, "prediction_summary_2026.txt")
)

# Build curve_2026 tibble for the visualization section
curve_2026 <- tibble(
  date       = full_dates,
  doy        = full_doys,
  pred_count = daily_pred,
  pred_lo    = daily_pred_lo,
  pred_hi    = daily_pred_hi
)

# ── 5. Historical Fit Visualizations ─────────────────────────────────────────

message("\nCreating historical fit plots ...")

# Built-in timing diagnostic (all years, log scale optional)
p_timing <- plot_diagnostics(best_fit, type = "timing", logspace = FALSE) +
  labs(
    title   = sprintf("Bonneville Sockeye timing fits – %s model", best_id),
    subtitle = sprintf(
      "Asymmetric: %s | Tail: %s | Family: %s",
      best_spec$asymmetric, best_spec$tail_model, best_spec$family
    ),
    x = "Day of year", y = "Daily count"
  ) +
  theme_bw(base_size = 9)

ggsave(file.path(out_dir, "historical_timing_fits.png"),
       p_timing, width = 14, height = 10, dpi = 150)
message("  Saved historical_timing_fits.png")

# Observed vs predicted scatter (historical years only)
p_scatter <- plot_diagnostics(best_fit, type = "scatter", logspace = FALSE) +
  labs(
    title = "Observed vs. Predicted daily counts – all historical years",
    x = "Predicted", y = "Observed"
  ) +
  theme_bw(base_size = 9)

ggsave(file.path(out_dir, "historical_obs_vs_pred.png"),
       p_scatter, width = 14, height = 10, dpi = 150)
message("  Saved historical_obs_vs_pred.png")

# ── 6. Timing Trends & Patterns ───────────────────────────────────────────────

message("\nExtracting timing trends ...")

# Peak timing (mu) by year
mu_df <- extract_means(best_fit) |>
  mutate(year = year_levels) |>
  mutate(
    lo95 = value - 1.96 * sd,
    hi95 = value + 1.96 * sd,
    date_approx = as.Date(paste0(year, "-01-01")) + value - 1  # approx calendar date
  )

# Sigma (spread) by year
sig_df <- extract_sigma(best_fit) |>
  mutate(year = rep(year_levels, length(unique(par)))) |>
  mutate(
    lo95 = value - 1.96 * sd,
    hi95 = value + 1.96 * sd
  )

# Lower / upper quartile by year
lower_df <- extract_lower(best_fit) |> mutate(year = year_levels)
upper_df <- extract_upper(best_fit) |> mutate(year = year_levels)

iqr_df <- inner_join(
  lower_df |> select(year, q25 = value),
  upper_df |> select(year, q75 = value),
  by = "year"
) |>
  mutate(iqr = q75 - q25)

# Plot: peak timing trend
p_mu <- ggplot(mu_df, aes(year, value)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linetype = "dashed",
              linewidth = 0.8) +
  scale_x_continuous(breaks = seq(min(mu_df$year), max(mu_df$year), 5)) +
  labs(
    title    = "Peak sockeye passage timing at Bonneville (mu)",
    subtitle = "Day of year with 95% CI; dashed = linear trend",
    x = "Year", y = "Peak timing (day of year)"
  ) +
  theme_bw()

# Plot: spread (sigma) over time
sig_wide <- sig_df |> pivot_wider(names_from = par, values_from = c(value, sd, lo95, hi95))

p_sigma <- ggplot(sig_df |> filter(par == "sigma1"), aes(year, value)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.2, fill = "darkorange") +
  geom_line(color = "darkorange", linewidth = 1) +
  geom_point(color = "darkorange", size = 2)

if ("sigma2" %in% unique(sig_df$par)) {
  p_sigma <- p_sigma +
    geom_ribbon(
      data = sig_df |> filter(par == "sigma2"),
      aes(ymin = lo95, ymax = hi95), alpha = 0.2, fill = "purple"
    ) +
    geom_line(
      data = sig_df |> filter(par == "sigma2"),
      color = "purple", linewidth = 1
    ) +
    geom_point(
      data = sig_df |> filter(par == "sigma2"),
      color = "purple", size = 2
    )
}

p_sigma <- p_sigma +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick",
              linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(min(sig_df$year), max(sig_df$year), 5)) +
  labs(
    title    = "Spread of sockeye run timing (sigma)",
    subtitle = if ("sigma2" %in% unique(sig_df$par))
      "Orange = sigma1 (pre-peak), Purple = sigma2 (post-peak), dashed = trend"
    else
      "Standard deviation of timing curve; dashed = linear trend",
    x = "Year", y = "Sigma (days)"
  ) +
  theme_bw()

# Plot: IQR (interquartile range) over time
p_iqr <- ggplot(iqr_df, aes(year, iqr)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick",
              linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(min(iqr_df$year), max(iqr_df$year), 5)) +
  labs(
    title    = "Interquartile range of sockeye run timing (75th – 25th percentile)",
    subtitle = "Number of days between 25th and 75th percentile arrival",
    x = "Year", y = "IQR (days)"
  ) +
  theme_bw()

# Combine timing panels
p_trends <- p_mu / p_sigma / p_iqr

ggsave(file.path(out_dir, "timing_trends.png"),
       p_trends, width = 10, height = 14, dpi = 150)
message("  Saved timing_trends.png")

# Print trend summaries
cat("\n── Linear trend in peak timing (mu) ────────────────────────────────\n")
lm_mu <- lm(value ~ year, data = mu_df)
print(summary(lm_mu)$coefficients)

cat("\n── Linear trend in sigma1 ───────────────────────────────────────────\n")
lm_sig <- lm(value ~ year, data = sig_df |> filter(par == "sigma1"))
print(summary(lm_sig)$coefficients)

if ("sigma2" %in% unique(sig_df$par)) {
  cat("\n── Linear trend in sigma2 ───────────────────────────────────────────\n")
  lm_sig2 <- lm(value ~ year, data = sig_df |> filter(par == "sigma2"))
  print(summary(lm_sig2)$coefficients)
}

# ── 7. Current Year 2026 Visualization ───────────────────────────────────────

message("\nCreating 2026 current-year plot ...")

# Observed daily counts through today
obs_2026 <- sk_2026 |>
  transmute(
    date  = CountDate,
    doy   = doy,
    count = count,
    type  = "Observed"
  )

# Predicted daily counts from tomorrow to July 31
future_dates <- seq(pred_date + 1, as.Date(paste0(year(pred_date), "-07-31")), by = 1)

pred_future <- curve_2026 |>
  filter(date %in% future_dates) |>
  transmute(
    date       = date,
    doy        = doy,
    count      = pred_count,
    lo95       = pred_lo,
    hi95       = pred_hi,
    type       = "Predicted"
  )

# Also grab the fitted values for observed days from curve_2026
fitted_obs_2026 <- curve_2026 |>
  filter(date <= pred_date) |>
  mutate(pred_natural = pred_count)

# Annotation string
ann_text <- sprintf(
  "Predicted total Jun 1 – Jul 31\n%s  (95%% CI: %s – %s)",
  format(round(pred_total_2026), big.mark = ","),
  format(round(pred_total_lo95), big.mark = ","),
  format(round(pred_total_hi95), big.mark = ",")
)

p_2026 <- ggplot() +

  # Predicted ribbon for future
  geom_ribbon(
    data = pred_future,
    aes(x = date, ymin = lo95, ymax = hi95),
    fill = "steelblue", alpha = 0.25
  ) +

  # Observed bars
  geom_col(
    data = obs_2026,
    aes(x = date, y = count, fill = "Observed"),
    width = 1, alpha = 0.8
  ) +

  # Predicted line for future
  geom_line(
    data = pred_future,
    aes(x = date, y = count, color = "Predicted"),
    linewidth = 1.2
  ) +

  # Fitted curve overlay for observed portion
  geom_line(
    data = fitted_obs_2026,
    aes(x = date, y = pred_natural, color = "Fitted (obs. period)"),
    linewidth = 0.9, linetype = "dotted"
  ) +

  # Cutoff line
  geom_vline(xintercept = pred_date, color = "grey30",
             linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = pred_date, y = Inf,
           label = format(pred_date, "Last obs.\n%b %d"),
           vjust = 1.5, hjust = 1.05, size = 3, color = "grey30") +

  # Annotation box with total prediction
  annotate("label", x = as.Date(paste0(year(pred_date), "-07-10")),
           y = max(c(obs_2026$count, pred_future$hi95)) * 0.9,
           label = ann_text, size = 3, hjust = 0.5, fill = "lightyellow") +

  scale_fill_manual(values  = c("Observed" = "grey50")) +
  scale_color_manual(values = c("Predicted" = "steelblue",
                                "Fitted (obs. period)" = "firebrick")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Bonneville Dam sockeye counts – 2026 in-season prediction",
    subtitle = sprintf(
      "Best model: %s (asym=%s, tail=%s, family=%s) | Fitted to %s",
      best_id, best_spec$asymmetric, best_spec$tail_model, best_spec$family,
      format(pred_date, "%b %d, %Y")
    ),
    x = NULL, y = "Daily count",
    fill = NULL, color = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave(file.path(out_dir, "current_year_2026.png"),
       p_2026, width = 12, height = 7, dpi = 150)
message("  Saved current_year_2026.png")

# ── 8. Summary Print ──────────────────────────────────────────────────────────

cat("\n════════════════════════════════════════════════════════════════════\n")
cat("  PHENOMIX SOCKEYE EXPLORATION SUMMARY\n")
cat("════════════════════════════════════════════════════════════════════\n")

cat("\nModel comparison (AIC):\n")
print(aic_table |> select(id, asymmetric, tail_model, family, AIC))

cat(sprintf(
  "\nBest model: %s | AIC = %.1f\n", best_id, AIC(best_fit)
))

cat(sprintf(
  "\n2026 prediction (Jun 1 – Jul 31 @ Bonneville):\n  Observed: %s\n  Predicted total: %s  [95%% CI: %s – %s]\n",
  format(round(obs_to_date),     big.mark = ","),
  format(round(pred_total_2026), big.mark = ","),
  format(round(pred_total_lo95), big.mark = ","),
  format(round(pred_total_hi95), big.mark = ",")
))

cat(sprintf(
  "\nTiming trend (peak doy per decade): %.2f days/decade\n",
  coef(lm_mu)["year"] * 10
))

cat(sprintf(
  "Sigma1 trend (days/decade): %.2f\n",
  coef(lm_sig)["year"] * 10
))

cat(sprintf("\nOutputs written to:\n  %s\n", out_dir))
cat("════════════════════════════════════════════════════════════════════\n")


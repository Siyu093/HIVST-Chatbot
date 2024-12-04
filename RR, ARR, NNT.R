library(dplyr)

# Sample data
df <- data.frame(
  group = c("Intervention", "Control"),
  outcome = c(111, 127),
  total = c(240, 230)
)

# Calculate the risk (event rate) in each group
df <- df %>%
  mutate(risk = outcome / total)

# Calculate the effect measures
rr <- df$risk[1] / df$risk[2]
arr <- df$risk[1] - df$risk[2]
nnt <- 1 / arr

# Calculate the 95% confidence intervals
rr_lower <- exp(log(rr) - 1.96 * sqrt(1/df$outcome[1] - 1/df$total[1] + 1/df$outcome[2] - 1/df$total[2]))
rr_upper <- exp(log(rr) + 1.96 * sqrt(1/df$outcome[1] - 1/df$total[1] + 1/df$outcome[2] - 1/df$total[2]))

arr_lower <- df$risk[1] - df$risk[2] - 1.96 * sqrt(df$risk[1] * (1 - df$risk[1]) / df$total[1] + df$risk[2] * (1 - df$risk[2]) / df$total[2])
arr_upper <- df$risk[1] - df$risk[2] + 1.96 * sqrt(df$risk[1] * (1 - df$risk[1]) / df$total[1] + df$risk[2] * (1 - df$risk[2]) / df$total[2])

# Calculate the 95% confidence intervals for NNT using the delta method
se_nnt <- sqrt(df$risk[1] * (1 - df$risk[1]) / (df$total[1] * arr^2) +
                 df$risk[2] * (1 - df$risk[2]) / (df$total[2] * arr^2))
z_score <- qnorm(0.975)
nnt_lower <- nnt - z_score * se_nnt
nnt_upper <- nnt + z_score * se_nnt

# Calculate the chi-square test
chisq_result <- chisq.test(matrix(c(df$outcome[1], df$total[1] - df$outcome[1], 
                                    df$outcome[2], df$total[2] - df$outcome[2]), 
                                  nrow = 2, ncol = 2))
# Calculate the P-value
p_value <- prop.test(c(df$outcome[1], df$outcome[2]), c(df$total[1], df$total[2]), alternative = "two.sided")$p.value

# Summarize the proportions of the two groups
prop_summary <- prop.test(c(df$outcome[1], df$outcome[2]), c(df$total[1], df$total[2]))

# Print the results
cat(paste0("Risk Ratio (RR): ", format(round(rr, 2), nsmall = 2), " (95% CI: ", format(round(rr_lower, 2), nsmall = 2), ", ", format(round(rr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Absolute Risk Reduction (ARR): ", format(round(arr, 2), nsmall = 2), " (95% CI: ", format(round(arr_lower, 2), nsmall = 2), ", ", format(round(arr_upper, 2), nsmall = 2), ")\n"))
cat(paste0("Number Needed to Treat (NNT): ", format(round(nnt, 2), nsmall = 2), " (95% CI: ", format(round(nnt_lower, 2), nsmall = 2), ", ", format(round(nnt_upper, 2), nsmall = 2), ")\n"))
cat("\nProportion Summary:\n")
print(prop_summary)
# Print the p-value
cat("P-value:", chisq_result$p.value, "\n")
cat(paste0("P-value: ", format(round(p_value, 4), nsmall = 4), "\n"))




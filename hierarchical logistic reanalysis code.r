# Purpose: Re-estimate Andersen-model hierarchical logistic regressions
# using continuous and multilevel predictors.


# A quick Helper function
or_ci <- function(fit) { est <- coef(fit)
                         ci  <- suppressMessages(confint(fit))  # profile likelihood CI
                         out <- exp(cbind(OR = est, ci))
                          out}

# Step 1: Predisposing (demographics + psychological)
fit_step1 <- glm(
  amhsu_p5 ~ age_r + gend_d + educ_t + income_t + marit_d +
    mhl_tot + atspphst + p_stigt + s_stigt + beaq_tot + selr_tot +
    pc_tot + neuroticism,
  data = dat, family = binomial
)
summary(fit_step1)
or_ci(fit_step1)

# Step 2: + Enabling (social variables)
fit_step2 <- glm(
  amhsu_p5 ~ age_r + gend_d + educ_t + income_t + marit_d +
    mhl_tot + atspphst + p_stigt + s_stigt + beaq_tot + selr_tot +
    pc_tot + neuroticism +
    moss_tot + rel_tot + lonely,
  data = dat, family = binomial
)
summary(fit_step2)
or_ci(fit_step2)

# Step 3 (Final): + Need factors (perceived need + distress + physical health)
fit_step3 <- glm(
  amhsu_p5 ~ age_r + gend_d + educ_t + income_t + marit_d +
    mhl_tot + atspphst + p_stigt + s_stigt + beaq_tot + selr_tot +
    pc_tot + neuroticism +
    moss_tot + rel_tot + lonely +
    need_r + k6_tot + hlth_tot,
  data = dat, family = binomial
)
summary(fit_step3)
or_ci(fit_step3)

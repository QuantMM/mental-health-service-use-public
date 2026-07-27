# Purpose: Estimate cross-validated LASSO logistic regression with
# all main effects and all two-way interactions.

# cv.glmnet LASSO logistic regression with all two-way interactions
library(glmnet)

set.seed(1)

# outcome: make sure it's 0/1 (adjust levels if needed)
y <- as.integer(dat$amhsu_p5 == "Yes")

# build design matrix with all main effects + all two-way interactions
# remove the intercept column because glmnet adds its own intercept
X <- model.matrix(
  amhsu_p5 ~ (age_r + gend_d + educ_t + income_t + marit_d +
                mhl_tot + atspphst + p_stigt + s_stigt + beaq_tot +
                selr_tot + pc_tot + neuroticism +
                moss_tot + rel_tot + lonely +
                need_r + k6_tot + hlth_tot)^2,
  data = dat
)[, -1]

# cross-validated LASSO (alpha=1) for binomial
cvfit <- cv.glmnet(
  x = X, y = y,
  family = "binomial",
  alpha  = 1,
  nfolds = 10,
  type.measure = "deviance"   # or "class" / "auc"
)

# chosen lambdas
cvfit$lambda.min
cvfit$lambda.1se

# selected coefficients (non-zero), at lambda.1se (more conservative)
coef_1se <- coef(cvfit, s = "lambda.1se")
sel_1se  <- coef_1se[coef_1se[, 1] != 0, , drop = FALSE]
sel_1se

# (optional) less conservative selection at lambda.min
coef_min <- coef(cvfit, s = "lambda.min")
sel_min  <- coef_min[coef_min[, 1] != 0, , drop = FALSE]
sel_min

# results:
> # chosen lambdas
> cvfit$lambda.min
[1] 0.009596809
> cvfit$lambda.1se
[1] 0.03530071
> 
> # selected coefficients (non-zero), at lambda.1se (more conservative)
> coef_1se <- coef(cvfit, s = "lambda.1se")
> sel_1se  <- coef_1se[coef_1se[, 1] != 0, , drop = FALSE]
> sel_1se
11 x 1 sparse Matrix of class "dgCMatrix"
                                                          s1
(Intercept)                                     -0.875479438
age_r:need_rNo perceived need                   -0.010662235
marit_dSeparated/Widowed/Divorced/Single:k6_tot  0.007559605
mhl_tot:neuroticism                              0.058291512
mhl_tot:k6_tot                                   0.005079667
atspphst:neuroticism                             0.001548197
atspphst:lonely                                  0.019600894
s_stigt:need_rNo perceived need                 -0.121731263
s_stigt:need_rDon't know                        -0.053736392
rel_tot:need_rNo perceived need                 -0.076308405
need_rNo perceived need:hlth_tot                -0.177461094
> 
> # (optional) less conservative selection at lambda.min
> coef_min <- coef(cvfit, s = "lambda.min")
> sel_min  <- coef_min[coef_min[, 1] != 0, , drop = FALSE]
> sel_min
23 x 1 sparse Matrix of class "dgCMatrix"
                                                           s1
(Intercept)                                     -0.7260120489
age_r:s_stigt                                   -0.0018876632
age_r:need_rNo perceived need                   -0.0125816569
age_r:hlth_tot                                  -0.0001745281
gend_dFemale:neuroticism                         0.0072209526
gend_dFemale:lonely                              0.0071490433
educ_tHS or less:k6_tot                          0.0032891219
income_t$40K–$79K:need_rDon't know              -0.1210612700
marit_dSeparated/Widowed/Divorced/Single:lonely  0.0301057329
marit_dSeparated/Widowed/Divorced/Single:k6_tot  0.0137323351
mhl_tot:atspphst                                 0.0151442918
mhl_tot:beaq_tot                                 0.0057629886
mhl_tot:neuroticism                              0.0685451295
mhl_tot:need_rDon't know                        -0.1131724869
atspphst:neuroticism                             0.0174299486
atspphst:lonely                                  0.0197686743
s_stigt:need_rNo perceived need                 -0.1801086350
s_stigt:need_rDon't know                        -0.2518669636
pc_tot:k6_tot                                    0.0020713921
neuroticism:moss_tot                             0.0012742535
rel_tot:need_rNo perceived need                 -0.0969866288
need_rNo perceived need:k6_tot                   0.0310774248
need_rNo perceived need:hlth_tot                -0.2380271573

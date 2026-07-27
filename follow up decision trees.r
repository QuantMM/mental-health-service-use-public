#############################
##--------------------------
## Decision Trees (Two Need-Subgroups)
##--------------------------
## Goal:
##   Fit conditional inference classification trees within two theory-motivated
##   subgroups defined by need_r.
##   Select tree complexity using stratified K-fold cross-validation (K=5 or 10)and
##   the 1-SE rule (to avoid subjective pruning).
##
## Subgroups:
##   (A) Perceived need group
##   (B) No perceived need OR Don't know group
##
## Workflow:
##   1) Fit initial ctrees for A and B using the same predictors and permissive controls.
##   2) Choose complexity via CV over (minbucket, maxdepth, mincriterion).
##   3) Use the 1-SE rule to fix the final complexity and refit the final tree.
#############################

dat_A <- subset(dat, need_r == "Perceived need")
dat_B <- subset(dat, need_r %in% c("No perceived need", "Don't know"))

# install.packages("partykit")   # if needed
library(partykit)

# Fit a conditional inference classification tree
# ctree uses test-based stopping rather than post-pruning.
# With the default mincriterion = 0.95, splits require Bonferroni-adjusted p < .05,
# subject to minsplit, minbucket, and maxdepth constraints.

ctrl <- ctree_control(
  testtype     = "Bonferroni", # adjusts for multiple testing over split candidates
  #mincriterion = 0.95,
  #minsplit     = 100,
  minbucket    = 10,
  maxdepth     = 10 
)

preds <- c(
  "pc_tot","mhl_tot","atspphst","k6_tot","s_stigt","p_stigt","neuroticism",
  "age_r","gend_d","educ_t","income_t","selr_tot","moss_tot","rel_tot",
  "beaq_tot","lonely","hlth_tot","marit_d"
)

form_tree <- as.formula(paste("amhsu_p5 ~", paste(preds, collapse = " + ")))

# Fit two separate classification trees
tree_A <- ctree(form_tree, data = dat_A, control = ctrl)
tree_B <- ctree(form_tree, data = dat_B, control = ctrl)

# Print and plot
print(tree_A)
plot(tree_A)  # basic plot

print(tree_B)
plot(tree_B)  # basic plot

# tree_A has single split 
# tree_B needs model complexity decision

# Because this structure was already maximally simple and substantively interpretable,
# the additional CV tuning below is shown for dat_B, where the initial tree was more complex.


library(partykit)

# --- formula ---
form <- amhsu_p5 ~ pc_tot + mhl_tot + atspphst + k6_tot + s_stigt + p_stigt +
  neuroticism + age_r + gend_d + educ_t + income_t + selr_tot + moss_tot +
  rel_tot + beaq_tot + lonely + hlth_tot + marit_d

# --- stratified folds (recommended) ---
set.seed(1)
K <- 5 # stratified 5-fold CV; use K = 10 as a sensitivity check if desired
fold_id <- integer(nrow(dat_B))

y_all <- dat_B$amhsu_p5
stopifnot(is.factor(y_all))

# Assign folds within each class to preserve class proportions across folds
for (lv in levels(y_all)) {
  idx <- which(y_all == lv)
  fold_id[idx] <- sample(rep(1:K, length.out = length(idx)))
}

# --- helper: extract P(Yes) robustly across predict() return types ---
get_p_yes <- function(pred_prob, yes_level) {
  # Case 1: list of named vectors (one per observation)
  if (is.list(pred_prob)) {
    p <- vapply(pred_prob, function(v) {
      if (yes_level %in% names(v)) v[[yes_level]] else NA_real_
    }, numeric(1))
    return(p)
  }

  # Case 2: matrix/data.frame (rows = observations, cols = classes)
  if (is.matrix(pred_prob) || is.data.frame(pred_prob)) {
    pred_prob <- as.matrix(pred_prob)
    if (yes_level %in% colnames(pred_prob)) return(pred_prob[, yes_level])
    return(rep(NA_real_, nrow(pred_prob)))
  }

  stop("Unexpected output from predict(type = 'prob').")
}

# --- CV objective: mean log-loss across folds ---
eval_one <- function(minbucket, maxdepth, mincriterion = 0.95) {
  ll <- numeric(K)

  for (k in 1:K) {
    tr <- dat_B[fold_id != k, , drop = FALSE]
    te <- dat_B[fold_id == k, , drop = FALSE]

    # Define the positive class:
    # use "Yes" if present, otherwise use the second factor level.
    lev <- levels(tr$amhsu_p5)
    yes_level <- if ("Yes" %in% lev) "Yes" else lev[min(2, length(lev))]

    ctrl <- ctree_control(
      testtype     = "Bonferroni",
      mincriterion = mincriterion,
      minsplit     = 2 * minbucket,
      minbucket    = minbucket,
      maxdepth     = maxdepth
    )

    fit <- ctree(form, data = tr, control = ctrl)

    pr <- predict(fit, newdata = te, type = "prob")
    p_yes <- get_p_yes(pr, yes_level)

    # If a fold produces NA probabilities (e.g., due to absent levels), drop those rows
    ok <- is.finite(p_yes)

    y01 <- as.integer(te$amhsu_p5[ok] == yes_level)
    eps <- 1e-12
    p <- pmin(pmax(p_yes[ok], eps), 1 - eps)

    ll[k] <- -mean(y01 * log(p) + (1 - y01) * log(1 - p))
  }

  mean(ll, na.rm = TRUE)
}

# --- grid search over complexity controls ---
grid <- expand.grid(
  minbucket = c(20, 30, 50),
  maxdepth  = c(3, 4),
  mincriterion = c(0.95, 0.99)   # compare less vs more stringent splitting
)

grid$cv_logloss <- mapply(eval_one,
                          minbucket = grid$minbucket,
                          maxdepth = grid$maxdepth,
                          mincriterion = grid$mincriterion)

grid[order(grid$cv_logloss), ]

# --- 1-SE rule: compute fold-wise log-loss per setting, then select simplest within 1 SE of the minimum ---
eval_one_vec <- function(minbucket, maxdepth, mincriterion = 0.95) {
  ll <- numeric(K)
  for (k in 1:K) {
    tr <- dat_B[fold_id != k, , drop = FALSE]
    te <- dat_B[fold_id == k, , drop = FALSE]

    lev <- levels(tr$amhsu_p5)
    yes_level <- if ("Yes" %in% lev) "Yes" else lev[min(2, length(lev))]

    fit <- ctree(form, data = tr, control = ctree_control(
      testtype     = "Bonferroni",
      mincriterion = mincriterion,
      minsplit     = 2 * minbucket,
      minbucket    = minbucket,
      maxdepth     = maxdepth
    ))

    pr <- predict(fit, newdata = te, type = "prob")
    p_yes <- get_p_yes(pr, yes_level)
    ok <- is.finite(p_yes)

    y01 <- as.integer(te$amhsu_p5[ok] == yes_level)
    eps <- 1e-12
    p <- pmin(pmax(p_yes[ok], eps), 1 - eps)

    ll[k] <- -mean(y01 * log(p) + (1 - y01) * log(1 - p))
  }
  ll
}

# Run fold-wise CV for each grid setting
ll_list <- mapply(eval_one_vec,
                  minbucket = grid$minbucket,
                  maxdepth  = grid$maxdepth,
                  mincriterion = grid$mincriterion,
                  SIMPLIFY = FALSE)

grid$mean <- vapply(ll_list, mean, numeric(1), na.rm = TRUE)
grid$se   <- vapply(ll_list, function(v) sd(v, na.rm = TRUE) / sqrt(K), numeric(1))

best <- which.min(grid$mean)
threshold <- grid$mean[best] + grid$se[best]

# Among candidates within 1 SE of the best, select the simplest:
# larger minbucket (more stable leaves) and smaller maxdepth (shallower tree).
cand <- grid[grid$mean <= threshold, ]
cand[order(-cand$minbucket, cand$maxdepth), ][1, ]


# --- Select final complexity from 1-SE rule ---
final_spec <- cand[order(-cand$minbucket, cand$maxdepth), ][1, ]

final_minbucket    <- final_spec$minbucket
final_maxdepth     <- final_spec$maxdepth
final_mincriterion <- final_spec$mincriterion

# --- Refit final tree on full dat_B using selected complexity ---
ctrl_final <- ctree_control(
  testtype     = "Bonferroni",
  mincriterion = final_mincriterion,
  minsplit     = 2 * final_minbucket,
  minbucket    = final_minbucket,
  maxdepth     = final_maxdepth
)

tree_final <- ctree(form, data = dat_B, control = ctrl_final)

# --- Output final model ---
print(tree_final)
plot(tree_final)

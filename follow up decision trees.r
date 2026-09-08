#############################
##--------------------------
## Decision Trees (Two Need-Subgroups)
##--------------------------
## Goal:
##   Fit conditional inference classification trees within two theory-motivated
##   subgroups defined by need_r.
##   Select tree complexity using stratified K-fold cross-validation and the
##   1-SE rule (to avoid subjective pruning).
##
## Subgroups:
##   (A) Perceived need group
##   (B) No perceived need OR Don't know group
##
## Workflow:
##   1) Define one shared grid of control settings and one shared cross-validation
##      routine.
##   2) Run that identical routine separately within subgroup A and subgroup B.
##   3) Use the 1-SE rule to fix the final complexity and refit the final tree
##      on the full subgroup.
##
## Both subgroups pass through exactly the same model-selection procedure; the
## selection is not assumed for either group. This mirrors the framework used
## for the global tree in `global tree _ pruning.r`, with the grid's third knob
## being ctree's statistical splitting criterion (mincriterion) in place of
## rpart's impurity measure.
#############################

library(partykit)
library(grid)

if (!exists("dat")) {
  stop("Object `dat` not found. Run `data prep.r` first to create the ",
       "complete-case analytic dataset.")
}

dat$amhsu_p5 <- factor(dat$amhsu_p5, levels = c("No", "Yes"))

dat_A <- subset(dat, need_r == "Perceived need")
dat_B <- subset(dat, need_r %in% c("No perceived need", "Don't know"))

cat("Subgroup A (perceived need)     : n =", nrow(dat_A), "\n")
cat("Subgroup B (no need / uncertain): n =", nrow(dat_B), "\n")

# Predictors: need_r is excluded because it defines the subgroups.
preds <- c(
  "pc_tot", "mhl_tot", "atspphst", "k6_tot", "s_stigt", "p_stigt", "neuroticism",
  "age_r", "gend_d", "educ_t", "income_t", "selr_tot", "moss_tot", "rel_tot",
  "beaq_tot", "lonely", "hlth_tot", "marit_d"
)
form <- as.formula(paste("amhsu_p5 ~", paste(preds, collapse = " + ")))

##############################
## 1) Shared grid of candidate control settings
##############################
# minbucket    : minimum number of observations in a terminal node
# maxdepth     : maximum depth of the tree
# mincriterion : statistical splitting criterion, i.e. the Bonferroni-adjusted
#                confidence level a split must reach to be retained
#
# ctree uses test-based stopping rather than post-pruning, so these three
# settings jointly determine how complex a candidate tree may become.

grid_template <- expand.grid(
  minbucket    = c(20, 30, 50),
  maxdepth     = c(3, 4),
  mincriterion = c(0.95, 0.99)
)

make_ctrl <- function(minbucket, maxdepth, mincriterion) {
  ctree_control(
    testtype     = "Bonferroni",   # adjusts for multiple testing over split candidates
    mincriterion = mincriterion,
    minsplit     = 2 * minbucket,
    minbucket    = minbucket,
    maxdepth     = maxdepth
  )
}

# Number of internal (splitting) nodes. `nodeids()` returns every node, so the
# terminal nodes must be subtracted to count splits.
n_splits <- function(tree) length(nodeids(tree)) - length(nodeids(tree, terminal = TRUE))

##############################
## 2) Shared cross-validation routine
##############################
# Folds are stratified on the outcome so that each subgroup's service-use rate is
# preserved in every fold; with a rare outcome, unstratified folds can leave a
# fold with too few service users to estimate log-loss stably.
#
# The seed is set inside the function so that each subgroup's fold assignment is
# reproducible and independent of the order in which the subgroups are analysed.

get_p_yes <- function(pred_prob) {
  if (is.list(pred_prob)) {
    return(vapply(pred_prob, function(v) v[["Yes"]], numeric(1)))
  }
  as.matrix(pred_prob)[, "Yes"]
}

tune_subgroup <- function(data, label, K = 5, seed = 1) {

  set.seed(seed)
  fold_id <- integer(nrow(data))
  for (lv in levels(data$amhsu_p5)) {
    idx <- which(data$amhsu_p5 == lv)
    fold_id[idx] <- sample(rep(seq_len(K), length.out = length(idx)))
  }

  # --- fold-wise log-loss for one control setting ---
  eval_setting <- function(minbucket, maxdepth, mincriterion) {
    ll <- numeric(K)
    for (k in seq_len(K)) {
      train <- data[fold_id != k, , drop = FALSE]
      test  <- data[fold_id == k, , drop = FALSE]

      fit <- ctree(form, data = train,
                   control = make_ctrl(minbucket, maxdepth, mincriterion))

      p <- get_p_yes(predict(fit, newdata = test, type = "prob"))
      p <- pmin(pmax(p, 1e-12), 1 - 1e-12)     # guard against log(0)
      y <- as.integer(test$amhsu_p5 == "Yes")

      ll[k] <- -mean(y * log(p) + (1 - y) * log(1 - p))
    }
    # Complexity is indexed by the tree this setting produces on the full subgroup.
    full_fit <- ctree(form, data = data,
                      control = make_ctrl(minbucket, maxdepth, mincriterion))
    c(mean = mean(ll), se = sd(ll) / sqrt(K), splits = n_splits(full_fit))
  }

  # The grid is kept in its original expand.grid order: the 1-SE tie-break below
  # is resolved by that order, so re-sorting here would silently change which
  # setting is retained. A sorted copy is printed for readability instead.
  grid <- grid_template
  grid <- cbind(grid, as.data.frame(t(mapply(
    eval_setting, grid$minbucket, grid$maxdepth, grid$mincriterion
  ))))

  # --- root-only reference model (reported, not a selection candidate) ---
  nl <- numeric(K)
  for (k in seq_len(K)) {
    train <- data[fold_id != k, , drop = FALSE]
    test  <- data[fold_id == k, , drop = FALSE]
    p <- rep(mean(train$amhsu_p5 == "Yes"), nrow(test))
    p <- pmin(pmax(p, 1e-12), 1 - 1e-12)
    y <- as.integer(test$amhsu_p5 == "Yes")
    nl[k] <- -mean(y * log(p) + (1 - y) * log(1 - p))
  }
  null_ll <- c(mean = mean(nl), se = sd(nl) / sqrt(K))

  # --- 1-SE rule ---
  # Among the settings whose cross-validated log-loss is within one standard
  # error of the best, retain the most conservative one: the largest minimum
  # terminal node size (more stable leaves), then the shallowest tree.
  best      <- which.min(grid$mean)
  threshold <- grid$mean[best] + grid$se[best]
  cand      <- which(grid$mean <= threshold)
  sel       <- cand[order(-grid$minbucket[cand], grid$maxdepth[cand])][1]
  selected  <- grid[sel, ]

  final_tree <- ctree(form, data = data,
                      control = make_ctrl(selected$minbucket, selected$maxdepth,
                                          selected$mincriterion))

  cat("\n########", label, "  (n =", nrow(data), ") ########\n")
  print(grid[order(grid$splits, grid$mean), ], row.names = FALSE, digits = 4)
  cat("Root-only reference log-loss:", round(null_ll["mean"], 4),
      "(SE", round(null_ll["se"], 4), ")\n")
  cat("1-SE threshold:", round(threshold, 4), "\n")
  cat("Selected setting:\n"); print(selected, row.names = FALSE, digits = 4)
  cat("\n--- Final tree:", label, "---\n")
  print(final_tree)

  list(grid = grid, threshold = threshold, selected = selected,
       sel_row = sel, null_ll = null_ll, tree = final_tree, label = label,
       n = nrow(data))
}

##############################
## 3) Run the identical procedure in both subgroups
##############################

res_A <- tune_subgroup(dat_A, "Group 1: Perceived need")
res_B <- tune_subgroup(dat_B, "Group 2: No need or uncertain")

tree_A <- res_A$tree
tree_B <- res_B$tree

##############################
## 4) Manuscript figures: final subgroup trees
##############################

if (!dir.exists("Figures")) dir.create("Figures")

# Figure 2: perceived-need subgroup
tiff("Figures/Figure2_ctree.tiff",
     width = 70, height = 110, units = "mm",
     res = 300, compression = "lzw",
     pointsize = 9)
plot(tree_A, gp = gpar(fontsize = 9))
dev.off()

# Figure 3: no-need/uncertain subgroup
tiff("Figures/Figure3_ctree_final.tiff",
     width = 170, height = 155, units = "mm",
     res = 300, compression = "lzw",
     pointsize = 8)
plot(tree_B,
     gp = gpar(fontsize = 8),
     tp_args = list(
       id      = TRUE,
       ylines  = 2.5,
       mainlab = function(id, nobs) paste0("Node ", id, " (n = ", nobs, ")")
     ))
dev.off()

##############################
## 5) Cross-validation figure for the subgroup tuning
##############################
# One point per candidate control setting, positioned at the number of splits
# that setting produces, with +/-1 standard error bars. Points at the same
# complexity are jittered horizontally so that overlapping settings remain
# visible. The dotted line is the 1-SE threshold and the highlighted point is
# the setting retained by the 1-SE rule.

panel_cv <- function(res) {
  g <- res$grid
  x <- g$splits + stats::ave(g$splits, g$splits,
                             FUN = function(v) seq(-0.22, 0.22, length.out = length(v)))
  ylim <- range(c(g$mean - g$se, g$mean + g$se, res$threshold))
  ylim <- ylim + c(0.08, 0.28) * diff(ylim) * c(-1, 1)   # headroom for the legend
  xr <- range(g$splits) + c(-0.6, 0.6)

  plot(x, g$mean, pch = 19, xlim = xr, ylim = ylim,
       xlab = "Number of Splits (Tree Complexity)",
       ylab = "CV Log-Loss", main = res$label, xaxt = "n")
  axis(1, at = sort(unique(g$splits)))
  arrows(x, g$mean - g$se, x, g$mean + g$se, angle = 90, code = 3, length = 0.03)
  abline(h = res$threshold, col = "red", lty = 3, lwd = 1.5)
  points(x[res$sel_row], g$mean[res$sel_row], pch = 21, bg = "lightblue", cex = 1.6)
  legend("topright",
         legend = c(paste0("Selected: minbucket ", res$selected$minbucket,
                           ", maxdepth ", res$selected$maxdepth,
                           ", mincriterion ", res$selected$mincriterion),
                    paste0("1-SE threshold = ", round(res$threshold, 3))),
         pch = c(21, NA), pt.bg = c("lightblue", NA),
         col = c("black", "red"), lty = c(NA, 3),
         bty = "n", cex = 0.75)
}

draw_subgroup_cv <- function() {
  op <- par(mfrow = c(2, 1), mar = c(4.2, 4.2, 2.2, 1))
  panel_cv(res_A)
  panel_cv(res_B)
  par(op)
}

png("subgroup tree CV results.png", width = 660, height = 860, res = 100)
draw_subgroup_cv()
dev.off()

##############################
## 6) Sanity checks against the values reported in the manuscript
##############################

report_nodes <- function(tree, label) {
  ids <- nodeids(tree, terminal = TRUE)
  y   <- factor(data_party(tree)[["(response)"]], levels = c("No", "Yes"))
  fit <- predict(tree, type = "node")
  cat("\n", label, "\n", sep = "")
  for (i in ids) {
    yi <- y[fit == i]
    cat(sprintf("  Node %2d: n = %4d, service use = %3d (%.1f%%)\n",
                i, length(yi), sum(yi == "Yes"), 100 * mean(yi == "Yes")))
  }
}

report_nodes(tree_A, "Group 1 terminal nodes (manuscript: 56.7% at n = 203; 79.6% at n = 225)")
report_nodes(tree_B, "Group 2 terminal nodes (manuscript: 7 terminal nodes; 56% at n = 50)")

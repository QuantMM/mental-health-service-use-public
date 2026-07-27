##############################
## Classification Tree (rpart)
## Outcome: amhsu_p5 (Yes/No)
## Predictors: all candidate predictors used above
## Procedure: grow a large tree + 10-fold CV + 1-SE rule pruning
## - Uses the existing object `dat` (complete-case dataset) from the earlier code.
## - Keeps factor reference levels already set; rpart uses the factor coding directly.
##############################

# Packages
library(rpart)
library(rpart.plot)

set.seed(1)

##############################
## 0) Basic checks / setup
##############################

# Ensure outcome is a 2-level factor with an explicit positive class ("Yes")
dat$amhsu_p5 <- factor(dat$amhsu_p5, levels = c("No", "Yes"))

# (Optional but recommended) Remove any pure ID columns if present
id_candidates <- intersect(names(dat), c("ID", "id", "Id", "participant_id", "caseid"))
tree_dat <- if (length(id_candidates) > 0) dat[, setdiff(names(dat), id_candidates), drop = FALSE] else dat

##############################
## 1) Fit a "large" tree with internal 10-fold CV
##############################
# Key control choices:
# cp: Split improvement must exceed cp times the root-node risk.
# minsplit: A node must contain at least minsplit observations for a split to be attempted.
# minbucket: Each child node after a split must contain at least minbucket observations.
# maxdepth: Limits the maximum depth of the tree. The root node is counted as depth 0.
# xval: Performs internal x-fold cross-validation to compute the complexity table (cptable).

fit0 <- rpart(
  amhsu_p5 ~ .,
  data   = tree_dat,
  method = "class",
  control = rpart.control(
    cp        = 0,  # disables CP-based pre-pruning; pruning is chosen later from cptable
    minsplit  = 20, # nodes with 19 or fewer obs become terminal nodes
    minbucket = 7,  # e.g., a split such as 16/4 from a parent node of size 20 is not allowed
    maxdepth  = 5,
    xval      = 10  # 10-fold CV errors to support pruning decisions
  )
)

# At each node, rpart effectively checks:
#   1) Does the node contain at least minsplit observations?
#   2) Has the tree not yet reached maxdepth?
#   3) Is there a candidate split satisfying minbucket for both child nodes?
#   4) Is the split improvement large enough to satisfy the cp criterion?
# If any of these conditions fail, the node becomes terminal.
#
# After the tree is grown subject to these constraints, xval = 10 evaluates the
# cross-validated performance of candidate subtrees. The results are stored in fit0$cptable.


##############################
## 2) Choose CP using the 1-SE rule
##############################
cp_tab   <- fit0$cptable
min_row  <- which.min(cp_tab[, "xerror"])
xerr_min <- cp_tab[min_row, "xerror"]
xerr_se  <- cp_tab[min_row, "xstd"]

# 1-SE rule: choose the simplest tree whose CV error is within 1 SE of the minimum.
# In rpart terms: choose the LARGEST cp such that xerror <= xerr_min + xerr_se
cp_1se <- max(cp_tab[cp_tab[, "xerror"] <= (xerr_min + xerr_se), "CP"])

##############################
## 3) Prune to the chosen CP
##############################
fit_final <- prune(fit0, cp = cp_1se)

##############################
## 4) Summaries: chosen size + terminal nodes
##############################
n_leaves <- sum(fit_final$frame$var == "<leaf>")
n_splits <- n_leaves - 1

cat("Chosen CP (1-SE rule):", cp_1se, "\n")
cat("Terminal nodes (leaves):", n_leaves, "\n")
cat("Internal splits:", n_splits, "\n")

##############################
## 5) Display: final tree plot
##############################
prp(
  fit_final,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  roundint = FALSE,
  box.palette = c("lightblue", "lightgreen"),
  branch.lty = 1,
  branch.lwd  = 2,
  branch      = 0.3,
  branch.col = "gray40",
  shadow.col = "gray80",
  split.cex = 1.0,
  split.font = 2,
  split.family = "sans",
  nn.cex = 0.9,
  tweak = 1.2,
  under = TRUE,
  varlen = 0,
  faclen = 0
)

##############################
## 6) Diagnostics: show how CP was chosen
##############################

# Full CP table + highlight the minimum and 1-SE choice
cp_df <- data.frame(
  cp_tab,
  selected_min = seq_len(nrow(cp_tab)) == min_row,
  selected_1se = cp_tab[, "CP"] == cp_1se
)

print(cp_df)
#   CP           nsplit rel.error xerror    xstd       selected_min selected_1se
#1  0.3047619048      0 1.0000000 1.0000000 0.03907654 FALSE        FALSE
#2  0.0057142857      1 0.6952381 0.6952381 0.03378843 TRUE         TRUE
#3  0.0028571429      7 0.6609524 0.7276190 0.03443733 FALSE        FALSE
#4  0.0006349206      9 0.6552381 0.7523810 0.03491777 FALSE        FALSE
#5  0.0000000000     12 0.6533333 0.7580952 0.03502676 FALSE        FALSE


# Built-in plot: cross-validated relative error (xerror) vs tree size
plotcp(fit0)

abline(v = row_1se, col = "lightblue", lty = 2)
abline(h = xerr_min + xerr_se, col = "red", lty = 3)

legend(
  "topright",
  legend = c(
    paste0("Selected CP = ", signif(cp_1se, 4), " by 1-SE rule"),
    paste0("1-SE cutoff = ", round(xerr_min + xerr_se, 3))
  ),
  col = c("lightblue", "red"),
  lty = c(2, 3),
  bty = "n",
  cex = 0.85
)

# The CP plot shows that the one-split tree achieved the lowest cross-validated relative error.
# More complex trees did not improve predictive performance, and 
# the 1-SE rule selected the same parsimonious tree with two terminal nodes.

##############################
## 7) Extract readable decision rules
##############################
print(fit_final)

# n= 2647 
# node), split, n, loss, yval, (yprob)
#       * denotes terminal node
# 1) root 2647 525 No (0.8016623 0.1983377)  
#   2) need_r=No perceived need,Don't know 2219 231 No (0.8958991 0.1041009) *
#   3) need_r=Perceived need 428 134 Yes (0.3130841 0.6869159) *

summary(fit_final)

```
Call:
rpart(formula = amhsu_p5 ~ ., data = tree_dat, method = "class", 
    control = rpart.control(cp = 0, minsplit = 20, minbucket = 7, 
        maxdepth = 5, xval = 10))
  n= 2647 

         CP nsplit rel error    xerror       xstd
1 0.304761905      0 1.0000000 1.0000000 0.03907654
2 0.005714286      1 0.6952381 0.6952381 0.03378843

Node number 1: 2647 observations,    complexity param=0.3047619
  predicted class=No  expected loss=0.1983377  P(node) =1
    class counts:  2122   525
   probabilities: 0.802 0.198 
  left son=2 (2219 obs) right son=3 (428 obs)
  Primary splits:
      need_r      splits as  RLL,         improve=243.74660, (0 missing)

Node number 2: 2219 observations
  predicted class=No  expected loss=0.1041009  P(node) =0.8383075
    class counts:  1988   231
   probabilities: 0.896 0.104 

Node number 3: 428 observations
  predicted class=Yes  expected loss=0.3130841  P(node) =0.1616925
    class counts:   134   294
   probabilities: 0.313 0.687 
```

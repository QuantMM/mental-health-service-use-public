# mental-health-service-use

This repository contains analysis code for the manuscript “(tentative) Conditional Pathways to Mental Health Service Use Among Older Adults.” The code reproduces the hierarchical logistic regression reanalysis, cross-validated LASSO interaction models, global decision tree, subgroup-specific decision trees, and manuscript figures/tables.

The raw survey data are not publicly posted because they are subject to data-use and ethics restrictions. Researchers with appropriate data access may reproduce the analyses by placing the analytic dataset in the local `data/` directory and following the workflow below.

## Analytic Workflow

The analyses are organized around three linked questions.

1. **Hierarchical logistic regression**  
   This model evaluates whether theoretically organized predictors from the Andersen behavioral model show stable average associations with past-five-year mental health service use under an additive specification.

2. **LASSO interaction model**  
   The penalized logistic regression model includes all main effects and every possible two-way interaction among the predictors, allowing the regularization procedure to determine, in a data-driven manner, whether the selected model would retain the conventional main-effects structure assumed in prior literature or instead favor interaction terms reflecting conditional relationships among predictors.

   Notably, none of the main effects survived the regularization procedure; the final model retained only two-way interaction terms, suggesting that the outcome may be more accurately characterized by conditional, rather than additive, relationships among the predictors.

4. **Decision tree analyses**  
   The tree analyses provide an interpretable representation of the conditional structure suggested by the interaction-dominant LASSO results. We first fit a global tree using the full sample. Because the global tree selected perceived need as the dominant first split, follow-up trees were then fit separately within perceived-need and no-need/uncertain subgroups.

## Repository Contents

| File | Purpose |
|---|---|
| `data prep.r` | Prepares the analytic dataset, including variable recoding, factor construction, and sample restrictions. |
| `hierarchical logistic reanalysis code.r` | Reproduces the hierarchical logistic regression models organized according to the Andersen behavioral model. |
| `LASSO interaction model code.r` | Fits the penalized logistic regression model with all two-way interactions and extracts retained interaction terms. |
| `global tree _ pruning.r` | Fits the full-sample classification tree, applies cross-validation, and selects the pruned global tree using the one-standard-error rule. |
| `follow up decision trees.r` | Fits subgroup-specific decision trees separately within the perceived-need subgroup and the no-need/uncertain subgroup. |

## Rationale for Tree-Based Analyses

The tree analyses were motivated by the interaction-dominant structure observed in the penalized logistic regression. Once all two-way interactions were allowed, the retained predictors were concentrated in interaction terms rather than global main effects. This pattern suggested that mental health service use may be governed by conditional relationships that are difficult to summarize with a single additive model. The figure below provides a conceptual illustration of this situation, highlighting how outcome relationships that depend on nonlinear or interaction-driven combinations of predictors cannot be adequately represented by linear models—even when those models are correctly specified and estimated.

<p align="center">
  <img src="conceptual%20illustration%20of%20nonlinear%20relationships.png" alt="Conceptual illustration of nonlinear relationships" width="400">
</p>

<p align="center"><em>Figure 1. Conceptual illustration of nonlinear relationships among predictors.</em></p>

In particular, a main-effects logistic regression, such as that used in the original study, is structurally incapable of capturing such conditional relationships. In contrast, tree-based methods are explicitly designed to accommodate this complexity by hierarchically partitioning the predictor space and allowing effects to vary across combinations of characteristics.

Consistent with this framework, the dominance of interaction terms in the LASSO analysis suggests that mental health service use in the present data is governed by conditional relationships, particularly involving perceived need and psychological vulnerability. Decision tree analysis therefore provides a principled extension, enabling these interaction structures to be expressed transparently in terms of thresholds, sequential splits, and subgroup-specific decision rules.

Decision trees were therefore used as a structural representation tool. Rather than treating the tree analysis as an independent exploratory search, we used it to clarify the form of the conditional relationships suggested by the LASSO interaction model: which variables split the sample first, at what thresholds, and how predictors combine into subgroup-specific decision rules.

## Global Decision Tree and Motivation for Subgroup-Specific Trees

We first fit a classification tree using all available predictors to examine which variables best distinguish individuals who used mental health services in the past five years from those who did not. Because decision trees are highly flexible and can adapt closely to the specific structure of a given dataset—especially when predictors are numerous and outcome rates are imbalanced—we applied stratified K-fold cross-validation (K=10) to evaluate how trees of different sizes would be expected to perform on new data. Cross-validation helps prevent overfitting by repeatedly splitting the data into training and validation folds, providing a more reliable estimate of model performance than apparent (in-sample) error alone. Candidate trees of differing complexity were generated by varying three control settings—the minimum terminal node size, the maximum tree depth, and the statistical splitting criterion—and each candidate was scored both by cross-validated log-loss and by cross-validated relative classification error. We then applied the standard one-standard-error rule, which selects the simplest tree whose cross-validated error is not meaningfully worse than the minimum observed error; both criteria selected the same tree.

<p align="center">
  <img src="Figures/Supplementary_Figure1_CV_Tree.tiff" alt="Supplementary Figure 1. Cross-validated tree" width="400">
</p>

<p align="center"><em>Supplementary Figure 1. Cross-validated relative error as a function of tree complexity. The plot shows cross-validated relative classification error (with ±1 standard error bars) for trees of increasing complexity, indexed by the number of splits, where zero denotes the unsplit (root-only) model, estimated using stratified 10-fold cross-validation. The dotted horizontal line denotes the one-standard-error threshold relative to the minimum cross-validated error. The selected model corresponds to the simplest tree whose error falls within this threshold, marked by the dashed vertical line and the highlighted point, yielding a single-split tree.</em></p>

As shown in the figure above, cross-validated predictive error decreased sharply with the first split but did not improve meaningfully with additional splits; more complex trees showed similar or slightly worse performance under cross-validation. Accordingly, the final selected model consisted of a single split on perceived need for mental health care, separating individuals who reported perceived need from those who reported no need or were uncertain.

This result reflects both the substantive role of perceived need and the statistical behavior of tree-based classification in the presence of outcome imbalance.

- In the present data, mental health service use is relatively rare (525 of 2,647 individuals; 19.8%). When the outcome is imbalanced in this way, classification trees tend to prioritize splits that isolate subgroups with markedly higher outcome prevalence, because such splits yield the largest immediate reduction in overall misclassification error.
- Perceived need functions precisely in this capacity: among individuals reporting perceived need (*n* = 428), 294 (68.7%) reported service use, compared with 9.7% among those reporting no perceived need and an intermediate rate among those reporting uncertainty. This pronounced separation in baseline prevalence creates a dominant first split—from the perspective of cross-validated prediction, no other variable produces a comparable reduction in classification error. As a result, additional splits contribute little incremental improvement once the sample is partitioned by perceived need.

Importantly, the dominance of this first split does not imply that other predictors are irrelevant:

1) **Dominance reflects scale of separation, not absence of other effects.** Because perceived need produces a dramatic shift in baseline service-use prevalence, no other variable yields a comparably large reduction in global misclassification error. Under cross-validation, additional splits must demonstrate stable improvements beyond this initial gain, and in the present data structure, no alternative predictor generates a reduction of similar scale. Once the sample is partitioned on perceived need, the remaining nodes are smaller and more homogeneous, and further reductions in overall error become necessarily modest. The single-split solution is therefore best understood as a consequence of the combined effects of outcome imbalance and the scale of prevalence separation induced by this gateway variable.

2) **Global predictive dominance is distinct from conditional, subgroup-specific effects.** This limitation becomes especially salient in light of the interaction-dominant structure identified in the penalized regression analysis. The LASSO results indicate that many predictors operate through interactions—conditional relationships that may be substantively important within particular segments of the population, yet contribute little to global classification error when evaluated in a single unified tree. As a result, cross-validated pruning favors the large prevalence contrast induced by perceived need, while smaller-scale interaction effects remain obscured at the global level.

3) **Perceived need as a process-defining variable.** Consistent with theoretical work conceptualizing perceived need as a process-defining variable that delineates qualitatively distinct help-seeking regimes (e.g., Kazdin, 2025), the initial split is interpreted as partitioning the population into qualitatively different contexts rather than simply identifying the single "most important" predictor in an additive sense.

Accordingly, to further characterize the conditional decision rules implied by the interaction-dominant structure identified in the penalized regression, follow-up decision tree analyses were conducted separately within each perceived-need subgroup to elucidate the secondary predictors governing service use within each context.

## Subgroup-Specific Decision Tree Analyses

To examine conditional decision structures within each perceived-need context, we implemented a cross-validated model selection framework consistent with the procedure described above. Structural parameters were tuned using a grid-based search over minimum terminal node size, maximum tree depth, and the statistical splitting criterion. Predictive performance was evaluated using cross-validated log-loss, and model selection followed the one-standard-error rule, retaining the simplest tree whose performance fell within one standard error of the minimum observed value.

Within the perceived-need subgroup (Group 1), this procedure consistently favored a stable, shallow structure across tuning specifications, yielding a minimal tree. In contrast, within the no-need/uncertain subgroup (Group 2), cross-validated tuning favored a substantially more complex structure than that observed in Group 1. Full tuning details and implementation code are provided in `follow up decision trees.r`. Please refer to the manuscript for the detailed split-level results and discussion.

## Summary

Each analytic step in this repository answers a distinct inferential question rather than redundantly re-estimating the same relationship:

- **Hierarchical GLM (Andersen model):** Which predictors have stable average effects under an additive, theory-constrained specification?
- **LASSO with all two-way interactions:** Once interaction structure is permitted—but aggressively penalized—does the data still support global main effects, or does explanatory power reside primarily in conditional relationships?
- **Decision trees:** If effects are conditional, what is the form of those conditions—that is, which variables split first, at what thresholds, and how do predictors combine sequentially into subgroup-specific rules?

Decision trees are used here as a structural representation tool, justified by prior evidence of interaction dominance rather than treated as an independent predictive exercise. Throughout, the analyses prioritize stability, interpretability, and faithful representation of the dominant interaction structure—consistent with the broader goal of avoiding overfitting while still capturing meaningful heterogeneity in the conditional pathways to service use.

Taken together, the workflow reflects a principled escalation of model flexibility rather than exploratory model shopping: flexibility was increased only when justified by the prior step, regularization was used as a diagnostic tool rather than an endpoint, trees were deployed for representation rather than brute-force prediction, and complexity at each stage was controlled in a way that aligns with the substantive goals of the analysis.

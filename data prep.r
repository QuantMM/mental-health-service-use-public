# Purpose: Create the final analytic dataset used in all manuscript analyses.

# Load data ---------------------------------------------------------------

d <- readRDS("..data.rds")

# Recode NA="Don't know" in perceived need
levels(d$need_r)
levels(d$need_r) <- c(levels(d$need_r), "Don't know")
d$need_r[is.na(d$need_r)] <- "Don't know"
levels(d$need_r)

# Important for interpretation: Reference level=Perceived need
d$need_r <- relevel(d$need_r, ref = "Perceived need")
contrasts(d$need_r)

# Recode lables in educ_t
table(d$educ_t)
levels(d$educ_t) <- c("HS or less", "Some post-sec", "Post-sec completed")
table(d$educ_t)

# Education reference set to Post-sec completed: so that
# interpretation of coefficients = HS or less vs Post-sec completed & some post-sec vs Post-sec completed
d$educ_t <- relevel(d$educ_t, ref = "Post-sec completed")
contrasts(d$educ_t)

# Recode lables in income_t
table(d$income_t)
levels(d$income_t) <- c("<$40K", "$40K–$79K", "$80K+")
table(d$income_t)

# Reference = $80K+ → <$40K / $40K–$79K vs $80K+
# (interpreted as lower income vs highest income)
d$income_t <- relevel(d$income_t, ref = "$80K+")
contrasts(d$income_t)

# Final sanity check: verify reference levels & contrasts for categorical variables
lapply(d[sapply(d, is.factor)], contrasts)

# FINAL DATA without missing: dat
sum(is.na(d))
dat <- na.omit(d)
c(before = nrow(d), after = nrow(dat))
lapply(dat[sapply(dat, is.factor)], contrasts)
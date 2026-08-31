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

# Figure 1 for manuscript
library(ggplot2)
library(scales)

# add service factor
dat$service_plot <- factor(
  dat$amhsu_p5,
  levels = c("No", "Yes"),
  labels = c("No Service Use", "Yes Service Use")
)

p <- ggplot(dat, aes(x = need_plot, fill = service_plot)) +
  geom_bar(position = "fill", width = 0.9) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(
    values = c("No Service Use"  = "#D2691E",
               "Yes Service Use" = "#009371"),
    name = "Service Use (Past 5 Years)"
  ) +
  labs(
    title    = "Past 5-Year Mental Health Service Use by Perceived Need",
    subtitle = paste0("Proportion of service users within each perceived need category (N=",
                      nrow(dat), ")"),
    x = "Perceived Need for Mental Health Help",
    y = "Proportion"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.title       = element_text(size = 11),
    plot.title         = element_text(face = "bold", hjust = 0),
    axis.text.x        = element_text(angle = 20, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

p

tiff("Figure1_MHSU_by_Perceived_Need.tiff",
     width = 170, height = 186, units = "mm",
     res = 300, compression = "lzw")
print(p)
dev.off()

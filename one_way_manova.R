library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)

iris2 <- read.csv('C:/Users/Administrator/Ai/R/inferential/MANOVA/one_way_manova.csv')
iris2

################################### Visualization
ggboxplot(
  iris2, x = "Species", y = c("Sepal.Length", "Petal.Length"), 
  merge = TRUE, palette = "jco"
)

################################### Summary statistics
iris2 %>%
  group_by(Species) %>%
  get_summary_stats(Sepal.Length, Petal.Length, type = "mean_sd")

################################### Assumptions
################################### Check sample size assumption
iris2 %>%
  group_by(Species) %>%
  summarise(N = n())

################################### Identify univariate outliers
iris2 %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)

iris2 %>%
  group_by(Species) %>%
  identify_outliers(Petal.Length)

################################### Detect multivariate outliers
# Compute distance by groups and filter outliers
# Use -id to omit the id column in the computation
iris2 %>%
  group_by(Species) %>%
  mahalanobis_distance(-id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

################################### Check univariate normality assumption
iris2 %>%
  group_by(Species) %>%
  shapiro_test(Sepal.Length, Petal.Length) %>%
  arrange(variable)

# QQ plot of Sepal.Length
ggqqplot(iris2, "Sepal.Length", facet.by = "Species",
         ylab = "Sepal Length", ggtheme = theme_bw())

################################### Multivariate normality
iris2 %>%
  select(Sepal.Length, Petal.Length) %>%
  mshapiro_test()

################################### Identify multicollinearity
iris2 %>% cor_test(Sepal.Length, Petal.Length)


################################### Check linearity assumption
# Create a scatterplot matrix by group
library(GGally)
results <- iris2 %>%
  select(Sepal.Length, Petal.Length, Species) %>%
  group_by(Species) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results

# Show the plots
results$plots

################################### Check the homogeneity of covariances assumption
box_m(iris2[, c("Sepal.Length", "Petal.Length")], iris2$Species)

################################### Check the homogneity of variance assumption
iris2 %>% 
  gather(key = "variable", value = "value", Sepal.Length, Petal.Length) %>%
  group_by(variable) %>%
  levene_test(value ~ Species)

################################### Computations
#pillai's
model <- lm(cbind(Sepal.Length, Petal.Length) ~ Species, iris2)
Manova(model, test.statistic = "Pillai")

################################### Post-hoc tests
########### Compute univariate one-way ANOVA
# Group the data by variable
grouped.data <- iris2 %>%
  gather(key = "variable", value = "value", Sepal.Length, Petal.Length) %>%
  group_by(variable)

# Do welch one way anova test
grouped.data %>% welch_anova_test(value ~ Species)

# or do Kruskal-Wallis test
grouped.data %>% kruskal_test(value ~ Species)

# or use aov()
grouped.data %>% anova_test(value ~ Species)

########### Compute multiple pairwise comparisons
pwc <- iris2 %>%
  gather(key = "variables", value = "value", Sepal.Length, Petal.Length) %>%
  group_by(variables) %>%
  games_howell_test(value ~ Species) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details
pwc


################################### Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Species")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 71.83, p= "<0.0001", parameter = "4,294",
  type = "expression", detailed = TRUE
)
ggboxplot(
  iris2, x = "Species", y = c("Sepal.Length", "Petal.Length"), 
  merge = TRUE, palette = "jco"
) + 
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0, 
    step.increase = 0.1, step.group.by = "variables",
    color = "variables"
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression")
  )



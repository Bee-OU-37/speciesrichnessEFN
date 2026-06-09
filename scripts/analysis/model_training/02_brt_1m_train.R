library(dismo)   # provides gbm.step
library(gbm)     # underlying gbm implementation
library(dplyr)
library(kernelshap)
library(shapviz)
library(ggplot2)
library(tidyr)

# -------------------------------------------------
#  Load ecological data
# -------------------------------------------------
#df <- read.csv("env_data_total_1m.csv")
df <- read.csv("env_data_total_1m_inclLocations.csv")

head(df)

# remove variables Temp_min, Temp_mean, Precipitation, and DTM because of VIF analysis
df_raw <- df %>%
  dplyr::select(-Temp_min, -Temp_mean, -Precipitation, -DTM)

cat("selected data set size     :", nrow(df_raw), "\n")

# Convert the location column to a factor
df_raw$location <- factor(df_raw$location)

# -------------------------------------------------
#  Create percentage‑based balanced sampling
# -------------------------------------------------
# reproducibility
set.seed(2026)   

pct_per_loc <- 0.80   # <-- 80% of each site's records go to the training set

train_balanced <- df_raw %>%
  group_by(location) %>%                     # work within each location
  slice_sample(prop = pct_per_loc) %>%       # sample the given proportion
  ungroup()

# Remaining rows become the external test set
test_set <- anti_join(df_raw, train_balanced, by = colnames(df_raw))

cat("Training set size :", nrow(train_balanced), "\n")
cat("Test set size     :", nrow(test_set), "\n")

# -------------------------------------------------
# Identify all character columns that are categorical
# -------------------------------------------------
char_cols <- names(train_balanced)[sapply(train_balanced, is.character)]

# -------------------------------------------------
# Turn them into factors (this gives  control over the levels)
# -------------------------------------------------
for (cn in char_cols) {
  train_balanced[[cn]] <- factor(train_balanced[[cn]])
  test_set[[cn]]       <- factor(test_set[[cn]])
}

# -------------------------------------------------
# Unify the levels across train and test for each of those factors
# -------------------------------------------------
for (cn in char_cols) {
  all_lev <- dplyr::union(levels(train_balanced[[cn]]),
                   levels(test_set[[cn]]))
  train_balanced[[cn]] <- factor(train_balanced[[cn]], levels = all_lev)
  test_set[[cn]]       <- factor(test_set[[cn]],   levels = all_lev)
}


# extract the factor variables for later use
my_factor_soil <- train_balanced[[char_cols[1]]]
my_factor_lc <- train_balanced[[char_cols[2]]]

#save levels of categorical variables
saveRDS(levels(my_factor_lc), "BRT_dismo_1m_levels_lc.rds")
saveRDS(levels(my_factor_soil), "BRT_dismo_1m_levels_soil.rds")

# -------------------------------------------------
# Run gbm.step
# -------------------------------------------------
set.seed(2025)

end_column <- ncol(train_balanced)-1  # predictor columns, do not include location column
print(end_column)

# #checks
# class(train_balanced$alpha_diversity)
# sum(is.na(train_balanced$alpha_diversity))
# is.vector(train_balanced$alpha_diversity)
# which(names(train_balanced) == "alpha_diversity")
# sapply(train_balanced[2:15], class)
# colSums(is.na(train_balanced[2:15]))
# str(train_balanced$alpha_diversity)
#train_balanced$alpha_diversity <- as.numeric(train_balanced$alpha_diversity)

gbm_x <- 2:end_column   
print(gbm_x)
print(names(train_balanced)[gbm_x])
colSums(is.na(train_balanced[gbm_x]))


#change train_balanced from tibble to data.frame to prevent errors
train_balanced <- as.data.frame(train_balanced)

any(train_balanced$alpha_diversity < 0)

# alpha_div.tc5.lr005 <- gbm.step(data=train_balanced, gbm.x = 2:15, gbm.y = 1,
#                             family = "poisson", tree.complexity = 5,
#                             learning.rate = 0.001, bag.fraction = 0.75, 
#                             silent = F)

#summary(alpha_div.tc5.lr005)

alpha_div.tc2.lr01 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
                                family = "gaussian", tree.complexity = 2,
                                learning.rate = 0.01, bag.fraction = 0.75,
                                silent = F)

# alpha_div.tc5.lr05 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
#                                family = "gaussian", tree.complexity = 5,
#                                learning.rate = 0.05, bag.fraction = 0.5, 
#                                n.folds = 5, silent = F)

# alpha_div.fixed <- gbm(data=train_balanced, gbm.x = 2:15, gbm.y = 1,
#                          learning.rate=0.005, tree.complexity=2, n.trees=3400)
# 
#summary(alpha_div.tc5.lr05)
# 
# names(alpha_div.tc2.lr01)

#create linear regression model for comparison
#alpha_div.lm <- lm(alpha_diversity ~ ., data = train_balanced %>% dplyr::select(-location))

pred_test <- predict(alpha_div.tc2.lr01, newdata = test_set)
#pred_test <- predict(alpha_div.lm, newdata = test_set)

test_r2   <- cor(test_set$alpha_diversity, pred_test)^2
test_RMSE <- sqrt(mean((test_set$alpha_diversity - pred_test)^2))

cat("\nExternal test performance:\n")
cat("  R²   =", round(test_r2, 3), "\n")
cat("  RMSE =", round(test_RMSE, 3), "\n")


# Compare the CV‑(R^{2}) to the naïve baseline (predict the mean of the training set)
train_mean <- mean(train_balanced$alpha_diversity)
print(train_mean)
#print CV R2


# Plot observed vs predicted
plot(test_set$alpha_diversity, pred_test,
     xlab = "Observed species richness",
     ylab = "Predicted species richness",
     main = "Observed vs Predicted Species Richness on Test Set")
abline(0, 1, col = "red")  # 1:1 line

alpha_div.simp <- gbm.simplify(alpha_div.tc2.lr01)

alpha_div.tc2.lr01.simp <- gbm.step(train_balanced,
                                  gbm.x=alpha_div.simp$pred.list[[2]], gbm.y=1,
                                  family = "gaussian", tree.complexity=5, 
                                  learning.rate=0.005)

summary(alpha_div.tc2.lr01.simp)

pred_test <- predict(alpha_div.tc2.lr01.simp, newdata = test_set)

test_rmse <- sqrt(mean((test_set$alpha_diversity - pred_test)^2))
test_r2   <- cor(test_set$alpha_diversity, pred_test)^2

cat("\nSimplified BRT - External test performance:\n")
cat("  RMSE =", round(test_rmse, 3), "\n")
cat("  R²   =", round(test_r2, 3), "\n")

plot(test_set$alpha_diversity, pred_test,
     xlab = "Observed alpha diversity",
     ylab = "Predicted alpha diversity",
     main = "Observed vs Predicted Alpha Diversity on Test Set (Simplified BRT)")
abline(0, 1, col = "red")  # 1:1 line

#save model to file
#saveRDS(alpha_div.tc2.lr01, file = "BRT_1m.rds")

# CV statistics:
alpha_div.tc2.lr01[34]

cat("\nCross-validated performance (on training set):\n")
cat("  CV R²   =", round(alpha_div.tc2.lr01$cv.statistics$correlation.mean^2, 3), "\n")
cat("  CV RMSE =", round(alpha_div.tc2.lr01$cv.statistics$deviance.mean^0.5, 3), "\n")
cat("  CV Deviance =", round(alpha_div.tc2.lr01$cv.statistics$deviance.mean, 3), "\n")
cat("  CV Correlation =", round(alpha_div.tc2.lr01$cv.statistics$correlation.mean, 3), "\n")


if (exists("alpha_div.tc2.lr01")) saveRDS(alpha_div.tc2.lr01, file = "BRT_1m.rds")

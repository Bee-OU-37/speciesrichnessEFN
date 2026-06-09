library(dismo)   # provides gbm.step
library(gbm)     # underlying gbm implementation
library(dplyr)
library(kernelshap)
library(shapviz)
library(readr)


# -------------------------------------------------
#  Load ecological data
# -------------------------------------------------
df_raw <- read.csv("env_data_total_100m.csv")  

#remove columns not required for training
df_raw <- df_raw %>%
  dplyr::select(-plot_code, -USP_location, -Latitude_GIS, -Longitude_GIS)

head(df_raw)
# Convert the location column to a factor
df_raw$location <- factor(df_raw$location)

# -------------------------------------------------
# Keep only required features
# -------------------------------------------------

# remove variables Temp_min, Temp_mean, Precipitation, and DTM because of VIF analysis
df_selection <- df_raw %>%
  dplyr::select(-Temp_min, -Temp_mean, -Precipitation, -DTM)

# remove one of the two responses: keep plot_total_alphadiv, remove plot_mean_alphadiv
response_col <- "plot_mean_alphadiv"
df_selection <- df_selection %>%
  dplyr::select(-plot_total_alphadiv)

# -------------------------------------------------
#  Create percentage‑based balanced sampling
# -------------------------------------------------
# reproducibility
set.seed(2026)   

pct_per_loc <- 0.80   # <-- 80% of each site's records go to the training set

# create unique id for each row
df_selection <- df_selection %>% mutate(row_id = row_number())

cat("selected data set size     :", nrow(df_selection), "\n")

train_balanced <- df_selection %>%
  group_by(location) %>%                     # work within each location
  slice_sample(prop = pct_per_loc) %>%       # sample the given proportion
  ungroup()

# Remaining rows become the external test set
test_set <- anti_join(df_selection, train_balanced, by = "row_id")

# Remove the temporary row_id columns
train_balanced <- train_balanced %>% dplyr::select(-row_id)
test_set       <- test_set %>% dplyr::select(-row_id)

cat("data set size     :", nrow(df_selection), "\n")
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



# -------------------------------------------------
# Run gbm.step
# -------------------------------------------------
set.seed(2025)

end_column <- ncol(train_balanced)-1  # predictor columns, do not include location column
print(end_column)

# #checks
# class(train_balanced[[end_column]])
# sum(is.na(train_balanced[[end_column]]))
# is.vector(train_balanced[[end_column]])
# which(names(train_balanced) == end_column)
# sapply(train_balanced[2:15], class)
# colSums(is.na(train_balanced[2:15]))
# str(train_balanced[[end_column]])
#train_balanced[[end_column]] <- as.numeric(train_balanced[[end_column]])

gbm_x <- 2:end_column   
print(gbm_x)
print(names(train_balanced)[gbm_x])
colSums(is.na(train_balanced[gbm_x]))


#change train_balanced from tibble to data.frame to prevent errors
train_balanced <- as.data.frame(train_balanced)

any(train_balanced[[end_column]] < 0)

# alpha_div.tc5.lr1 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
#                              family = "gaussian", tree.complexity = 2,
#                              learning.rate = 0.1, bag.fraction = 0.75,
#                              silent = F)

#summary(alpha_div.tc5.lr1)

# alpha_div.tc5.lr01 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
#                                 family = "gaussian", tree.complexity = 2,
#                                 learning.rate = 0.005, bag.fraction = 1,
#                                 n.trees = 50, step.size = 1, silent = F)

alpha_div.tc2.lr01 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
                                family = "gaussian", tree.complexity = 2,
                                learning.rate = 0.005, bag.fraction = 0.75,
                                silent = F)

# alpha_div.tc5.lr05 <- gbm.step(data=train_balanced, gbm.x = 2:end_column, gbm.y = 1,
#                                family = "gaussian", tree.complexity = 5,
#                                learning.rate = 0.05, bag.fraction = 0.75,
#                                silent = F)

# alpha_div.fixed <- gbm(data=train_balanced, gbm.x = 2:15, gbm.y = 1,
#                          learning.rate=0.005, tree.complexity=2, n.trees=3400)
# 
summary(alpha_div.tc2.lr01)
# 
# names(alpha_div.tc2.lr01)

#create linear regression model for comparison
alpha_div.lm <- lm(plot_mean_alphadiv ~ ., data = train_balanced %>% dplyr::select(-location))

# -------------------------------------------------
#  External test performance
# -------------------------------------------------
pred_test <- predict(alpha_div.lm, newdata = test_set)
#pred_test <- predict(alpha_div.tc2.lr01, newdata = test_set)
#pred_test <- predict(alpha_div.tc5.lr01, newdata = test_set)

test_r2   <- cor(test_set[[response_col]], pred_test)^2
test_RMSE <- sqrt(mean((test_set[[response_col]] - pred_test)^2))

cat("\nExternal test performance:\n")
cat("  R²   =", round(test_r2, 3), "\n")
cat("  RMSE =", round(test_RMSE, 3), "\n")

# Compare the CV‑(R^{2}) to the naïve baseline (predict the mean of the training set)
train_mean <- mean(train_balanced[[response_col]])
print(train_mean)
#print CV R2

# Plot observed vs predicted
plot(test_set[[response_col]], pred_test,
     xlab = "Observed alpha diversity",
     ylab = "Predicted alpha diversity",
     main = "Observed vs Predicted Alpha Diversity on Test Set")
abline(0, 1, col = "red")  # 1:1 line

alpha_div.simp <- gbm.simplify(alpha_div.tc5.lr05)

alpha_div.tc5.lr05.simp <- gbm.step(train_balanced,
                                  gbm.x=alpha_div.simp$pred.list[[2]], gbm.y=1,
                                  family = "gaussian", tree.complexity=5, 
                                  learning.rate=0.05)

summary(alpha_div.tc5.lr05.simp)

pred_test <- predict(alpha_div.tc5.lr05.simp, newdata = test_set)

test_rmse <- sqrt(mean((test_set[[response_col]] - pred_test)^2))
test_r2   <- cor(test_set[[response_col]], pred_test)^2

cat("\nSimplified BRT - External test performance:\n")
cat("  RMSE =", round(test_rmse, 3), "\n")
cat("  R²   =", round(test_r2, 3), "\n")

plot(test_set[[response_col]], pred_test,
     xlab = "Observed alpha diversity",
     ylab = "Predicted alpha diversity",
     main = "Observed vs Predicted Alpha Diversity on Test Set (Simplified BRT)")
abline(0, 1, col = "red")  # 1:1 line


# CV statistics:
alpha_div.tc2.lr01[34]

cat("\nCross-validated performance (on training set):\n")
cat("  CV R²   =", round(alpha_div.tc2.lr01$cv.statistics$correlation.mean^2, 3), "\n")
cat("  CV RMSE =", round(alpha_div.tc2.lr01$cv.statistics$deviance.mean^0.5, 3), "\n")
cat("  CV Deviance =", round(alpha_div.tc2.lr01$cv.statistics$deviance.mean, 3), "\n")
cat("  CV Correlation =", round(alpha_div.tc2.lr01$cv.statistics$correlation.mean, 3), "\n")


if (exists("alpha_div.tc2.lr01")) saveRDS(alpha_div.tc2.lr01, file = "BRT_100m.rds")

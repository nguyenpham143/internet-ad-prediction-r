library(caTools)
library(caret)
library(pROC)
library(car)

set.seed(1)

prep_candidates <- c("data/preprocessed.csv", "../data/preprocessed.csv", "preprocessed.csv")
prep_file <- prep_candidates[file.exists(prep_candidates)][1]
if (is.na(prep_file)) prep_file <- "preprocessed.csv"
ads_data <- read.csv(file = prep_file, header = TRUE, sep = ",")

train_index <- sample.split(ads_data$Is_Ad, SplitRatio = 0.8)

train_set <- subset(ads_data, train_index)
test_set  <- subset(ads_data, !train_index)

# Logistic regression models
mdel1 <- glm(Is_Ad ~ Height + Width,
             data = train_set,
             family = binomial)

mdel2 <- glm(Is_Ad ~ Height + Width + Aspect_ratio,
             data = train_set,
             family = binomial)

mdel3 <- glm(Is_Ad ~ Height + Width + Aspect_ratio + Local,
             data = train_set,
             family = binomial)

summary(mdel1)
summary(mdel2)
summary(mdel3)

# Compare nested models
anova(mdel1, mdel2, test = "Chisq")
anova(mdel2, mdel3, test = "Chisq")

# Select the second model
best_model <- mdel2

# Check multicollinearity
library(car)
vif(best_model)

# Check contribution of each variable
drop1(best_model, test = "Chisq")

# 95% confidence intervals
confint(best_model)

# Odds ratios
exp(coef(best_model))

# Predicted probabilities on test set
pred_prob <- predict(best_model, newdata = test_set, type = "response")

library(caret)

predicted <- factor(ifelse(pred_prob > 0.5, 1, 0), levels = c(0, 1))
actual <- factor(test_set$Is_Ad, levels = c(0, 1))

# Confusion matrix
conf_mat <- confusionMatrix(predicted, actual, positive = "1")
print(conf_mat)

# ROC and AUC
roc_curve <- roc(test_set$Is_Ad, pred_prob)

plot(roc_curve, 
     main = "ROC Curve", 
     col = "blue", 
     print.auc = TRUE)

auc(roc_curve)
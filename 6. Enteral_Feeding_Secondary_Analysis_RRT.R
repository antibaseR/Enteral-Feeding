library(tidyverse)
library(patchwork)
library(caret)
library(pROC)


options(scipen=999)

# Set the working directory
setwd("G:/Ivan")

# Load the data
data = readRDS("./Xinlei/Data/merged_data.rds")


## Normalize variables
data$total_LA_volume_sqrt = sqrt(data$total_LA_volume)
hist(data$total_LA_volume_sqrt)
data$total_LA_volume_sqrt_z = scale(data$total_LA_volume_sqrt)[,1]
hist(data$total_LA_volume_sqrt_z)

data$total_Kcal_sqrt = sqrt(data$total_Kcal)
data$total_Kcal_sqrt_z = scale(data$total_Kcal_sqrt)[,1]
hist(data$total_Kcal_sqrt_z)

data$apache3_sqrt = sqrt(data$apache3)
hist(data$apache3_sqrt)
data$apache3_sqrt_z = scale(data$apache3_sqrt)[,1]
hist(data$apache3_sqrt_z)

data$charlson_sqrt = sqrt(data$charlson)
hist(data$charlson_sqrt)
data$charlson_sqrt_z = scale(data$charlson_sqrt)[,1]
hist(data$charlson_sqrt_z)


logidata = data %>%  
  mutate(rrt_discharge = ifelse(rrt_discharge == 1, "Yes", "No")) %>%
  select(rrt_discharge, total_LA_volume_sqrt_z, 
         age_at_admission, gender, race, charlson_sqrt_z, apache3_sqrt_z, 
         total_Kcal_sqrt_z)

completeData = logidata[complete.cases(logidata), ]


# split the data into the training and testing set
set.seed(960725)
trainIndex <- createDataPartition(completeData$rrt_discharge, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)
train = completeData[trainIndex,]
test = completeData[-trainIndex,]

trainLabel = train$rrt_discharge
trainData = train[, -(which("rrt_discharge"==colnames(train)))]
testLabel = test$rrt_discharge
testData = test[, -(which("rrt_discharge"==colnames(test)))]

# model to predict
control = trainControl(method = "repeatedcv", number = 5, repeats = 1, 
                       search = "grid", savePredictions = "final", 
                       summaryFunction = twoClassSummary, classProbs = TRUE, 
                       verboseIter = TRUE)

# run a single rpart tree to find interactions
#cart.model = train(x = trainData,
#                   y = trainLabel,
#                   method = "rpart", 
#                   trControl = control,
#                   metric = "ROC")
#plot(cart.model$finalModel, uniform=TRUE,
#     main="Classification Tree")
#text(cart.model$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
#cart_mod_list[[i]] = cart.model


# run a logistic regression
logi.model = train(x = trainData,
                   y = trainLabel,
                   method = "glm", 
                   family = "binomial",
                   trControl = control,
                   metric = "ROC")

glm_fit = summary(logi.model)
var_imp_list = varImp(logi.model)

# evaluating on the testing set
pred_prob = predict(logi.model, newdata = testData, type = "prob")
test_sum_list = roc(testLabel, pred_prob$Yes) # 0.82

glm_fit_df = data.frame(variable = rownames(glm_fit$coefficients),
                        beta = glm_fit$coefficients[,c("Estimate")],
                        se = glm_fit$coefficients[,c("Std. Error")],
                        p = glm_fit$coefficients[,c("Pr(>|z|)")]) %>%
  mutate(odds = exp(beta),
         lower.ci = exp(beta - 1.96 * se),
         upper.ci = exp(beta + 1.96 * se)) %>%
  select(variable, beta, odds, se, lower.ci, upper.ci, p)

names(glm_fit_df) = c("Variable", "Coefficient", "Odds Ratio", "Std. Err.", "Lower 95% CI", "Upper 95% CI", "P-value")

glm_fit_df$Variable = c("(Intercept)", "Total LA Volume", "Age", "Gender: Male", "Race: Black", "Race: Other", "Charlson", "APACHE III", "Total Kcal")


# Save the result
write.csv(glm_fit_df, "./Xinlei/Result/Secondary_Analysis_RRT_glm_fit_result.csv")


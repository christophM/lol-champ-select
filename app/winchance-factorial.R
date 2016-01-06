library("data.table")
library("dplyr")

source("champ-learner.R")
source("variables.R")

champ_infos = read.csv("../champ-info.csv", stringsAsFactors = FALSE)

matches = data.frame(fread("matches/champion-wins-factorial.csv", stringsAsFactors = FALSE))
## remove "empty" rows
matches <- matches[matches$winner_team100 != "0",]

non_zero_rows <- rowSums(matches[TEAM_LANE_ROLES] == 0) == 0
matches <- matches[non_zero_rows,]

matches$winner_team100 <- 1 * as.logical(matches$winner_team100)

set.seed(421)
train_index = sample(x = 1:nrow(matches), size = round(0.5 * nrow(matches)), replace=FALSE)
test_index = setdiff(1:nrow(matches), train_index)


drop_variables <- c("match_id", "winner_team100", "season", "queueType", "region", "matchDuration")
## Champion roles + is_blue remain
predictor_variables <- setdiff(names(matches), drop_variables)


X_train <- matches[train_index,]
y_train <- matches[train_index,]$winner_team100

X_test <- matches[test_index,]
y_test <- matches[test_index,]$winner_team100


## TODOS
### Equals 17 count learner variables
## Create Sheet with 1 row per champion and each column is an attribute like tankiness, cc, ...
## Download sheet and load here
## create function for building team summaries from sheet
## Include variables from team summaries
## Split data into training and test data set
## Train linear model with count learner + summaries
## Test model on test data set with classification rate and log loss
## export model somehow



cl_index <- sample(x=1:nrow(X_train), size=round(0.5*nrow(X_train)))
lm_index <- setdiff(1:nrow(X_train), cl_index)

champ_model <- learn_champ_model(X=X_train[cl_index, c(predictor_variables)], y=y_train[cl_index], 
                                 X_lm=X_train[lm_index, c(predictor_variables)],  y_lm=y_train[lm_index], 
                                 champ_infos=champ_infos)


summary(champ_model$model)


pred <- predict(champ_model, X_train[predictor_variables])


correct_class <- function(ypred, ytrue){
  tab <- table(ypred, ytrue)
  sum(diag(tab)) / sum(tab)
}


correct_class(round(pred, 0), y_train)


pred2 <- predict.champ_model(champ_model, X_test[predictor_variables])

print("Correctly classified:")
print(correct_class(round(pred2, 0), y_test))

print("LogLoss:")
print(LogLoss(y_test, pred2))



cl_index <- sample(x=1:nrow(matches), size=round(0.5*nrow(matches)))
lm_index <- setdiff(1:nrow(matches), cl_index)

## champ_model <- learn_champ_model(X=matches[predictor_variables], y=matches$winner_team100)

champ_model <- learn_champ_model(X=matches[cl_index, c(predictor_variables)], y=matches$winner_team100[cl_index], 
                                 X_lm=matches[lm_index, c(predictor_variables)],  y_lm=matches$winner_team100[lm_index], 
                                 champ_infos=champ_infos)
## TODO: Retrain with all data 
save(champ_model, file="./model/champ_model_factorial.RData")




## folded models 
cls <- learn_champ_model_folds(X_train[predictor_variables], y_train, nfolds=2)
pred3 <- predict(cls, newdata=X_test[predictor_variables])

print("Correctly classified:")
print(correct_class(round(pred3, 0), y_test))






library("xgboost")
library("data.table")
library("glmnet")



## TODO:
## Automatically save report
## Predict time duration
## Build predictions models for every early, mid and late game



LogLoss<-function(actual, predicted)
{
    result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
    return(result)
}

matches = fread("../champion-wins.csv")
## remove "empty" rows
matches <- matches[winner_team100 != "0",]

champion_lane_combos <- data.frame("features"=colnames(matches))
write.csv(champion_lane_combos, file="./app/model/champion_lane_combos.csv")

set.seed(42)
train_index = sample(x = 1:nrow(matches), size = round(0.5 * nrow(matches)), replace=FALSE)
test_index = setdiff(1:nrow(matches), train_index)


drop_variables <- c("match_id", "winner_team100", "season", "queueType", "region", "matchDuration")
predictor_variables <- setdiff(names(matches), drop_variables)


X_train <- sapply(matches[train_index, predictor_variables, with=FALSE], as.numeric)
X_train <- Matrix:::Matrix(X_train, sparse=TRUE)

X_test <- sapply(matches[test_index, predictor_variables, with=FALSE], as.numeric)
X_test <- Matrix:::Matrix(X_test, sparse=TRUE)


y_train <- as.numeric(as.logical(matches[train_index]$winner_team100))
y_test <- as.numeric(as.logical(matches[test_index]$winner_team100))

xgtrain <- xgb.DMatrix(data = X_train, label= y_train)
xgval <-  xgb.DMatrix(data = X_test, label= y_test)

                                        # setup watchlist to enable train and validation, validation must be first for early stopping
watchlist <- list(val=xgval, train=xgtrain)
                                        # to train with watchlist, use xgb.train, which contains more advanced features

param <- list("objective" = "binary:logistic",
              "eta" = 0.005,
              "min_child_weight" = 10,
              "subsample" = .8,
              "colsample_bytree" = .8,
              "scale_pos_weight" = 1.0,
              "max_depth" = 3,
              "eval_metric"="logloss")
NROUND = 7000

## this will use default evaluation metric = rmse which we want to minimise
bst <- xgb.train(params = param, data = xgtrain,
                 print.every.n = 50,
                 nround=NROUND,
                 watchlist=watchlist,
                 maximize = FALSE, early.stop.round=50)

pred <- predict(bst, newdata=X_test)
pred_hard <- round(pred, 0)

print("Classification rate")
print(sum(diag(table(1 * y_test, pred_hard))) / length(y_test))

print("Logloss")
print(LogLoss(y_test, pred))


##  cv <- cv.glmnet(x=X_train,y_train, type.measure="deviance")
## mod <- glmnet(x=X_train, y=y_train,lambda=cv$lambda.min, family="binomial")
## pred_glmnet = predict(mod, newx=X_test, type="response")
## pred_hard_glmnet <- round(pred_glmnet, 0)


## sum(diag(table(1*y_test, pred_hard_glmnet))) / length(y_test)

## LogLoss(y_test, pred_glmnet)

## coefs <- coefficients(mod)
## coef_names <- rownames(coefs)
## coefs <- data.frame(as.matrix(coefs))
## coefs$names  <- coef_names
## ## coefs <- coefs[coefs$s0 != 0, ]


## get_match_factors <- function(row_id, coefs){
##     relevant_variables <- names(X_train[row_id, ][X_train[row_id,] != 0])
##     coefs[relevant_variables, ]
## }


## summarise_match <- function(row_id, coefs){
##     print(matches[row_id, "winner_team100"])
##     print(get_match_factors(row_id, coefs))
##     print(paste0("Winchance: ", predict(mod, newx=X_train[row_id,,drop=FALSE], type="response")))
## }

## summarise_match(row_id=108, coefs=coefs)



################################################################################
##
## Retrain model with all matches X_train and X_test combinded
##
################################################################################

X <- sapply(matches[, predictor_variables, with=FALSE], as.numeric)
X <- Matrix:::Matrix(X, sparse=TRUE)
y <-  as.numeric(as.logical(matches$winner_team100))

xgb_complete_data <- xgb.DMatrix(data = X, label= y)


xgb_final <- xgb.train(params = param,
                       data = xgb_complete_data,
                       print.every.n = 20,
                       nround=bst$bestInd,
                       watchlist=watchlist,
                       maximize = FALSE)



xgb.save(xgb_final, "./app/model/xgboost.model")

importance <- xgb.importance(model=xgb_final,
                             feature_names=predictor_variables,
                             label=y_train,
                             data=X_test)
print(importance)



mod = lm(as.logical(winner_team100) ~ Brand_BOTTOM_DUO_SUPPORT_100, data=matches)

summary(mod)

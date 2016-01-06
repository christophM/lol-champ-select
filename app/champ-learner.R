library("tidyr")


##TODO: 
## make champ leaner a two fold model (one fold for cond prob and one for lm + vice versa) => in the end two or more models

LogLoss<-function(actual, predicted)
{
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}


#' Train a count learner
#'
#' A count learner is bascially a table with conditional probabilities given a factor variable. 
#'
#' @param factor_var The covariable with factors (character)
#' @param target_var The binary target variable
#' @param colname The colname of the factor variable
#' @param alpha The smoothing parameter to prevent overfitting for small categories
#' @param min_matches Cutoff value for small categories. Also prevents overfitting
#'
count_learner <- function(factor_var, target_var, colname, alpha=100, min_matches=50){
  ## TODO: include case for unkown champ, which is general mean
  df = data.frame("champ"=factor_var, "win100"=target_var, stringsAsFactors=FALSE)
  df <- df %>% 
    group_by(champ) %>%
    filter(n() > min_matches) %>%
    summarise(win100_by_champ= (sum(win100) + alpha) / (n() + 2 * alpha), n=n()) %>%
    data.frame()
  
  list(count_table=df, 
       factor_levels=unique(factor_var), 
       colname=colname)
}

apply_count_learner <- function(count_learner, factor_var){
  count_table <- count_learner$count_table
  df <- data.frame("champ"=factor_var, stringsAsFactors = FALSE)
  df %>%
    left_join(count_table, by="champ") %>%
    select(win100_by_champ) %>%
    mutate(win100_by_champ=ifelse(is.na(win100_by_champ), 0.5, win100_by_champ))
}

#'Adds synergy and matchup variables to dataset
#'
#'@param X data.frame with columns TOP_SOLO_100, TOP_SOLO_200, JUNGLE_NONE_100, ...
#'@return dataset with additional columns
create_synergy_and_matchup_vars <- function(X){
  
  # Synergy
  X$synergy_carry_supp_100 <- paste(X$BOTTOM_DUO_CARRY_100, X$BOTTOM_DUO_SUPPORT_100, sep="-")
  X$synergy_carry_supp_200 <- paste(X$BOTTOM_DUO_CARRY_200, X$BOTTOM_DUO_SUPPORT_200, sep="-")
  
  ## Lane matchups
  X$matchup_top <-  paste(X$TOP_SOLO_100, X$TOP_SOLO_200, sep="-")
  X$matchup_jungle <-  paste(X$JUNGLE_NONE_100, X$JUNGLE_NONE_200, sep="-")
  X$matchup_middle <-  paste(X$MIDDLE_SOLO_100, X$MIDDLE_SOLO_200, sep="-")
  X$matchup_carry <-  paste(X$BOTTOM_DUO_CARRY_100, X$BOTTOM_DUO_CARRY_200, sep="-")
  X$matchup_support <-  paste(X$BOTTOM_DUO_SUPPORT_100, X$BOTTOM_DUO_SUPPORT_200, sep="-")
  
  ##X <- X[setdiff(names(X), TEAM_LANE_ROLES)]
  X
}


create_matchup_diffs <- function(X){
  X$matchup_top <- X$matchup_top -  0.5 * (X$TOP_SOLO_100 + X$TOP_SOLO_200 )
  X$matchup_jungle <-  X$matchup_jungle - 0.5 * (X$JUNGLE_NONE_100  +  X$JUNGLE_NONE_200) 
  X$matchup_middle <-  X$matchup_middle - 0.5 * (X$MIDDLE_SOLO_100 +  X$MIDDLE_SOLO_200)
  X$matchup_carry <-  X$matchup_carry - 0.5 * (X$BOTTOM_DUO_CARRY_100 +  X$BOTTOM_DUO_CARRY_200)
  X$matchup_support <-  X$matchup_support - 0.5 * (X$BOTTOM_DUO_SUPPORT_100 +  X$BOTTOM_DUO_SUPPORT_200 )
  
  X
}


## returns list of count learners for the single positions count learner list with 10 items
champ_count_learner <- function(X, y){
  predictor_variables <- colnames(X)
  predictor_variables <- setdiff(predictor_variables, "blue_side")
  ## all single positions
  single_positions <- lapply(predictor_variables, function(x){count_learner(X[,x], y, colname=x)})
  names(single_positions) <- predictor_variables
  ## all lane-wise counters
  ## supp / carry synergies
  single_positions
}


##' Function to train a champ model
##'
##'@param X data.frame with training data for count_learner
##'@param X_lm data.frame with training_data for linear model
##'@param y vector with target variable
##'@param y_lm vector with target variable for linear model
##'
##'@return Object of class champ_models
learn_champ_model <- function(X, y, X_lm=NULL, y_lm=NULL, champ_infos=NULL){
  
  ## Splitting the data set is optional
  if(is.null(X_lm)){
    X_lm <- X
    y_lm <- y
  }
  
  ## Count learner for combination of support and carry:
  X <- create_synergy_and_matchup_vars(X)
  X_lm <- create_synergy_and_matchup_vars(X_lm)
  
  
  ## Count learner for each single lane and position plus synergies and matchups
  count_learner <- champ_count_learner(X[setdiff(names(X), c("blue_side"))], y)
  predictor_variables <- names(count_learner)
  predictor_variables <- setdiff(predictor_variables, c("blue_side"))
  
  count_predictions <- data.frame(lapply(predictor_variables, function(x){
    apply_count_learner(count_learner[[x]], X_lm[,x])
  }), stringsAsFactors = FALSE)
  names(count_predictions) <- names(count_learner)
  
  df <- count_predictions
  
  df <- create_matchup_diffs(df)
  ## df$match_id = X_lm$match_id
  ## team_summaries <- summarise_teams(X_lm, champ_infos)
  ## df <- df %>% left_join(team_summaries, by="match_id")
  
  df$blue_side <- X_lm$blue_side
  df$win100 <- y_lm
  ##mod <- glm(win100 ~ ., data=df, family="binomial")
  mod <- lm(win100 ~ ., data=df)
  
  structure(list(count_learner=count_learner, 
                 model=mod, 
                 df=df,
                 champ_infos=champ_infos
  ), 
  class="champ_model")
}




## champ_model
## Add new predict function to method dispatcher for champ_select object
predict.champ_model <- function(object, newdata, type="response"){
  
  ## Count learner for combination of support and carry: 
  newdata <- create_synergy_and_matchup_vars(newdata)
  
  predictor_variables <- setdiff(names(object$count_learner), "blue_side")
  
  count_predictions <- data.frame(lapply(predictor_variables, function(x){
    apply_count_learner(object$count_learner[[x]], newdata[,x])
  }), stringsAsFactors = FALSE)
  
  names(count_predictions) <- predictor_variables
  df <- count_predictions
  df <- create_matchup_diffs(df)
  
  ## team_summaries <- summarise_teams(newdata, champ_infos=object$champ_infos)
  ## df <- df %>% left_join(team_summaries, by="match_id")
  df$blue_side <- newdata$blue_side
  predict(object$model, newdata=df, type=type)
}
# 
# ## TODO: try more attributes and remove if you dont find any that explain something
# summarise_teams <- function(team_df, champ_infos){
#   ## melt team_df
#   team_df %>%
#     gather_(key="role", value="champ_name", gather_cols=TEAM_LANE_ROLES) %>%
#     # merge teams with champ infos
#     left_join(champ_infos, by="champ_name") %>% 
#     mutate(team100=grepl("100", role)) %>%
#     group_by(match_id) %>%
#     summarise(attack_rel = sum(attack[team100])/sum(attack),
#               def_diff = sum(attack[team100])/sum(attack[!team100]), 
#               sum_tanks100 = sum(tag1[team100] == "Tank"), 
#               sum_tanks200 = sum(tag1[!team100] == "Tank"), 
#               no_tanks200 = sum_tanks200 == 0, 
#               diff_tanks  = sum_tanks100 - sum_tanks200, 
#               diff_difficulty = sum(difficulty[team100]) - sum(difficulty[!team100])) 
# }
# 
# 


learn_champ_model_folds <- function(X, y, champ_infos, nfolds=10){
  
  folds <- 1:nfolds
  
  ## split data into cross validation peaces
  fold_index <- sample(folds, size=nrow(X), replace=TRUE)
  
  cl_folds <- lapply(folds, function(fold){
    X_cl <- X[!(fold_index == fold),]
    y_cl <- y[!(fold_index == fold)]
    
    X_lm <- X[fold_index == fold,]
    y_lm <- y[fold_index == fold]
    
    learn_champ_model(X=X_cl, y=y_cl, X_lm=X_lm, y_lm=y_lm)
  })
  ## return structure champ_model2 of linear models und count learners
  class(cl_folds) <- "champ_model_fold"
  cl_folds
}

predict.champ_model_fold <- function(object, newdata, type="response"){
  
  predictions <- lapply(object, function(obj){
    predict(object=obj, newdata=newdata, type=type)
  })
  ## average outcomes
  if(type=="response"){
    rowMeans(data.frame(predictions))
  } else if(type=="terms"){
    print("Not yet implemented")
  }
}

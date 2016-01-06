library("xgboost")
library("data.table")
library("ggplot2")
library("ggthemes")
library("dplyr")


source("variables.R")
source("functions.R")
source("champ-learner.R")



## TODO:
## Use is_blue side in prediction
## For the matchup and synergy effects, example top lane:  add effects for top_100, top200, matchup_top 
## Think about how you display the interaction effects between matchups and synergies
## Speicherlimit bei MongoDB höher setzen, damit alle spiele sicher gespeichert werden
## Sort effects by: top, jgl, mid, carry, adc
## Add filter for selected champions, so you dont have one champion twice
## Retrain champ-learner with model stacking
## duplicate matches by switching teams and introduce indicator for team 100 
## Find nicer solution how to drag the match_id through the count learner thing


champion_summary <- function(champions, 
                             champ_role_counter, 
                             roles=LANE_ROLES, 
                             team_roles=TEAM_LANE_ROLES){
  
  all_roles = c(roles, roles)
  pick_rates = sapply(1:10, function(x){
    champ_role_counter[[all_roles[x]]][[champions[[x]]]] / max_champ_role_count
  })
  data.frame(champion=champions, 
             team=c(rep("100", times=5), rep("200", times=5)),
             team_role=team_roles,
             role=all_roles,
             ## champion_roles = paste0(champions, roles),
             pick_rate=pick_rates)
}


## Load model
load(sprintf("%s/champ_model_factorial.RData", MODEL_DIR))

#' Extracts the terms from the champ count learner for vis
#' 
#' @param features data.frame with the champ selection
#' @param champ_model The loaded champ_model
#' 
#' @return data.frame with effects for champion selection win probabilities
get_terms <- function(features, champ_model){
  
  ## outcome from predict(type=="terms") is beta * (covar_value - mean(covar_value)) for each variable in the dataset
  terms <- data.frame(predict(champ_model, newdata=features, type="terms"))
  print(terms)
  terms <- data.frame(effect_name=colnames(terms), 
                      effect=unlist(terms[1,,drop=TRUE]))
  
  effect_champs <- create_synergy_and_matchup_vars(features)
  effect_champs <- data.frame(effect_name=colnames(effect_champs), 
                              champs=unlist(effect_champs[1,,drop=TRUE]))
  terms <- terms %>% 
    left_join(effect_champs, by="effect_name")
  
  terms$team <- "both"
  terms$team[grepl("100", terms$effect_name)] <- "100"
  terms$team[grepl("200", terms$effect_name)] <- "200"
  
  ## Flip effects for team 200 because negative is good for team 200
  terms$effect[terms$team == "200"] <- -1 * terms$effect[terms$team == "200"]
  
  ## Flip effects for matchups for visualisation 
  terms$effect[terms$team == "both"] <- -1 * terms$effect[terms$team == "both"]
  
  terms$n_champs <- 1
  terms$n_champs[grepl("synergy|matchup", terms$effect_name)] <- 2
  
  terms$champ100 <- NA
  terms$champ100[terms$n_champs == 2] <- gsub("\\-(.*)", "", terms$champs[terms$n_champs == 2])
  
  terms$champ200 <- NA
  terms$champ200[terms$n_champs == 2] <- gsub("(.*)\\-", "", terms$champs[terms$n_champs == 2])
  
  terms$role <- gsub("_[1|2]00", "", terms$effect_name)
  
  
  terms$effect_direction <- ifelse(terms$effect > 0, "+", "-")
  terms$effect_string <- sprintf("%s%.1f", terms$effect_direction, 100 * abs(terms$effect))
  terms
}

## Create vector of champions from champions and team combos

shinyServer(function(input, output) {
  
  picked_champions = reactive({
    
    list("TOP_SOLO_100"=input$champ_team_top, 
         "JUNGLE_NONE_100"=input$champ_team_jgl, 
         "MIDDLE_SOLO_100"=input$champ_team_mid, 
         "BOTTOM_DUO_CARRY_100"=input$champ_team_adc,
         "BOTTOM_DUO_SUPPORT_100"=input$champ_team_supp, 
         "TOP_SOLO_200"=input$champ_enemy_top, 
         "JUNGLE_NONE_200"=input$champ_enemy_jgl, 
         "MIDDLE_SOLO_200"=input$champ_enemy_mid, 
         "BOTTOM_DUO_CARRY_200"=input$champ_enemy_adc,
         "BOTTOM_DUO_SUPPORT_200"=input$champ_enemy_supp, 
         "blue_side"= as.logical(input$blue_side)
    )
  })
  
  ## Server functions
  win_probability_team <- reactive({
    features = data.frame(picked_champions(), stringsAsFactors=FALSE)
    print(features)
    win100 <- predict.champ_model(champ_model, newdata=features)
    win100
  })
  
  output$win_probability_team <- reactive({
    win_chance_team <- 100 * round(win_probability_team(), 4)
    sprintf("%s%%", win_chance_team)
  })
  
  output$win_probability_enemy <- reactive({
    win_chance_enemy <- 100 * round(1  - win_probability_team(), 4)
    sprintf("%s%%", win_chance_enemy)
  })
  
  
  output$effects <- renderPlot({
    features = data.frame(picked_champions(), stringsAsFactors=FALSE)
    

    terms <- get_terms(features, champ_model)
  
    p <- ggplot(terms[terms$team != "both", ]) +
      geom_bar(aes(x=role, y=effect, fill=team), stat="identity", alpha =0.7) + 
      coord_flip() +
      facet_grid(. ~ team) + 
      geom_text(aes(x=role,label=champs), y=0, hjust=1, vjust=0, size=8) + 
      geom_text(aes(x=role, y=0.1, label=effect_string, color=effect_direction), size=8) + 
      scale_y_continuous(limits=c(-0.2, 0.2)) + 
      theme_bw()
    
    
    print(p)
})
  
  output$matchups <- renderPlot({
    features = data.frame(picked_champions(), stringsAsFactors=FALSE)
    
    terms <- get_terms(features, champ_model)
    
    p <- ggplot(terms[terms$team == "both", ]) +
      geom_bar(aes(x=role, y=effect, fill=team), stat="identity", alpha =0.7) + 
      coord_flip() +
      facet_grid(. ~ team) + 
      geom_text(aes(x=role,label=champs), y=0, hjust=1, vjust=0, size=8) + 
      scale_y_continuous(limits=c(-0.2, 0.2)) + 
      theme_bw()
    
    
    print(p)
  })
  
  
  output$summary_plot <- renderPlot({
    pick_rates <- champion_summary(unlist(picked_champions()), champ_role_counter=champ_role_counter)
    pick_rates_team <- pick_rates[pick_rates$team == "100",]
    pick_rates_enemy <- pick_rates[pick_rates$team == "200",]
    
    p <- ggplot(pick_rates) + 
      geom_bar(aes(x=role, y=pick_rate, fill=team), stat="identity", alpha =0.7) + 
      geom_text(aes(x=role,label=champion), y=0.05, hjust=0, size=8) + 
      coord_flip() + 
      scale_fill_manual(name="Team", values=c("100"="Team", "200"="Enemy")) +
      scale_y_continuous(limits=c(0,1)) + 
      facet_grid(. ~ team)
    print(p)
  })
  
  
})


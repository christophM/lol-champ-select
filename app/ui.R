
library("jsonlite")
source("variables.R")


shinyUI(
  fluidPage(
    titlePanel("League of Legends Team Builder"),
    wellPanel(
      fluidRow(
        column(width=2, h3("Team")),
        column(width=2, selectInput("champ_team_top", "Top", champions_top, selected = "Wukong")),
        column(width=2, selectInput("champ_team_jgl", "Jungle", champions_jungle, selected = "Nocturne")),
        column(width=2, selectInput("champ_team_mid", "Mid", champions_middle, selected = "Annie")),
        column(width=2, selectInput("champ_team_adc", "ADC", champions_carry, selected = "Tristana")),
        column(width=2, selectInput("champ_team_supp", "Support", champions_support, selected = "Janna"))
      )
      ,
      fluidRow(
        column(width=2, h3("Enemy")),
        column(width=2, selectInput("champ_enemy_top", "Top", champions_top, selected = "Malphite")),
        column(width=2, selectInput("champ_enemy_jgl", "Jungle", champions_jungle, selected = "Xin Zhao")),
        column(width=2, selectInput("champ_enemy_mid", "Mid", champions_middle, selected = "Yasuo")),
        column(width=2, selectInput("champ_enemy_adc", "ADC", champions_carry, selected = "Jinx")),
        column(width=2, selectInput("champ_enemy_supp", "Support", champions_support, selected = "Tahm Kench"))
      ), fluidRow(
        column(width=4, h3("Playing blue side (left lower corner):")),
        column(width=4, selectInput("blue_side", "Blue side", choices=c("Team"=TRUE, "Enemy"=FALSE)))
      )
    ),
    fluidRow(column(width=10, h1("Chance to win the match", style="align:center"), offset=1)),
    fluidRow(
      column(4, h1("Your team: ", textOutput("win_probability_team"), style= "color:blue"), offset=2),
      column(4, h1("Enemy team: ", textOutput("win_probability_enemy"), style="color:red"), offset=1)
    ),
    fluidRow(column(10, h1("Champion strength"), offset=1)),
    fluidRow(column(10, plotOutput("effects"), offset=1)),
    fluidRow(column(10, h1("Champion matchups"), offset=1)),
    fluidRow(column(10, plotOutput("matchups"), offset=1)),
    fluidRow(column(10, h1("Champion popularity"), offset=1)),
    fluidRow(column(10, plotOutput("summary_plot"), offset=1))
  )
)

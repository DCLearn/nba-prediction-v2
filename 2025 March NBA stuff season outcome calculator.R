library(hoopR)
library(ggplot2)
library(corrplot)
library(tidyr)
library(tidyverse)
library(factoextra)
library(rdist)
library(randomForest)
library(xgboost)
#simulate season performance with random forest
#each.player=rnorm(games.played, mean = average.stat.per.touch, sd = std.dev.stat.per.touch)

#simulate opponent team performance randomly
sim.opp.stats=data.frame(matrix(ncol=25,nrow=0))
colnames(sim.opp.stats)= c("GAME_ID","TEAM_ID","TEAM_NAME","TEAM_ABBREVIATION","TEAM_CITY","MIN",
                           "FGM","FGA","FG_PCT","FG3M","FG3A","FG3_PCT", "FTM","FTA" ,"FT_PCT" ,"OREB" ,
                            "DREB","REB","AST","STL","BLK","TO","PF","PTS","PLUS_MINUS")
nba.teams.ids = nba_teams()$team_id

# for (i in 1:82){
#   tryCatch({
#   team.id = sample((nba_teams()$team_id),1)
#   game.id = (nba_teamgamelog(season=2023, team_id = team.id)$TeamGameLog$Game_ID)[i]
#   opp.stat= as.data.frame(nba_boxscoretraditionalv2(game_id = game.id, season=2023)$TeamStats[sample(1:2,1),])
#   sim.opp.stats[i,] = opp.stat
#   }, error=function(e){})
# }

for (i in (1:82)){
  tryCatch({
  opp.stat = NULL
  game.id = NULL
  team.id = NULL
  attempt = 1
  while (is.null(game.id) & is.null(team.id) & is.null(opp.stat) & attempt < 3) {
    attempt = attempt + 1
    try({team.id = sample(nba.teams.ids,1)
        game.id = (nba_teamgamelog(season=2023, team_id = team.id)$TeamGameLog$Game_ID)[i]
        opp.stat= as.data.frame(nba_boxscoretraditionalv3(game_id = game.id, season=2023)[sample(3:4,1)])[-(1:3)]
    }, silent = TRUE)
  }
  sim.opp.stats[i,] = opp.stat
  }, error=function(e){})
}

sim.opp.stats=sim.opp.stats[- which(is.na(sim.opp.stats[1])),]

sim.opp.stats


training.set=data.frame(matrix(ncol=length(data.frame(nba_teamgamelog(season=2023, team_id = team.id))),nrow=0))
colnames(training.set)=c("GAME_ID","TEAM_ID","TEAM_NAME","TEAM_ABBREVIATION","TEAM_CITY","MIN",
                         "FGM","FGA","FG_PCT","FG3M","FG3A","FG3_PCT", "FTM","FTA" ,"FT_PCT" ,"OREB" ,
                         "DREB","REB","AST","STL","BLK","TO","PF","PTS","PLUS_MINUS")
#sort(as.data.frame(nba_teamgamelog(season=2023, team_id = team.id)$TeamGameLog$Game_ID))
#sort_by.data.frame(as.data.frame(nba_teamgamelog(season=2023, team_id = team.id)$TeamGameLog),as.data.frame(nba_teamgamelog(season=2023, team_id = team.id)$TeamGameLog$Game_ID))

for (i in 1:length(nba.teams.ids)){
  tryCatch({
  team.stats= NULL
  attempt = 1
  j = paste0((nba.teams.ids)[i])
  while (is.null(team.stats) & attempt < 3){
    attempt = attempt + 1
    try({
      team.stats= data.frame(nba_teamgamelog(season=2023, team_id = j))}
      , silent = TRUE)
  }
  training.set= rbind(team.stats,training.set)
  }, error=function(e){})
}

training.set=sort_by.data.frame(training.set, training.set$TeamGameLog.Game_ID)

unique.game.id=sort(unique(training.set$TeamGameLog.Game_ID))

unique.game.id
team.id=sample(nba.teams.ids,1)
WL.set=data.frame(matrix(ncol=2*length(data.frame(nba_teamgamelog(season=2023, team_id = team.id))),nrow=0))

for (i in unique.game.id){
  games=training.set[training.set$TeamGameLog.Game_ID== i ,]
  games[1,]
  gamie=cbind(team1=games[1,], team2=games[2,])
  WL.set = rbind(gamie, WL.set)
}

WL.set=WL.set[-c(2,29,32)]

WL.set=na.omit(WL.set)
WL.set$team1.TeamGameLog.WL = ifelse(WL.set$team1.TeamGameLog.WL=="W", 1, 0)
#WL.set$team1.TeamGameLog.GAME_DATE=as.numeric(as.POSIXct(strptime(WL.set$team1.TeamGameLog.GAME_DATE, format="%b %d, %Y")))

#sim.WL.set = cbind(,sim.opp.stats)
rf.guess= randomForest(x = WL.set[-c(1,2,3,4,5,6,7,8,27,28,29,30,31,32,34)], y = as.factor(WL.set$team1.TeamGameLog.WL), ntree = 500)
#randomForest underperformed, only captured 7 variables
y_pred = predict(rf.guess, sim.all.games) 
y_pred
#dubious results with random forest

sim.opp.stats=sim.opp.stats[-c(1,2,3,4,5,6,25,26)]
fake.sim.opp.stats=(na.omit(rbind(sim.opp.stats,sim.opp.stats,sim.opp.stats,sim.opp.stats[1:10,])))
sim.all.games = cbind(total.sim.gamelog,fake.sim.opp.stats[1:82,])
colnames(sim.all.games)=colnames(WL.set[-c(1,2,3,4,5,6,7,8,27,28,29,30,31,32,34)])
sim.all.games=na.omit(sim.all.games)

sim.all.games

Xtest = xgb.DMatrix(data = as.matrix(sapply(X=sim.all.games, FUN= as.numeric)))
Dtrain = xgb.DMatrix(data = as.matrix(sapply(X=WL.set[-c(1,2,3,4,5,6,7,8,27,28,29,30,31,32,34)], FUN= as.numeric)), label = as.vector.data.frame(WL.set$team1.TeamGameLog.WL))

param_list = list( 
  eta = 0.7,
  objective = "binary:hinge", 
  max_depth = 69
) 
set.seed(112) # Setting seed 
xgbcv = xgb.cv(params = param_list, 
               data = Dtrain, 
               nrounds = 3000, 
               nfold = 15, 
               print_every_n = 10, 
               early_stopping_rounds = 100, 
               maximize = F) 



xgboost.guess=xgb.train(data = Dtrain, params = param_list, nrounds = xgbcv$best_iteration) 
#xgboost(data=xgboost.guess, max.depth = 5, nrounds=41, verbose = 0)
predict(newdata=Xtest,object=xgboost.guess)
sum(predict(newdata=Xtest,object=xgboost.guess))
#XGBoost outperforms random forest on dummy roster (only 5 players)
#XGBoost correctly predicts overpowered roster
#XGBoost predicts 44, 46 games won for the 23-24 Orlando Magic. Actual = 47.
#XGBoost predicts 48, 57 games won for the 23-24 Atlanta Hawks. Actual = 36
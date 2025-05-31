library(hoopR)
library(ggplot2)
library(corrplot)
library(tidyr)
library(tidyverse)
library(factoextra)
library(rdist)
library(randomForest)
library(xgboost)
library(purrr)

teamstatistic = as.data.frame(nba_leaguegamelog(season=2024))
teamstatistic$LeagueGameLog.GAME_DATE = as.Date.character(teamstatistic$LeagueGameLog.GAME_DATE, 
                                                          "%Y-%m-%d")
teams=unique(teamstatistic$LeagueGameLog.TEAM_NAME)

nba.teams.ids = nba_teams()$team_id

for (i in 1:30){
  this.team=teams[i]
  x=as.character(this.team)
  teams.games= as.data.frame(teamstatistic[which(teamstatistic$LeagueGameLog.TEAM_NAME == this.team),])[-c(1,2,3,4,5,6,7,8,9,28,29)]
  games.id = teamstatistic$LeagueGameLog.GAME_ID[which(teamstatistic$LeagueGameLog.TEAM_NAME == this.team)]
  opp.boxscore = data.frame(matrix(nrow=length(games.id), ncol = length(names(teams.games))))
  for (j in 1:length(games.id)){
    this.game = games.id[j]
    this.opp = as.data.frame(teamstatistic[which(teamstatistic$LeagueGameLog.GAME_ID == as.character(this.game) &
                                     teamstatistic$LeagueGameLog.TEAM_NAME != this.team),])
    this.opp = this.opp[-c(1,2,3,4,5,6,7,8,9,28,29)]
    opp.boxscore[j,] = cbind(teams.games[j,], this.opp)
  }
  teams.games=cbind(teams.games,opp.boxscore)
  colnames(teams.games) = c("team1.TeamGameLog.FGM","team1.TeamGameLog.FGA","team1.TeamGameLog.FG_PCT","team1.TeamGameLog.FG3M",
                            "team1.TeamGameLog.FG3A","team1.TeamGameLog.FG3_PCT","team1.TeamGameLog.FTM","team1.TeamGameLog.FTA",
                            "team1.TeamGameLog.FT_PCT","team1.TeamGameLog.OREB","team1.TeamGameLog.DREB","team1.TeamGameLog.REB",
                            "team1.TeamGameLog.AST","team1.TeamGameLog.STL","team1.TeamGameLog.BLK","team1.TeamGameLog.TOV",
                            "team1.TeamGameLog.PF","team1.TeamGameLog.PTS","team2.TeamGameLog.MIN","team2.TeamGameLog.FGA",
                            "team2.TeamGameLog.FG_PCT","team2.TeamGameLog.FG3M","team2.TeamGameLog.FG3A","team2.TeamGameLog.FG3_PCT",
                            "team2.TeamGameLog.FTM","team2.TeamGameLog.FTA","team2.TeamGameLog.FT_PCT","team2.TeamGameLog.OREB",
                            "team2.TeamGameLog.DREB","team2.TeamGameLog.REB","team2.TeamGameLog.AST","team2.TeamGameLog.STL",
                            "team2.TeamGameLog.BLK","team2.TeamGameLog.TOV","team2.TeamGameLog.PF","team2.TeamGameLog.PTS")
  assign(paste(x), teams.games)
}





team.id=sample(nba.teams.ids,1)
training.set=data.frame(matrix(ncol=length(data.frame(nba_teamgamelog(season=2023, team_id = team.id))),nrow=0))

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

wins=as.data.frame(nba_leaguestandings(season = 2024))[c(4,5,13)]
wins[2]=paste(wins$Standings.TeamCity, wins$Standings.TeamName)

xgb.check = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
xgb.check = merge(wins, xgb.check, by.x = "Standings.TeamName", by.y="Teams")[-2]


for (i in 1: length(teams)){
  j = eval(as.name(as.character(teams[i])))
  Xtest = xgb.DMatrix(data = as.matrix(sapply(X= j, FUN= as.numeric)))
  Dtrain = xgb.DMatrix(data = as.matrix(sapply(X=WL.set[-c(1,2,3,4,5,6,7,8,27,28,29,30,31,32,34)], FUN= as.numeric)), label = as.vector.data.frame(WL.set$team1.TeamGameLog.WL))

  param_list = list( 
    eta = 0.61,
    objective = "binary:hinge", 
    max_depth = 12
  ) 
#  set.seed(11) # Setting seed 
  xgbcv = xgb.cv(params = param_list, 
                 data = Dtrain, 
                 nrounds = 100, 
                 nfold = 15, 
                 print_every_n = 80, 
                 early_stopping_rounds = 85, 
                 maximize = F) 
  
  
  
  xgboost.guess=xgb.train(data = Dtrain, params = param_list, nrounds = xgbcv$best_iteration) 
  predict(newdata=Xtest,object=xgboost.guess)
  xgb.check[i,3] = sum(predict(newdata=Xtest,object=xgboost.guess))
}

xgb.check
plot(x=as.numeric(xgb.check$Standings.WINS), y=as.numeric(xgb.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))
abline(coef= c(0,1))

for (i in 1: length(teams)){
  j = eval(as.name(as.character(teams[i])))
  Xtest = xgb.DMatrix(data = as.matrix(sapply(X= j, FUN= as.numeric)))
  Dtrain = xgb.DMatrix(data = as.matrix(sapply(X=WL.set[-c(1,2,3,4,5,6,7,8,27,28,29,30,31,32,34)], FUN= as.numeric)), label = as.vector.data.frame(WL.set$team1.TeamGameLog.WL))
  
  param_list = list( 
    eta = 0.9,
    objective = "binary:hinge", 
    max_depth = 27
  ) 
  #  set.seed(11) # Setting seed 
  xgbcv = xgb.cv(params = param_list, 
                 data = Dtrain, 
                 nrounds = 50, 
                 nfold = 15, 
                 print_every_n = 5, 
                 early_stopping_rounds = 50, 
                 maximize = T) 
  
  
  
  xgboost.guess=xgb.train(data = Dtrain, params = param_list, nrounds = xgbcv$best_iteration) 
  predict(newdata=Xtest,object=xgboost.guess)
  xgb.check[i,3] = sum(predict(newdata=Xtest,object=xgboost.guess))
}

plot(x=as.numeric(xgb.check$Standings.WINS), y=as.numeric(xgb.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
abline(coef= c(0,1))

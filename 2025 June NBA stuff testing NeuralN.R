library(hoopR)
library(corrplot)
library(tidyr)
library(tidyverse)
library(factoextra)
library(rdist)
library(neuralnet)
library(glmnet)
library(caret)
library(MASS)

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
                            "team1.TeamGameLog.PF","team1.TeamGameLog.PTS","team2.TeamGameLog.FGM","team2.TeamGameLog.FGA",
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
  gamie=cbind(team1=games[1,], team2=games[2,])
  WL.set = rbind(gamie, WL.set)
}

WL.set=WL.set[-c(2,29,32)]

WL.set=na.omit(WL.set)
WL.set$team1.TeamGameLog.WL = ifelse(WL.set$team1.TeamGameLog.WL=="W", 1, 0)
WL.set=WL.set[-c(1,2,3,5,6,7,8,27,28,29,30,31,32,33)]

WL.set$team1.TeamGameLog.WL=as.factor(WL.set$team1.TeamGameLog.WL)
WL.set[2:37]=apply(WL.set[2:37],2,as.numeric)

wins=as.data.frame(nba_leaguestandings(season = 2024))[c(4,5,13)]
wins[2]=paste(wins$Standings.TeamCity, wins$Standings.TeamName)

pca.run=summary(prcomp(WL.set[-1], scale. = T, center = T))

pca.run$importance #PC18
pca.check=c()

for (i in 1:18){
  sephir=apply(pca.run$rotation,2, min)
  component = pca.run$rotation[,i]
  # print(i)
  # print(which(component==sephir[i]))
  pca.check = rbind(pca.check, names(which(component==sephir[i])))
}
pca.check=as.vector(unique(pca.check))
pca.WL.set=data.frame(WL.set$team1.TeamGameLog.WL,  WL.set[pca.check])
colnames(pca.WL.set) = c(colnames(WL.set[1]), pca.check)

names(pca.WL.set)
head(pca.WL.set)

nbalda=lda(team1.TeamGameLog.WL~. , data=pca.WL.set)

lda.check = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
lda.check = merge(wins, lda.check, by.x = "Standings.TeamName", by.y="Teams")[-2]

for (i in 1: length(teams)){
  lda.test = eval(as.name(as.character(teams[i])))
  lda.predict=predict(object = nbalda, data.frame(apply(lda.test,2,as.numeric)))
  lda.check[i,3] = sum(lda.predict$class == 1)
}

plot(x=as.numeric(lda.check$Standings.WINS), y=as.numeric(lda.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
  abline(coef= c(0,1))+
  abline(lm(as.numeric(Predicted.Wins)~as.numeric(Standings.WINS), data=lda.check))

stepper = step(glm(team1.TeamGameLog.WL~., data=WL.set, family = binomial(link = logit)), direction = "both")
glm.run=glm(stepper$formula, data =WL.set, family = binomial(link = "logit"))

glm.check = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
glm.check = merge(wins, glm.check, by.x = "Standings.TeamName", by.y="Teams")[-2]

for (i in 1: length(teams)){
  glm.test = eval(as.name(as.character(teams[i])))
  glm.test = data.frame(apply(glm.test[c(1,4,7,19,22,25)],2, as.numeric))
  names(glm.test) = names(glm.run$coefficients)[-1]
  glm.predict=predict.glm(glm.run, glm.test)
  glm.check[i,3] = sum(glm.predict > 0)
}

plot(x=as.numeric(glm.check$Standings.WINS), y=as.numeric(glm.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
  abline(coef= c(0,1))+
  abline(lm(as.numeric(Predicted.Wins)~as.numeric(Standings.WINS), data=glm.check))


NNmodel = neuralnet(team1.TeamGameLog.WL~.,
  data=WL.set,
  hidden=c(30, 16, 6),
  linear.output = F,
  algorithm = 'slr'
)

nn.check = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
nn.check = merge(wins, nn.check, by.x = "Standings.TeamName", by.y="Teams")[-2]

for (i in 1: length(teams)){
  NNtest = eval(as.name(as.character(teams[i])))
  NNtest=apply(NNtest,2,as.numeric)
  nn.predict=predict(NNmodel, NNtest)
  nn.check[i,3] = round(sum(as.data.frame(nn.predict)[1]),0)
}

plot(x=as.numeric(nn.check$Standings.WINS), y=as.numeric(nn.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
abline(coef= c(0,1))+
abline(lm(as.numeric(Predicted.Wins)~as.numeric(Standings.WINS), data=nn.check))

WL.set[2:37]=data.matrix(lapply(WL.set[2:37],as.numeric))

ridge.model=cv.glmnet(x=(as.matrix(WL.set[2:37])), 
                      y=(WL.set$team1.TeamGameLog.WL), 
                      family ="binomial",
                      type.measure = "class",
                      nlambda=200)

ridge.check = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
ridge.check = merge(wins, ridge.check, by.x = "Standings.TeamName", by.y="Teams")[-2]

for (i in 1: length(teams)){
  ridge.test = eval(as.name(as.character(teams[i])))
  ridge.test=apply(ridge.test,2,as.numeric)
  ridge.predict=predict(ridge.model, s=ridge.model$lambda.1se, newx=ridge.test)
  ridge.check[i,3] = sum(ridge.predict > 0)
}

plot(x=as.numeric(ridge.check$Standings.WINS), y=as.numeric(ridge.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
  abline(coef= c(0,1))+
  abline(lm(as.numeric(Predicted.Wins)~as.numeric(Standings.WINS), data=ridge.check))


lasso.model=cv.glmnet(x=(as.matrix(WL.set[2:37])), 
                      y=WL.set$team1.TeamGameLog.WL, 
                      family ="binomial",
                      type.measure = "class",
                      alpha=1)

lasso.check = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
lasso.check = merge(wins, lasso.check, by.x = "Standings.TeamName", by.y="Teams")[-2]

for (i in 1: length(teams)){
  lasso.test = eval(as.name(as.character(teams[i])))
  lasso.test=apply(lasso.test,2,as.numeric)
  lasso.predict=predict(lasso.model, s=lasso.model$lambda.1se, new=lasso.test, type = "class")
  lasso.check[i,3] = sum(lasso.predict > 0)
}

plot(x=as.numeric(lasso.check$Standings.WINS), y=as.numeric(lasso.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
  abline(coef= c(0,1))+
  abline(lm(as.numeric(Predicted.Wins)~as.numeric(Standings.WINS), data=lasso.check))

control=trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

elastic.model = train(x=(as.matrix(WL.set[2:37])), 
                      y=(WL.set$team1.TeamGameLog.WL),
                       data = WL.set,
                       method = "glmnet",
                       preProcess = c("center", "scale"),
                       tuneLength = 25,
                       trControl = control,
                      metric = "Accuracy",
                      maximize = TRUE)

#predict(elastic.model, `Los Angeles Lakers`)

#names(`Los Angeles Lakers`)

#elastic.model=cv.glmnet(x=WL.set[2:37], y=WL.set[1], family ="binomial")

elastic.check = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
elastic.check = merge(wins, elastic.check, by.x = "Standings.TeamName", by.y="Teams")[-2]

for (i in 1: length(teams)){
  elastic.test = eval(as.name(as.character(teams[i])))
  elastic.test = apply(elastic.test,2,as.numeric)
  elastic.predict= predict(elastic.model, elastic.test)
  elastic.check[i,3] = sum(elastic.predict == 1)
}

elastic.predict
elastic.check
plot(x=as.numeric(elastic.check$Standings.WINS), y=as.numeric(elastic.check$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
  abline(coef= c(0,1))+
  abline(lm(as.numeric(Predicted.Wins)~as.numeric(Standings.WINS), data=elastic.check))

NNmodel2 = neuralnet(team1.TeamGameLog.WL~.,
                    data=pca.WL.set,
                    hidden=c(30, 16, 6),
                    linear.output = F,
                    #  rep= 2,
                    algorithm = 'slr'
)

nn.check2 = data.frame("Teams"=teams, "Predicted Wins" = sample(0,30,T))
nn.check2 = merge(wins, nn.check2, by.x = "Standings.TeamName", by.y="Teams")[-2]

for (i in 1: length(teams)){
  NNtest = eval(as.name(as.character(teams[i])))
  NNtest=apply(NNtest,2,as.numeric)
  nn.predict2=predict(NNmodel2, NNtest)
  nn.check2[i,3] = round(sum(as.data.frame(nn.predict2)[1]),0)
}

nn.check2

plot(x=as.numeric(nn.check2$Standings.WINS), y=as.numeric(nn.check2$Predicted.Wins), xlim = c(0,82), ylim=c(0,82))+
  abline(coef= c(0,1))+
  abline(lm(as.numeric(Predicted.Wins)~as.numeric(Standings.WINS), data=nn.check2))


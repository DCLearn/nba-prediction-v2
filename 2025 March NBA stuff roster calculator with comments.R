library(hoopR)
library(ggplot2)
library(corrplot)
library(tidyr)
library(tidyverse)
library(factoextra)
library(rdist)
library(stringi)
library(EnvStats)
library(neuralnet)
library(glmnet)
library(caret)
library(MASS)

starter.touch.ranges= data.frame("1" = c(94,74), "2"=c(74, 68), "3"=c(68, 54), "4"=c(54, 39), "5"=c(39, 24),
                                 "6" = c(68,50), "7"=c(50,49), "8"=c(49,37), "9"=c(37,35), "10"=c(35,27),
                                 "11" = c(27,26), "12"=c(26,18), "13"=c(18,14), "14"=c(14,6), "15"=c(6,0))

#Make sure abbreviated names have periods after each abbreviation letter
# no special letters
#list players in descending order of touches

starter.names=c("Trae Young", "Dejounte Murray", "Jalen Johnson", "Saddiq Bey", "Clint Capela")
bench.names=c( "Bogdan Bogdanovic", "De'Andre Hunter", "Onyeka Okongwu", "Vit Krejci", "Bruno Fernando")
third.names=c("Garrison Mathews"
              ,"Wesley Matthews"
              ,"Trent Forrest" 
              #,"Patty Mills" 
              #,"AJ Griffin"
              )
all.names=c(starter.names, bench.names, third.names)
the.lineups = c("starter.names", "bench.names", "third.names")

sample.touch.ranges =c()

#select the number of touches associated with each level of offense
for (k in 1:length(all.names)){
  a=starter.touch.ranges[1,k]
  b=starter.touch.ranges[2,k]
  j = sample(a:b, 1)
  sample.touch.ranges = c(sample.touch.ranges, j)
}

# names of all players
index=as.data.frame(nba_playerindex(season = year_to_season(most_recent_nba_season() - 1)))

#remove unusual characters and put names together

index$PlayerIndex.PLAYER_FIRST_NAME = stri_trans_general(str = index$PlayerIndex.PLAYER_FIRST_NAME, id = "Latin-ASCII")
index$PlayerIndex.PLAYER_LAST_NAME = stri_trans_general(str = index$PlayerIndex.PLAYER_LAST_NAME, id = "Latin-ASCII")
index$Player.Names = paste(index$PlayerIndex.PLAYER_FIRST_NAM, index$PlayerIndex.PLAYER_LAST_NAME, sep = " ")

all.starters.sim.stats = data.frame(matrix(nrow=18, ncol=0))
all.starters.sim.sd = data.frame(matrix(nrow=18, ncol=0))
starters.gp = c()
player.row = c()
per.touch.stat = data.frame(matrix(nrow=18, ncol=0))
per.touch.sd = data.frame(matrix(nrow=18, ncol=0))
player.sim.gamelog.TFdf = data.frame(matrix(nrow = 82, ncol = length(all.names)))
player.sim.gamelog.touches = data.frame(matrix(nrow = 82, ncol = length(all.names)))

#all players id numbers
for (k in 1:length(all.names)){
  j = as.character(all.names[k])
  player.row = c(player.row, index$PlayerIndex.PERSON_ID[grep(j,index$Player.Names,ignore.case = TRUE)])
}

#for the constantly breaking API
try_function=function(func){
  attempt = 1
  variable = NULL
  while (is.null(variable) & attempt < 3){
    attempt = attempt + 1
    variable = try(func, silent = TRUE)
    Sys.sleep(5 ^ attempt)
  }
  return(variable)
}

#work in batches of each lineup to prevent the API from breaking
for (lineup in 1:length(the.lineups)){
  this.lineup = (eval(as.name(the.lineups[lineup])))
  message("This lineup: ", this.lineup)
  for (this.player in 1:length(this.lineup)){
    i=match((this.lineup[this.player]),all.names) #better match the original 15 length vector system without major changes
    message("This player: ", this.lineup[this.player], " number: ", i) #track progress
    
    #pull the number of ganes the player has played and their 2023 season game log
    tryCatch({
    player.idnum = as.numeric(player.row[i])
    
    games.played=try_function(as.vector(nba_playercareerstats(player_id = player.idnum)$SeasonTotalsRegularSeason$GP))
    player.gamelog= try_function(nba_playergamelog(player_id = player.idnum, season = 2023)$PlayerGameLog)
    
    #split away and home games processing because they use different conventions
    away.games=player.gamelog$Game_ID[grepl("@", player.gamelog$MATCHUP, fixed=TRUE)]
    home.games=player.gamelog$Game_ID[grepl("vs", player.gamelog$MATCHUP, fixed=TRUE)]
    
    h.games=data.frame(matrix(nrow=0, ncol = 17))
    h.touches=c()
    
    message("First step data retrieval for this player: ", this.lineup[this.player], " number:", i, " completed")
    
    #retrieve traditional box score and tracking box score. use tracking box score to find player touches
    for (l in 1:length(home.games)){
      tryCatch({
      j=home.games[l]
      retrieved.home=try_function(nba_boxscoretraditionalv3(game_id = j)$home_team_player_traditional)
      h.player.stat=retrieved.home[which(retrieved.home$person_id == as.numeric(player.idnum)), c(18:35)]
      h.track=try_function(nba_boxscoreplayertrackv3(game_id = j)$home_team_player_player_track)
      message("Captured home tracking data for game", l, " for player",this.lineup[this.player], " number: ", i)
      h.touch=h.track$touches[(h.track$person_id == as.numeric(player.idnum))]
      h.touches = c(h.touches, h.touch)
      h.games = rbind(h.games, as.data.frame(h.player.stat))
      }, error=function(e){})
    }
    message("Home data retrieval for this player: ", this.lineup[this.player], " number:", i, "completed")
    
    a.touches=c()
    a.games=data.frame(matrix(nrow=0, ncol = 17))
    
    for (l in 1:length(away.games)){
      tryCatch({
      j=away.games[l]
      retrieved.away=try_function(nba_boxscoretraditionalv3(game_id = j)$away_team_player_traditional)
      a.player.stat=retrieved.away[which(retrieved.away$person_id == as.numeric(player.idnum)), c(18:35)]
      a.track=try_function(nba_boxscoreplayertrackv3(game_id = j)$away_team_player_player_track)
      message("Captured away tracking data for game", l, "for player",this.lineup[this.player], " number:", i)
      a.touch=a.track$touches[(a.track$person_id == as.numeric(player.idnum))]
      a.touches = c(a.touches, a.touch)
      a.games = rbind(a.games, as.data.frame(a.player.stat))
      }, error=function(e){})
    }
    message("Away data retrieval for this player: ", this.lineup[this.player], " number:", i, "completed")
    
    #combine total touches to determine average touches and then determine mean and standard deviation touch per stat
    all.stats=rbind(a.games, h.games)
    avg.touches=mean(c(h.touches, a.touches))
    this.per.touch.stat =colMeans(all.stats)/avg.touches
    this.per.touch.sd = apply(all.stats, MARGIN=2, FUN=sd)/avg.touches
    per.touch.stat = cbind(per.touch.stat, this.per.touch.stat)
    per.touch.sd = cbind(per.touch.sd, this.per.touch.sd)
    
    message("Per touch stats compilation for this player: ", this.lineup[this.player], " number:", i, "completed")
    #pick the number of games played in simulated season data
    starters.gp = c(starters.gp,as.numeric(sample(games.played,1))) 
    
    #the first nine players play directly from their availability
    if (i < 10 ) {
      player.sim.gamelog.TF = vector(mode="numeric",length = 82)
      player.sim.gamelog.TF[sample(1:82, starters.gp[i])] = as.numeric(i)
      player.sim.gamelog.TFdf[i] = player.sim.gamelog.TF
    }else{ 
      player.sim.gamelog.TFdf[i] = vector(mode="numeric",length = 82)
      
      #players 10+ are considered deep bench and mostly player when better players are unavailable
    }
    }, error=function(e){})
  }
}

#shift the touch distribution up every time a top 9 player is unable to play in a game

for (i in 1:82){
  games.playing = player.sim.gamelog.TFdf[i,]
  held=1
  for (k in 1:length(player.sim.gamelog.TFdf)){
    if (games.playing[k] == 0 & k < 10){
      next
    }else{
      if (held<10){
      player.sim.gamelog.TFdf[i,k] = held
      held=held+1
      }
    }
  }
  for (l in 10:length(player.sim.gamelog.TFdf)){
    size=sample(c(1:25), 1)
    if (size == 1){
      player.sim.gamelog.TFdf[i,l] = as.numeric(l)
    }
  }
}

ball.starters.sim.stats = data.frame(matrix(nrow=18, ncol=0))
ball.starters.sim.sd = data.frame(matrix(nrow=18, ncol=0))

#transform the offensive levels touch number into actual stats using the per touch stats

for (i in 1:length(per.touch.stat)){
  tryCatch({
  ample.touch.ranges = c(0, sample.touch.ranges)
  player.sim.gamelog.touches[i] = ample.touch.ranges[(player.sim.gamelog.TFdf[,i]+1)]
  sample.list=mean((player.sim.gamelog.touches[,i])[which(player.sim.gamelog.touches[,i]>0)])
  per.game.avg = per.touch.stat[i] * sample.list
  per.game.sd = per.touch.sd[i] * sample.list
  ball.starters.sim.stats = cbind(ball.starters.sim.stats, as.data.frame(per.game.avg))
  ball.starters.sim.sd = cbind(ball.starters.sim.sd, as.data.frame(per.game.sd))
  }, error=function(e){})
}

team.total.stats = data.frame(matrix(nrow = 18, ncol = 0))

for (i in 1:length(ball.starters.sim.stats)){
  team.total.stats = cbind(team.total.stats, (ball.starters.sim.stats[i] * as.numeric(starters.gp[i])))
}

team.avg.stats = rowSums(team.total.stats)/82

#sim 2023-2024 Magic = FGM:40.954748, FGA:90.412735, FG%:0.4529754, 3PM:14.1628321, 3PA:39.7791150,
  #3P%:0.3560369, FTM:18.3166863, FTA:24.2859413, FT%:0.7542094, ORB:9.3705479, DRB:  32.0935348, TRB: 41.4640826,
  #AST: 28.0112592, STL:9.0147976, BLK:4.8095072, TOV:14.2896383, PF: 21.5426214, PTS: 114.3890139                   

#true 2023-2024 Magic = FGM:40.5, FGA:84.9, FG%:0.476, 3PM:11.0, 3PA:31.3,
  #3P%:0.352, FTM:18.5, FTA:24.4, FT%:0.759, ORB:10.5, DRB:  31.8, TRB: 42.3,
  #AST: 24.7, STL:8.2, BLK:5.2, TOV:14.7, PF: 19.7, PTS: 110.5   

#sim 2023-2024 HAWKS = FGM:43.607241, FGA:97.7679059, FG%:0.4460282, 3PM:16.1413969, 3PA:45.6801798,
#3P%:0.3533567, FTM:20.2251538, FTA:24.1052899, FT%:0.8390338, ORB:12.0126662, DRB:31.1905132, TRB: 43.2031794,
#AST: 27.7759979, STL:8.2243853, BLK:3.9457967, TOV:13.1308061, PF: 20.9849999, PTS: 123.5810335                   

#true 2023-2024 HAWKS = FGM:43.0, FGA:92.5, FG%:.465, 3PM:13.7, 3PA:37.7,
#3P%:.364, FTM:18.5, FTA:23.2, FT%:.797, ORB:12.5, DRB:32.2, TRB:44.7,
#AST:26.6, STL:7.5, BLK:4.5, TOV:13.5, PF: 18.6, PTS: 118.3   

team.avg.stats[3] = team.avg.stats[1]/team.avg.stats[2]
team.avg.stats[6] = team.avg.stats[4]/team.avg.stats[5]
team.avg.stats[9] = team.avg.stats[7]/team.avg.stats[8]

print(team.avg.stats)

#now create the simulated game log

total.sim.gamelog = data.frame(matrix(nrow = 82, ncol = 18))
colnames(total.sim.gamelog) =  row.names(team.total.stats)


for (k in 1:length(row.names(team.total.stats))){
  k.sum = vector(mode="numeric", length = 82)
  for (l in 1:length(team.total.stats)){
    meandf = per.touch.stat[k,l]
    sddf = per.touch.sd[k,l]
    tryCatch({
    per.touch.guess = rnormTrunc(n = 82, mean = meandf, sd = sddf, min = 0, max = meandf+0.4*sddf)
    #use a restricted normal distribution to avoid both negative and inflated values
    }, error=function(e){})
    j.sum=player.sim.gamelog.touches[l]*(per.touch.guess)
    k.sum = k.sum + j.sum  
  }
  total.sim.gamelog[,k] = k.sum
}

#round to whole numbers, then calculate the decimals
total.sim.gamelog[-c(3,6,9)] = round(total.sim.gamelog[-c(3,6,9)],0)

total.sim.gamelog[3] = total.sim.gamelog[1]/total.sim.gamelog[2]
total.sim.gamelog[6] = total.sim.gamelog[4]/total.sim.gamelog[5]
total.sim.gamelog[9] = total.sim.gamelog[7]/total.sim.gamelog[8]

# now estimate the team's number of wins

#simulate opponent team performance randomly
sim.opp.stats=data.frame(matrix(ncol=25,nrow=0))
colnames(sim.opp.stats)= c("GAME_ID","TEAM_ID","TEAM_NAME","TEAM_ABBREVIATION","TEAM_CITY","MIN",
                           "FGM","FGA","FG_PCT","FG3M","FG3A","FG3_PCT", "FTM","FTA" ,"FT_PCT" ,"OREB" ,
                           "DREB","REB","AST","STL","BLK","TO","PF","PTS","PLUS_MINUS")
nba.teams.ids = nba_teams()$team_id

for (i in (1:82)){
  message("Starting retrieval of game ", i)
  tryCatch({
    team.id = sample(nba.teams.ids,1)
    game.id = try_function((nba_teamgamelog(season=2023, team_id = team.id)$TeamGameLog$Game_ID)[i])
    opp.stat= try_function(as.data.frame(nba_boxscoretraditionalv3(game_id = game.id, season=2023)[sample(3:4,1)])[-(1:3)])
    sim.opp.stats[i,] = opp.stat
  }, error=function(e){})
}

#create the training data from 2023 season data
training.set=data.frame(matrix(ncol=length(data.frame(nba_teamgamelog(season=2023, team_id = team.id))),nrow=0))
colnames(training.set)=c("GAME_ID","TEAM_ID","TEAM_NAME","TEAM_ABBREVIATION","TEAM_CITY","MIN",
                         "FGM","FGA","FG_PCT","FG3M","FG3A","FG3_PCT", "FTM","FTA" ,"FT_PCT" ,"OREB" ,
                         "DREB","REB","AST","STL","BLK","TO","PF","PTS","PLUS_MINUS")

for (i in 1:length(nba.teams.ids)){
  tryCatch({
    j = paste0((nba.teams.ids)[i])
    team.stats= try_function(data.frame(nba_teamgamelog(season=2023, team_id = j)))
    training.set= rbind(team.stats,training.set)
  }, error=function(e){})
}

training.set=sort_by.data.frame(training.set, training.set$TeamGameLog.Game_ID)

unique.game.id=sort(unique(training.set$TeamGameLog.Game_ID))

team.id=sample(nba.teams.ids,1)
WL.set=data.frame(matrix(ncol=2*length(data.frame(nba_teamgamelog(season=2023, team_id = team.id))),nrow=0))

for (i in unique.game.id){
  games=training.set[training.set$TeamGameLog.Game_ID== i ,]
  games[1,]
  gamie=cbind(team1=games[1,], team2=games[2,])
  WL.set = rbind(gamie, WL.set)
}

#format the training data correctly
WL.set=WL.set[-c(2,29,32)]

WL.set=na.omit(WL.set)
WL.set$team1.TeamGameLog.WL = ifelse(WL.set$team1.TeamGameLog.WL=="W", 1, 0)
WL.set$team1.TeamGameLog.WL = as.factor(WL.set$team1.TeamGameLog.WL)
WL.set = WL.set[-c(1,2,3,5,6,7,8,27,28,29,30,31,32,33)]

#format the simulated data correctly 
sim.opp.stats=sim.opp.stats[-c(1,2,3,4,5,6,25)]
fake.sim.opp.stats=(na.omit(rbind(sim.opp.stats,sim.opp.stats,sim.opp.stats,sim.opp.stats[1:10,])))
sim.all.games = cbind(total.sim.gamelog,fake.sim.opp.stats[1:82,])
colnames(sim.all.games)=colnames(WL.set[-c(1)])
sim.all.games=na.omit(sim.all.games)

WL.set[2:37]=data.matrix(lapply(WL.set[2:37],as.numeric))

#run both models and print their predictions 
NNmodel = neuralnet(team1.TeamGameLog.WL~.,
                    data=WL.set,
                    hidden=c(30, 16, 6),
                    linear.output = F,
                    algorithm = 'slr'
)

nn.predict=predict(NNmodel, sim.all.games)

prediction.number = round(sum(nn.predict[,1]))

print(paste("Neural Net result: ", prediction.number))

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

elastic.predict= predict(elastic.model, sim.all.games)
print(paste0("Elastic Net result: ", sum(elastic.predict == 1)))

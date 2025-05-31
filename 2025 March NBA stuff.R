library(hoopR)
library(ggplot2)
library(corrplot)
library(tidyr)
library(tidyverse)
library(factoextra)
library(rdist)
library(nortest)
library(ggpubr)
library(rcompanion)


all_games=nba_leaguegamefinder(player_or_team = "T", season = "2023-24")
each_game = unique(all_games$LeagueGameFinderResults[5])

each_game=unlist(each_game[1])

megadf_col= c("Id", "PTS", "Pos", "Mins", 
              "Touches","Bench", "FTA","FTM", 
              "PF","AST", "FGA", "FGM",
              "DEFR", "OFFR", "TOTR","STL",
              "BLK","TO")

megadf=data.frame(matrix(ncol=length(megadf_col),nrow=0))
colnames(megadf) = megadf_col

for (i in 1:length(each_game)){
  tryCatch({
  pasty = paste0(each_game[i])
  a=nba_boxscoreplayertrackv3(game_id = pasty)
  ahome=data.frame(a$home_team_player_player_track)
  aaway=data.frame(a$away_team_player_player_track)
  b=nba_boxscoretraditionalv3(game_id = pasty)
  bhome= data.frame(b$home_team_player_traditional)
  baway= data.frame(b$away_team_player_traditional)
  
    away_bench = baway$position==""
    home_bench = bhome$position==""
    
    
    c= data.frame(ahome$person_id,
                bhome$points,
                bhome$position, 
                ahome$minutes, 
                ahome$touches, 
                home_bench, 
                bhome$free_throws_attempted,
                bhome$free_throws_made,
                bhome$fouls_personal,
                bhome$assists,
                bhome$field_goals_attempted,
                bhome$field_goals_made,
                bhome$rebounds_defensive,
                bhome$rebounds_offensive,
                bhome$rebounds_total,
                bhome$steals,
                bhome$blocks,
                bhome$turnovers)
  
    megadf_col= c("Id", "PTS", "Pos", "Mins", 
                "Touches","Bench", "FTA","FTM", 
                "PF","AST", "FGA", "FGM",
                "DEFR", "OFFR", "TOTR","STL",
                "BLK","TO")
  
  colnames(c)=megadf_col
  
  
  d= data.frame(aaway$person_id,
                baway$points,
                baway$position, 
                aaway$minutes, 
                aaway$touches, 
                away_bench, 
                baway$free_throws_attempted,
                baway$free_throws_made,
                baway$fouls_personal,
                baway$assists,
                baway$field_goals_attempted,
                baway$field_goals_made,
                baway$rebounds_defensive,
                baway$rebounds_offensive,
                baway$rebounds_total,
                baway$steals,
                baway$blocks,
                baway$turnovers)
  
  colnames(d)=megadf_col
  
  megadf=rbind(megadf,c)
  megadf=rbind(megadf,d)
  }, error=function(e){})
}


# a= data.frame(nba_boxscoreplayertrackv3(game_id = "0022200021"))
# 
# j=nba_boxscoreplayertrackv3(game_id = "0022301188")
# j$home_team_player_player_track
# 
# h=(nba_boxscoretraditionalv3(game_id = "0022301188"))
# h = h$home_team_player_traditional
# h$position
# 
# b=data.frame(nba_boxscoretraditionalv3(game_id = "0022200021"))
# 
# away_bench = b$away_team_player_traditional.position==""
# home_bench = b$away_team_player_traditional.position==""
# 
# c= data.frame(a$home_team_player_player_track.person_id,
#               b$home_team_player_traditional.points,
#               b$home_team_player_traditional.position, 
#               a$home_team_player_player_track.minutes, 
#              a$home_team_player_player_track.touches, 
#              home_bench, 
#              b$home_team_player_traditional.free_throws_attempted,
#              b$home_team_player_traditional.free_throws_made,
#              b$home_team_player_traditional.fouls_personal,
#              b$home_team_player_traditional.assists,
#              b$home_team_player_traditional.field_goals_attempted,
#              b$home_team_player_traditional.field_goals_made,
#              b$home_team_player_traditional.rebounds_defensive,
#              b$home_team_player_traditional.rebounds_offensive,
#              b$home_team_player_traditional.rebounds_total,
#              b$home_team_player_traditional.steals,
#              b$home_team_player_traditional.blocks,
#              b$home_team_player_traditional.turnovers)
# 
# megadf_col= c("Id", "PTS", "Pos", "Mins", 
#               "Touches","Bench", "FTA","FTM", 
#               "PF","AST", "FGA", "FGM",
#               "DEFR", "OFFR", "TOTR","STL",
#               "BLK","TO")
# 
# 
# colnames(c)=megadf_col
# 
# d= data.frame(a$away_team_player_player_track.person_id,
#                  b$away_team_player_traditional.points,
#                  b$away_team_player_traditional.position, 
#                  a$away_team_player_player_track.minutes, 
#                  a$away_team_player_player_track.touches, 
#                  away_bench, 
#                  b$away_team_player_traditional.free_throws_attempted,
#                  b$away_team_player_traditional.free_throws_made,
#                  b$away_team_player_traditional.fouls_personal,
#                  b$away_team_player_traditional.assists,
#                  b$away_team_player_traditional.field_goals_attempted,
#                  b$away_team_player_traditional.field_goals_made,
#                  b$away_team_player_traditional.rebounds_defensive,
#                  b$away_team_player_traditional.rebounds_offensive,
#                  b$away_team_player_traditional.rebounds_total,
#                  b$away_team_player_traditional.steals,
#                  b$away_team_player_traditional.blocks,
#                  b$away_team_player_traditional.turnovers)
# 
# colnames(d)=megadf_col
# 
# megadf=rbind(c,d)



min_to_dec = function(x){
  listy=list()
  for (i in 1:length(x)) {
    x1=x[i]
    y=strsplit(x1,":")
    z=unlist(y[1])
    z1=as.numeric(z[1])+(as.numeric(z[2])/60)
    listy[i]=z1
  }
  return(unlist(listy))
}

megadf$Mins=min_to_dec(megadf$Mins)

megadf$tch.min = ifelse(as.numeric(megadf$Mins)==0, 0, as.numeric(megadf$Touches)/as.numeric(megadf$Mins))
megadf$min.tch = ifelse(as.numeric(megadf$Touches)==0, 0, as.numeric(megadf$Mins)/as.numeric(megadf$Touches))

megadf=subset(megadf, !(megadf$Touches==0 & megadf$PTS>2))

head(megadf)

summary(megadf)

cor(megadf[-c(1,3)])

plot(megadf$PTS~megadf$tch.min)
plot(megadf$PTS~megadf$min.tch)

ggplot(megadf, aes(min.tch, PTS))+
  geom_point(aes(color=factor(Bench), shape=factor(Pos)))

ggplot(megadf, aes(tch.min, PTS))+
  geom_point(aes(color=factor(Bench), shape=factor(Pos)))

ggplot(megadf[megadf$Bench==TRUE,], aes(tch.min, PTS))+
  geom_point(aes(color=factor(Bench), shape=factor(Pos)))

ggplot(megadf[megadf$Bench==FALSE,], aes(tch.min, PTS))+
  geom_point(aes(color=factor(Bench), shape=factor(Pos)))

ggplot((megadf[megadf$Bench==TRUE,]), aes(Touches, PTS))+
  geom_point(aes(color=factor(Bench), shape=factor(Pos)))

ggplot((megadf[megadf$Bench==FALSE,]), aes(Touches, PTS))+
  geom_point(aes(color=factor(Bench), shape=factor(Pos)))

ggplot((megadf[megadf$Bench==FALSE,]), aes((Touches)))+
  geom_histogram()

#investigate normality

ad.test(megadf$Touches[megadf$Bench==FALSE])
ks.test(megadf$Touches[megadf$Bench==FALSE], rnorm(n=length(megadf$Touches[megadf$Bench==FALSE]), 
                                                   mean=mean(megadf$Touches[megadf$Bench==FALSE]), 
                                                   sd=sd(megadf$Touches[megadf$Bench==FALSE])))
ggqqplot(megadf$Touches[megadf$Bench==FALSE])
transformTukey(megadf$Touches[megadf$Bench==FALSE], statistic=2)

ggplot((megadf[megadf$Bench==FALSE,]), aes((Touches^(0.65))))+
  geom_histogram()

norm.mean=mean((megadf$Touches[megadf$Bench==FALSE])^0.65)
norm.sd=sd((megadf$Touches[megadf$Bench==FALSE])^0.65)

bottom.touch = qnorm(0.000001)*norm.sd + norm.mean
(qnorm(0.99999)*norm.sd + norm.mean - bottom.touch)^(1/0.65)
(qnorm(.80)*norm.sd + norm.mean - bottom.touch)^(1/0.65)
(qnorm(.60)*norm.sd + norm.mean - bottom.touch)^(1/0.65)
(qnorm(.40)*norm.sd + norm.mean- bottom.touch)^(1/0.65)
(qnorm(.20)*norm.sd + norm.mean- bottom.touch)^(1/0.65)
(qnorm(0.000001)*norm.sd + norm.mean - bottom.touch)^(1/0.65)
S
length(megadf$Touches[megadf$Bench==FALSE])

sort(megadf$Touches[megadf$Bench==FALSE], decreasing=TRUE)[2415]

# reject method using normal distribution

ggplot((megadf[megadf$Bench==TRUE,]), aes(Touches))+
  geom_histogram()
ggqqplot(megadf$Touches[megadf$Bench==TRUE])

summary((megadf$Touches[megadf$Bench==FALSE]))
summary((megadf$Touches[megadf$Bench==TRUE]))

ggplot((megadf[megadf$Bench==FALSE,]), aes(PTS))+
  geom_histogram()

ggplot((megadf[megadf$Bench==TRUE,]), aes(PTS))+
  geom_histogram()


pcacheck=princomp(megadf[-c(1,3)])
summary(pcacheck)
fviz_cos2(pcacheck, choice = "var")
fviz_pca_var(pcacheck, col.var = "black")

fullmodelglm=glm(PTS ~ Pos + Mins + Touches + Bench + tch.min + min.tch , data=megadf)
fullmodellm=lm(PTS ~ Pos + Mins + Touches + Bench + tch.min + min.tch , data=megadf)

summary(step(fullmodelglm, direction = "both"))
summary(step(fullmodellm, direction = "both"))

step(fullmodelglm, direction = "both", k = log(nrow(megadf)))
step(fullmodellm, direction = "both", k = log(nrow(megadf)))


Kmeansmegadf=megadf[-c(1, 3, 6)]


start=kmeans((Kmeansmegadf[megadf$Bench==FALSE,]), centers=6)
bench=kmeans((Kmeansmegadf[megadf$Bench==TRUE,]), centers=11)


start$centers[,3]
bench$centers[,3]

# Taurean Prince Stats

player.row = index$PlayerIndex.PERSON_ID[grep("Taurean Prince",index$Player.Names,ignore.case = TRUE)]

player.idnum = player.row

games.played=as.vector(nba_playercareerstats(player_id = player.idnum)$SeasonTotalsRegularSeason$GP)

player.gamelog=nba_playergamelog(player_id = player.idnum, season = 2023)$PlayerGameLog

away.games=player.gamelog$Game_ID[grepl("@", player.gamelog$MATCHUP, fixed=TRUE)]
home.games=player.gamelog$Game_ID[grepl("vs", player.gamelog$MATCHUP, fixed=TRUE)]

h.bench.games=data.frame(matrix(nrow=0, ncol = 17))
h.start.games=data.frame(matrix(nrow=0, ncol = 17))
h.starter.touches=c()
h.bench.touches=c()

for (i in 1:length(home.games)){
  j=home.games[i]
  retrieved.home=nba_boxscoretraditionalv3(game_id = j)$home_team_player_traditional
  h.player.stat=retrieved.home[which(retrieved.home$person_id == as.numeric(player.idnum)), c(18:35)]
  benchstart=retrieved.home$position[which(retrieved.home$person_id == as.numeric(player.idnum))]
  if(benchstart== ""){
    h.bench.track=nba_boxscoreplayertrackv3(game_id = j)$home_team_player_player_track
    h.bench.touch=h.bench.track$touches[(h.bench.track$person_id == as.numeric(player.idnum))]
    h.bench.touches = c(h.bench.touches, h.bench.touch)
    h.bench.games = rbind(h.bench.games, as.data.frame(h.player.stat))
  }
  else if(benchstart!= ""){
    h.starter.track=nba_boxscoreplayertrackv3(game_id = j)$home_team_player_player_track
    h.starter.touch=h.starter.track$touches[h.starter.track$person_id == as.numeric(player.idnum)]
    #print(h.starter.touch)
    h.starter.touches = c(h.starter.touches, h.starter.touch)
    h.start.games = rbind(h.start.games, as.data.frame(h.player.stat))
  }
}

a.starter.touches=c()
a.bench.touches=c()
a.bench.games=data.frame(matrix(nrow=0, ncol = 17))
a.start.games=data.frame(matrix(nrow=0, ncol = 17))

for (i in 1:length(away.games)){
  j=away.games[i]
  retrieved.away=nba_boxscoretraditionalv3(game_id = j)$away_team_player_traditional
  a.player.stat=retrieved.away[which(retrieved.away$person_id == as.numeric(player.idnum)), c(18:35)]
  benchstart=retrieved.away$position[which(retrieved.away$person_id == as.numeric(player.idnum))]
  if(benchstart== ""){
    a.bench.track=nba_boxscoreplayertrackv3(game_id = j)$away_team_player_player_track
    a.bench.touch=a.bench.track$touches[(a.bench.track$person_id == as.numeric(player.idnum))]
    a.bench.touches = c(a.bench.touches, a.bench.touch)
    a.bench.games = rbind(a.bench.games, as.data.frame(a.player.stat))
  }
  else if(benchstart!= ""){
    a.starter.track=nba_boxscoreplayertrackv3(game_id = j)$away_team_player_player_track
    a.starter.touch=a.starter.track$touches[(a.starter.track$person_id == as.numeric(player.idnum))]
    #print(a.starter.touch)
    a.starter.touches = c(a.starter.touches, a.starter.touch)
    a.start.games = rbind(a.start.games, as.data.frame(a.player.stat))
  }
}

start.stats=rbind(a.start.games, h.start.games)
bench.stats= rbind(a.bench.games, a.bench.games)

per.touch.bench
bench.stats

avg.starter.touches=mean(c(h.starter.touches, a.starter.touches))
bench.touches = ifelse(is.na(mean(h.bench.touches, a.bench.touch)), 0, mean(h.bench.touches, a.bench.touch))

per.touch.start=colMeans(start.stats)/avg.starter.touches
per.touch.bench=ifelse(bench.touches==0, 0, colMeans(bench.stats)/bench.touches)

per.touch.bench=colMeans(bench.stats)/bench.touches
per.touch.start

#apply(start.stats, MARGIN=2, FUN=sd)/avg.starter.touches

per.touch.start[3] = per.touch.start[1]/per.touch.start[2]
per.touch.start[6] = per.touch.start[4]/per.touch.start[5]
per.touch.start[9] = per.touch.start[7]/per.touch.start[8]

per.touch.bench[3] = per.touch.bench[1]/per.touch.bench[2]
per.touch.bench[6] = per.touch.bench[4]/per.touch.bench[5]
per.touch.bench[9] = per.touch.bench[7]/per.touch.bench[8]

ptb=as.vector(per.touch.bench)
pts=as.vector(per.touch.start)

t.test(ptb, pts, mu = 0)
t.test(ptb, pts, paired=TRUE)


starters.gp = c()
starters.gp = c(sample(games.played,1), starters.gp)
all.starters.per.touch = data.frame(matrix(nrow=0, ncol=17))

per.game.avg = per.touch.start * sample(74:94,1) #first option per game average
per.game.avg[3] = per.game.avg[1]/per.game.avg[2]
per.game.avg[6] = per.game.avg[4]/per.game.avg[5]
per.game.avg[9] = per.game.avg[7]/per.game.avg[8]
per.game.avg
           
`sim 2023-2024 Magic` = data.frame("FGM"=40.954748, "FGA"=90.412735, "FG%"=0.4529754, "3PM"=14.1628321, "3PA"=39.7791150,
"3P%"=0.3560369, "FTM"=18.3166863, "FTA"=24.2859413, "FT%"=0.7542094, "ORB"=9.3705479, "DRB"=  32.0935348, "TRB"= 41.4640826,
"AST"= 28.0112592, "STL"=9.0147976, "BLK"=4.8095072, "TOV"=14.2896383, "PF"= 21.5426214, "PTS"= 114.3890139)                   

`true 2023-2024 Magic` = data.frame("FGM"=40.5, "FGA"=84.9, "FG%"=0.476, "3PM"=11.0, "3PA"=31.3,
"3P%"=0.352, "FTM"=18.5, "FTA"=24.4, "FT%"=0.759, "ORB"=10.5, "DRB"=  31.8, "TRB"= 42.3,
"AST"= 24.7, "STL"=8.2, "BLK"=5.2, "TOV"=14.7, "PF"= 19.7, "PTS"= 110.5)   

`sim 2023-2024 HAWKS` =data.frame("FGM"=43.607241, "FGA"=97.7679059, "FG%"=0.4460282, "3PM"=16.1413969, "3PA"=45.6801798,
"3P%"=0.3533567, "FTM"=20.2251538, "FTA"=24.1052899, "FT%"=0.8390338, "ORB"=12.0126662, "DRB"=31.1905132, "TRB"= 43.2031794,
"AST"= 27.7759979, "STL"=8.2243853, "BLK"=3.9457967, "TOV"=13.1308061, "PF"= 20.9849999, "PTS"= 123.5810335)                   

`true 2023-2024 HAWKS` = data.frame("FGM"=43.0, "FGA"=92.5, "FG%"=.465, "3PM"=13.7, "3PA"=37.7,
"3P%"=.364, "FTM"=18.5, "FTA"=23.2, "FT%"=.797, "ORB"=12.5, "DRB"=32.2, "TRB"=44.7,
"AST"=26.6, "STL"=7.5, "BLK"=4.5, "TOV"=13.5, "PF"= 18.6, "PTS"= 118.3)



xy=data.frame()

for (i in names(`sim 2023-2024 Magic`)){
  xy = data.frame(stat =c(`sim 2023-2024 Magic`[,i], `sim 2023-2024 HAWKS`[,i],
                  `true 2023-2024 Magic`[,i],`true 2023-2024 HAWKS`[,i]),
                 sim.true =c(0,0,1,1))
  #xy$sim.true = as.factor(xy$sim.true)
  xy$stat = sapply(xy$stat, as.numeric)
  assign(paste0(i), xy)
}

for (i in names(`sim 2023-2024 Magic`)){
  lol = eval(as.name(i))$stat[1:2]
  ha = eval(as.name(i))$stat[3:4]
  lmao = t.test(lol,ha, paired = T)
  print(c(i, lmao$p.value))
}


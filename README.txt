A short explanation of each file:
2025 March NBA stuff.R is the data exploration and summary. Generates the graphs of several NBA statistics and the k-means of player touches in the 2023-2024 season.

2025 March NBA stuff roster calculator.R encompasses the entire simulation. First, the roster is constructed, then player data is used to estimate the team's performance statistics and total wins.

2025 March stuff season outcome calculator.R is an older version of code used to test randomforest and XGBoost for predicting the team's total wins after the team's performance statistics were simulated . (unsucessful)

2025 May NBA stuff testing XGBoost.R tests XGBoost with varying parameters trained on 2023 NBA data to predict 2024 teams' success. (unsuccessful)

2025 June NBA stuff testing Neural N.R tests multiple models and methods of variable selection (Principal Component Analysis, Linear Discriminat Analysis, forward selection, Generalized Linear Model, Neural Nets, ridge, lasso, elastic net) trained on 2023 NBA data to predict 2024 teams' success.

2025 June NBA stuff paired testing.R tests the differences in the means between the simulated and actual team stats using the paired t.test

Project Description:

After conducting initial exploratory analysis, I used k-means to stratify the average player touches into 15 ranges. I assigned 12 players to options 1-12, with the first option assigned the most touches in the game, the second option assigned the second most options, so on and so forth. A number was random selected from within the corresponding range and assigned to each player. Player's names were transformed to simple letters for searching ease. With HoopR, I extracted each player's "Games Played" statistic over their career, then randomly selected a number from the pool as the player's number of games played in the simulation.

Due to API limits, a try_function was created and used to try and pause hoopR functions when the API blocked retrieval. Using hoopR to extract NBA stats, the player's touches in every game during the 2023-2024 season was compiled then averaged to determine the mean touch per game for each player, as well as boxscore statistics. The mean of each boxscore statistic was collected for each player, then divided by the player's mean touches, giving the stat per touch. This information is stored in "per.touch.stat" and the accompanying standard deviation is stored in "per.touch.sd"

For players who were assigned option 1-9, a vector of length 82 was created. Using the sampled games played number, random positions within the vector were assigned the option of their assigned option, representing a game played, while all others were assigned "0", representing a game not played. This was done to provide a mock up of player availability. The vectors were then assigned to empty dataframe "player.sim.gamelog.TFdf".

For players who were assigned option 10 and onwards, they were assigned an 82-length vector of "0" by default. Within the dataframe "player.sim.gamelog.TFdf", each game was represented by each row number and each player option was represented by the column number. When any player option 1-9 was "0" within a row, the options would "shift", a number lower than the column number might be assigned to the column in that row, representing bench players more when other players are injured. Then with a 4% chance random sample, when successful, the number assigned to the column would match the column number. 

Then for each player, represented by each column in the "player.sim.gamelog.TFdf" dataframe, assign the touch output for each game, represented by each row, retrieved from the vector containing assigned touches for each option and the dataframe containing each player's per-touch output, "per.touch.stat" and "per.touch.sd". The resultant vector of statistics and standard deviations were assigned to the "ball.starters.sim.stats" and "ball.starters.sim.sd" data frames.

The team's total statistic output was assigned to dataframe "team.total.stats" and the team's average output throughout the season was determined dividing each rowSum by 82 then stored in "team.avg.stats". The accuracy of the model was judged by comparing the output in "team.avg.stats" to the team's actual performance in the following 2023-2024 season using the paired t-test. 

To determine the team's success, a simulated team boxscore for each game was created by creating a sample pool using a truncated normal distribution, with the maximum limited to the mean + 40% of the standard deviation and the minimum restricted to 0 for each statistical category. Oppenent's boxscore was created by randomly selecting the boxscore of a team when they played the same game number. 

The neural net was trained on 2023 NBA data using the home and away boxscore of every game and assign a success or failure based on the home team winning or losing. 

The LDA, XGBoost, ridge, lasso and elastic learning models were considered but the results produced were insufficently accurate. The accuracy of each model was determined by comparing the estimated wins of each of the NBA teams to their actual win count. In a 2-point coordinate system, if the estimated wins are considered the X-coordinate and the actual win count are considered the Y-coordinate, a perfect model would produce a line with a slope of 1 and a y-intercept of 0. Thus the accuracy of the model would be able to be determined by the absolute value of the area between the line of slope 0 and intercept 0 and the line of best fit of the points produced using the previous method (limit of 0,82 on both axis) or the average absolute distance of each point from the ideal line. 

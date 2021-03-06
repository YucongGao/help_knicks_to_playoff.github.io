# help_knicks_to_playoff.github.io



## Group Members
Yucong Gao, yg2834 <br>
Xuehan Yang, xy2517 <br>
Hao Zheng, hz2772 <br>
Pei Hsin Lin, pl2811 <br>


## Motivation

As Knicks fans, we are witnessing probably the greatest revolution ever on the court, which is called "Small ball era". Teams tend to use small and faster players instead of traditional giants to accelerate moving speed and improve shooting efficiency. Last year, the Knicks returned to playoff season after eight years, which brought great joy to the fans. In order to make this performance long-lasting, we feel obligated to research the key variables that contribute to the winning of a game, to help Knicks maintain existing strengths and make up for the disadvantages. The most obvious feature of the "small ball era" is the rise of 3 point shot attempt. In this way, we focus on three-point related variables along with other factors to conduct our analysis. Due to the huge volume of data released by NBA, our project seems promising and practical.

## Intended Final Product 

Our intended final product is a winning strategy for New York Knicks, based on the analysis of the relation between game winning and some influencing factors like the proportion of three points attempts, location(home or road), the number of fouls per game and so on, .

## Data Source
Our datasets mainly come from the following two website:

https://www.basketball-reference.com/ <br>
https://www.nba.com/stats/ <br>

These websites contains up-to-date NBA box scores for each team per game and Player shooting data log which we can leverage in our data analysis. As there is no direct API for the datasets we need, we plan to use scrapping to get our data.

## Analysis/Visualizations

### Data Analysis 

Our analysis contains three parts, all of which are based on data from last 5 NBA regular seasons, from 2015-2016 season to 2020-2021 season.

The First analysis is to develop regression models to quantify the relation between game winning with potential factors. The potential factors selected are related with three points shooting. The dependent variables are expected to be game win or lose, 1 if win and 0 if lose. The potential independent variables are position(home, road), the proportion of three points shooting attempts, three points shooting rate, the number of AST, the number of TOV and possessions.

The second analysis is descriptive analysis, focusing on how to improve three point shooting. Variables such as Position in the team(PG, SG), position on the court(corner or not), number of dribbles before shot, time on the court and the position of the passers are taken into consideration .

The third analysis gives suggestions to a specific NBA team, New York Knicks. The advice mainly cover three point shooting strategy on the court and three points training strategy according to the results from the first two analysis.
 
### Data Visualization

* A line plot of the average team three point shooting attempts trend over time<br>
* A parallel coordinates of three point attempt rate trend overtime breakdown by playoff teams and other teams<br>
* A hex plot that shows the three points shooting rate of a team on different position<br> 
* A panel plot shows characteristics difference between playoff teams and other teams<br>

## Challenges

* There is no direct API from NBA official website about shooting log data, how to use scrapper to get the data is a big challenge.

* Because the research object involves specific players, player status and on-site audience factors may affect player performance and match results, which may result in a decrease in prediction accuracy. 

* Choose a suitable statistical regression model to fit the data, and eliminate possible problems such as multicollinearity, heteroscedasticity, and autocorrelation.
 



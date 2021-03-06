# Understanding Fantasy Premier League Player Prices using Regression
Detailed analysis of FPL player prices for 2017-18

## Business Problem

Fantasy Premier League(FPL) is a virtual game which is based on the most popular game in the world, soccer. It is based on the English 1st division soccer league. In a nutshell, the games rules are as follows: You have a budget of 100 million pounds, and you get to select 15 players for your team. Their actions in real life games such as goals scored, assists, clean sheets(stopping the opponent from scoring) are all actions that yield points in the game. The players of the game are allowed 1 free transfer every week and can make additional ones in exchange for points. The premier league itself is a billion-dollar industry and has over 600 million viewers every season, spanning 150 countries. FPL is one way they bring in new viewers and retain current viewers. Players of the game are now watching not only their favorite teams play, but also the lesser performing teams as well. This directly leads to more sponsorships for the premier league, and as a result more revenue. The number of people playing the game has grown hugely, starting in 2002 and going on to get 2 million players in 2010 and over 6 million in 2019. it is imperative to the league that the game players keep playing. To keep this profitable base of their viewers happy, they need to optimize the game as best they can.<br>

The price of the soccer players in FPL is a highly debated topic. Some believe a player is underpriced and the others are convinced he is overpriced. To make things clearer, let’s look at an example. One of the best players in the league right now, Raheem Sterling, was priced this year at £12 million. It is key to note however that he is only selected by 30% of players of the game. If he was instead priced at £6m he would have been in more than 90% of teams. To make sure that the game is competitive and doesn’t get boring, FPL must come up with an algorithm that correctly sets the price for these players. There have been instances where people just stop playing the game if their favorite player is priced too high, or if a well performing player is priced low, and consequently ends up in every team. This is the question that we are looking to answer. What could be the various factors that go into the algorithm that the FPL uses to generate the player prices? How are each of these factors related to the price of a player? How far can we see into this black box of an algorithm?

## Data Description
To answer these questions, we needed a comprehensive data set. We got our dataset from Kaggle. The data was for the 2017-18 season. The data we got was inherently messy and we had to clean it up before we could use it. We removed null values, impractical outliers and redundant or irrelevant columns. The final data set then contained the following information for each soccer player who played for the 2017-18 season: <br>
● Name : Name of the soccer player <br>
● FPL value : The value of the soccer player in the game (in millions of pounds)<br>
• Market Value : The value of the soccer player in real life (in millions of pounds) <br>
• FPL points : The points that the soccer player got in the game<br>
• Page views : Number of Wikipedia page views for the player <br>
• Club : The soccer club that the player plays for<br>
Age : We segregated the players into 6 age brackets as per the recommended business standards. The reason for this categorization is that a player aged 18 and a player aged 19 is treated and priced in a similar fashion provided all other metrics of measurements are same. <br>
○ Category 1: 17 - 21 <br>
○ Category 2: 22 - 24 <br>
○ Category 3: 25 - 27<br>
○ Category 4: 28 - 31 <br>
○ Category 5: 32 - 33 <br>
○ Category 6: 34 - 38<br>
● Position : The position where the player plays on the soccer pitch<br>
● Position Category : We categorized all the players into 4 distinct categories- <br>
○ Category 1: Attackers (goal scorers)<br>
○ Category 2: Midfielders (goal creators) <br>
○ Category 3: Defenders (goal protectors)<br>
○ Category 4: Goalkeepers (last line of defense) <br>
● Region : Nationality of the player<br>
● New signing : Whether the player was signed from another club this year<br>
● FPL selection : Percentage of FPL players who have selected that player in their team <br>
● Big Club : Does the player play for a top 6 club? Manchester United F.C, Chelsea F.C, Manchester City F.C, Arsenal F.C, Liverpool F.C and Tottenham Hotspur F.C. If a player plays for any of these clubs, then the big club value for that player is marked as Boolean 1.<br>
● Club : The soccer club that the player plays for<br>
In the collected dataset, there are a total of 457 players and the average FPL value of these players is 5.447 million pounds. The maximum FPL value in the dataset is 12.5 million pounds and the minimum FPL value is 4 million pounds. The average market value of these players is 11.02 million pounds. The maximum market value of the players is 75 million pounds and the minimum market value is as low as 0.05 million pounds. The minimum age of player in the data set is 17 while the maximum age is 38. The median age of players is 27 years. Minimum FPL points for a player in the dataset are 0 while maximum are 264 points. The average FPL points scored by the players in dataset is 57.44 points. There is a total of 156 players from England, 28 from Spain, 25 from France, 20 from Netherlands, 18 from Belgium, 17 from Argentina and another 196 players from other countries. The maximum number of Wikipedia page views for a player is 7664 while the minimum number of page views for a player is 3. The average value of Wikipedia page views is 765.3.<br>

## Interpretations and conclusions
After performing multiple regression on this dataset, we recognized that only Big club, age, position, market value, FPL points and nationality of a player impacts his FPL price. Variables like FPL selection percentage, page views, new signing do not have such a significant impact on FPL price of a player. We hence did not consider these variables in our final model. Our final model equation is:<br>

Y(FPL value) = 4.77 + 0.131(Market value) + 0.002(FPL points) + 0.108(Age Category 2) + 0.411(Age Category 3) + 0.191(Age Category 4) + 0.161(Age Category 5) – 0.053(Age Category 6) – 0.387(Position Category 2) – 0.547(Position Category 3) – 0.514(Position Category 4) – 0.263(No Big Club) + 0.326(Non British Player) - 0.0627(Market Value * Age Category 2) – 0.0722(Market Value * Age Category 3) – 0.068(Market Value * Age Category 4) – 0.057(Market Value * Age Category 5) – 0.0004(Market Value * Age Category 6) + 0.008(FPL Points * Age Category 2) + 0.005(FPL Points * Age Category 3) + 0.007(FPL Points * Age Category 4) + 0.008(FPL Points * Age Category 5) + 0.010(FPL Points * Age Category 6) - 0.049(Market Value * Position Category 2) – 0.044(Market Value * Position Category 3) - 0.047(Market Value * Position Category 4) – 0.0005(FPL Points * Position Category 2) – 0.001(FPL Points * Position Category 3) – 0.005(FPL Points * Position Category 4) + 0.020(Market Value * No Big Club) -0.003(Market Value * No Big Club) + 0.0328(Market Value * Non British Player)<br>

Based on the above equation, we can derive the following relationships between FPL value and all the predictor variables:
● For a British center forward player, who is playing for a big club and is aged between 18-21, FPL value increases by 13.12% for every dollar increase in market value.<br>
● For a British center forward player, who is playing for a big club, is aged between 18-21 and has a market value of 0, FPL value increases by 0.2% for every unit increase in FPL points. Since it’s not possible for a player to have a market value of 0, this interpretation is meaningless.<br>
● For a British center forward player, having FPL points 0, who is playing for a big club and is aged between 22-24, the degree of change in FPL value is 6.27% less than that caused by a British center forward player, who is playing for a big club and is aged between 18-21 for every dollar increase in market value.<br>
● For a British center forward player, having FPL points 0, who is playing for a big club and is aged between 25-27, the degree of change in FPL value is 7.22% less than that caused by a British center forward player, who is playing for a big club and is aged between 18-21 for every dollar increase in market value.<br>
● For a British center forward player, having FPL points 0, who is playing for a big club and is aged between 28-31, the degree of change in FPL value is 6.88% less than that caused by a British center forward player, who is playing for a big club and is aged between 18-21 for every dollar increase in market value.<br>
● For a British center forward player, having FPL points 0, who is playing for a big club and is aged between 32-33, the degree of change in FPL value is 5.73% less than that caused by a British center forward player, who is playing for a big club and is aged between 18-21 for every dollar increase in market value.<br>
● For a British center forward player, having FPL points 0, who is playing for a big club and is aged 34 and above, the degree of change in FPL value is 0.04% less than that caused by a British center forward player, who is playing for a big club and is aged between 18-21 for every dollar increase in market value. <br>
● For a British mid-fielder, having FPL points 0, who is playing for a big club and is aged between 18-21, the degree of change in FPL value is 4.97% less than that caused by a British center forward player having similar characteristics for every dollar increase in market value.<br>
● For a British defender, having FPL points 0, who is playing for a big club and is aged between 18-21, the degree of change in FPL value is 4.42% less than that caused by a British center forward player having similar characteristics for every dollar increase in market value.<br>
● For a British goalkeeper, having FPL points 0, who is playing for a big club and is aged between 18-21, the degree of change in FPL value is 4.70% less than that caused by a British center forward player having similar characteristics for every dollar increase in market value.<br>
● For a British center forward player, having FPL points 0, who is not playing for a big club and is aged between 18-21, the degree of change in FPL value is 2.09% more than that caused by a British center forward player, who is playing for a big club and has similar characteristics for every dollar increase in market value.<br>
● For a British center forward player, having 0 FPL points, who is playing for a big club and is aged between 18-21, the degree of change in FPL value is 3.28% more than that caused by a center forward player, who is not British and has similar characteristics for every dollar increase in market value.<br>

## Conclusion
From our analysis we can conclude that age of a player, his market value, FPL points scored in the season, nationality and club impact his FPL value. From these insights, we can gain a clearer understanding about how FPL is pricing players within the game. These insights could be used to further investigate the rising debate on whether a player is underpriced or overpriced within the game. Subsequently, whether the factors influencing the FPL price of a player are justified or need to be modified can also be established.<br>

## Limitations
● Data set is restricted to FPL season 2017-18. A wider data set with more variables and across more seasons would help us get an even deeper understanding of the variables which contribute to the FPL value. <br>
● In our model, we have restricted the nationality of a player as either British or non-British. Our interactions might get influenced by factors like if a player is French versus a player who is from Spain or Brazil since their skillset levels might be different.<br>
● With our model, we are only establishing a relationship between FPL value and dependent variables like age, position, nationality, club, market value and FPL points. Since we have got a high R2 from this model, we could probably further increase the scope in order to predict the FPL value of a player.<br>

## Contributors
Sampada Sathe, Rishab Prashanth, Shobhit Mishra, Yuxuan Wang

# Data and References
https://www.kaggle.com/karangadiya/fifa19

```python
import requests
import pandas as pd
```


```python
# box scores by game - 2020-2021 season
all_game_url = "https://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision="
headers = {
    'Connection':'keep-alive', 
    'Accept':'application/json, text/plain, */*', 
    'x-nba-stats-token': 'true', 
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36', 
    'x-nba-stats-origin': 'stats', 
    'Sec-Fetch-Site': 'same-origin', 
    'Sec-Fetch-Mode': 'cors', 
    'Referer': 'https://www.nba.com/', 
    'Accept-Encoding': 'gzip, deflate, br', 
    'Accept-Language': 'en-US,en;q=0.9', 
}

response_1 = requests.get(url = all_game_url, headers = headers).json()
data = response_1['resultSets'][0]['rowSet']
col_names = response_1['resultSets'][0]['headers']
last_season_df = pd.DataFrame(data, columns = col_names)
```


```python
box_score_urls = []
seasons = ["2020-21", "2019-20", "2018-19", "2017-18", "2016-17"]

for i in seasons:
    url = "https://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision=".format(season = i)
    response_boxscores = requests.get(url = url, headers = headers).json()
    data = response_boxscores['resultSets'][0]['rowSet']
    col_names = response_boxscores['resultSets'][0]['headers']
    boxscore_df = pd.DataFrame(data, columns = col_names)
    boxscore_df.to_csv("box_score_advanced_{}.csv".format(i), index = False)

```


```python
# team shooting log data
knicks_url = "https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=155&CFPARAMS=2021-22&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID=0&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=1610612752&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID="
headers = {
    'Connection':'keep-alive', 
    'Accept':'application/json, text/plain, */*', 
    'x-nba-stats-token': 'true', 
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36', 
    'x-nba-stats-origin': 'stats', 
    'Sec-Fetch-Site': 'same-origin', 
    'Sec-Fetch-Mode': 'cors', 
    'Referer': 'https://www.nba.com/', 
    'Accept-Encoding': 'gzip, deflate, br', 
    'Accept-Language': 'en-US,en;q=0.9', 
}
response = requests.get(url = knicks_url, headers = headers).json()
knicks_info = response['resultSets'][0]
row_sets = response['resultSets'][0]['rowSet']
knicks_shot_log = pd.DataFrame(row_sets, columns = knicks_info['headers'])
knicks_shot_log.to_csv("knicks_shot_log_21_22.csv", index = False)
```

data\_scrapping
================

<style type="text/css">

h1.title {
  text-align: center;
}

</style>

# Using function to scrap data from NBA.stats

## Scrapping function

use function to get data - define a function to scrap data from NBA.stat

``` r
scrapping_data = function(url) {
  headers = headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/95.0.4638.69 Safari/537.36', 
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9')
  response = GET(url, add_headers(headers))
  data = fromJSON(content(response, as = "text"))
  df = data.frame(data$resultSets$rowSet[[1]], stringAsFactors = FALSE)
  names(df) = tolower(data$resultSets$headers[[1]])
  return(df)
}

drop_last_column = function(df) {
  df = df %>% select(-names(df)[[length(names(df))]])
  return(df)
}
```

## Box scores

Get individual game statistics data from 2001-02 season to 2020-21
season

``` r
season_years = c("2020-21", "2019-20", 
           "2018-19", "2017-18", 
           "2016-17", "2015-16", 
           "2014-15", "2013-14", 
           "2012-13", "2011-12", 
           "2010-11", "2009-10", 
           "2008-09", "2007-08", 
           "2006-07", "2005-06", 
           "2004-05", "2003-04", 
           "2002-03", "2001-02")

box_score_all = tibble(
  season_year = season_years,
  url = str_c("https://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=", season_year, "&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision="),
  box_score = map(url, scrapping_data)) %>% 
  mutate(box_score = map(box_score, drop_last_column)) %>% # last column of each box score is NA
  select(-season_year, -url) %>% 
  unnest(cols = box_score)
```

## Transition data

Get average transition data of every team in the last 9 seasons.

``` r
season_years = c(
  "2020-21", "2019-20", 
  "2018-19", "2017-18", 
  "2016-17", "2015-16", 
  "2014-15", "2013-14", "2012-13")

transition_df = tibble(
  season_year = season_years,
  url = str_c("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Transition&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=", season_year, "&TypeGrouping=offensive"),
  transition = map(url, scrapping_data)) %>% 
  mutate(transition = map(transition, drop_last_column)) %>% 
  select(-season_year, -url) %>% 
  unnest(cols = transition)
```

## Isolation data

Get average Isolation data of every team in the last 9 seasons.

``` r
season_years = c(
  "2020-21", "2019-20", 
  "2018-19", "2017-18", 
  "2016-17", "2015-16", 
  "2014-15", "2013-14", "2012-13")

isol_df = 
  tibble(
  season_year = season_years,
  url = str_c("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Isolation&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=", season_year, "&TypeGrouping=offensive"),
  isol = map(url, scrapping_data)) %>% 
  mutate(isol = map(isol, drop_last_column)) %>% 
  select(-season_year, -url) %>% 
  unnest(cols = isol)
```

## pick and roll ball handler data

Get average pick and roll ball handler data of every team in the last 9
seasons.

``` r
season_years = c(
  "2020-21", "2019-20", 
  "2018-19", "2017-18", 
  "2016-17", "2015-16", 
  "2014-15", "2013-14", "2012-13")

prbh_df = 
  tibble(
  season_year = season_years) %>% 
  mutate(url = str_c("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=PRBallHandler&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=", season_year, "&TypeGrouping=offensive")) %>% 
  mutate(prbh = map(url, scrapping_data)) %>% 
  mutate(prbh = map(prbh, drop_last_column)) %>% 
  select(-season_year, -url) %>% 
  unnest(cols = prbh)
```

## pick and roll roll man

Get average pick and roll roll man data of every team in the last 9
seasons.

``` r
season_years = c(
  "2020-21", "2019-20", 
  "2018-19", "2017-18", 
  "2016-17", "2015-16", 
  "2014-15", "2013-14", "2012-13")

prrm_df = 
  tibble(
  season_year = season_years) %>% 
  mutate(url = str_c("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=PRRollman&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=", season_year, "&TypeGrouping=offensive")) %>% 
  mutate(prrm = map(url, scrapping_data)) %>% 
  mutate(prrm = map(prrm, drop_last_column)) %>% 
  select(-season_year, -url) %>% 
  unnest(cols = prrm)
```

## Passing data

Get average passing data of every team in the last 8 seasons.

``` r
season_years = c(
  "2020-21", "2019-20", 
  "2018-19", "2017-18", 
  "2016-17", "2015-16", 
  "2014-15", "2013-14")

pass_df = 
  tibble(
  season_year = season_years) %>% 
  mutate(url = str_c("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=", season_year, "&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")) %>%
  mutate(passing = map(url, scrapping_data)) %>% 
  mutate(passing = map(passing, drop_last_column)) %>% 
  select(-season_year, -url) %>% 
  unnest(cols = passing)
```

## Defensive impact

Get average defensive data of every team in the last 8 seasons.

``` r
season_years = c(
  "2020-21", "2019-20", 
  "2018-19", "2017-18", 
  "2016-17", "2015-16", 
  "2014-15", "2013-14")

defensive_impact_df = 
  tibble(
  season_year = season_years) %>% 
  mutate(url = str_c("https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Defense&Season=", season_year, "&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")) %>%
  mutate(defend = map(url, scrapping_data)) %>% 
  mutate(defend = map(defend, drop_last_column)) %>% 
  select(-season_year, -url) %>% 
  unnest(cols = defend)
```

## Shooting Log Data of Knicks

Get the current up to date shooting location data of Knicks.

``` r
url = "https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=155&CFPARAMS=2020-21&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID=0&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=1610612752&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID="

shooting_log_df = 
  scrapping_data(url) %>% 
  drop_last_column()
```

## Some data cleaning

some datasets are without the column - season\_year, for later analysis,
we need to add this column

``` r
add_season_year = function(season_years, df) {
  df = df %>% 
    mutate(season_year = season_years) %>% 
    relocate(season_year) 
  
  return(df)
}
```

``` r
season_years =  c(replicate(30, "2020-21"), 
  replicate(30, "2019-20"), 
  replicate(30, "2018-19"), 
  replicate(30, "2017-18"), 
  replicate(30, "2016-17"), 
  replicate(30, "2015-16"), 
  replicate(30, "2014-15"), 
  replicate(30, "2013-14"), 
  replicate(30, "2012-13"))

isol_df = add_season_year(season_years, isol_df)
transition_df = add_season_year(season_years, transition_df)
prbh_df = add_season_year(season_years, prbh_df)
prrm_df = add_season_year(season_years, prrm_df)

season_years_pass =  c(replicate(30, "2020-21"), 
  replicate(30, "2019-20"), 
  replicate(30, "2018-19"), 
  replicate(30, "2017-18"), 
  replicate(30, "2016-17"), 
  replicate(30, "2015-16"), 
  replicate(30, "2014-15"), 
  replicate(30, "2013-14"))

pass_df = add_season_year(season_years_pass, pass_df)
defensive_impact_df = add_season_year(season_years_pass, defensive_impact_df)
```

## Save to the local for further use

Save dataset for further use.

``` r
write_csv(box_score_all, "./data2/box_score_all.csv")
write_csv(transition_df, "./data2/transition_df.csv")
write_csv(isol_df, "./data2/isol_df.csv")
write_csv(prbh_df, "./data2/prbh_df.csv")
write_csv(prrm_df, "./data2/prrm_df.csv")
write_csv(pass_df, "./data2/pass_df.csv")
write_csv(defensive_impact_df, "./data2/defensive_impact_df.csv")
```

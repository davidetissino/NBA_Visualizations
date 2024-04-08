#### BEST LINEUPS ####

library(httr)
library(extrafont)
library(tidyverse)
library(janitor)
library(readr)
library(ggimage)
library(gt)

# increase connection buffer 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)

# csv with teams info 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# csv with active players index
act <- read_csv('/Users/davidetissino/Desktop/R/data/Active Index.csv')

# headerS
headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# function to access all lineups historical data 
lineups <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguedashlineups?Conference=&DateFrom=&DateTo=&Division=&GameSegment=&GroupQuantity=3&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=', 
                season, '&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&TeamID=0&VsConference=&VsDivision=')
  
  res <- GET(url = url, add_headers(.headers = headers))
  resp <- fromJSON(content(res, 'text'))
  
  trios <- data.frame(resp$resultSets$rowSet)
  colnames(trios) <- resp[['resultSets']][['headers']][[1]]
  
  trios <- trios %>%
    clean_names() %>% 
    retype()
  
  return(trios)
}

# vector with seasons of available tracking 
s <- lineups(2007)





url <- 'https://stats.nba.com/stats/leaguedashlineups?Conference=&DateFrom=&DateTo=&Division=&GameSegment=&GroupQuantity=3&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&TeamID=0&VsConference=&VsDivision='

res <- GET(url = url, add_headers(.headers = headers))
resp <- fromJSON(content(res, 'text'))

trio <- data.frame(resp$resultSets$rowSet)

colnames(trio) <- resp[['resultSets']][['headers']][[1]]











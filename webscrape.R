# RVEST - SCRAPE WIKI ####

library(rvest)

url <- 'https://en.wikipedia.org/wiki/World_population'

paragraphs <- read_html(url) %>% 
  html_nodes('p') %>%             # returns corresponding node (paragraph)
  html_text()                     # converts node into text 

links <- read_html(url) %>%
  html_nodes('a')                 # returns links 

tables <- read_html(url) %>%
  html_nodes('table') %>%
  html_table()


# most densely populated countries: 8th table from url 

various_tables <- read_html(url) 

most_dens <- various_tables %>%
  html_nodes('table') %>%
  `[[`(8) %>%     # Tab of interest is the 8th from the url 
  # `` of `[[` done using option9
  # ATTENTION: sometimes also graphs are considered 
  html_table()    # transforms into table 

most_dens <- most_dens[,2:5]  # select specified columns 

names(most_dens) <- c('Country', 'Tot pop.', 'Area', 'Dens') 
# reassign names of the column 

# most populated & densely countries: 9th table from url

various_tables_a <- read_html(url) 

dual_dens <- various_tables_a %>%
  html_nodes('table') %>%
  `[[`(9) %>%
  html_table()

dual_dens <- dual_dens[,2:6]

names(dual_dens) <- c('Country', 'Population', 'Area','Density', 'Trend')



# XPATH - SCRAPE WIKI ####

url <- "https://en.wikipedia.org/wiki/World_population"
ten_most_populous <- read_html(url) 

ten_most_df <- ten_most_populous %>% 
  html_nodes(xpath="/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[7]") %>% 
  html_table()

ten_most_df <- ten_most_df[[1]]   # we have to use only first element of list 



# BASKETBALL REFERENCE ####

### Simple Scraping Table ====

library(rvest)  

url <- 'https://www.basketball-reference.com/leagues/NBA_2024_per_game.html'

#Here, we indicate that this is the table we want to extract
all_stats <- read_html(url) %>%  
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) 



## Function Dep. on Season ====
library(rvest)

# Function 
logs <- function(year) {
  
  url <- paste0('https://www.basketball-reference.com/players/d/duranke01/gamelog/', year)
  
  log <- read_html(url) %>% html_nodes('table') %>% .[[8]] %>% html_table(fill = T)
  
  return(log) %>% as.data.frame()
  
}

# Call only for 2010 game logs
KD24 <- logs(2024)

# Automate for ALL game logs
library(tidyverse)

# vector of seasons played
seasons <- c(2008:2024)

# final df with ALL gamelogs 
df <- map_df(seasons, logs)



## Func Career RS Logs ====
library(rvest)
library(tidyverse)

# Function
gamelogs <- function(init, abbr, year) {
  
  url <- paste0('https://www.basketball-reference.com/players/', init, '/', abbr, '01/gamelog/', year)
  
  all <- read_html(url) %>% html_nodes('table') %>% .[[8]] %>% html_table(fill = T)
  
  return(all) %>% as.data.frame()
  
}

# Call for Steph, only 2024 season 
# some differ in abbreviation (eg: KD = duranke and not durantke)
# (up) ABBR must be first 5 letters of surname + first 2 letters of name 
sc30 <- gamelogs('c', 'curryst', 2024)



## Func Career Playoff Logs ====
po_logs <- function(init, abbr) {
  
  url <- paste0('https://www.basketball-reference.com/players/', init, '/', abbr, '01/gamelog-playoffs/')
  
  logs <- read_html(url) %>% html_nodes('table') %>% .[[8]] %>% html_table(fill = T)
  
  return(logs) %>% as.data.frame()
  
}

# call for steph 
steph <- po_logs('c', 'curryst')




#### ---------------------- ###

# NBA ####


##### Team Logos ====
library(rvest)

url <-'https://www.nba.com/teams'

logos <- read_html(url) %>%
  html_nodes(css='img') %>%
  html_attr('src') %>%
  .[-c(1, 32:67)] %>%
  as.data.frame()

# new logos, small change in url
log <- gsub('primary', 'global', logos$.) %>% as.data.frame()


# assign name to links column 
colnames(logos)[1] <- 'link'
# sort in ascending link order 
logos <- logos %>% arrange(link)
# getting number of rows
rows <- nrow(logos) 
# extracting odd rows  
odd_rows <- seq_len(rows) %% 2 
# get data from odd data frame 
logos <- logos[odd_rows == 1, ] %>%
  as.data.frame()

logos$id = logos$link



### Player Index ====
library(httr)
library(jsonlite)

# increase buffer to scrape 
Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 2)

# headers
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

url <- 'https://stats.nba.com/stats/playerindex?College=&Country=&DraftPick=&DraftRound=&DraftYear=&Height=&Historical=1&LeagueID=00&Season=2023-24&SeasonType=Regular%20Season&TeamID=0&Weight='

res <- GET(url = url, add_headers(.headers = headers))

resp <- fromJSON(content(res, 'text'))

df <- data.frame(resp$resultSets$rowSet)

colnames(df) <- resp[['resultSets']][['headers']][[1]]



### Players Photos ====
library(rvest)

# first 87 players
url <- 'https://www.nba.com/players'

headshots <- read_html(url) %>%
  html_nodes(css = 'img') %>%
  html_attr('src') %>%
  as.data.frame()


### Func ALL Season GAMELOGS ====
library(httr)
library(jsonlite)

# headers
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

# function returning ALL individual gamelogs for the season
gamelogs <- function(season) {
  
  url <- paste0('https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=', 
                season, '&SeasonType=Regular%20Season&Sorter=DATE')
  
  res <- GET(url = url, add_headers(.headers = headers))
  resp <- fromJSON(content(res, 'text'))
  
  logs <- data.frame(resp$resultSets$rowSet)
  colnames(logs) <- resp[['resultSets']][['headers']][[1]]
  
  return(logs)
  
}

#!! season = year - 1 
#!! example: for 2023-24 gamelogs use 
logs23 <- gamelogs(2023)

gamelogs(2022)



# NBA PER GAME STATS ####

library(tidyverse)
library(jsonlite)
library(httr)

# headers Ryan Davis (?)
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

url <- 'https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&ISTRound=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight='

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
df <- data.frame(json_resp$resultSets$rowSet)

colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]  



# NBA PBP (AS ABOVE) ####

library(tidyverse)
library(jsonlite)
library(httr)

# set headers h/t Ryan Davis
headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

url <- "https://cdn.nba.com/static/json/liveData/playbyplay/playbyplay_0042000404.json"

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
df <- data.frame(json_resp[["game"]][["actions"]])



# NBA PLAYER SHOT DASHBOARD ####

library(tidyverse)
library(jsonlite)
library(httr)

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

url <- 'https://stats.nba.com/stats/leaguedashplayerptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=24-22&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight='

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
df <- data.frame(json_resp$resultSets$rowSet)

colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]  



# NBA SEASON DATA ##### 

pacman::p_load(jsonlite, dplyr, data.table)

# Create an empty list to store data frames for each season
dfs <- list()

# Iterate over each season from 2000-01 to 2022-23
for (season in seq(2000, 2022)) {
  # Construct the URL for the API request
  url <- sprintf("https://api.pbpstats.com/get-totals/nba?Season=%d-%02d&SeasonType=Regular%%2BSeason&Type=Lineup", season, season + 1 - 2000)
  
  # Fetch the JSON data from the API
  api <- read_json(url)
  
  
  # Extract the data frame from the API response
  df <- api[["multi_row_table_data"]]
  df <- Map(as.data.frame, df)
  df <- rbindlist(df, fill = TRUE)
  
  # Add a column to indicate the season
  df$season <- season + 1
  
  # Store the data frame in the list
  dfs[[season - 1999]] <- df
}

# Combine all data frames into a single data table
final_result <- rbindlist(dfs, fill = TRUE)



# SPOTRAC SALARY DATA ####

## Team salary cap info ====
library(rvest)

url <- 'https://www.spotrac.com/nba/golden-state-warriors//cap'

gsw_salary <- read_html(url)%>%
  html_nodes('table') %>%
  .[[1]]%>%
  html_table(fill = TRUE)


## Function Dep. on Team ====
library(rvest)
library(ggdist)
library(scales)  

# Function 
get_data <- function(team) {
  
  # set URL
  url <- paste0("https://www.spotrac.com/nba/rankings/2023-24/base/", team)
  
  # read in URL
  team_page <- url %>% read_html() 
  
  # Create a tibble (dataframe) with player names, salaries, and team name
  df <- tibble(player = team_page %>% html_elements('a.team-name') %>% html_text2(),
               salary = team_page %>% html_elements('span.info') %>% html_text2() %>% parse_number(),
               team = team)
  
  return(df)
  
}

# Call  
df <- get_data('golden-state-warriors')


## Team base salary simple table (names + salary) ## 

library(rvest)
library(nbastatR)

url <- 'https://www.spotrac.com/nba/rankings/2023-24/base/golden-state-warriors/'

# to extract team players names
team_names <- read_html(url) %>%
  html_elements('a.team-name') %>%
  html_text2()

# to extract associated salaries
team_salaries <- read_html(url) %>%
  html_elements('span.info') %>%
  html_text2()

# combine previous data into one df
final <- data.frame(team_names, team_salaries)


## Iterate for each NBA team ##

library(dplyr) # for filter function 
library(nbastatR)
library(rvest)
library(readr) # for parse function 
library(tidyverse) # for map_df function 

# get NBA team names
teams <- nba_teams() %>% filter(isNonNBATeam == 0)

# removes first row (not nba team)
teams <- teams[-1,]

# create hyphenated team names for our URLs
team_names <- gsub(" ", "-",tolower(teams$nameTeam)) 

# write function to get salaries for each player on a team
get_data <- function(team) {
  
  # set URL
  url <- paste0("https://www.spotrac.com/nba/rankings/2023-24/base/", team)
  
  # read in URL
  team_page <- url %>% read_html() 
  
  # Create a tibble (dataframe) with player names, salaries, and team name
  df <- tibble(player = team_page %>% html_elements('a.team-name') %>% html_text2(),
               salary = team_page %>% html_elements('span.info') %>% html_text2() %>% parse_number(),
               team = team)
  
  return(df)
  
}

# get data for all teams 
tot_salaries <- map_df(team_names, get_data)

# salary as % of league cap (2023: 13602100)
tot_salaries$cap_pct <- (tot_salaries$salary / 136021000)*100






'https://www.basketball-reference.com/allstar/NBA_2023.html#Giannis'



library(rvest)

# Function 
logs <- function(year) {
  
  url <- paste0('https://www.basketball-reference.com/allstar/NBA_', year, '.html')
  
  log <- read_html(url) %>% html_nodes('table') %>% .[[2]] %>% html_table(fill = T)
  

  data.frame(log)

  }


library(tidyverse)
seasons <- c(1999:2023)

e99 <- logs(1999)

east_allstars <- map_df(seasons, logs)

west_allstars <- map_df(seasons, logs)


url <- 'https://www.basketball-reference.com/allstar/NBA_2023.html'

#Here, we indicate that this is the table we want to extract
allstar <- read_html(url) %>%  
  html_nodes("table") %>% 
  .[[2]] %>% 
  html_table(fill=T) 









url <- 'https://www.basketball-reference.com/allstar/NBA_2023.html'

df <- read_html(url) %>% 
  html_nodes('table') %>% 
  .[[2]] %>% 
  html_table(fill = T)



url <- 'https://www.basketball-reference.com/allstar/NBA_2023.html#Giannis'

df <- read_html(url) %>% 
  html_nodes('table') %>%
  .[[2]] %>% 
  html_table(fill = T)




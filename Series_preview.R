#### PLAYTYPE EFFICIENCY ####

library(jsonlite)
library(httr)
library(extrafont)
library(tidyverse)
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

# ISO totals
url <- 'https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Isolation&PlayerOrTeam=P&SeasonType=Regular%20Season&SeasonYear=2023-24&TypeGrouping=offensive'

res <- GET(url = url, add_headers(.headers = headers))
json_resp <- fromJSON(content(res, 'text'))
df <- data.frame(json_resp$resultSets$rowSet)
colnames(df) <- json_resp[['resultSets']][['headers']][[1]]

# keep only relevant columns
df <- df[,c(3,5,12:13, 20, 21)]

# new column with rank 
df$rank <- c(1:nrow(df))

colnames(df)[2] <- 'slugTeam'

df <- merge(df, tms, by = 'slugTeam')

df <- df[,-c(8:11)]

df <- df[order(df$rank, decreasing = F), ]

df <- df[-c(11:nrow(df)),]

df <- df[order(df$PPP, decreasing = T), ]

df$rank <- c(1:nrow(df))

df <- df[,-1]

df <- df[,c(6, 1, 7, 4, 5, 3, 2)]

df$FG_PCT <- as.numeric(df$FG_PCT)

df$FG_PCT <- df$FG_PCT * 100

df$PPP <- as.numeric(df$PPP) 
df$PPP <- round(df$PPP, 2)

df %>% 
  gt() %>%
  cols_label(PLAYER_NAME = 'PLAYER', 
             PPP = 'PPP', 
             FG_PCT = 'FG%', 
             POSS = 'POS/G', 
             rank = 'RK', 
             logo = 'TEAM', 
             PTS = 'PTS/G') %>% 
  tab_header(
    title = md("**Isolation Efficiency Report**"), 
    subtitle = paste0("2023-24 Regular Season | Updated ", format(Sys.Date(), format="%B %d, %Y"))) %>%
  text_transform (
    locations = cells_body(vars(logo)),
    fn = function(x) {
      web_image(url = x, 
                height = px(25))}) %>% 
  tab_options(
    table.background.color = "grey99",
    column_labels.font.size = 11,
    table.font.size = 13,
    heading.title.font.size  = 20,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 13,
    table.border.top.color = "transparent",
    data_row.padding = px(2), 
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1), 
  ) %>%
  tab_footnote(footnote = '@dvdtssn | stats.nba.com') %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = vars(c(rank, PPP))
  )) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) %>% 
  data_color(
    columns = vars(PPP), 
    colors = scales::col_numeric(
      palette = c(
        "azure2", "indianred2"),
      domain = NULL)) %>%
  tab_style(
    style = cell_text(color = 'black', weight = 'bold'), 
    locations = cells_body(columns = vars(PPP))
  ) %>%
  opt_horizontal_padding(scale = 1.5) %>%
  gtsave('/Users/davidetissino/Desktop/ISO.png')


### PROJECT 4: SHOOTING DATA & PLOT ON HALFCOURT GRAPH ####

## CUSTOM THEME ####

library(ggthemes)
library(extrafont)
library(tidyverse)

theme_davide <- function() {
  theme_fivethirtyeight(base_family = 'avenir') %+replace%  
    theme(
      text = element_text(family='avenir'), 
      panel.background = element_rect('floralwhite'), 
      plot.background = element_rect('floralwhite'),
      plot.title = element_text(size=22, hjust=.5, vjust = -3, face = 'bold'),
      plot.subtitle = element_text(size=12, vjust=-8.5, face = 'italic'), 
      plot.margin = margin(0, 0, 0, 0, "cm"), 
      panel.grid.major = element_line(color='floralwhite')
    ) 
}

# Load pacakges
library(tidyverse)
library(nbastatR)
library(jsonlite)
library(ggridges)
library(extrafont)
library(prismatic)
library(paletteer)
library(cowplot)
library(ggfx)
library(ggimage)

# Load NBA court dimensions from github
devtools::source_url("https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE")

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

# Shot location data                                                                                                                                                                                                                                                                                                                                                                                    # PLAYER ID                                                                                                                                                                                                                                   # TEAM ID        
url <- 'https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=&CFPARAMS=&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OppPlayerID=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID=201939&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season=2023-24&SeasonSegment=&SeasonType=Regular%20Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID='

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
df <- data.frame(json_resp$resultSets$rowSet[[1]])
colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]

# remove shots from other halfcourt
df <- df[-c(26, 138),]

# render values with appropriate measure for our court
df$LOC_X <- as.numeric(as.character(df$LOC_X)) / 10
df$LOC_Y <- as.numeric(as.character(df$LOC_Y)) / 10 + hoop_center_y

# select only certain columns 
df <- df %>% select(SHOT_TYPE, SHOT_DISTANCE, LOC_X, LOC_Y, SHOT_ATTEMPTED_FLAG, SHOT_MADE_FLAG)

# create the graph 
df %>%
  # color based on whether shot made or missed
  ggplot(aes(LOC_X, LOC_Y, color=SHOT_MADE_FLAG)) +
  geom_point(size=2.5, alpha=0.6) +
  # specify different color for missed and made shot
  scale_color_manual(values=c('#fdb927','#1D428A' ), labels=c('Missed', 'Made')) +
  # unknown 
  geom_path(data = court_points,
                           aes(x = x, y = y, group = desc, linetype=dash),
                           color = "gray10", size = .25) +
  # unknown
  coord_fixed(clip='off') +
  scale_y_continuous(limits = c(0, 45)) +
  scale_x_continuous(limits = c(-27.5, 27.5)) + 
  # custom theme 
  theme_davide() +
  # modify theme
  theme(
        line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        plot.margin = margin(1, 1, 1.5, 1),
        strip.text.x = element_text(vjust = 5), 
        plot.background = element_rect(fill='floralwhite'), 
        panel.background = element_rect(fill='floralwhite'),
        # legend 
        legend.position = 'bottom',
        legend.title = element_text(size=12),
        legend.text = element_text(size=12),
        legend.background = element_blank(),
        legend.key=element_rect(fill='floralwhite'),
        legend.key.size = unit(1, 'pt'),
        legend.margin = margin(b=50)) +
  # removes dash type from legend 
  guides(linetype='none') + 
  labs(title = 'Stephen Curry', 
       subtitle = '2023 - 2024 Regular Season | Updated November 14, 2023') +
  # assigns title to legend
  guides(color = guide_legend(title = 'Field goal: ', 
                              reverse = T))



  
  

library(grid)
# save png
# ggsave('/Users/davidetissino/Desktop/semifinal.png', dpi='retina')

library(magick)
gsw <- image_read('https://cdn.nba.com/logos/nba/1610612744/primary/L/logo.svg')



# csv file with team colors & logos 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/team infos.csv')


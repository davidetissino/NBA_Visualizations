library(janitor)
library(hablar)
library(jsonlite)
library(httr)
library(ggtext)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)

library(magick)

# increase buffer size for scraping
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

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

## SCRAPE 
url <- "https://stats.nba.com/stats/gamerotation?GameID=0042300152"

res <- GET(url = url, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, "text"))

# two tabs, home & away
home <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
away <- data.frame(json_resp[["resultSets"]][["rowSet"]][[1]])

colnames(home) <- json_resp[["resultSets"]][["headers"]][[1]]
colnames(away) <- json_resp[["resultSets"]][["headers"]][[1]]

home <- home %>%
  clean_names() %>%
  retype()

away <- away %>%
  clean_names() %>%
  retype()

# one team column 
home$player <- paste0(home$player_first, ' ', home$player_last)
away$player <- paste0(away$player_first, ' ', away$player_last)

# add location column 
home$location <- 'home'
away$location <- 'away'


# remove & clean some columns
home <- home[, c(14, 13, 8:12, 1:7)]
away <- away[, c(14, 13, 8:12, 1:7)]

### merge into one ####
df <- rbind(home, away)


# time in strange format, function to convert to correct format
convert_to_time_string <- function(seconds) {
  time_strings <- sprintf("%02d:%02d", as.integer(seconds) %/% 60, as.integer(seconds) %% 60)
  return(time_strings)
}

# Correct format to in & out time home & away
df$in_time_real <- lapply(df$in_time_real / 10, convert_to_time_string)
df$out_time_real <- lapply(df$out_time_real / 10, convert_to_time_string)

# epoch to convert to UNIX time
date <- as.Date("2000-01-01")

# Convert "mm:ss" to Unix time (seconds since Unix Epoch)
df$in_unix_time <- as.integer(as.POSIXct(paste(date, df$in_time_real), format = "%Y-%m-%d %M:%S"))
df$out_unix_time <- as.integer(as.POSIXct(paste(date, df$out_time_real), format = "%Y-%m-%d %M:%S"))



aggregate(.~player,data=home,FUN=sum) 


# Plot 
df %>% 
  ggplot(aes(y = player_first, xmin = in_unix_time, xmax = out_unix_time, color = pt_diff)) +
  geom_linerange(size = 5) +
  scale_x_continuous(breaks = c(946681200, 946681920, 946682640, 946683360, 946684080),
                     labels = c("0", "12", "24", "36", "48")) +
  facet_wrap(~location, nrow = 2, strip.position = 'bottom') +
  labs(x = "", y = "", title="Rockets @ Warriors - Rotation Plot", subtitle="20/11/2023, 116 - 121") +
  theme_minimal() +
  coord_cartesian(xlim = c(946681200, 946684080)) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_color_gradient2(low = 'firebrick3', mid = 'white', high = 'forestgreen') +
  # scale_color_gradient2(low = "#36648B", mid="#FFFAFA", high = "#D91A1A") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 16, face = "italic", family = "Archivo"),
    axis.title.y = element_text(vjust = 2, size = 16, face = "italic", family = "Archivo"),
    plot.caption = element_text(color = 'gray40', size = 10, family = "Archivo"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, family = "Archivo"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, family = "Archivo"),
    axis.text = element_text(size = 9, family = "Archivo"),
    legend.position = "none",
    legend.box = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(family = "Archivo", size = 10),
    legend.key.height = unit(0.2, "in"),
    legend.key.width = unit(0.6, "in"),
    legend.background = element_rect(fill = '#efe8e8', color = '#efe8e8'),
    panel.grid = element_line(color = "#afa9a9"),
    plot.background = element_rect(fill = '#efe8e8', color = '#efe8e8'),
    panel.background = element_rect(fill = '#efe8e8', color = '#efe8e8'),
    panel.grid.minor = element_blank(),
    plot.margin = margin(b=0.1, r=0.2, unit = "cm"),
    axis.text.y = element_markdown(margin = margin(r = -15, unit = "pt"))
  ) +
  theme(
    legend.position = "bottom",  # Position the legend at the top of the plot
    legend.title = element_text(size = 8, hjust = 0.5)  # Set the legend title
  ) +
  guides(
    color = guide_colorbar(barwidth = 8, barheight=0.5, title = "Points Differential per Minute", title.position = "top", title.hjust = 0.5, label.theme = element_text(size = 6))  # Adjust the color legend appearance
  ) 


#ggsave('/Users/davidetissino/Desktop/rotation5.png', dpi = 'retina')



row <- c(1, 1, 2)
size <- c(10, 15, 10)
df=as.data.frame(row,size)
result  <- aggregate(size ~ row, df, sum)







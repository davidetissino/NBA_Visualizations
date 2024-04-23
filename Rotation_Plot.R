library(janitor)
library(hablar)
library(tidyverse)
library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(magick)
library(nbastatR)
library(ggtext)
library(RColorBrewer)

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
subs_team_home <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
subs_team_away <- data.frame(json_resp[["resultSets"]][["rowSet"]][[1]])

colnames(subs_team_home) <- json_resp[["resultSets"]][["headers"]][[1]]
colnames(subs_team_away) <- json_resp[["resultSets"]][["headers"]][[1]]

subs_team_home <- subs_team_home %>%
  clean_names() %>%
  retype()

subs_team_away <- subs_team_away %>%
  clean_names() %>%
  retype()

# time in strange format, function to convert to correct format
convert_to_time_string <- function(seconds) {
  time_strings <- sprintf("%02d:%02d", as.integer(seconds) %/% 60, as.integer(seconds) %% 60)
  return(time_strings)
}

# Correct format to in & out time home & away
subs_team_home$in_time_real <- lapply(subs_team_home$in_time_real / 10, convert_to_time_string)
subs_team_home$out_time_real <- lapply(subs_team_home$out_time_real / 10, convert_to_time_string)

subs_team_away$in_time_real <- lapply(subs_team_away$in_time_real / 10, convert_to_time_string)
subs_team_away$out_time_real <- lapply(subs_team_away$out_time_real / 10, convert_to_time_string)

# epoch to convert to UNIX time
date <- as.Date("2000-01-01")

# Convert "mm:ss" to Unix time (seconds since Unix Epoch)
subs_team_home$in_unix_time <- as.integer(as.POSIXct(paste(date, subs_team_home$in_time_real), format = "%Y-%m-%d %M:%S"))
subs_team_home$out_unix_time <- as.integer(as.POSIXct(paste(date, subs_team_home$out_time_real), format = "%Y-%m-%d %M:%S"))

subs_team_away$in_unix_time <- as.integer(as.POSIXct(paste(date, subs_team_away$in_time_real), format = "%Y-%m-%d %M:%S"))
subs_team_away$out_unix_time <- as.integer(as.POSIXct(paste(date, subs_team_away$out_time_real), format = "%Y-%m-%d %M:%S"))

# to plot point differential per minute
subs_team_home$time_played <- subs_team_home$out_unix_time - subs_team_home$in_unix_time
subs_team_home$point_diff_minute = subs_team_home$pt_diff/subs_team_home$time_played*60

subs_team_away$time_played <- subs_team_away$out_unix_time - subs_team_away$in_unix_time
subs_team_away$point_diff_minute = subs_team_away$pt_diff/subs_team_away$time_played*60




link_to_img <- function(x, width = 28) {
  glue::glue("<img src='{x}' width='{width}'/>")
}

image_df=nba_players()
image_df=image_df%>%
  select(idPlayer, urlPlayerHeadshot)

subs_team_away=merge(subs_team_away, image_df, by.x="person_id", by.y="idPlayer")
subs_team_home=merge(subs_team_home, image_df, by.x="person_id", by.y="idPlayer")


summary_home <- subs_team_home %>%
  retype()%>%
  group_by(urlPlayerHeadshot) %>%
  summarize(min_in_unix_time = min(in_unix_time))%>%
  arrange(desc(min_in_unix_time))%>%
  mutate(urlPlayerHeadshot = link_to_img(urlPlayerHeadshot))

summary_away <- subs_team_away %>%
  retype()%>%
  group_by(urlPlayerHeadshot) %>%
  summarize(min_in_unix_time = min(in_unix_time))%>%
  arrange(desc(min_in_unix_time))%>%
  mutate(urlPlayerHeadshot = link_to_img(urlPlayerHeadshot))


p_away <- subs_team_away %>%
  mutate(urlPlayerHeadshot = link_to_img(urlPlayerHeadshot))





subs_team_away %>% 
  ggplot(aes(y = player_first, xmin = in_unix_time, xmax = out_unix_time, color = pt_diff)) +
  geom_linerange(size = 5) +
  scale_x_continuous(breaks = c(946681200, 946681920, 946682640, 946683360, 946684080),
                     labels = c("0", "12", "24", "36", "48")) +
  labs(x = "", y = "", title="Rockets @ Warriors - Rotation Plot", subtitle="20/11/2023, 116 - 121") +
  theme_minimal() +
  coord_cartesian(xlim = c(946681200, 946684080)) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_color_gradient2(low = "#36648B", mid="#FFFAFA", high = "#D91A1A") +
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
    legend.position = "top",  # Position the legend at the top of the plot
    legend.title = element_text(size = 8, hjust = 0.5)  # Set the legend title
  ) +
  guides(
    color = guide_colorbar(barwidth = 8, barheight=0.5, title = "Points Differential per Minute", title.position = "top", title.hjust = 0.5, label.theme = element_text(size = 6))  # Adjust the color legend appearance
  ) 

ggsave('/Users/davidetissino/Desktop/rotation1.png', dpi = 'retina')





p_home <- subs_team_home %>%
  mutate(urlPlayerHeadshot = link_to_img(urlPlayerHeadshot))

p_home %>%
  ggplot(aes(y = factor(urlPlayerHeadshot, levels=summary_home$urlPlayerHeadshot), xmin = in_unix_time, xmax = out_unix_time, color = point_diff_minute)) +
  geom_linerange(size = 5) +
  scale_x_continuous(breaks = c(946681200, 946681920, 946682640, 946683360, 946684080),
                     labels = c("0", "12", "24", "36", "48")) +
  labs(x = "", y = "", caption = '@dvdtssn | stats.nba.com') +
  theme_minimal() +
  coord_cartesian(xlim = c(946681200, 946684080)) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
  scale_color_gradient2(low = "#D91A1A", mid="#ffcb2d", high = "#549935") +
  theme(
    axis.title.x = element_text(vjust = 0, size = 16, face = "italic", family = "Archivo"),
    axis.title.y = element_text(vjust = 2, size = 16, face = "italic", family = "Archivo"),
    plot.caption = element_text(color = 'gray40', size = 12, family = "Archivo"),
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
    plot.margin = margin(t=1, unit = "cm", r=0.2),
    axis.text.y = element_markdown(margin = margin(r = -15, unit = "pt"))
  ) +
  theme(
    legend.position = "top",  # Position the legend at the top of the plot
    legend.title = element_text(size = 8, hjust = 0.5, family = "Archivo")  # Set the legend title
  ) +
  guides(
    color = guide_colorbar(barwidth = 8, barheight=0.5, title = "Points Differential per Minute", title.position = "top", title.hjust = 0.5, label.theme = element_text(size = 6, family = "Archivo"))  # Adjust the color legend appearance
  ) 

ggsave('/Users/davidetissino/Desktop/rotation2.png', dpi = 'retina')


home <- image_read("/Users/davidetissino/Desktop/rotation1.png")
away <- image_read("/Users/davidetissino/Desktop/rotation2.png")


img <- c(home, away)
final_subs_img=image_append(img, stack = TRUE)
image_write(final_subs_img, "/Users/davidetissino/Desktop/RotationPlot.png", format = "png", density=3000)




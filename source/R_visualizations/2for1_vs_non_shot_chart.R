library(tidyverse)
library(devtools)
library(ggplot2)
library(cowplot)

font_add_google(
  family = "Roboto",
  regular.wt = 400,
  "Roboto"
)

showtext_auto()

setwd("~/Desktop/Data_Projects/nba_2for1_analysis")

source_url(
  "https://github.com/Henryjean/NBA-Court/blob/main/CourtDimensions.R?raw=TRUE"
)

df <- read_csv(
  "~/Desktop/Data_Projects/nba_2for1_analysis/data/final_2for1_stats.csv"
)

two_for_one <- df %>%
  filter(
    FIRST_SHOT_RESULT %in% c("Made", "Missed"),
    TWO_FOR_ONE_ATTEMPT == 1
  ) %>%
  mutate(
    Shot_Type = "2-for-1",
    Shot_Result = case_when(
      FIRST_SHOT_RESULT == "Made" ~ "Makes",
      FIRST_SHOT_RESULT == "Missed" ~ "Misses"
    ),
    locationX = FIRST_SHOT_X_LEGACY / 10 * -1,
    locationY = FIRST_SHOT_Y_LEGACY / 10 + hoop_center_y
  )

no_two_for_one <- df %>%
  filter(
    FIRST_SHOT_RESULT %in% c("Made", "Missed"),
    NO_TWO_FOR_ONE_ATTEMPT == 1
  ) %>%
  mutate(
    Shot_Type = "Non 2-for-1",
    Shot_Result = case_when(
      FIRST_SHOT_RESULT == "Made" ~ "Makes",
      FIRST_SHOT_RESULT == "Missed" ~ "Misses"
    ),
    locationX = FIRST_SHOT_X_LEGACY / 10 * -1,
    locationY = FIRST_SHOT_Y_LEGACY / 10 + hoop_center_y
  )

shot_data <- bind_rows(
  two_for_one,
  no_two_for_one
) %>%
  mutate(
    Shot_Type = factor(
      Shot_Type,
      levels = c("2-for-1", "Non 2-for-1")
    )
  )

p <- ggplot() +
  
  # Court
  geom_path(
    data = court_points,
    aes(
      x = x,
      y = y,
      group = desc,
      linetype = dash
    ),
    color = "black",
    linewidth = 0.25,
    show.legend = FALSE
  ) +
  
  # Shot locations
  geom_point(
    data = shot_data,
    aes(
      x = locationX,
      y = locationY,
      color = Shot_Result,
      shape = Shot_Result,
      size = Shot_Result
    ),
    alpha = 0.9,
    stroke = 0.8
  ) +
  
  scale_size_manual(
    values = c(
      "Makes" = 2,
      "Misses" = 1.5
    ),
    guide = "none"
  ) +
  
  scale_color_manual(
    values = c(
      "Makes" = "#0B6E4F",
      "Misses" = "#CC2D2D"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "Makes" = 16,
      "Misses" = 1
    )
  ) +
  
  coord_fixed(
    clip = "off"
  ) +
  
  scale_x_continuous(
    limits = c(-30, 30)
  ) +
  
  scale_y_continuous(
    limits = c(-2.5, 45)
  ) +
  
  # Labels underneath the charts
  facet_wrap(
    ~Shot_Type,
    nrow = 1,
    strip.position = "bottom"
  ) +
  
  theme_minimal(
    base_family = "Roboto"
  ) +
  
  theme(
    
    # Legend
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(
      family = "Roboto",
      size = 12
    ),
    legend.key = element_blank(),
    
    # Grid
    panel.grid = element_blank(),
    
    # Axes
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    # Put facet labels outside the chart
    strip.placement = "bottom",
    
    strip.background = element_blank(),
    
    # Facet labels
    strip.text = element_text(
      family = "Roboto",
      face = "bold",
      size = 46,
      hjust = 0.5,
      margin = margin(
        t = 0,
        b = 0
      )
    ),
    
    # Background
    plot.background = element_rect(
      fill = "#FCFBF8",
      color = "#FCFBF8"
    ),
    
    panel.background = element_rect(
      fill = "#FCFBF8",
      color = "#FCFBF8"
    ),
    
    # Margins
    plot.margin = margin(
      10,
      10,
      10,
      10
    )
  ) +
  
  labs(
    color = NULL,
    shape = NULL,
    title = "2-for-1 vs. Non 2-for-1 Shot Attempts",
    subtitle = "Shot locations and results for the regular season and playoffs (2025–26)",
    caption = "Source: NBA.com via nba_api"
  ) +
  
  theme(
    
    # Title
    plot.title = element_text(
      family = "Roboto",
      face = "bold",
      size = 76,
      hjust = 0.5,
      margin = margin(
        t = 25,
        b = 0
      )
    ),
    
    # Subtitle
    plot.subtitle = element_text(
      family = "Roboto",
      size = 42,
      color = "#666666",
      hjust = 0.5,
      margin = margin(
        t = 6,
        b = 0
      )
    ),
    
    # Caption
    plot.caption = element_text(
      family = "Roboto",
      size = 28,
      hjust = 0,
      margin = margin(
        t = 15
      )
    )
  )

p

ggsave( 
  "images/shot_locations.png", 
  p, 
  width = 9,
  height = 5,
  dpi = 300,
  bg = "#FCFBF8"
)
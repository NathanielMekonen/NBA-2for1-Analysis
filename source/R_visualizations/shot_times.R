library(tidyverse)
library(ggplot2)
library(ggridges)
library(showtext)
library(sysfonts)

font_add_google(
  family = "Roboto",
  regular.wt = 400,
  "Roboto"
)

showtext_auto()

setwd("~/Desktop/Data_Projects/nba_2for1_analysis")

df <- read_csv(
  "~/Desktop/Data_Projects/nba_2for1_analysis/data/final_2for1_stats.csv"
)

# Create data for ridgeline plot
ridge_df <- bind_rows(
  
  # Two-for-one sequences — first shot
  df %>%
    filter(
      TWO_FOR_ONE_ATTEMPT == 1,
      !is.na(FIRST_SHOT_SECONDS)
    ) %>%
    transmute(
      seconds = FIRST_SHOT_SECONDS,
      sequence = "Two-for-One Sequences"
    ),
  
  # Two-for-one sequences — second shot
  df %>%
    filter(
      TWO_FOR_ONE_ATTEMPT == 1,
      !is.na(SECOND_POSSESSION_SHOT_SECONDS)
    ) %>%
    transmute(
      seconds = SECOND_POSSESSION_SHOT_SECONDS,
      sequence = "Two-for-One Sequences"
    ),
  
  # Non two-for-one sequences — first shot
  df %>%
    filter(
      TWO_FOR_ONE_ATTEMPT == 0,
      !is.na(FIRST_SHOT_SECONDS)
    ) %>%
    transmute(
      seconds = FIRST_SHOT_SECONDS,
      sequence = "Non Two-for-One Sequences"
    ),
  
  # Non two-for-one sequences — second shot
  df %>%
    filter(
      TWO_FOR_ONE_ATTEMPT == 0,
      !is.na(SECOND_POSSESSION_SHOT_SECONDS)
    ) %>%
    transmute(
      seconds = SECOND_POSSESSION_SHOT_SECONDS,
      sequence = "Non Two-for-One Sequences"
    )
)

# Set order
ridge_df$sequence <- factor(
  ridge_df$sequence,
  levels = c(
    "Two-for-One Sequences",
    "Non Two-for-One Sequences"
  )
)

# Ridgeline plot
ridge_plot <- ggplot(
  ridge_df,
  aes(
    x = seconds,
    y = sequence
  )
) +
  
  geom_density_ridges(
    fill = "#BA4A1E",
    color = "#BA4A1E",
    alpha = 0.9
  ) +
  
  # Orange gradient
  scale_fill_gradient(
    low = "#E8B8A6",
    high = "#BA4A1E"
  ) +
  
  # Reverse game clock
  scale_x_reverse(
    limits = c(40, 0),
    breaks = seq(40, 0, -5)
  ) +
  
  # Rename y-axis labels
  scale_y_discrete(
    labels = c(
      "Two-for-One Sequences" = "2-for-1",
      "Non Two-for-One Sequences" = "Non 2-for-1"
    )
  ) +
  
  labs(
    title = "When Do Teams Take Their Shots?",
    subtitle = "Comparing shot timing in 2-for-1 and non 2-for-1 sequences (2025–26)",
    x = "Game Clock (Seconds)",
    y = "Shot Attempts",
    caption = "Source: NBA.com via nba_api"
  ) +
  
  theme_minimal(
    base_family = "Roboto"
  ) +
  
  theme(
    # Background
    plot.background = element_rect(
      fill = "#FCFBF8",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "#FCFBF8",
      color = NA
    ),
    
    # Grid
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    # Remove axis lines
    axis.line = element_blank(),
    
    plot.margin = margin(
      10,
      10,
      10,
      10
    ),
    
    # X-axis title
    axis.title.x = element_text(
      size = 40,
      face = "bold"
    ),
    
    # Y-axis title
    axis.title.y = element_text(
      size = 40,
      face = "bold"
    ),
    
    # Y-axis labels
    axis.text.y = element_text(
      size = 42,
      hjust = 1,
      color = "#000000"
    ),
    
    # X-axis labels
    axis.text.x = element_text(
      size = 36
    ),
    
    # Title
    plot.title = element_text(
      size = 66,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 40
    ),
    
    plot.caption = element_text(
      family = "Roboto",
      size = 34,
      hjust = 0
    ),
    
    plot.title.position = "plot",
    
    # Remove legend
    legend.position = "none"
  )

ridge_plot

ggsave(
  filename = "images/shot_times.png",
  plot = ridge_plot,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "#FCFBF8"
)
library(tidyverse)
library(ggplot2)
library(ggimage)
library(showtext)
library(sysfonts)
library(rsvg)

font_add_google(
  family = "Roboto",
  regular.wt = 400,
  "Roboto"
)

showtext_auto()

setwd("~/Desktop/Data_Projects/nba_2for1_analysis")

# Load data
df <- read_csv(
  "~/Desktop/Data_Projects/nba_2for1_analysis/data/final_2for1_stats.csv"
)

dir.create(
  "images/team_logos",
  showWarnings = FALSE,
  recursive = TRUE
)

logo_df <- df %>%
  dplyr::select(
    TEAM_ABBREV,
    logo
  ) %>%
  dplyr::filter(
    !is.na(TEAM_ABBREV),
    !is.na(logo)
  ) %>%
  dplyr::distinct()

for (i in seq_len(nrow(logo_df))) {
  
  team_abbrev <- logo_df$TEAM_ABBREV[i]
  logo_url <- logo_df$logo[i]
  
  svg_path <- file.path(
    "images/team_logos",
    paste0(team_abbrev, ".svg")
  )
  
  png_path <- file.path(
    "images/team_logos",
    paste0(team_abbrev, ".png")
  )
  
  if (!file.exists(png_path)) {
    
    tryCatch({
      
      download.file(
        url = logo_url,
        destfile = svg_path,
        mode = "wb",
        quiet = TRUE
      )
      
      rsvg::rsvg_png(
        svg_path,
        file = png_path,
        width = 500,
        height = 500
      )
      
    }, error = function(e) {
      
      message(
        "Could not process ",
        team_abbrev,
        ": ",
        e$message
      )
      
    })
  }
}

team_df <- df %>%
  group_by(
    team,
    TEAM_ABBREV
  ) %>%
  summarise(
    
    Two_For_One_Attempts = sum(
      TWO_FOR_ONE_ATTEMPT == 1,
      na.rm = TRUE
    ),
    
    Non_Two_For_One = sum(
      NO_TWO_FOR_ONE_ATTEMPT == 1,
      na.rm = TRUE
    ),
    
    Total_Opportunities =
      Two_For_One_Attempts +
      Non_Two_For_One,
    
    Two_For_One_Rate =
      Two_For_One_Attempts /
      Total_Opportunities,
    
    .groups = "drop"
  ) %>%
  
  filter(
    Total_Opportunities > 0
  ) %>%
  
  mutate(
    
    Logo_Path = file.path(
      "images/team_logos",
      paste0(
        TEAM_ABBREV,
        ".png"
      )
    )
    
  ) %>%
  
  filter(
    file.exists(Logo_Path)
  ) %>%
  
  arrange(
    Two_For_One_Rate
  ) %>%
  
  mutate(
    TEAM_ABBREV = factor(
      TEAM_ABBREV,
      levels = TEAM_ABBREV
    )
  )

p <- ggplot(
  team_df,
  aes(y = TEAM_ABBREV)
) +
  
  # Full opportunity bar
  geom_col(
    aes(x = 1),
    fill = "#D6D0C4",
    width = 0.65
  ) +
  
  # 2-for-1 rate
  geom_col(
    aes(x = Two_For_One_Rate),
    fill = "#C75A30",
    width = 0.65
  ) +
  
  # Team logos
  geom_image(
    aes(
      x = -0.04,
      image = Logo_Path
    ),
    size = 0.055,
    asp = 1
  ) +
  
  # Percentage labels
  geom_text(
    aes(
      x = 1.005,
      label = paste0(
        "  (",
        Total_Opportunities,
        ")"
      )
    ),
    hjust = 0,
    family = "Roboto",
    fontface = "bold",
    size = 14,
    color = "#333333"
  ) +
  
  scale_x_continuous(
    limits = c(
      -0.08,
      1.09
    ),
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    labels = scales::percent_format(
      accuracy = 1
    ),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Which Teams Go for the 2-for-1 Most Often?",
    subtitle = "% of potential 2-for-1 sequences where teams chose to go quick (Total Opportunities)",
    x = "2-for-1 Rate",
    y = NULL,
    caption = "Source: NBA.com via nba_api"
  ) +
  
  theme_minimal(
    base_family = "Roboto"
  ) +
  
  theme(
    
    plot.background = element_rect(
      fill = "#FCFBF8",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "#FCFBF8",
      color = NA
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    axis.line.x = element_line(
      color = "#666666",
      linewidth = 0.5
    ),
    
    axis.title.x = element_text(
      size = 40,
      face = "bold"
    ),
    
    axis.text.x = element_text(
      size = 36
    ),
    
    plot.title = element_text(
      size = 66,
      face = "bold"
    ),
    
    plot.subtitle = element_text(
      size = 40
    ),
    
    plot.caption = element_text(
      size = 34,
      hjust = 0
    ),
    
    plot.margin = margin(
      10,
      20,
      10,
      20
    )
  )

p

ggsave(
  filename = "images/team_2for1_rate.png",
  plot = p,
  width = 8,
  height = 9,
  dpi = 300,
  bg = "#FCFBF8"
)

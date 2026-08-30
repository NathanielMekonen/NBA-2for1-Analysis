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

two_for_one_df <- df %>%
  dplyr::group_by(
    team,
    TEAM_ABBREV
  ) %>%
  dplyr::summarise(
    
    Two_For_One_Attempts = sum(
      TWO_FOR_ONE_ATTEMPT == 1,
      na.rm = TRUE
    ),
    
    Total_2For1_Sequence_Points = sum(
      TOTAL_PTS_ON_ALL_2FOR1_POSS[TWO_FOR_ONE_ATTEMPT == 1],
      na.rm = TRUE
    ),
    
    Sequence_PPP = ifelse(
      Two_For_One_Attempts > 0,
      Total_2For1_Sequence_Points / Two_For_One_Attempts,
      NA_real_
    ),
    
    .groups = "drop"
  ) %>%
  
  dplyr::filter(
    Two_For_One_Attempts > 0,
    !is.na(Sequence_PPP)
  ) %>%
  
  dplyr::mutate(
    Logo_Path = file.path(
      "images/team_logos",
      paste0(
        TEAM_ABBREV,
        ".png"
      )
    )
  ) %>%
  
  dplyr::filter(
    file.exists(Logo_Path)
  )

two_for_one_plot <- ggplot(
  two_for_one_df,
  aes(
    x = Two_For_One_Attempts,
    y = Sequence_PPP
  )
) +
  
  geom_image(
    aes(
      image = Logo_Path,
      x = Two_For_One_Attempts + ifelse(TEAM_ABBREV == "DAL", -0.05, 0),
      y = Sequence_PPP + ifelse(TEAM_ABBREV == "DAL", -0.05, 0)
    ),
    size = 0.13,
    asp = 1
  ) +
  
  labs(
    title = "Which Teams Get the Most Out of 2-for-1 Sequences?",
    subtitle = "Comparing two-for-one volume with points generated per opportunity (2025–26)",
    x = "2-for-1 Attempts",
    y = "Points per 2-for-1 Sequence",
    caption = "Source: NBA.com via nba_api"
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 3, by = 0.3),
    labels = scales::number_format(
      accuracy = 0.1
    )
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
    
    panel.grid.minor = element_blank(),
    
    axis.line = element_line(
      color = "#666666",
      linewidth = 0.5
    ),
    
    plot.margin = margin(
      10,
      20,
      10,
      10
    ),
    
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
    
    axis.title = element_text(
      size = 40,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 36
    )
  )

two_for_one_plot


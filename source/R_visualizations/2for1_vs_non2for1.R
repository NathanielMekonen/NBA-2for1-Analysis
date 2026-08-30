library(tidyverse)
library(showtext)
library(sysfonts)
library(grid)
library(ggimage)

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

# Create groups
two_for_one <- df %>%
  filter(TWO_FOR_ONE_ATTEMPT == 1)

non_two_for_one <- df %>%
  filter(NO_TWO_FOR_ONE_ATTEMPT == 1)

# Calculate Two-for-One statistics
two_stats <- c(
  
  paste0(
    sum(two_for_one$FIRST_SHOT_RESULT == "Made", na.rm = TRUE),
    "/",
    sum(
      two_for_one$FIRST_SHOT_RESULT %in% c("Made", "Missed"),
      na.rm = TRUE
    )
  ),
  
  sprintf(
    "%.1f%%",
    sum(two_for_one$FIRST_SHOT_RESULT == "Made", na.rm = TRUE) /
      sum(
        two_for_one$FIRST_SHOT_RESULT %in% c("Made", "Missed"),
        na.rm = TRUE
      ) * 100
  ),
  
  sprintf(
    "%.2f",
    sum(two_for_one$POSS_PTS, na.rm = TRUE) /
      nrow(two_for_one)
  ),
  
  sprintf(
    "%.1f%%",
    sum(
      two_for_one$`TWO_FOR_ONE_SECOND_POSSESSION?` == 1,
      na.rm = TRUE
    ) /
      nrow(two_for_one) * 100
  ),
  
  paste0(
    sum(
      two_for_one$SECOND_POSSESSION_SHOT_RESULT == "Made",
      na.rm = TRUE
    ),
    "/",
    sum(
      two_for_one$SECOND_POSSESSION_SHOT_RESULT %in% c("Made", "Missed"),
      na.rm = TRUE
    )
  ),
  
  sprintf(
    "%.1f%%",
    sum(
      two_for_one$SECOND_POSSESSION_SHOT_RESULT == "Made",
      na.rm = TRUE
    ) /
      sum(
        two_for_one$SECOND_POSSESSION_SHOT_RESULT %in% c("Made", "Missed"),
        na.rm = TRUE
      ) * 100
  ),
  
  sprintf(
    "%.2f",
    sum(
      two_for_one$SECOND_POSSESSION_PTS[
        two_for_one$`TWO_FOR_ONE_SECOND_POSSESSION?` == 1
      ],
      na.rm = TRUE
    ) /
      sum(
        two_for_one$`TWO_FOR_ONE_SECOND_POSSESSION?` == 1,
        na.rm = TRUE
      )
  ),
  
  sprintf(
    "%.2f",
    sum(
      two_for_one$TOTAL_PTS_ON_ALL_2FOR1_POSS,
      na.rm = TRUE
    ) /
      nrow(two_for_one)
  )
)

# Calculate Non Two-for-One statistics
non_two_stats <- c(
  
  paste0(
    sum(
      non_two_for_one$FIRST_SHOT_RESULT == "Made",
      na.rm = TRUE
    ),
    "/",
    sum(
      non_two_for_one$FIRST_SHOT_RESULT %in% c("Made", "Missed"),
      na.rm = TRUE
    )
  ),
  
  sprintf(
    "%.1f%%",
    sum(
      non_two_for_one$FIRST_SHOT_RESULT == "Made",
      na.rm = TRUE
    ) /
      sum(
        non_two_for_one$FIRST_SHOT_RESULT %in% c("Made", "Missed"),
        na.rm = TRUE
      ) * 100
  ),
  
  sprintf(
    "%.2f",
    sum(non_two_for_one$POSS_PTS, na.rm = TRUE) /
      nrow(non_two_for_one)
  ),
  
  sprintf(
    "%.1f%%",
    sum(
      non_two_for_one$`NON_TWO_FOR_ONE_SECOND_POSSESSION?` == 1,
      na.rm = TRUE
    ) /
      nrow(non_two_for_one) * 100
  ),
  
  paste0(
    sum(
      non_two_for_one$SECOND_POSSESSION_SHOT_RESULT == "Made",
      na.rm = TRUE
    ),
    "/",
    sum(
      non_two_for_one$SECOND_POSSESSION_SHOT_RESULT %in% c("Made", "Missed"),
      na.rm = TRUE
    )
  ),
  
  sprintf(
    "%.1f%%",
    sum(
      non_two_for_one$SECOND_POSSESSION_SHOT_RESULT == "Made",
      na.rm = TRUE
    ) /
      sum(
        non_two_for_one$SECOND_POSSESSION_SHOT_RESULT %in% c("Made", "Missed"),
        na.rm = TRUE
      ) * 100
  ),
  
  sprintf(
    "%.2f",
    sum(
      non_two_for_one$SECOND_POSSESSION_PTS[
        non_two_for_one$`NON_TWO_FOR_ONE_SECOND_POSSESSION?` == 1
      ],
      na.rm = TRUE
    ) /
      sum(
        non_two_for_one$`NON_TWO_FOR_ONE_SECOND_POSSESSION?` == 1,
        na.rm = TRUE
      )
  ),
  
  sprintf(
    "%.2f",
    sum(
      non_two_for_one$TOTAL_PTS_ON_ALL_2FOR1_POSS,
      na.rm = TRUE
    ) /
      nrow(non_two_for_one)
  )
)

# Create display data
scorecard <- tibble(
  
  Statistic = c(
    "First Shot FG",
    "First Shot FG%",
    "First Poss. PPP",
    "Second Poss. Rate",
    "Second Shot FG",
    "Second Shot FG%",
    "Second Poss. PPP",
    "Points per Sequence"
  ),
  
  Two_for_One = c(
    two_stats[1],
    two_stats[2],
    two_stats[3],
    two_stats[4],
    two_stats[5],
    two_stats[6],
    two_stats[7],
    two_stats[8]
  ),
  
  Non_Two_for_One = c(
    non_two_stats[1],
    non_two_stats[2],
    non_two_stats[3],
    non_two_stats[4],
    non_two_stats[5],
    non_two_stats[6],
    non_two_stats[7],
    non_two_stats[8]
  ),
  
  row = c(8, 7, 6, 5, 4, 3, 2, 1)
)

# Determine which side has the advantage
scorecard <- scorecard %>%
  mutate(
    Two_numeric = case_when(
      Statistic %in% c(
        "First Shot FG",
        "Second Shot FG"
      ) ~ as.numeric(
        str_extract(Two_for_One, "^[0-9]+")
      ),
      
      TRUE ~ as.numeric(
        str_remove(Two_for_One, "%$")
      )
    ),
    
    Non_numeric = case_when(
      Statistic %in% c(
        "First Shot FG",
        "Second Shot FG"
      ) ~ as.numeric(
        str_extract(Non_Two_for_One, "^[0-9]+")
      ),
      
      TRUE ~ as.numeric(
        str_remove(Non_Two_for_One, "%$")
      )
    ),
    
    Advantage = case_when(
      Two_numeric > Non_numeric ~ "Two-for-One",
      Non_numeric > Two_numeric ~ "Non Two-for-One",
      TRUE ~ "Tie"
    )
  )

# Create triangle coordinates
triangle_data <- scorecard %>%
  filter(Advantage != "Tie") %>%
  rowwise() %>%
  mutate(
    x_tip = if_else(
      Advantage == "Two-for-One",
      3.55,
      6.45
    ),
    x_base = if_else(
      Advantage == "Two-for-One",
      3.75,
      6.25
    )
  ) %>%
  ungroup()

triangle_points <- bind_rows(
  triangle_data %>%
    transmute(
      row,
      x = x_tip,
      y = row
    ),
  triangle_data %>%
    transmute(
      row,
      x = x_base,
      y = row + 0.11
    ),
  triangle_data %>%
    transmute(
      row,
      x = x_base,
      y = row - 0.11
    )
)

# Get player images
player_images <- tibble(
  x = c(0, 9.7),
  y = c(1.2, 1),
  image = c(
    "images/booker.png",
    "images/shai.png"
  )
)

two_x <- 2.3
label_x <- 5
non_x <- 7.7

# Create scorecard
p <- ggplot() +
  
  # Background
  annotate(
    "rect",
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 11,
    fill = "#FCFBF8"
  ) +
  
  geom_image(
    data = player_images,
    aes(
      x = x,
      y = y,
      image = image
    ),
    size = 0.575
  ) +
  
  # Main title
  annotate(
    "text",
    x = 5,
    y = 10.5,
    label = "How Effective is the 2-for-1?",
    family = "Roboto",
    fontface = "bold",
    size = 18
  ) +
  
  # Subtitle
  annotate(
    "text",
    x = 5,
    y = 9.95,
    label = "NBA League-Wide Stats (2025-26)",
    family = "Roboto",
    size = 10,
    color = "#666666"
  ) +
  
  # Column headers
  annotate(
    "text",
    x = two_x,
    y = 9.25,
    label = "2-for-1",
    family = "Roboto",
    fontface = "bold",
    size = 12,
    color = "#0B6E4F"
  ) +
  
  annotate(
    "text",
    x = non_x,
    y = 9.25,
    label = "Non 2-for-1",
    family = "Roboto",
    fontface = "bold",
    size = 12,
    color = "#555555"
  ) + 
  
  # Divider
  annotate(
    "segment",
    x = 0.8,
    xend = 9.2,
    y = 8.9,
    yend = 8.9,
    linewidth = 0.5,
    color = "#CCCCCC"
  ) +
  
  # Two-for-One values
  geom_text(
    data = scorecard,
    aes(
      x = two_x,
      y = row,
      label = Two_for_One
    ),
    family = "Roboto",
    fontface = "bold",
    size = 12,
    color = "#0B6E4F"
  ) +
  
  geom_text(
    data = scorecard %>%
      filter(Statistic == "Second Poss. PPP"),
    aes(
      x = two_x,
      y = row,
      label = Two_for_One
    ),
    family = "Roboto",
    fontface = "bold",
    size = 12,
    color = "#444444"
  ) +
  
  # Non Two-for-One values
  geom_text(
    data = scorecard,
    aes(
      x = non_x,
      y = row,
      label = Non_Two_for_One
    ),
    family = "Roboto",
    fontface = "bold",
    size = 12,
    color = "#444444"
  ) +
  
  geom_text(
    data = scorecard,
    aes(
      x = label_x,
      y = row,
      label = Statistic
    ),
    family = "Roboto",
    fontface = "bold",
    size = 8,
    color = "#555555"
  ) +
  
  # Divider between first possession and second possession stats
  geom_segment(
    aes(
      x = 1.4,
      xend = 8.6,
      y = 4.5,
      yend = 4.5
    ),
    linetype = "dotted",
    color = "#999999",
    linewidth = 0.5
  ) +
  
  # Divider before final total
  geom_segment(
    aes(
      x = 1.4,
      xend = 8.6,
      y = 1.5,
      yend = 1.5
    ),
    linetype = "dotted",
    color = "#999999",
    linewidth = 0.5
  ) +
  
  # Advantage triangles
  geom_polygon(
    data = triangle_points,
    aes(
      x = x,
      y = y,
      group = row
    ),
    fill = "#0B6E4F",
    color = "#0B6E4F"
  ) +
  
  # Theme
  coord_cartesian(
    xlim = c(0, 10),
    ylim = c(0.5, 11),
    clip = "off"
  ) +
  
  theme_void(
    base_family = "Roboto"
  ) +
  
  theme(
    plot.background = element_rect(
      fill = "#FCFBF8",
      color = "#FCFBF8"
    ),
    
    panel.background = element_rect(
      fill = "#FCFBF8",
      color = "#FCFBF8"
    ),
    
    plot.margin = margin(
      15,
      20,
      15,
      20
    )
  )

p

ggsave(
  filename = "images/2for1_final_stats.png",
  plot = p,
  width = 6,
  height = 5,
  dpi = 300,
  bg = "#FCFBF8"
)
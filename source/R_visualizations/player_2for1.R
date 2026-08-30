library(tidyverse)
library(scales)
library(showtext)
library(sysfonts)
library(base64enc)
library(gt)
library(gtExtras)

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

# Correct B. Williams player image
df <- df %>%
  mutate(
    Images = if_else(
      FIRST_SHOT_PLAYER == "B. Williams",
      "https://cdn.nba.com/headshots/nba/latest/1040x760/1630314.png",
      Images
    )
  )

# Create player-level 2-for-1 data
player_df <- df %>%
  dplyr::filter(
    TWO_FOR_ONE_ATTEMPT == 1,
    !is.na(Player),
    FIRST_SHOT_RESULT %in% c("Made", "Missed")
  ) %>%
  dplyr::mutate(
    FIRST_SHOT_DISTANCE_NUM = as.numeric(
      str_extract(
        as.character(FIRST_SHOT_DISTANCE),
        "\\d+"
      )
    )
  )

# Calculate player-level statistics
player_summary <- player_df %>%
  dplyr::group_by(Player) %>%
  dplyr::summarise(
    TWO_FOR_ONE_SHOTS = n(),
    MAKES = sum(FIRST_SHOT_RESULT == "Made"),
    MISSES = sum(FIRST_SHOT_RESULT == "Missed"),
    FG_PCT = MAKES / TWO_FOR_ONE_SHOTS,
    AVG_DISTANCE = mean(
      FIRST_SHOT_DISTANCE_NUM,
      na.rm = TRUE
    ),
    TEAM = first(TEAM),
    PLAYER_IMAGE = first(Images),
    TEAM_LOGO = first(logo),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    TWO_FOR_ONE_SHOT_RECORD = paste0(
      MAKES,
      "/",
      TWO_FOR_ONE_SHOTS
    )
  ) %>%
  dplyr::arrange(desc(TWO_FOR_ONE_SHOTS)) %>%
  dplyr::slice_head(n = 10)

# Create combined image field
player_summary <- player_summary %>%
  dplyr::mutate(
    Player_Image = paste0(
      TEAM_LOGO,
      "|||",
      PLAYER_IMAGE
    )
  )

# Maximum average distance used to scale the bars
max_distance <- max(
  player_summary$AVG_DISTANCE,
  na.rm = TRUE
)

# Create GT table
top_2for1_table <- player_summary %>%
  
  dplyr::select(
    Player_Image,
    Player,
    TEAM,
    TWO_FOR_ONE_SHOT_RECORD,
    FG_PCT,
    AVG_DISTANCE
  ) %>%
  
  gt() %>%
  
  # Font
  opt_table_font(
    font = list(
      google_font("Roboto")
    )
  ) %>%
  
  # Player image and team logo
  text_transform(
    locations = cells_body(columns = Player_Image),
    fn = function(x) {
      
      sapply(x, function(val) {
        
        parts <- strsplit(
          val,
          "\\|\\|\\|"
        )[[1]]
        
        logo <- parts[1]
        image <- parts[2]
        
        # Embed team logo
        logo_base64 <- base64enc::dataURI(
          file = logo,
          mime = "image/svg+xml"
        )
        
        # Embed player image
        image_base64 <- base64enc::dataURI(
          file = image,
          mime = "image/png"
        )
        
        htmltools::HTML(
          paste0(
            
            "<div style='position:relative;",
            "width:50px;",
            "height:50px;'>",
            
            # Team logo behind player
            "<img src='",
            logo_base64,
            "' style='position:absolute;",
            "left:50%;",
            "transform:translateX(-50%);",
            "top:-15px;",
            "height:75px;",
            "opacity:0.50;'>",
            
            # Player headshot
            "<img src='",
            image_base64,
            "' style='position:absolute;",
            "left:50%;",
            "transform:translateX(-50%);",
            "top:-5px;",
            "height:60px;'>",
            
            "</div>"
          )
        )
      })
    }
  ) %>%
  
  # Format FG%
  fmt_percent(
    columns = FG_PCT,
    decimals = 1
  ) %>%
  
  # Average distance orange bars
  text_transform(
    locations = cells_body(columns = AVG_DISTANCE),
    fn = function(x) {
      
      sapply(x, function(val) {
        
        distance <- as.numeric(val)
        
        bar_width <- (
          distance /
            max_distance
        ) * 100
        
        htmltools::HTML(
          paste0(
            
            "<div style='
              display:flex;
              align-items:center;
              height:22px;
              width:140px;
            '>",
            
            # Bar container
            "<div style='
              width:100px;
              height:14px;
              position:relative;
            '>",
            
            # Orange bar
            "<div style='
              position:absolute;
              left:0;
              top:0;
              width:",
            bar_width,
            "%;
              height:14px;
              background-color:#BA4A1E;
              border-radius:2px;
            '></div>",
            
            "</div>",
            
            # Distance value
            "<span style='
              font-family:Roboto;
              font-size:13px;
              font-weight:normal;
              white-space:nowrap;
              margin-left:-",
            100 - bar_width,
            "px;
            '>",
            
            sprintf(
              "%.1f ft",
              distance
            ),
            
            "</span>",
            
            "</div>"
          )
        )
      })
    }
  ) %>%
  
  # Column labels
  cols_label(
    Player_Image = "",
    Player = "Player",
    TEAM = "Team",
    TWO_FOR_ONE_SHOT_RECORD = "Makes / Shots",
    FG_PCT = "FG%",
    AVG_DISTANCE = "Avg. Distance per Shot"
  ) %>%
  
  # Center all columns
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  
  # Left align player names
  cols_align(
    align = "left",
    columns = Player
  ) %>%
  
  # Left align average distance header
  tab_style(
    style = cell_text(
      align = "left"
    ),
    locations = cells_column_labels(
      columns = AVG_DISTANCE
    )
  ) %>%
  
  # Header
  tab_header(
    title = "Who Took the Most 2-for-1 Shots?",
    subtitle = "Top 10 players by 2-for-1 shot attempts (2025–26)"
  ) %>%
  
  # Column header styling
  tab_style(
    style = cell_text(
      weight = "bold",
      size = px(12)
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Title styling
  tab_style(
    style = cell_text(
      weight = "bold",
      size = px(26)
    ),
    locations = cells_title(
      groups = "title"
    )
  ) %>%
  
  # Subtitle styling
  tab_style(
    style = cell_text(
      weight = "normal",
      size = px(16)
    ),
    locations = cells_title(
      groups = "subtitle"
    )
  ) %>%
  
  # Bold player names
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_body(
      columns = Player
    )
  ) %>%
  
  # Row striping
  opt_row_striping(
    row_striping = TRUE
  ) %>%
  
  # Source
  tab_source_note(
    source_note = "Source: NBA.com via nba_api"
  ) %>%
  
  # Source styling
  tab_style(
    style = cell_text(
      size = px(14),
      style = "italic"
    ),
    locations = cells_source_notes()
  ) %>%
  
  # Column widths
  cols_width(
    Player_Image ~ px(40),
    Player ~ px(90),
    TEAM ~ px(55),
    TWO_FOR_ONE_SHOT_RECORD ~ px(55),
    FG_PCT ~ px(55),
    AVG_DISTANCE ~ px(70)
  ) %>%
  
  # Table options
  tab_options(
    heading.align = "left",
    table.background.color = "#FCFBF8",
    heading.background.color = "#FCFBF8",
    row.striping.include_table_body = TRUE,
    row.striping.background_color = "#FFFFFF",
    heading.title.font.size = 22,
    heading.subtitle.font.size = 12,
    table.width = pct(80),
    data_row.padding = px(4)
  )

top_2for1_table

gtsave(
  top_2for1_table,
  "images/2for1_by_player.png"
)
# Create Execution+ Shiny App

# load libraries ----
library(tidyverse)
library(showtext)
library(httr)
library(magick)
library(grid)
library(gridExtra)
library(jsonlite)
library(here)
library(zoo)
library(scales)
library(shiny)
library(rsconnect)
library(baseballr)

# set plotting preferences ----
base_size <- 12 * 1.5

custom_theme <- theme_minimal(base_family = "sans", base_size = base_size) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 12)
  )

theme_set(custom_theme)

pitch_colors <- tribble(
  ~code, ~color, ~name,
  "FF", "#D22D49", "4-Seam Fastball",
  "FA", "#888", "Fastball",
  "SI", "#FE9d00", "Sinker",
  "FC", "#933F2C", "Cutter",
  "CH", "#1DBE3A", "Changeup",
  "FS", "#3BACAC", "Splitter",
  "CS", "#0068FF", "Slow Curve",
  "CU", "#00D1ED", "Curveball",
  "EP", "#888", "Eephus",
  "FO", "#55CCAB", "Forkball",
  "KC", "#6236CD", "Knuckle Curve",
  "PO", "#888", "Pitchout",
  "SC", "#60DB33", "Screwball",
  "SL", "#EEE716", "Slider",
  "ST", "#DDB33A", "Sweeper",
  "SV", "#93AFD4", "Slurve"
) |> 
  mutate(y = row_number())

dict_color <- setNames(pitch_colors$color, pitch_colors$code)
dict_pitch <- setNames(pitch_colors$name, pitch_colors$code)

# load pitcher map ----
pitcher_map <- fg_fielder_leaders(startseason = 2025, endseason = 2025) |>
  janitor::clean_names() |>
  filter(pos == "P" & inn > 0) |>
  select(x_mlbamid, player_name)

# existing functions ----
player_headshot <- function(pitcher_id) {
  
  headshot_url <- paste0(
    "https://img.mlbstatic.com/mlb-photos/image/",
    "upload/d_people:generic:headshot:67:current.png",
    "/w_640,q_auto:best/v1/people/", pitcher_id, "/headshot/silo/current"
  )
  
  img <- try(image_read(headshot_url), silent = TRUE)
  
  if (inherits(img, "try-error") || length(img) == 0) {
    warning("Headshot image could not be loaded.")
    return(nullGrob())
  }
  
  info <- image_info(img)
  aspect_ratio <- info$width / info$height
  
  # Build a child viewport with fixed aspect ratio, then use full grob size
  vp <- viewport(
    width = unit(1, "npc"),
    height = unit(1 / aspect_ratio, "npc"),
    just = "center",
    name = "image_vp"
  )
  
  # Use the fixed viewport inside the grob
  grobTree(
    rasterGrob(img),
    vp = vp
  )
}

player_info <- function(pitcher_id) {
  url <- paste0(
    "https://statsapi.mlb.com/api/v1/people?personIds=",
    pitcher_id,
    "&hydrate=currentTeam"
  )
  
  response <- GET(url)
  data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  player_name <- data$people$fullName
  pitcher_hand <- data$people$pitchHand$code
  age <- data$people$currentAge
  height <- data$people$height
  weight <- data$people$weight
  
  g <- grobTree(
    textGrob(player_name, y = 0.95, gp = gpar(fontsize = 26, fontface = "bold")),
    textGrob(paste0(pitcher_hand, "HP, Age: ", age, ", ", height, "/", weight),
             y = 0.70, gp = gpar(fontsize = 16)),
    textGrob("Execution+ Season Summary", y = 0.45, gp = gpar(fontsize = 20)),
    textGrob("2025 Regular Season", y = 0.20, gp = gpar(fontsize = 16, fontface = "italic"))
  )
  
  return(g)
}

mlb_teams <- tribble(
  ~team, ~logo_url,
  "AZ", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/ari.png&h=500&w=500",
  "ATH", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/ath.png&h=500&w=500",
  "ATL", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/atl.png&h=500&w=500",
  "BAL", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/bal.png&h=500&w=500",
  "BOS", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/bos.png&h=500&w=500",
  "CHC", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/chc.png&h=500&w=500",
  "CHW", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/chw.png&h=500&w=500",
  "CIN", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/cin.png&h=500&w=500",
  "CLE", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/cle.png&h=500&w=500",
  "COL", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/col.png&h=500&w=500",
  "DET", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/det.png&h=500&w=500",
  "HOU", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/hou.png&h=500&w=500",
  "KC", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/kc.png&h=500&w=500",
  "LAA", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/laa.png&h=500&w=500",
  "LAD", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/lad.png&h=500&w=500",
  "MIA", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/mia.png&h=500&w=500",
  "MIL", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/mil.png&h=500&w=500",
  "MIN", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/min.png&h=500&w=500",
  "NYM", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/nym.png&h=500&w=500",
  "NYY", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/nyy.png&h=500&w=500",
  "PHI", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/phi.png&h=500&w=500",
  "PIT", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/pit.png&h=500&w=500",
  "SD", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/sd.png&h=500&w=500",
  "SF", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/sf.png&h=500&w=500",
  "SEA", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/sea.png&h=500&w=500",
  "STL", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/stl.png&h=500&w=500",
  "TB", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/tb.png&h=500&w=500",
  "TEX", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/tex.png&h=500&w=500",
  "TOR", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/tor.png&h=500&w=500",
  "WSH", "https://a.espncdn.com/combiner/i?img=/i/teamlogos/mlb/500/scoreboard/wsh.png&h=500&w=500"
)

player_logo <- function(pitcher_id) {
  url <- paste0("https://statsapi.mlb.com/api/v1/people?personIds=", pitcher_id, "&hydrate=currentTeam")
  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  if (data$people$currentTeam$id < 108 | data$people$currentTeam$id > 160) {
    url_team <- paste0("https://statsapi.mlb.com/api/v1/teams/", data$people$currentTeam$parentOrgId)
  } else {
    url_team <- paste0("https://statsapi.mlb.com/", data$people$currentTeam$link)
  }
  response_team <- GET(url_team)
  data_team <- fromJSON(content(response_team, "text", encoding = "UTF-8"))
  
  team_abb <- data_team$teams$abbreviation
  logo_url <- mlb_teams$logo_url[mlb_teams$team == team_abb]
  img <- try(image_read(logo_url), silent = TRUE)
  
  if (inherits(img, "try-error") || length(img) == 0) {
    warning("Team logo could not be loaded.")
    return(nullGrob())
  }
  
  grob <- rasterGrob(img)
  return(grob)
}

velocity_kde <- function(predictions, df_statcast_grouped, pitch_colors) {
  items_in_order <- predictions |> 
    count(pitch_type, sort = TRUE) |> 
    pull(pitch_type)
  
  means_pred <- predictions |> 
    group_by(pitch_type) |> 
    summarize(mean_velocity = mean(release_speed, na.rm = TRUE),
              .groups = "drop") |> 
    filter(pitch_type %in% items_in_order)
  
  means_group <- df_statcast_grouped |> 
    group_by(pitch_type) |> 
    summarize(mean_velocity_group = mean(release_speed, na.rm = TRUE),
              .groups = "drop") |> 
    filter(pitch_type %in% items_in_order)
  
  plot_df <- predictions |> 
    filter(pitch_type %in% items_in_order) |> 
    mutate(pitch_type = factor(pitch_type, levels = items_in_order))
  
  means_all <- means_pred |> 
    left_join(means_group, by = "pitch_type")
  
  kde <- plot_df |> 
    ggplot(aes(x = release_speed, fill = pitch_type, color = pitch_type)) +
    geom_density(alpha = 0.4) +
    geom_vline(data = means_all, aes(xintercept = mean_velocity, color = pitch_type), linetype = "dashed") +
    geom_vline(data = means_all, aes(xintercept = mean_velocity_group, color = pitch_type), linetype = "dotted") +
    scale_color_manual(values = dict_color) +
    scale_fill_manual(values = dict_color) +
    facet_wrap(~pitch_type, ncol = 1, strip.position = "right", scales = "free_y") +
    labs(title = "Pitch Velocity Distribution", x = "Velocity (mph)", y = NULL) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_text(hjust = 1),
      panel.spacing.y = unit(1.2, "lines")
    )
  
  return(kde)
}

execution_plus_bar <- function(predictions) {
  summ_execution_plus <- predictions |> 
    group_by(pitch_type) |> 
    summarize(execution_plus = round(mean(pitch_grade, na.rm = TRUE)),
              .groups = "drop")
  
  usage_df <- predictions |> 
    count(pitch_type) |> 
    mutate(percent = n/sum(n)*100) |> 
    left_join(summ_execution_plus, by = "pitch_type") |> 
    arrange(desc(percent)) |> 
    mutate(pitch_type = factor(pitch_type, levels = pitch_type))
  
  bar_chart <- usage_df |> 
    ggplot(aes(x = pitch_type, y = percent, fill = pitch_type)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = execution_plus,
                  y = ifelse(percent < 5, percent + 1.5, percent - 1.5)),
              color = ifelse(usage_df$percent < 5, "black", "white"),
              size = 4,
              fontface = "bold",
              hjust = 0.5) +
    scale_fill_manual(values = dict_color) +
    labs(
      title = "Pitches by Usage and Execution+",
      x = NULL,
      y = "% Used"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      panel.grid.major.x = element_blank()
    )
  
  return(bar_chart)
}

rolling_execution_plus <- function(predictions) {
  grouped_pred <- predictions |> 
    group_by(game_pk, game_date, pitch_type) |> 
    summarize(execution_plus = mean(pitch_grade, na.rm = TRUE),
              .groups = "drop")
  
  all_combinations <- expand_grid(
    game_pk = unique(grouped_pred$game_pk),
    pitch_type = unique(grouped_pred$pitch_type)
  )
  
  df_distinct <- predictions |> 
    distinct(game_pk, game_date)
  
  complete_pred <- all_combinations |> 
    left_join(grouped_pred, by = c("game_pk", "pitch_type")) |> 
    left_join(df_distinct, by = "game_pk") |> 
    distinct(game_pk, pitch_type, execution_plus, game_date = game_date.y)
  
  game_order <- predictions |> 
    arrange(game_date) |> 
    distinct(game_pk, game_date) |> 
    mutate(game_number = row_number())
  
  complete_pred <- complete_pred |> 
    left_join(game_order, by = c("game_pk", "game_date")) |> 
    arrange(game_date, pitch_type)
  
  pitch_levels <- predictions |> 
    count(pitch_type, sort = TRUE) |> 
    pull(pitch_type)
  
  roll_pred <- complete_pred |> 
    group_by(pitch_type) |> 
    arrange(game_number) |> 
    mutate(rolling_eplus = zoo::rollmean(execution_plus, k = 5, fill = NA, align = "right")) |> 
    ungroup() |> 
    filter(!is.na(rolling_eplus)) |> 
    mutate(pitch_type = factor(pitch_type, levels = pitch_levels)) 
  
  roll <- roll_pred |> 
    ggplot(aes(x = game_number, y = rolling_eplus, color = pitch_type)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = dict_color) +
    scale_x_continuous(breaks = pretty(roll_pred$game_number)) +
    labs(
      title = "5 Game Rolling Execution+",
      x = "Game",
      y = "Execution+"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(roll)
}

rolling_pitch_usage <- function(predictions) {
  group_pred <- predictions |> 
    group_by(game_pk, game_date, pitch_type) |> 
    summarize(n = n(), .groups = "drop") |> 
    group_by(game_pk, game_date) |> 
    mutate(usage = n / sum(n)) |> 
    ungroup()
  
  all_combinations <- expand_grid(
    game_pk = unique(group_pred$game_pk),
    pitch_type = unique(group_pred$pitch_type)
  )
  
  complete_pred <- all_combinations |> 
    left_join(group_pred, by = c("game_pk", "pitch_type")) |> 
    mutate(usage = replace_na(usage, 0)) |> 
    left_join(predictions |> distinct(game_pk, game_date),
              by = "game_pk") |> 
    distinct(game_pk, pitch_type, usage, game_date = game_date.y)
  
  game_order <- predictions |> 
    arrange(game_date) |> 
    distinct(game_pk, game_date) |> 
    mutate(game_number = row_number())
  
  complete_pred <- complete_pred |> 
    left_join(game_order, by = c("game_pk", "game_date")) |> 
    arrange(game_date, pitch_type)
  
  roll_pred <- complete_pred |> 
    group_by(pitch_type) |> 
    arrange(game_number) |> 
    mutate(rolling_usage = zoo::rollmean(usage, k = 5, fill = NA, align = "right")) |> 
    ungroup() |> 
    filter(!is.na(rolling_usage)) |> 
    mutate(pitch_type = factor(pitch_type, levels = predictions |> count(pitch_type, sort = TRUE) |> pull(pitch_type)))
  
  roll <- roll_pred |> 
    ggplot(aes(x = game_number, y = rolling_usage, color = pitch_type)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = dict_color) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = pretty(roll_pred$game_number)) +
    labs(
      title = "5 Game Rolling Pitch Usage",
      x = "Game",
      y = "Pitch Usage"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(roll)
}

aggregate_pred <- function(predictions) {
  group_pred <- predictions |> 
    group_by(pitch_type) |> 
    summarize(pitch = n(),
              execution_plus = mean(pitch_grade, na.rm = TRUE),
              release_speed = mean(release_speed, na.rm = TRUE),
              pfx_z = mean(pfx_z, na.rm = TRUE),
              pfx_x = mean(pfx_x, na.rm = TRUE),
              release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
              delta_run_exp = sum(delta_run_exp, na.rm = TRUE),
              swing = sum(swing, na.rm = TRUE),
              whiff = sum(whiff, na.rm = TRUE),
              in_zone = sum(in_zone, na.rm = TRUE),
              out_zone = sum(out_zone, na.rm = TRUE),
              chase = sum(chase, na.rm = TRUE),
              xwoba = mean(estimated_woba_using_speedangle, na.rm = TRUE)) |> 
    mutate(pitch_usage = pitch/sum(pitch),
           whiff_rate = whiff/swing,
           in_zone_rate = in_zone/pitch,
           chase_rate = chase/out_zone,
           delta_run_exp_per_100 = -delta_run_exp/pitch*100,
           color = dict_color[pitch_type]) |> 
    arrange(desc(pitch_usage))
  
  color_list <- group_pred$color
  
  plot_table_all <- tibble(
    pitch_type = "All",
    pitch = nrow(predictions),
    pitch_usage = 1,
    execution_plus = mean(predictions$pitch_grade, na.rm = TRUE),
    release_speed = NA_real_,
    pfx_z = NA_real_,
    pfx_x = NA_real_,
    release_spin_rate = NA_real_,
    delta_run_exp_per_100 = -sum(predictions$delta_run_exp, na.rm = TRUE)/nrow(predictions)*100,
    whiff_rate = sum(predictions$whiff, na.rm = TRUE)/sum(predictions$swing, na.rm = TRUE),
    in_zone_rate = sum(predictions$in_zone, na.rm = TRUE)/nrow(predictions),
    chase_rate = sum(predictions$chase, na.rm = TRUE)/sum(predictions$out_zone, na.rm = TRUE),
    xwoba = mean(predictions$estimated_woba_using_speedangle, na.rm = TRUE),
    color = NA_character_
  )
  
  plot_pred <- bind_rows(group_pred, plot_table_all)
  
  return(list(group_pred = plot_pred, color_list = color_list))
}

pitch_stats_list <- list(
  pitch = list(table_header = "Count", format = ".0f"),
  execution_plus = list(table_header = "Execution+", format = ".0f"),
  release_speed = list(table_header = "Velocity", format = ".1f"),
  pfx_z = list(table_header = "IVB", format = ".1f"),
  pfx_x = list(table_header = "HB", format = ".1f"),
  release_spin_rate = list(table_header = "Spin", format = ".0f"),
  xwoba = list(table_header = "xwOBA", format = ".3f"),
  pitch_usage = list(table_header = "Pitch%", format = ".1%"),
  whiff_rate = list(table_header = "Whiff%", format = ".1%"),
  in_zone_rate = list(table_header = "Zone%", format = ".1%"),
  chase_rate = list(table_header = "Chase%", format = ".1%"),
  delta_run_exp_per_100 = list(table_header = "RV/100", format = ".1f")
)

table_cols <- c(
  "pitch_usage",
  "execution_plus",
  "release_speed",
  "pfx_z",
  "pfx_x",
  "release_spin_rate",
  "delta_run_exp_per_100",
  "whiff_rate",
  "in_zone_rate",
  "chase_rate",
  "xwoba"
)

metric_table_format <- function(predictions) {
  group_pred <- predictions[, table_cols, drop = FALSE]
  
  for (col in names(group_pred)) {
    if (is.character(group_pred[[col]]) || is.factor(group_pred[[col]]))
      group_pred[[col]][is.na(group_pred[[col]])] <- "—"
  }
  
  for (col in table_cols) {
    if (col %in% names(pitch_stats_list)) {
      fmt <- pitch_stats_list[[col]]$format
      
      group_pred[[col]] <- sapply(group_pred[[col]], function(x) {
        if (is.numeric(x)) {
          if (fmt == ".0f") return(sprintf("%.0f", x))
          if (fmt == ".1f") return(sprintf("%.1f", x))
          if (fmt == ".3f") return(sprintf("%.3f", x))
          if (fmt == ".1%") return(paste0(sprintf("%.1f", x * 100), "%"))
        }
        return(x)
      })
    }
  }
  
  return(as_tibble(group_pred))
}

get_color <- function(value, normalize_range, cmap, n = 100) {
  if (is.na(value)) return("#FFFFFF")
  min_val <- normalize_range[1]
  max_val <- normalize_range[2]
  if (max_val == min_val || is.na(max_val) || is.na(min_val)) return("#FFFFFF")
  
  norm_val <- (value - min_val) / (max_val - min_val)
  norm_val <- min(max(norm_val, 0), 1)
  
  colors <- cmap(n)
  
  index <- floor(norm_val * (n - 1)) + 1
  colors[index]
}

get_cell_colors <- function(pred_df,
                            league_df,
                            color_stats,
                            cmap_sum,
                            cmap_sum_r,
                            table_cols_clean) {
  # Just return a matrix filled with white
  matrix(
    "#FFFFFF",
    nrow = nrow(pred_df),
    ncol = length(table_cols_clean),
    dimnames = list(pred_df$pitch_type, table_cols_clean)
  )
}

metric_table <- function(pred_df, 
                         league_df, 
                         color_stats = c("release_speed", "pfx_z", "pfx_x", "release_spin_rate",
                                         "delta_run_exp_per_100", "whiff_rate", "chase_rate",
                                         "in_zone_rate", "xwoba"), 
                         cmap_sum = colorRampPalette(c("#325aa1", "#FFFFFF", "#d82129")),
                         cmap_sum_r = colorRampPalette(c("#d82129", "#FFFFFF", "#325aa1")),
                         table_cols, pitch_stats_list, font_size = 20) {
  group_pred <- aggregate_pred(pred_df)
  plot_pred <- group_pred[[1]]
  
  formatted_pred <- metric_table_format(plot_pred)
  table_cols_clean <- intersect(names(formatted_pred), color_stats)
  
  formatted_pred_color <- formatted_pred[, table_cols_clean, drop = FALSE]
  
  if (length(table_cols_clean) == 0) {
    stop("No valid numeric columns to color in formatted_pred.")
  }
  
  color_matrix <- get_cell_colors(plot_pred, df_statcast_grouped, color_stats, cmap_sum, cmap_sum_r, table_cols_clean)
  
  stopifnot(nrow(plot_pred) == nrow(color_matrix))
  
  display_pred <- cbind(pitch_name = plot_pred$pitch_type, formatted_pred)
  
  pitch_col_vector <- left_join(
    tibble::tibble(pitch = plot_pred$pitch_type),
    pitch_colors,
    by = c("pitch" = "code")
  )$color
  
  pitch_col_vector[is.na(pitch_col_vector)] <- "#FFFFFF"
  
  n_color <- ncol(color_matrix)
  n_display <- ncol(display_pred)
  n_noncolor <- n_display - n_color - 1
  
  color_matrix_full <- cbind(
    pitch_col_vector,
    rep("#FFFFFF", nrow(color_matrix)),
    color_matrix,
    matrix("#FFFFFF", nrow = nrow(color_matrix), ncol = n_noncolor)
  )
  
  stopifnot(nrow(display_pred) == nrow(color_matrix_full))
  
  new_colnames <- c("Pitch Name", sapply(names(formatted_pred), function(x) {
    if (x %in% names(pitch_stats_list)) {
      pitch_stats_list[[x]]$table_header 
    } else {
      x
    }
  })
  )
  
  table_grob <- gridExtra::tableGrob(
    display_pred, 
    rows = NULL,
    cols = new_colnames,
    theme = gridExtra::ttheme_minimal(
      core = list(
        fg_params = list(fontsize = font_size),
        bg_params = list(fill = color_matrix_full, col = NA)
      ),
      col_head = list(fg_params = list(fontsize = font_size, fontface = "bold"))
    )
  )
  
  for (i in seq_len(nrow(display_pred))) {
    cell_id <- which(table_grob$layout$name == paste0("core-", i, "-1"))
    if (length(cell_id) == 1) {
      table_grob$grobs[[cell_id]]$gp <- grid::gpar(
        fontface = "bold", col = "#FFFFFF", fontsize = font_size
      )
    }
  }
  
  return(table_grob)
}

execution_plus_card <- function(pitcher_id, predictions = predictions) {
  color_stats = c("release_speed", "pfx_z", "pfx_x", "release_spin_rate",
                  "delta_run_exp_per_100", "whiff_rate", "chase_rate",
                  "in_zone_rate", "xwoba")
  
  cmap_sum = colorRampPalette(c("#325aa1", "#FFFFFF", "#d82129"))
  cmap_sum_r = colorRampPalette(c("#d82129", "#FFFFFF", "#325aa1"))
  
  df_statcast_grouped <- predictions |>
    filter(!is.na(pitch_type)) |>
    group_by(pitch_type) |>
    summarize(pitch = n(),
              release_speed = mean(release_speed, na.rm = TRUE),
              pfx_z = mean(pfx_z, na.rm = TRUE),
              pfx_x = mean(pfx_x, na.rm = TRUE),
              release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
              release_pos_x = mean(release_pos_x, na.rm = TRUE),
              release_pos_z = mean(release_pos_z, na.rm = TRUE),
              release_extension = mean(release_extension, na.rm = TRUE),
              delta_run_exp = mean(delta_run_exp, na.rm = TRUE),
              swing = sum(swing, na.rm = TRUE),
              whiff = sum(whiff, na.rm = TRUE),
              in_zone = sum(in_zone, na.rm = TRUE),
              out_zone = sum(out_zone, na.rm = TRUE),
              chase = sum(chase, na.rm = TRUE),
              xwoba = mean(estimated_woba_using_speedangle, na.rm = TRUE)) |> 
    mutate(pitch_usage = pitch/sum(pitch),
           whiff_rate = whiff/sum(pitch),
           in_zone_rate = in_zone/sum(pitch),
           chase_rate = chase/sum(pitch),
           delta_run_exp_per_100 = (delta_run_exp*100)/sum(pitch))
  
  summary_row <- predictions |> 
    summarize(pitch = n(),
              release_speed = mean(release_speed, na.rm = TRUE),
              pfx_z = mean(pfx_z, na.rm = TRUE),
              pfx_x = mean(pfx_x, na.rm = TRUE),
              release_spin_rate = mean(release_spin_rate, na.rm = TRUE),
              release_pos_x = mean(release_pos_x, na.rm = TRUE),
              release_pos_z = mean(release_pos_z, na.rm = TRUE),
              release_extension = mean(release_extension, na.rm = TRUE),
              delta_run_exp = mean(delta_run_exp, na.rm = TRUE),
              swing = sum(swing, na.rm = TRUE),
              whiff = sum(whiff, na.rm = TRUE),
              in_zone = sum(in_zone, na.rm = TRUE),
              out_zone = sum(out_zone, na.rm = TRUE),
              chase = sum(chase, na.rm = TRUE),
              xwoba = mean(estimated_woba_using_speedangle, na.rm = TRUE)) |> 
    mutate(pitch_usage = pitch/sum(pitch),
           whiff_rate = whiff/sum(pitch),
           in_zone_rate = in_zone/sum(pitch),
           chase_rate = chase/sum(pitch),
           delta_run_exp_per_100 = (delta_run_exp*100)/sum(pitch)) |> 
    mutate(pitch_type = "All",
           .before = 1)
  
  df_statcast_grouped <- bind_rows(df_statcast_grouped, summary_row)
  
  grid.newpage()
  
  pushViewport(viewport(layout = grid.layout(
    nrow = 6,
    ncol = 8,
    widths = unit(c(1, rep(18, 6), 1), "null"),
    heights = unit(c(2, 20, 9, 36, 36, 7), "null")
  )))
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  grid.draw(editGrob(player_headshot(pitcher_id), vp = vplayout(2:3, 2:3)))
  grid.draw(editGrob(player_info(pitcher_id), vp = vplayout(2:3, 4:5)))
  grid.draw(editGrob(player_logo(pitcher_id), vp = vplayout(2:3, 6:7)))
  
  grid.draw(editGrob(ggplotGrob(execution_plus_bar(predictions)), vp = vplayout(4, 2:3)))
  grid.draw(editGrob(ggplotGrob(rolling_execution_plus(predictions)), vp = vplayout(4, 4:5)))
  grid.draw(editGrob(ggplotGrob(rolling_pitch_usage(predictions)), vp = vplayout(4, 6:7)))
  
  table_grob <- metric_table(
    predictions,
    league_df = df_statcast_grouped,
    font_size = 16,
    color_stats = color_stats,
    cmap_sum = cmap_sum,
    cmap_sum_r = cmap_sum_r,
    pitch_stats_list = pitch_stats_list,
    table_cols = table_cols
  )
  
  grid.draw(editGrob(table_grob, vp = viewport(layout.pos.row = 5, layout.pos.col = 2:7, width = unit(1, "npc"))))
  
  grid.text("By Leo Tesler", x = unit(0, "npc"), y = unit(0.8, "npc"), 
            just = c("left", "top"), gp = gpar(fontsize = 12), vp = vplayout(6, 2:7))
  grid.text("Data from Statcast, Images from MLB and ESPN", x = 1, y = 0.8, 
            just = c("right", "top"), gp = gpar(fontsize = 12), vp = vplayout(6, 2:7))
  
  popViewport()
}

# app code ----
ui <- fluidPage(
  titlePanel("Execution+ Pitching Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pitcher", "Select a Pitcher:",
                  choices = pitcher_map$player_name),
      actionButton("go", "Generate Summary")
    ),
    
    mainPanel(
      plotOutput("execCard", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  pitcher_id <- eventReactive(input$go, {
    pitcher_map |> 
      filter(player_name == input$pitcher) |> 
      pull(x_mlbamid) |> 
      as.character()
  })
  
  observeEvent(pitcher_id(), {
    print(pitcher_id())
  })
  
  predictions <- reactive({
    req(pitcher_id())
    file_path <- paste0("predictions/", pitcher_id(), ".rds")
    message("Attempting to load: ", file_path)
    readRDS(file_path) |> 
      mutate(game_date = as.Date(game_date))
  })
  
  
  df_statcast_grouped <- reactive({
    req(pitcher_id())
    readRDS("predictions/df_statcast_grouped.rds")
  })
  
  output$execCard <- renderPlot({
    req(pitcher_id())
    execution_plus_card(pitcher_id = pitcher_id(),
                        predictions = predictions())
  })
  
}

shinyApp(ui, server)
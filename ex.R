# get free agency data

# load libraries ----
library(tidyverse)
library(baseballr)
library(rvest)
library(httr)
library(jsonlite)
library(chromote)

# pull spotrac data ----
teams <- c("arizona-diamondbacks", "athletics", "atlanta-braves",
           "baltimore-orioles", "boston-red-sox", "chicago-cubs",
           "chicago-white-sox", "cincinnati-reds", "cleveland-guardians",
           "colorado-rockies", "detroit-tigers", "houston-astros",
           "kansas-city-royals", "los-angeles-angels", "los-angeles-dodgers",
           "miami-marlins", "milwaukee-brewers", "minnesota-twins",
           "new-york-mets", "new-york-yankees", "philadelphia-phillies",
           "pittsburgh-pirates", "san-diego-padres", "san-francisco-giants",
           "seattle-mariners", "st-louis-cardinals", "tampa-bay-rays",
           "texas-rangers", "toronto-blue-jays", "washington-nationals")

contract_data <- list()

b <- ChromoteSession$new()

for (team_name in teams) {
  url <- paste0("https://www.spotrac.com/mlb/", team_name, "/yearly/_/sort/cap_total2/view/roster")
  
  message("Loading ", team_name)
  b$Page$navigate(url)
  Sys.sleep(5)
  
  html <- b$DOM$getDocument()
  node <- b$DOM$getOuterHTML(nodeId = html$root$nodeId)
  page <- read_html(node$outerHTML)
  
  tables <- page |> 
    html_elements("table") |> 
    html_table(fill = TRUE) |> 
    keep(~ any(grepl("Player", names(.x))) && nrow(.x) > 0) |> 
    map(~ mutate(.x, across(everything(), as.character)))
  
  contract_status <- bind_rows(tables) |> 
    janitor::clean_names()
  
  contract_data[[team_name]] <- contract_status
}

contract_data <- bind_rows(contract_data)

# clean data ----
contract_clean <- contract_data |> 
  pivot_longer(cols = starts_with("player"), values_to = "player") |> 
  separate(player, into = c("delete", "player"), sep = "\\n\\n ") |> 
  select(!delete) |> 
  mutate(player = trimws(player)) |> 
  filter(!is.na(player)) |> 
  print(n = Inf)

# get free agent year ----
fill_ufa <- function(df) {
  n <- ncol(df)
  
  for (i in seq_len(n)) {
    this_col <- df[[i]]
    
    if (all(is.na(this_col) | this_col == "")) next
    
    for (row in seq_len(nrow(df))) {
      val <- this_col[row]
      if (is.na(val) || val == "") next
      
      # Skip if "UFA" already exists anywhere in the row
      if ("UFA" %in% df[row, ]) next
      
      # how far to look ahead?
      offset <- dplyr::case_when(
        val %in% c("ARB 3", "ARB 4", "\\$") ~ 1,
        val == "ARB 2" ~ 2,
        val == "ARB 1" ~ 3,
        val == "PRE-ARB" ~ 5,
        TRUE ~ 1  # default
      )
      
      j <- i + offset
      if (j <= n && (is.na(df[[j]][row]) || df[[j]][row] == "")) {
        df[[j]][row] <- "UFA"
      }
    }
  }
  
  df
}

contract_cleaner <- contract_clean |> 
  rowwise() |> 
  fill_ufa() |> 
  mutate(when_free_agent = names(contract_clean)[which(c_across(everything()) == "UFA")[1]]) |> 
  ungroup() |> 
  mutate(when_free_agent = if_else(is.na(when_free_agent), "x2040", when_free_agent),
         when_free_agent = as.numeric(str_replace(when_free_agent, "x", ""))) |> 
  select(player, when_free_agent) 

fg_data <- fg_batter_leaders(startseason = 2025, endseason = 2025) |> 
  select(id = xMLBAMID, player = PlayerNameRoute, player_secondary = PlayerName)

contract_cleaner |> 
  left_join(fg_data, by = join_by(player)) |> 
  left_join(fg_data, by = join_by(player == player_secondary))

# try to pull from bbref
url <- "https://www.baseball-reference.com/players/f/freelal01.shtml"

page <- read_html(url)

text <- page |> 
  html_nodes("p") |> 
  html_text(trim = TRUE)

node <- text[str_detect(text, "Free Agent")]

fa_year <- as.numeric(str_split_fixed(node, "Free Agent: ", 2)[,2])

pitchers <- fg_pitcher_leaders(startseason = 2025, endseason = 2025)

# explore ----
predictions |> 
  group_by(pitcher_name, pitch_type) |> 
  summarize(pitch_grade = mean(pitch_grade, na.rm = TRUE),
            n = n()) |> 
  filter(n >= 500, pitch_type == "FF") |> 
  arrange(desc(pitch_grade))

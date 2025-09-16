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
contract_clean |> 
  rowwise() |> 
  mutate(when_free_agent = names(contract_clean)[which(c_across(everything()) == "UFA")[1]]) |> 
  ungroup() |> 
  filter(is.na(when_free_agent)) |> 
  select(player, when_free_agent, x2025:x2039)

# explore ----
predictions |> 
  group_by(pitcher_name, pitch_type) |> 
  summarize(pitch_grade = mean(pitch_grade, na.rm = TRUE),
            n = n()) |> 
  filter(n >= 500, pitch_type == "FF") |> 
  arrange(desc(pitch_grade))


setwd("~/Documents/SSAC/Stanford Baseball/Building Database")

library(rvest)
library(purrr)
library(stringr)

getGameInfo <- function(page_source) {
  page <- read_html(page_source)
  tables <- html_nodes(page, "table")
  
  # Once we have webpage, extracting game info
  innings_table_indicies <- which(html_attr(tables, "class") == "sidearm-table play-by-play")
  innings_tables <- tables[innings_table_indicies]
  unique_innings_indicies <- which(!duplicated(sapply(innings_tables, extractInningName)))
  innings_tables <- innings_tables[unique_innings_indicies]
  
  game_info <- map_dfr(innings_tables, getInningInfo)
  # game_info <- getInningInfo(innings_tables[1])
  # for (abc in 1:length(innings_tables)) {
  #   game_info <- bind_rows(game_info, getInningInfo(innings_tables[abc]))
  # }
  
  if (sum(game_info$description == "No play.") > 0) {
    indicies <- which(game_info$description == "No play.")
    leading_indicies <- append(indicies[1], indicies[which(diff(indicies) != 1) + 1])
    first_pitches <- leading_indicies[which(game_info$first_pitch_of_inning[leading_indicies] == 1)]
    game_info <- game_info[-which(game_info$description == "No play."), ]
    if (length(first_pitches) > 0) {
      game_info$first_pitch_of_inning[first_pitches] <- 1
    }
  }
  if (sum(grepl("challenge", tolower(game_info$description))) > 0) {
    game_info <- game_info[-which(grepl("challenge", tolower(game_info$description))), ]  
  }
  if (sum(grepl("foul ball", tolower(game_info$description))) > 0) {
    game_info <- game_info[-which(grepl("foul ball", tolower(game_info$description))), ]
  }
  if (sum(grepl("review", tolower(game_info$description))) > 0) {
    game_info <- game_info[-which(grepl("review", tolower(game_info$description))), ]
  }
  if (sum(grepl("mound visit", tolower(game_info$description))) > 0) {
    game_info <- game_info[-which(grepl("mound visit", tolower(game_info$description))), ]
  }
  if (sum(grepl("ejected", tolower(game_info$description))) > 0) {
    game_info <- game_info[-which(grepl("ejected", tolower(game_info$description))), ]
  }
  if (sum(grepl("delay", tolower(game_info$description))) > 0) {
    game_info <- game_info[-which(grepl("delay", tolower(game_info$description))), ]
  }
  if (sum(grepl("moved in as the", tolower(game_info$description))) > 0) {
    game_info <- game_info[-which(grepl("moved in as the", tolower(game_info$description))), ]
  }
  if (sum(grepl("Fordham eliminated", game_info$description)) > 0) {
    game_info <- game_info[-which(grepl("Fordham eliminated", game_info$description)), ]
  }
  if (sum(grepl("Attendance", game_info$description)) > 0) {
    game_info <- game_info[-which(grepl("Attendance", game_info$description)), ]
  }
  if (sum(grepl("suspended", game_info$description)) > 0) {
    game_info <- game_info[-which(grepl("suspended", game_info$description)), ]
  }
  if (sum(grepl("was playing in 2B position during shift", game_info$description)) > 0) {
    game_info <- game_info[-which(grepl("was playing in 2B position during shift", game_info$description)), ]
  }
  if (sum(grepl("inherited", game_info$description)) > 0) {
    game_info <- game_info[-which(grepl("inherited", game_info$description)), ]
  }
  
  # get unique description id (one id for every event)
  game_info$unique_description_id <- getUniqueDescriptionID(game_info$description)
  
  # game_info <- getInningInfo(innings_tables[1])
  # for (i in 2:length(innings_tables)) {
  #   game_info <- bind_rows(game_info, getInningInfo(innings_tables[i]))
  # }
  
  # Adding Game Information
  h1s <- html_nodes(page, "h1")
  title <- h1s[which(html_attr(h1s, "class") == "text-center text-uppercase hide-on-medium-down")]
  game_title <- html_text(title)
  
  dds <- html_nodes(page, "dd")
  dds_text <- dds %>% html_text()
  date <- dds_text[grep("/20[0-2][0-9]", dds_text)]
  
  game_info$game_title <- game_title
  game_info$game_date <- date
  
  # Adding Runs Information
  score_table <- getScoreTable(page)
  visiting_team <- score_table$Team[1]
  home_team <- score_table$Team[2]
  visiting_team_final_score <- score_table$R[1]
  home_team_final_score <- score_table$R[2]
  
  game_info$visitor <- visiting_team
  game_info$home <- home_team
  game_info$visitor_score <- visiting_team_final_score
  game_info$home_score <- home_team_final_score
  game_info$home_win <- home_team_final_score > visiting_team_final_score
  
  unique_inning_names <- unique(game_info$inning_name)
  
  runs <- map_chr(unique_inning_names, getInningRuns, score_table) %>% as.double()
  # runs <- rep(NA, length(unique_inning_names))
  # for (i in 1:length(unique_inning_names)) {
  #   runs[i] <- getInningRuns(unique_inning_names[i], score_table)
  # }
  
  inning_runs_df <- data.frame("inning_name" = unique_inning_names,
                               "runs_in_inning" = runs)
  # runs_so_far are the number of runs scored in that inning so far at the point in time directly after 
  # the description occurs
  game_info <- left_join(game_info, inning_runs_df, by = "inning_name")
  
  # Adding Base State Information
  descriptions_base_states <- getDescriptionsBaseStates(game_info, innings_tables)
  game_info <- left_join(game_info, descriptions_base_states, by = "unique_description_id")
  
  base_states_pre <- getBaseStatePre(game_info$description, game_info$base_1_post,
                                     game_info$base_2_post, game_info$base_3_post,
                                     game_info$first_pitch_of_inning)
  game_info <- left_join(game_info, base_states_pre, by = "unique_description_id")
  game_info$base_1_pre[which(game_info$first_pitch_of_inning == 1)] <- NA
  game_info$base_2_pre[which(game_info$first_pitch_of_inning == 1)] <- NA
  game_info$base_3_pre[which(game_info$first_pitch_of_inning == 1)] <- NA
  
  final <- game_info %>%
    rename("event" = "description", "event_id" = "unique_description_id") %>%
    select(game_title, game_date, inning_name, first_pitch_of_inning, event_id, event, event_type, 
           walk_or_k, pitch_result, runs_in_inning, runs_in_inning_so_far_before_event,
           runs_in_inning_so_far_after_event, outs_in_inning_so_far_before_event,
           outs_in_inning_so_far_after_event, base_1_pre:base_3_pre, base_1_post:base_3_post,
           visitor, home, visitor_score, home_score, home_win)
  
  final_upd <- cleanBIPids(final)
  final_upd$order_id <- 1:nrow(final_upd)
  
  return (final_upd)
}

getInningInfo <- function(inning_table) {
  inning_name <- extractInningName(inning_table)
  descriptions <- extractDescriptions(inning_table)
  if (sum(grepl("weather delay", tolower(descriptions))) > 0) {
    descriptions <- descriptions[-which(grepl("weather delay", tolower(descriptions)))]
  }
  if (sum(grepl("review", tolower(descriptions))) > 0) {
    descriptions <- descriptions[-which(grepl("review", tolower(descriptions)))]
  }
  player_names_first_guess <- map_chr(descriptions, getPlayerName)
  if (sum(grepl(" ", player_names_first_guess)) > 0) {
    descriptions <- cleanPlayerNames(descriptions)
  }
  walk_or_k_index <- c(grep("walk", descriptions), grep("struck out", descriptions))
  if (length(descriptions) == 0) {
    # nothing happened during this inning
    blank_df <- data.frame("description" = NA, "pitch_result" = NA, "inning_name" = NA,
                       "walk_or_k" = NA, "first_pitch_of_inning" = NA,
                       "runs_in_inning_so_far_after_event" = NA,
                       "runs_in_inning_so_far_before_event" = NA,
                       "outs_in_inning_so_far_after_event" = NA,
                       "outs_in_inning_so_far_before_event" = NA,
                       "event_type" = NA)
    return (blank_df[-1, ])
  }
  info <- data.frame("description" = descriptions,
                     "inning_name" = rep(inning_name, length(descriptions)),
                     "walk_or_k" = FALSE)
  info$walk_or_k[walk_or_k_index] <- TRUE
  info$description <- as.character(info$description)
  pitch_results <- map2_dfr(info$description, info$walk_or_k, buildPitchResultsDF)
  
  info <- left_join(pitch_results, info, by = "description")
  
  info$first_pitch_of_inning <- 0
  info$first_pitch_of_inning[1] <- 1
  
  info$runs_in_inning_so_far_after_event <- getCurrentInningRuns(info$description)
  info$runs_in_inning_so_far_before_event <- getCurrentInningRunsPre(info$description,
                                                                     info$runs_in_inning_so_far_after_event)
  
  info$outs_in_inning_so_far_after_event <- getCurrentInningOuts(info$description)
  info$outs_in_inning_so_far_before_event <- getCurrentInningOutsPre(info$description,
                                                                     info$outs_in_inning_so_far_after_event)
  if (sum(grepl("challenge", tolower(info$description))) > 0) {
    info <- info[-which(grepl("challenge", tolower(info$description))), ]  
  }
  if (sum(grepl("foul ball", tolower(info$description))) > 0) {
    info <- info[-which(grepl("foul ball", tolower(info$description))), ]
  }
  if (sum(grepl("review", tolower(info$description))) > 0) {
    info <- info[-which(grepl("review", tolower(info$description))), ]
  }
  if (sum(grepl("mound visit", tolower(info$description))) > 0) {
    info <- info[-which(grepl("mound visit", tolower(info$description))), ]
  }
  info$event_type <- getUniqueMainOutcomes(info$description)
  
  return (info)
}

#### Inning Functions ####

extractInningName <- function(table_node) {
  text <- html_text(table_node)
  name <- strsplit(text, "\n")[[1]][1]
  if (length(grep("\t", name)) == 1) {
    name <- substr(name, 1, nchar(name) - 1)
  }
  return (name)
}

extractDescriptions <- function(table_node) {
  th_elems <- html_nodes(table_node, "th")
  descriptions <- th_elems[which(is.na(html_attr(th_elems, "class")))]
  return (html_text(descriptions))
}

extractPinchRunners <- function(table_node) {
  th_elems <- html_nodes(table_node, "th")
  subs <- th_elems[which(html_attr(th_elems, "class") == "text-italic")]
  return (html_text(subs)[grepl("pinch ran", html_text(subs))])
}

cleanPlayerNames <- function(descriptions) {
  # player names are of format: "T. Dillard"
  player_names <- map_chr(descriptions, getPlayerName)
  player_names_upd <- gsub(" ", "", player_names)
  for (iii in 1:length(descriptions)) {
    for (jjj in 1:length(player_names)) {
      descriptions[iii] <- gsub(player_names[jjj], player_names_upd[jjj], descriptions[iii])
    }
  }
  return (descriptions)
}

buildPitchResultsDF <- function(description, walk_or_k) {
  # description is the description
  # walk_or_k is TRUE if it's a walk or a strike out, FALSE if at bat ends on contact
  after_paren <- strsplit(description, "(", fixed = T)[[1]][2]
  before_close_paren <- strsplit(after_paren, ")", fixed = T)[[1]][1]
  pitch_seq <- strsplit(before_close_paren, " ")[[1]][2]
  if (is.na(pitch_seq)) {
    # the at bat ended on contact on the first pitch
    return (data.frame("description" = description, "pitch_result" = "X"))
  }
  df <- as.data.frame(matrix(ncol = 2, nrow = nchar(pitch_seq)))
  names(df) <- c("description", "pitch_result")
  df$description <- description
  for (i in 1:nchar(pitch_seq)) {
    pitch_result <- substr(pitch_seq, i, i)
    df$pitch_result[i] <- pitch_result
  }
  if (!walk_or_k) {
    df[nrow(df) + 1, ] <- c(description, "X")
  }
  return (df)
}

getCurrentInningRuns <- function(descriptions, first_pitch_of_inning) {
  unique_descriptions <- unique(descriptions)
  running_tally <- 0
  current_runs <- rep(NA, length(unique_descriptions))
  for (i in 1:length(unique_descriptions)) {
    running_tally <- running_tally + str_count(unique_descriptions[i], "scored") + str_count(unique_descriptions[i], "homered")
    current_runs[i] <- running_tally
  }
  descriptions_table <- data.frame("description" = descriptions)
  descriptions_runs <- data.frame("description" = unique_descriptions,
                                  "runs" = current_runs)
  joined <- left_join(descriptions_table, descriptions_runs, by = "description")
  return (joined$runs)
}

getCurrentInningRunsPre <- function(descriptions, runs_so_far_after_event) {
  descriptions_table <- data.frame("description" = descriptions)
  unique_descriptions <- descriptions[!duplicated(descriptions)]
  corresponding_runs <- runs_so_far_after_event[!duplicated(descriptions)]
  pre_runs <- c(0, corresponding_runs[-length(corresponding_runs)])
  pre_runs_table <- data.frame("description" = unique_descriptions,
                               "pre_runs" = pre_runs)
  joined <- left_join(descriptions_table, pre_runs_table, by = "description")
  return (joined$pre_runs)
}

getCurrentInningOuts <- function(descriptions) {
  # given the unique descriptions for a game, and whether that description is the first of a new inning,
  # this gives the descriptions and the number of outs 
  unique_descriptions <- unique(descriptions)
  running_tally <- 0
  current_outs <- rep(NA, length(unique_descriptions))
  for (i in 1:length(unique_descriptions)) {
    outs_recorded <- sum(grep("out", unique_descriptions[i]),
                         grep("popped", unique_descriptions[i]),
                         grep("flied", unique_descriptions[i]),
                         grep("lined", unique_descriptions[i]),
                         grep("grounded", unique_descriptions[i]))
    # if it was "____ out", then we've double counted
    excess_outs_recorded <- sum(grep("popped out", unique_descriptions[i]),
                                grep("flied out", unique_descriptions[i]),
                                grep("lined out", unique_descriptions[i]),
                                grep("grounded out", unique_descriptions[i]))
    outs_recorded <- outs_recorded - excess_outs_recorded
    
    running_tally <- running_tally + outs_recorded
    current_outs[i] <- running_tally
  }
  descriptions_table <- data.frame("description" = descriptions)
  unique_descriptions_outs_table <- data.frame("description" = unique_descriptions,
                                               "outs" = current_outs)
  joined <- left_join(descriptions_table, unique_descriptions_outs_table, by = "description")
  return (joined$outs)
}

getCurrentInningOutsPre <- function(descriptions, outs_so_far_after_event) {
  descriptions_table <- data.frame("description" = descriptions)
  unique_descriptions <- descriptions[!duplicated(descriptions)]
  corresponding_outs <- outs_so_far_after_event[!duplicated(descriptions)]
  pre_outs <- c(0, corresponding_outs[-length(corresponding_outs)])
  pre_outs_table <- data.frame("description" = unique_descriptions,
                               "pre_outs" = pre_outs)
  joined <- left_join(descriptions_table, pre_outs_table, by = "description")
  return (joined$pre_outs)
}

getUniqueMainOutcomes <- function(descriptions) {
  des_same_as_prev <- c(FALSE, descriptions[-1] == descriptions[-length(descriptions)])
  if (sum(des_same_as_prev) < 1) {
    unique_des <- descriptions
  } else {
    unique_des <- descriptions[-which(des_same_as_prev == TRUE)]
  }
  outcome <- rep(NA, length(unique_des))
  for (i in 1:length(unique_des)) {
    des <- unique_des[i]
    des_split <- strsplit(des, split = " ")[[1]]
    player_name <- getPlayerName(des)
    player_name_index <- which(des_split == player_name)
    k <- 1
    action <- des_split[player_name_index + k]
    while (substr(action, 1, 1) == toupper(substr(action, 1, 1))) {
      k <- k + 1
      action <- des_split[player_name_index + k]
    }
    action_clean <- gsub("[^[:alnum:] ]", "", action)    # retains only alpha-numeric chars
    outcome[i] <- action_clean
  }
  outcome[which(outcome == "struck")] <- "struck out"
  outcome[which(outcome == "grounded")] <- "grounded out"
  outcome[which(outcome == "hit")] <- "hit by pitch"
  outcome[which(outcome == "flied")] <- "flied out"
  outcome[which(outcome == "lined")] <- "lined out"
  outcome[which(outcome == "fouled")] <- "fouled out"
  
  unique_des_outcomes <- data.frame("description" = unique_des, "main_outcome" = outcome)
  description_table <- data.frame("description" = descriptions)
  joined <- left_join(description_table, unique_des_outcomes, by = "description")
  
  return (joined$main_outcome)
}

#### Game Functions ####

getScoreTable <- function(page) {
  tables <- html_nodes(page, "table")
  tables_classes <- html_attr(tables, "class")
  score_table <- html_table(tables[which(tables_classes == "sidearm-table")])[[1]]
  score_table$Team <- lapply(strsplit(score_table$Team, "\t"), rev) %>% map(1) %>% unlist()
  return (score_table)
}

extractInningTeamNumber <- function(inning_name) {
  team <- strsplit(inning_name, " - ")[[1]][1]
  num_raw <- lapply(strsplit(inning_name, " "), rev)[[1]][1]
  num <- substr(num_raw, 1, nchar(num_raw) - 2)
  return (c("team" = team, "num" = num))
}

getInningRuns <- function(inning_name, score_table) {
  team_number <- extractInningTeamNumber(inning_name)
  runs <- score_table[which(score_table$Team == team_number[1]), 
                      which(names(score_table) == team_number[2])]
  return (runs)
}

getChangesInVector <- function(vector) {
  # given a character vector, this returns a binary every time that vector changes
  # example: getChanges(c("a", "a", "a", "b", "b")) ==> c(1, 0, 0, 1, 0)
  factor_vector <- as.factor(vector)
  unique_indicator_vector <- as.numeric(factor_vector)
  diffs <- c(1, diff(unique_indicator_vector))
  diffs[which(diffs != 0)] <- 1
  return (diffs)
}

getPlayerName <- function(player_description) {
  # check if the word starts with an uppercase letter
  player_description_split <- strsplit(player_description, " ")[[1]]
  i <- 1
  while (i <= length(player_description_split)) {
    player_name <- player_description_split[i]
    if (player_name == "" | substr(player_name, 1, 1) != toupper(substr(player_name, 1, 1))) {
      # If the word doesn't start with a capital letter, keep looking
      i <- i + 1
    } else {
      break
    }
  }
  
  if (nchar(player_name) == 2 & substr(player_name, 2, 2) == ".") {
    # Then name is of format: "T. Dillard" 
    player_name <- paste(player_name, player_description_split[i + 1])
  }
  
  return (player_name)
}

getPlayerBaseContribution <- function(player_description) {
  # given the description of what a specific player does 
  # (ie the part of description between a semi-colon)
  # this returns the base that they ended up at
  # c(NA, NA, NA) if they scored or were out
  # c("Jeff", NA, NA) if first, c(NA, "Jeff", NA) if second, c(NA, NA, "Jeff") if third
  
  player_name <- getPlayerName(player_description)
  
  if (sum(length(grep("scored", player_description)),
          length(grep("homered", player_description)),
          length(grep("advanced to home", player_description)),
          length(grep("stole home", player_description))) > 0) {
    # if they scored, they aren't on first, second, or third
    return (c(NA_character_, NA_character_, NA_character_))
  }
  
  if (sum(length(grep("out", player_description)),
          length(grep("popped", player_description)),
          length(grep("flied", player_description)),
          length(grep("lined", player_description)),
          length(grep("grounded", player_description)),
          length(grep("infield fly", player_description))) > 0) {
    # if they're out, they aren't on first, second, or third
    # EXCEPTION: "Ty Johnson struck out swinging, reached first on a passed ball"
    if (grepl("struck out", player_description) & grepl("reached first", player_description)) {
      return (c(player_name, NA_character_, NA_character_))
    }
    
    return (c(NA_character_, NA_character_, NA_character_))
  }
  
  # at this point, we can assume they're on first, second, or third
  location_in_vector <- c("first" = as.double(str_locate(player_description, "first")[, 1]),
                          "second" = as.double(str_locate(player_description, "second")[, 1]),
                          "third" = as.double(str_locate(player_description, "third")[, 1]))
  if (sum(!is.na(location_in_vector)) == 0 |  (length(grep("base", player_description)) == 1)) {
    # then they didn't end up at a weird base 
    if (sum(length(grep("single", player_description)),
            length(grep("reached", player_description)),
            length(grep("walked", player_description)),
            length(grep("hit by pitch", player_description))) > 0) {
      if (is.na(location_in_vector[1]) & (!is.na(location_in_vector[2]) |
                                          !is.na(location_in_vector[3]))) {
        if (!is.na(location_in_vector[2])) {
          return (c(NA, player_name, NA))
        } else {
          return (c(NA, NA, player_name))
        }
      }
      return (c(player_name, NA, NA))
    } else if (length(grep("double", player_description)) > 0) {
      return (c(NA, player_name, NA))
    } else if (length(grep("triple", player_description)) > 0) {
      return (c(NA, NA, player_name))
    } else {
      stop (paste0("I'm confused: I'm not sure which base ", player_name, " ended up at"))
    }
  } 
  # then they did end up at a weird base (meaning that base is stated explicitly)
  contribution_to_base_state <- c(NA, NA, NA)
  contribution_to_base_state[which.max(location_in_vector)] <- player_name
  return (contribution_to_base_state)
}

getReplacedRunner <- function(pinch_runner_des) {
  replaced_raw <- substr(pinch_runner_des, str_locate(pinch_runner_des, "for ")[2] + 1, nchar(pinch_runner_des))
  if (substr(replaced_raw, nchar(replaced_raw), nchar(replaced_raw)) == ".") {
    replaced_raw <- substr(replaced_raw, 1, nchar(replaced_raw) - 1)
  }
  replaced <- getPlayerName(replaced_raw)
  if (grepl(" ", replaced)) {
    replaced <- gsub(" ", "", replaced)
  }
  return (replaced)
}

getNewRunner <- function(pinch_runner_des) {
  new <- getPlayerName(pinch_runner_des)
  if (grepl(" ", new)) {
    new <- gsub(" ", "", new)
  }
  return(new)
}

getBaseStatePost <- function(description, base_state_pre, new_inning = F, pinch_runner_des) {
  if (length(pinch_runner_des) > 0) {
    replaced <- getReplacedRunner(pinch_runner_des[1])
    new <- getNewRunner(pinch_runner_des[1])
    if (length(pinch_runner_des) > 1) {
      for (jjj in 2:length(pinch_runner_des)) {
        replaced <- append(replaced, getReplacedRunner(pinch_runner_des[jjj]))
        new <- append(new, getNewRunner(pinch_runner_des[jjj]))
      }
    }
  }
  if (new_inning) {
    base_state_pre <- rep(NA_character_, 3)
  }
  description <- as.character(description)
  players_on <- base_state_pre[which(!is.na(base_state_pre))]
  base_state <- base_state_pre
  if (length(pinch_runner_des) > 0) {
    replaced_runner_on_base <- sum(replaced %in% players_on) > 0
    replaced_runner_in_des <- FALSE
    for (replaced_runner in replaced) {
      if (grepl(replaced_runner, gsub(" ", "", description))) {
        replaced_runner_in_des <- TRUE
        break
      }
    }
    if (replaced_runner_on_base == TRUE & replaced_runner_in_des == FALSE) {
      replaced_in_this_case <- replaced[which(replaced %in% players_on)]
      players_on[which(players_on == replaced_in_this_case)] <- new[which(replaced == replaced_in_this_case)]
      base_state[which(base_state == replaced_in_this_case)] <- new[which(replaced == replaced_in_this_case)]
    }
  }
  players_on_mentioned <- F
  if (length(players_on) > 0) {
    for (ii in 1:length(players_on)) {
      if (length(grep(substr(players_on[ii], 3, nchar(players_on[ii])), description)) > 0) {
        players_on_mentioned <- T
        break
      }
    }
  }
  # at this point, if any of the players on base are mentioned, players_on_mentioned is true
  description_split <- cleanPlayerNames(strsplit(description, ";")[[1]])
  player_names <- map_chr(description_split, getPlayerName)
  players_handled <- c()
  # first, see what happens to all the players who are on base
  if (length(players_on) > 0 & players_on_mentioned) {
    for (i in 1:length(players_on)) {
      player_name <- players_on[i]
      players_handled <- append(players_handled, player_name)
      player_specific_description <- description_split[which(player_names == player_name)]
      if (length(player_specific_description) == 0) {
        # then the player is still where they were, base_state is unchanged
      } else {
        # there is some description, but they didn't advance if it includes "no advance"
        if (grepl("no advance", player_specific_description)) {
          # then the player is still where they were, base_state is unchanged
        } else {
          base_state[which(base_state == player_name)] <- NA_character_     # removing player from base state
          base_state <- coalesce(getPlayerBaseContribution(player_specific_description), base_state)
        }
      }
    }
  }
  
  # then, see what happens to the batter (the only person left in player_names)
  if (length(players_handled) > 0) {
    player_remaining <- player_names[-which(player_names %in% players_handled)]
    if (length(player_remaining) == 0) {
      # this occurs when the batter doesn't do anything
      return (base_state)
    }
  } else {
    # if no players were handled, then the only player in player_names is the batter
    player_remaining <- player_names
  }
  base_state <- coalesce(base_state, 
                         getPlayerBaseContribution(description_split[which(player_names == player_remaining)]))
  return (base_state)
}

getDescriptionsBaseStates <- function(game_info, innings_tables) {
  des_and_first_pitch <- select(game_info, description, first_pitch_of_inning)
  same_as_previous_des <- c(FALSE, des_and_first_pitch$description[-1] == des_and_first_pitch$description[-length(des_and_first_pitch$description)])
  if (sum(same_as_previous_des) < 1) {
    unique_descriptions <- des_and_first_pitch
  } else {
    unique_descriptions <- des_and_first_pitch[-which(same_as_previous_des == T), ]
  }
  unique_descriptions$description <- as.character(unique_descriptions$description)
  unique_descriptions$base_1 <- as.character(NA_character_)
  unique_descriptions$base_2 <- as.character(NA_character_)
  unique_descriptions$base_3 <- as.character(NA_character_)
  
  for (j in 1:nrow(unique_descriptions)) {
    des <- unique_descriptions$description[j]
    new_inning_tf <- unique_descriptions$first_pitch_of_inning[j]
    if (j == 1) {
      base_state_pre <- c(NA_character_, NA_character_, NA_character_)
    } else {
      base_state_pre <- c(unique_descriptions$base_1[j - 1],
                          unique_descriptions$base_2[j - 1],
                          unique_descriptions$base_3[j - 1])
    }
    # get inning so that we can account for pinch runners
    innings <- unique(game_info$inning_name)
    inning_name <- game_info$inning_name[which(game_info$description == des)]
    inning_num <- which(innings == inning_name)
    inning_table <- innings_tables[inning_num]
    pinch_runner_des <- extractPinchRunners(inning_table)
    
    base_state_post <- getBaseStatePost(des, base_state_pre, new_inning_tf, pinch_runner_des)
    unique_descriptions$base_1[j] <- as.character(base_state_post[1])
    unique_descriptions$base_2[j] <- as.character(base_state_post[2])
    unique_descriptions$base_3[j] <- as.character(base_state_post[3])
  }
  
  unique_descriptions <- unique_descriptions %>%
    rename("base_1_post" = "base_1", "base_2_post" = "base_2", "base_3_post" = "base_3") %>%
    select(-first_pitch_of_inning)
  
  unique_descriptions$unique_description_id <- getUniqueDescriptionID(unique_descriptions$description)
  unique_descriptions <- select(unique_descriptions, -description)
  
  return (unique_descriptions)
}

getBaseStatePre <- function(descriptions, base_1_post, base_2_post, base_3_post,
                            first_pitch_of_inning) {
  same_as_prev_des <- c(FALSE, descriptions[-1] == descriptions[-length(descriptions)])
  if (sum(same_as_prev_des) < 1) {
    unique_des <- descriptions
    corresponding_b1 <- base_1_post
    corresponding_b2 <- base_2_post
    corresponding_b3 <- base_3_post
    first_pitch <- first_pitch_of_inning
  } else {
    unique_des <- descriptions[-which(same_as_prev_des == TRUE)]
    corresponding_b1 <- base_1_post[-which(same_as_prev_des == TRUE)]
    corresponding_b2 <- base_2_post[-which(same_as_prev_des == TRUE)]
    corresponding_b3 <- base_3_post[-which(same_as_prev_des == TRUE)]
    first_pitch <- first_pitch_of_inning[-which(same_as_prev_des == TRUE)]
  }
  b1_pre <- c(NA, corresponding_b1[-length(corresponding_b1)])
  b2_pre <- c(NA, corresponding_b2[-length(corresponding_b2)])
  b3_pre <- c(NA, corresponding_b3[-length(corresponding_b3)])
  
  b1_pre[first_pitch] <- NA
  b2_pre[first_pitch] <- NA
  b3_pre[first_pitch] <- NA
  
  base_pre_df <- data.frame("description" = unique_des, "base_1_pre" = b1_pre, 
                     "base_2_pre" = b2_pre, "base_3_pre" = b3_pre)
  base_pre_df$unique_description_id <- getUniqueDescriptionID(base_pre_df$description)
  return (select(base_pre_df, -description))
}

cleanBIPids <- function(game_info_final) {
  # x_des <- game_info_final$event[which(game_info_final$pitch_result == "X")]
  foul_out_events <- game_info_final$event[grepl("fouled out", game_info_final$event)] %>% unique()
  if (length(foul_out_events) == 0) {
    return (game_info_final)
  }
  for (k in 1:length(foul_out_events)) {
    event_df_upd <- cleanFoulOuts(foul_out_events[k], game_info_final)
    old_event_rows <- which(game_info_final$event == foul_out_events[k])
    # event_df_upd is never more rows than the length of old event rows
    game_info_final[old_event_rows, ] <- NA
    game_info_final$pitch_result <- as.character(game_info_final$pitch_result)
    game_info_final[old_event_rows[1]:(old_event_rows[1] + nrow(event_df_upd) - 1), ] <- event_df_upd
  }
  
  if (sum(is.na(game_info_final$game_title)) > 0) {
    game_info_final <- game_info_final[-which(is.na(game_info_final$game_title)), ]
  }
  game_info_final$pitch_result[which(game_info_final$event_type == "advanced" | 
                                       game_info_final$event_type == "stole")] <- "R"
  return (game_info_final)
}

cleanFoulOuts <- function(foul_out_event, game_info_final) {
  foul_out_df <- game_info_final[which(game_info_final$event == foul_out_event), ]
  foul_out_df$pitch_result <- as.character(foul_out_df$pitch_result)
  if (foul_out_df$pitch_result[nrow(foul_out_df)] == "X") {
    if (nrow(foul_out_df) > 1) {
      foul_out_df <- foul_out_df[-nrow(foul_out_df), ]
    } 
  }
  foul_out_df$pitch_result[nrow(foul_out_df)] <- "FO"
  return (foul_out_df)
}

getUniqueDescriptionID <- function(descriptions) {
  des_same_as_prev <- c(FALSE, descriptions[-1] == descriptions[-length(descriptions)])
  des_new <- !des_same_as_prev
  return (cumsum(des_new))
}


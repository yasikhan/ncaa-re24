
setwd("~/Documents/SSAC/Stanford Baseball/Basis Stats")
library(dplyr)
game_log_folder <- "~/Documents/SSAC/Stanford Baseball/Building Database/database/"

master <- bind_rows(readRDS(paste0(game_log_folder, "play_by_play_2019.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2018.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2017.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2016.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2015.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2014.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2013.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2012.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2011.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2010.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2009.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2008.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2007.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2005.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2004.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2003.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2002.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2001.rds")),
                    readRDS(paste0(game_log_folder, "play_by_play_2000.rds")))

master$base_1_pre_occupied <- "1"
master$base_2_pre_occupied <- "2"
master$base_3_pre_occupied <- "3"
master$base_1_pre_occupied[which(is.na(master$base_1_pre))] <- "_"
master$base_2_pre_occupied[which(is.na(master$base_2_pre))] <- "_"
master$base_3_pre_occupied[which(is.na(master$base_3_pre))] <- "_"
master$base_state_pre <- paste(master$base_1_pre_occupied, master$base_2_pre_occupied, master$base_3_pre_occupied)
master$runs_in_rest_of_inning <- master$runs_in_inning - master$runs_in_inning_so_far_before_event


#### Attempt 1: Empirical ####

getRunExpectancyTable <- function(log) {
  table <- log %>%
    group_by(outs_in_inning_so_far_before_event, base_state_pre) %>%
    summarize("n" = n(), "exp_runs" = mean(runs_in_rest_of_inning), 
              "prob_0_runs" = mean(runs_in_rest_of_inning == 0), 
              "prob_1_runs" = mean(runs_in_rest_of_inning == 1), 
              "prob_2_runs" = mean(runs_in_rest_of_inning == 2), 
              "prob_3_runs" = mean(runs_in_rest_of_inning == 3), 
              "prob_4_runs" = mean(runs_in_rest_of_inning == 4), 
              "prob_5_plus_runs" = mean(runs_in_rest_of_inning >= 5)) %>%
    rename("outs" = outs_in_inning_so_far_before_event,
           "base_runners" = base_state_pre) %>%
    filter(outs < 3)
  return (table)
}

run_expectancy_table <- getRunExpectancyTable(master)
View(run_expectancy_table)

#### Attempt 2: Ordered Logit ####

# todo: try with run categories up to 10+ instead up to 5+

master_upd <- master[-which(master$runs_in_rest_of_inning < 0), ]
master_upd$base_1_occupied_pre <- !is.na(master_upd$base_1_pre)
master_upd$base_2_occupied_pre <- !is.na(master_upd$base_2_pre)
master_upd$base_3_occupied_pre <- !is.na(master_upd$base_3_pre)

runs_in_rest_of_inning_factor <- master_upd$runs_in_rest_of_inning
runs_in_rest_of_inning_factor[which(runs_in_rest_of_inning_factor >= 5)] <- 5
runs_in_rest_of_inning_factor <- factor(runs_in_rest_of_inning_factor,
                                        levels = c(0, 1, 2, 3, 4, 5))
master_upd$runs_in_rest_of_inning_factor <- runs_in_rest_of_inning_factor

library(MASS)

model <- polr(runs_in_rest_of_inning_factor ~ base_1_occupied_pre + 
                base_2_occupied_pre + base_3_occupied_pre + 
                outs_in_inning_so_far_before_event, data = master_upd)
fake_test_data <- data.frame("base_1_occupied_pre" = rep(c(F, T, F, F, T, T, F, T), 3),
                             "base_2_occupied_pre" = rep(c(F, F, T, F, T, F, T, T), 3),
                             "base_3_occupied_pre" = rep(c(F, F, F, T, F, T, T, T), 3),
                             "outs_in_inning_so_far_before_event" = rep(c(0, 1, 2), each = 8))
table <- cbind(fake_test_data, predict(model, fake_test_data, type = "p"))
table$run_exp <- 0 * table$`0` + 1 * table$`1` + 2 * table$`2` + 3 * table$`3` + 4 * table$`4` + 5 * table$`5`

# Predictors: is.na(base_1_pre), is.na(base_2_pre), is.na(base_3_pre), outs_before
# Response: runs_in_rest_of_inning

#### Attempt 3: Markov Chain ####

# investigate whether the distribution of outcomes is significantly different across base state
# 
table(master$event_type[which(master$outs_in_inning_so_far_before_event == 0 &
                                master$base_1_pre_occupied == "_" &
                                master$base_2_pre_occupied == "_" &
                                master$base_3_pre_occupied == "_")])



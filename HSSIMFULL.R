rm(list=ls())

library(dplyr);library(readr);library(tensorflow)


num.sim <-200000
full_results <- data.frame(match_result = rep(NA, num.sim), my_ban = rep(NA, num.sim),
                           opp_ban = rep(NA, num.sim), first_queue = rep(NA, num.sim), game_result_1 =rep(NA, num.sim))
Hearthstone_MU_Data <- read_csv("matchup data - wrs.csv")

for (i in 1:num.sim){
  Matchup_Sim <- function(my_decks=c("Enrage Warrior", "Malygos Druid", "Soul Demon Hunter", "Highlander Priest"), 
                          opp_decks=c("Bomb Warrior", "Stealth Rogue", "Malygos Druid", "Soul Demon Hunter")){
    n1 <- sample.int(4, 1)
    n2 <- sample.int(4, 1)
    decks <- list()
    original_decks <- list()
    post_ban_decks <- list()
    banned_decks <- list()
    post_ban_decks$my <- my_decks[-n1]
    post_ban_decks$opp <- opp_decks[-n2]
    original_decks$my <- my_decks
    original_decks$opp <- opp_decks
    banned_decks$my <- my_decks[n1]
    banned_decks$opp <- opp_decks[n2]
    upcoming_matchup <- list()
    unused_decks <- post_ban_decks
    game_5_result <- list()
    game_4_result <- list()
    game_3_result <- list()
    game_2_result <- list()
    game_1_result <- list()
    game_5_result$my <- "tbd"
    game_5_result$opp <- "tbd"
    game_4_result$my <- "tbd"
    game_4_result$opp <- "tbd"
    game_3_result$my <- "tbd"
    game_3_result$opp <- "tbd"
    game_2_result$my <- "tbd"
    game_2_result$opp <- "tbd"
    game_1_result$my <- "tbd"
    game_1_result$opp <- "tbd"
    decks <- list(original_decks=original_decks, 
                  post_ban_decks=post_ban_decks, 
                  banned_decks=banned_decks, 
                  upcoming_matchup=upcoming_matchup, 
                  unused_decks=unused_decks,
                  game_1_result=game_1_result,
                  game_2_result=game_2_result,
                  game_3_result=game_3_result,
                  game_4_result=game_4_result,
                  game_5_result=game_5_result)
    return(decks)
  }
  
  decks <- Matchup_Sim()
  
  Game_1_Matchup <- function(decks){
    n1 <- sample.int(3, 1)
    n2 <- sample.int(3, 1)
    my_deck_1 <- decks$post_ban_decks$my[n1]
    opp_deck_1 <- decks$post_ban_decks$opp[n2]
    decks$unused_decks$my <- decks$unused_decks$my[-n1]
    decks$unused_decks$opp <- decks$unused_decks$opp[-n2]
    decks$upcoming_matchup$my <- my_deck_1
    decks$upcoming_matchup$opp <- opp_deck_1
    decks$first_queue <- my_deck_1
    return(decks)
  }
  
  decks <- Game_1_Matchup(decks)
  
  Game_Result_1 <- function(decks, matchup_stats=Hearthstone_MU_Data){
    result <- matchup_stats %>% filter(player_archetype==decks$upcoming_matchup$my, 
                                       opponent_archetype==decks$upcoming_matchup$opp) %>% mutate(win = 1*(runif(1)<win_rate/100)) %>% select(win) %>% as.numeric()
    decks$game_1_result$my <- NA
    decks$game_1_result$opp <- NA
    if(result>0){
      decks$game_1_result$my = "win"; decks$game_1_result$opp = "loss";
      decks$upcoming_matchup$opp = NA
    }
    else{
      decks$game_1_result$my = "loss"; decks$game_1_result$opp = "win";
      decks$upcoming_matchup$my = NA
    }
    
    
    return(decks) 
  }
  
  decks <- Game_Result_1(decks)
  
  Game_Matchup_2 <- function(decks, matchup_stats = Hearthstone_MU_Data){
    
    if(decks$game_1_result$my=="win"){
      decks$upcoming_matchup$my = decks$upcoming_matchup$my; 
      decks$upcoming_matchup$opp = matchup_stats %>% 
        filter(player_archetype==decks$upcoming_matchup$my, 
               opponent_archetype %in% decks$unused_decks$opp) %>%
        top_n(1, desc(win_rate)) %>% select(opponent_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$opp==decks$upcoming_matchup$opp)
      decks$unused_decks$opp <- decks$unused_decks$opp[left.over]
    }
    else{
      decks$upcoming_matchup$opp = decks$upcoming_matchup$opp; 
      decks$upcoming_matchup$my = matchup_stats %>% 
        filter(player_archetype %in% decks$unused_decks$my, 
               opponent_archetype==decks$upcoming_matchup$opp) %>%
        top_n(1, win_rate) %>% select(player_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$my==decks$upcoming_matchup$my)
      decks$unused_decks$my <- decks$unused_decks$my[left.over]
    }
    return(decks)
  }
  
  decks <- Game_Matchup_2(decks)
  
  Game_Result_2 <- function(decks, matchup_stats=Hearthstone_MU_Data){
    result <- matchup_stats %>% 
      filter(player_archetype==decks$upcoming_matchup$my, 
             opponent_archetype==decks$upcoming_matchup$opp) %>% 
      mutate(win = 1*(runif(1)<win_rate/100)) %>% select(win) %>% as.numeric()
    decks$game_2_result$my <- NA
    decks$game_2_result$opp <- NA
    if(result>0){
      decks$game_2_result$my = "win"; decks$game_2_result$opp = "loss";
      decks$upcoming_matchup$opp = NA
    }
    else{
      decks$game_2_result$my = "loss"; decks$game_2_result$opp = "win"
      decks$upcoming_matchup$my = NA
    }
    
    
    return(decks) 
  }
  
  decks <- Game_Result_2(decks)
  
  Game_Matchup_3 <- function(decks, matchup_stats = Hearthstone_MU_Data){
    
    if(decks$game_2_result$my=="win"){
      decks$upcoming_matchup$my = decks$upcoming_matchup$my; 
      decks$upcoming_matchup$opp = matchup_stats %>% filter(player_archetype==decks$upcoming_matchup$my, 
                                                            opponent_archetype %in% decks$unused_decks$opp) %>% top_n(1, desc(win_rate)) %>% select(opponent_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$opp==decks$upcoming_matchup$opp)
      decks$unused_decks$opp <- decks$unused_decks$opp[left.over]
    }
    else{
      decks$upcoming_matchup$opp = decks$upcoming_matchup$opp; 
      decks$upcoming_matchup$my = matchup_stats %>% filter(player_archetype %in% decks$unused_decks$my, 
                                                           opponent_archetype==decks$upcoming_matchup$opp) %>% top_n(1, win_rate) %>% select(player_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$my==decks$upcoming_matchup$my)
      decks$unused_decks$my <- decks$unused_decks$my[left.over]
    }
    return(decks)
  }
  
  decks <- Game_Matchup_3(decks)
  
  Game_Result_3 <- function(decks, matchup_stats=Hearthstone_MU_Data){
    result <- matchup_stats %>% filter(player_archetype==decks$upcoming_matchup$my, 
                                       opponent_archetype==decks$upcoming_matchup$opp) %>% mutate(win = 1*(runif(1)<win_rate/100)) %>% select(win) %>% as.numeric()
    decks$game_3_result$my <- NA
    decks$game_3_result$opp <- NA
    if(result>0){
      decks$game_3_result$my = "win"; decks$game_3_result$opp = "loss";
      decks$upcoming_matchup$opp = NA
    }
    else{
      decks$game_3_result$my = "loss"; decks$game_3_result$opp = "win"
      decks$upcoming_matchup$my = NA
    }
    
    
    return(decks) 
  }
  
  decks <- Game_Result_3(decks)
  
  
  Game_Matchup_4 <- function(decks, matchup_stats = Hearthstone_MU_Data){
    
    if(decks$game_3_result$my=="win"){
      decks$upcoming_matchup$my = decks$upcoming_matchup$my; 
      decks$upcoming_matchup$opp = matchup_stats %>% 
        filter(player_archetype==decks$upcoming_matchup$my, 
               opponent_archetype %in% decks$unused_decks$opp) %>%
        top_n(1, desc(win_rate)) %>% select(opponent_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$opp==decks$upcoming_matchup$opp)
      decks$unused_decks$opp <- decks$unused_decks$opp[left.over]
    }
    else{
      decks$upcoming_matchup$opp = decks$upcoming_matchup$opp; 
      decks$upcoming_matchup$my = matchup_stats %>% 
        filter(player_archetype %in% decks$unused_decks$my, 
               opponent_archetype==decks$upcoming_matchup$opp) %>%
        top_n(1, win_rate) %>% select(player_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$my==decks$upcoming_matchup$my)
      decks$unused_decks$my <- decks$unused_decks$my[left.over]
    }
    return(decks)
  }
  
  tryCatch(decks <- Game_Matchup_4(decks))
  
  Game_Result_4 <- function(decks, matchup_stats=Hearthstone_MU_Data){
    decks$match_result <- NA
    if(decks$upcoming_matchup$my == "character(0)"){
      decks$match_result = 0
    }
    else{
      decks$match_result = NA
    }
    if(decks$upcoming_matchup$opp == "character(0)"){
      decks$match_result = 1
    }
    
    if(is.na(decks$match_result)){
      result <- matchup_stats %>% filter(player_archetype==decks$upcoming_matchup$my, 
                                         opponent_archetype==decks$upcoming_matchup$opp) %>% mutate(win = 1*(runif(1)<win_rate/100)) %>% select(win) %>% as.numeric()
      decks$game_4_result$my <- NA
      decks$game_4_result$opp <- NA
    
      if(result>0){
        decks$game_4_result$my = "win"; decks$game_4_result$opp = "loss";
        decks$upcoming_matchup$opp = NA
      }
      else{
        decks$game_4_result$my = "loss"; decks$game_4_result$opp = "win"
        decks$upcoming_matchup$my = NA
      }
    }
    
    return(decks) 
  }
  
  tryCatch(decks <- Game_Result_4(decks))
  
  
  Game_Matchup_5 <- function(decks, matchup_stats = Hearthstone_MU_Data){
    
    if(decks$game_4_result$my=="win"){
      decks$upcoming_matchup$my = decks$upcoming_matchup$my; 
      decks$upcoming_matchup$opp = matchup_stats %>% 
        filter(player_archetype==decks$upcoming_matchup$my, 
               opponent_archetype %in% decks$unused_decks$opp) %>%
        top_n(1, desc(win_rate)) %>% select(opponent_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$opp==decks$upcoming_matchup$opp)
      decks$unused_decks$opp <- decks$unused_decks$opp[left.over]
    }
    else{
      decks$upcoming_matchup$opp = decks$upcoming_matchup$opp; 
      decks$upcoming_matchup$my = matchup_stats %>% 
        filter(player_archetype %in% decks$unused_decks$my, 
               opponent_archetype==decks$upcoming_matchup$opp) %>%
        top_n(1, win_rate) %>% select(player_archetype) %>% as.character();
      left.over <- !(decks$unused_decks$my==decks$upcoming_matchup$my)
      decks$unused_decks$my <- decks$unused_decks$my[left.over]
    }
    return(decks)
  }
  
  tryCatch(decks <- Game_Matchup_5(decks))
  
  Game_Result_5 <- function(decks, matchup_stats=Hearthstone_MU_Data){
    if(decks$upcoming_matchup$my == "character(0)"){
      if(decks$upcoming_matchup$my == "character(0)"){
        decks$match_result = 0
      }
    }
    else{
      decks$match_result = NA
    }
    try(if(decks$upcoming_matchup$opp == "character(0)"){
      decks$match_result = 1
    })
    if(is.na(decks$match_result)){
      result <- matchup_stats %>% filter(player_archetype==decks$upcoming_matchup$my, 
                                         opponent_archetype==decks$upcoming_matchup$opp) %>% mutate(win = 1*(runif(1)<win_rate/100)) %>% select(win) %>% as.numeric()
      decks$game_5_result$my <- NA
      decks$game_5_result$opp <- NA
      if(result>0){
        decks$game_5_result$my = "win"; decks$game_5_result$opp = "loss";
        decks$upcoming_matchup$opp = NA
      }
      else{
        decks$game_5_result$my = "loss"; decks$game_5_result$opp = "win"
        decks$upcoming_matchup$my = NA
      }
    }
    
    return(decks) 
  }
  
  tryCatch(decks <- Game_Result_5(decks))
  
  Match_Result <- function(decks){
    if(decks$game_5_result$my=="win"){
      decks$match_result = 1
    }
    else{
      decks$match_result <- ifelse(is.na(decks$match_result),  0, decks$match_result)
    }
    return(decks)
  }

  tryCatch(decks <- Match_Result(decks))
  
  print(i)
  full_results$match_result[i] <- decks$match_result
  full_results$my_ban[i] = decks$banned_decks$my
  full_results$opp_ban[i] = decks$banned_decks$opp
  full_results$first_queue[i] = decks$first_queue
  full_results$game_result_1[i] = decks$game_1_result
}
full_results
full_results %>% summarise(mean(match_result))

library(tidyverse)
library(patchwork)

#---------------------------------------------------------DISCOUNT WORDLE, A KNOCK-OFF VERSION
#---------------------------------------------------------BY HILARY KILLAM
#---------------------------------------------------------TO PLAY, IN RSTUDIO CLICK "SOURCE" UP HERE ^
#---------------------------------------------------------THEN, ENTER YOUR GUESSES IN THE CONSOLE
#---------------------------------------------------------GUESSED LETTERS IN THE CORRECT PLACE IN THE TARGET WORD WILL APPEAR GREEN
#---------------------------------------------------------GUESSED LETTERS IN THE INCORRECT PLACE IN THE TARGET WORD WILL APPEAR YELLOW
#---------------------------------------------------------GUESSED LETTERS NOT IN THE TARGET WORD WILL APPEAR GREY
#---------------------------------------------------------YOU HAVE 6 TURNS TO CORRECTLY GUESS THE WORD
#---------------------------------------------------------ENJOY!

wordle <- function() {
  
  #---------------------------------------------------------Get target word
  words <- read_csv("words.csv", col_types = cols(word = col_character())) %>%
    mutate(char1 = str_sub(word, 1, 1),
           char2 = str_sub(word, 2, 2),
           char3 = str_sub(word, 3, 3),
           char4 = str_sub(word, 4, 4),
           char5 = str_sub(word, 5, 5))
  
  #Randomly choose word
  target <- words %>%
    sample_n(1)
  
  #---------------------------------------------------------Initialize alphabet plot
  #alphabet
  
  alphabet <- tibble(letter = LETTERS, position = c(rep(2, 13), rep(1, 13)), id = rep(1:13, 2))
  
  p_abc <- alphabet %>%
    ggplot(aes(x = id, y = position)) +
    geom_tile(fill = "grey", color = "white") +
    geom_text(aes(label = letter), vjust = .5, size = 5) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none")
  
  guess_list <- NULL
  
  #---------------------------------------------------------Initialize blank board
  
  theme_set(theme_void())
  theme_update(plot.title = element_text(hjust = 0.5))
  
  blank_board <- tibble(position = 1:5, y = rep(1, 5)) %>%
    ggplot(aes(x = position, y = y)) +
    geom_tile(color = "gray70", fill = "white") +
    coord_equal() +
    theme(legend.position = "none") 
  
  title <- ggtitle("DISCOUNT WORDLE")
  
  #---------------------------------------------------------Initialize plot list
  
  p_list <- list(blank_board + title, blank_board, blank_board, blank_board, blank_board, blank_board, p_abc)
  
  #---------------------------------------------------------Initialize guess function
  ask_for_guess <- function() {
    
    guess <<- readline("Type your guess, then press ENTER > ")
    
    while ((str_length(guess) != 5)) { 
      print("Guess must be 5 characters in length.")
      guess <<- readline("Type your guess, then press ENTER > ")
    }
    
  }
  
  #---------------------------------------------------------START
  
  print(wrap_plots(p_list, ncol = 1))
  
  #---------------------------------------------------------TURNS
  
  for(turn in 1:6) {
    
    ask_for_guess()
    
    #If guess is correct, all green and print winning message
    
    guess_df <- tibble(word = guess, 
                       char1 = str_sub(word, 1, 1),
                       char2 = str_sub(word, 2, 2),
                       char3 = str_sub(word, 3, 3),
                       char4 = str_sub(word, 4, 4),
                       char5 = str_sub(word, 5, 5)) 
    
    guess_long <- guess_df %>%
      pivot_longer(starts_with("char"), names_to = "position", values_to = "letter") %>%
      mutate(letter = str_to_upper(letter),
             position = parse_number(position)) %>%
      add_count(letter, name = "g_n_letter") 
    
    guess_list <- unique(c(guess_list, guess_long$letter))
    
    alphabet <- alphabet %>%
      mutate(in_guess = case_when(letter %in% guess_list ~ "guessed",
                                  TRUE ~ "not_guessed"))
    
    p_abc_fills <- c(not_guessed = "grey50", guessed = "grey30")
    p_abc_colors <- c(not_guessed = "white", guessed = "grey50")
    
    p_abc <- alphabet %>%
      ggplot(aes(x = id, y = position, fill = in_guess)) +
      geom_tile(color = "white") +
      geom_text(aes(label = letter, color = in_guess), vjust = .5, size = 5) +
      coord_equal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = p_abc_fills) +
      scale_color_manual(values = p_abc_colors)
    
    # If guess is correct, end game and display winning gameboard and message
    
    if(guess == target$word) {
      
      turn_plot <-  guess_long %>%
        mutate(y = 1) %>%
        ggplot(aes(x = position, y = y)) +
        geom_tile(fill = "#34A151", color = "black", size = 2) +
        geom_text(aes(label = letter), vjust = .5, size = 10) +
        coord_equal() +
        theme(legend.position = "none")
      
      title <- ggtitle(paste("YOU GOT IT IN", turn, "OUT OF 6!"))
      p_list[[turn]] <- turn_plot
      p_list[[1]] <- p_list[[1]] + title
      p_list[[7]] <- p_abc
      print(wrap_plots(p_list, ncol = 1))
      
      return("You won!")
      
      # If guess is incorrect, display letter match information and restart loop to ask for next guess
      
    } else {
      
      target_long <- tibble(target = target$word,
                            t_char1 = str_sub(target, 1, 1),
                            t_char2 = str_sub(target, 2, 2),
                            t_char3 = str_sub(target, 3, 3),
                            t_char4 = str_sub(target, 4, 4),
                            t_char5 = str_sub(target, 5, 5)) %>%
        pivot_longer(starts_with("t_char"), names_to = "t_position", values_to = "t_letter") %>%
        mutate(t_letter = str_to_upper(t_letter),
               t_position = parse_number(t_position),
               id = row_number()) 
      
      guess_df <- guess_long %>%
        mutate(id = row_number()) %>%
        left_join(target_long, by = "id") %>% 
        select(-id) %>%
        mutate(match = case_when(letter == t_letter ~ "green",
                                 TRUE ~ "not_green")) 
      
      matches <- guess_df %>%
        filter(match == "green")
      
      non_matches <- guess_df %>%
        filter(match == "not_green")
      
      non_match_letters <- non_matches$t_letter
      
      for(i in 1:nrow(non_matches)) {  
        if(non_matches[[i, "letter"]] %in% non_match_letters) {
          non_matches[[i, "match"]] <- "yellow"
          non_match_letters <- non_match_letters[-match(non_matches[[i, "letter"]], non_match_letters)]
        } else {
          non_matches[[i, "match"]] <- "grey"
        }
      }
      rm(non_match_letters)
      
      guess_df <-matches %>%
        bind_rows(non_matches) %>%
        arrange(position) %>%
        mutate(fill_colour = case_when(match == "green" ~ "#34A151",
                                       match == "yellow" ~ "#F0D44E",
                                       match == "grey" ~ "#675E61")) 
      
      fills <- c(green = "#34A151", yellow = "#F0D44E", grey = "#675E61")
      
      turn_plot <-  guess_df %>%
        mutate(y = 1) %>%
        ggplot(aes(x = position, y = y, fill = match)) +
        geom_tile(color = "white") +
        geom_text(aes(label = letter), vjust = .5, size = 10) +
        coord_equal() +
        scale_fill_manual(values = fills) +
        theme(legend.position = "none")
      
      p_list[[turn]] <- turn_plot
      p_list[[1]] <- p_list[[1]] + title
      p_list[[7]] <- p_abc
      print(wrap_plots(p_list, ncol = 1))
      
    }
  }
  
  # After 6 wrong guesses, loop ends and game is over. Display losing message and what the word was.
  
  print("GAME OVER")
  print(paste("The correct word was", str_to_upper(target$word)))
  
  title <- ggtitle(paste("YOU LOST - word was", target$word))
  p_list[[1]] <- p_list[[1]] + title
  print(wrap_plots(p_list, ncol = 1))
  
}

print("Welcome to Discount Wordle! If you want to exit the game, press ESC")
play <- T
while(play == T) {
  wordle()
  again <<- readline("Play again? (Y/N) ")
  if(again == "Y" | again == "y" | again == "yes" | again == "Yes" | again == T) {
    print("Excellent choice!")
  } else { 
    play <- F
    print("Goodbye!")
  }
}
# Description: Monty game- A counter-intuitive statistics puzzle

# -- STEP1: SET UP ####

# Import the packages to use throughout the program
# magrittr - this library has 'pipe' %>% operator to take the value on the
# left hand side and pass it it into the function call on the right side

install.packages("magrittr") # install the package in case we don't have it
library(magrittr) # then "switch on" this package


# -- STEP2: CREATE THE FUNCTION THAT SIMULATES GAME ####

# HOST knows the door with the prize behind it
# REMEMBER: host will always open the door that doesn't have the prize 

# Function name: play_monty takes number of doors and, depending on 
# the player's  initial choice of door, mimics the game host.
# Then, depending on the specified strategy (always_switch or 
# never_switch), it returns the outcome of the final choice door.

#Inputs: 

#ndoors     number of doors (this can be any integer>= 3)
#strategy   player's strategy (always_switch, never_switch)

play_monty <- function(ndoors, strategy) {
  
  
  #Check if the number of doors>=3
  if(ndoors < 3) stop("We need more number of doors: at least 3")
  
  
  #Set the number of doors
  doors <- paste0("door_", 1:ndoors) 
  
  #Randomly select a door to be the one with prize
  prize_door <- sample(doors, 1)
  
  #Randomly select a door as a player's initial choice
  player_first_choice <- sample(doors, 1)
  
  #The host selects a door other than  the prize_door or the player's 
  #first choice door.
  
  # Note: setdiff(A-B) is the function that computes the set of all the 
  # elements in A that are not elements of B
  # For example: in case ndoors=3, if prize_door=door_1 and player_first_
  # choice=door_2 then host_opens_doors will be door_1
  # Essentially, setdiff(A-B) gives the "left over" door/s
  
  host_opens_doors <- setdiff(doors, c(prize_door, player_first_choice)) %>%
    sample(1) # pick 1 door for host to open 
              # (i.e., host picks a door that is not the prize nor the player's first choice door)
  
  #Set the final choice door
  #Two possible strategies: 
  #the player will stick to the first choice of door
  #the player will select another door different from the first door
  
  player_final_choice <- switch(strategy,
                                "always_switch" = setdiff(doors, c(player_first_choice, host_opens_doors)) %>% sample(1),
                                "never_switch" = player_first_choice
  )
  
  #WON-if the final choice is same as the prize door, otherwise LOST
  ifelse(player_final_choice == prize_door, "WON", "LOST")
  
  
}#END of function 

# -- STEP3: NOW USE ("CALL") THE ABOVE FUNCTION TO SIMULATE THE GAME ####

#Initialize random numbers
set.seed(123) 

#Use the function replicate to create simulations (call the play_monty function) for specific number of times

##For example: play 1000 times with selection of 3 doors with the strategy="always_switch" ####
replicate(1000, play_monty(ndoors = 3, "always_switch")) %>%  
  table() %>%
  prop.table()

##play 1000 times with selection of 3 doors with the strategy="never_switch" ####
replicate(1000, play_monty(ndoors = 3, "never_switch")) %>% 
  table() %>% 
  prop.table()


# -- STEP4: EXTENSION - What if the number of doors increases? ####

# play 1000 times with 5 doors and "always_switch" strategy...
replicate(1000, play_monty(ndoors = 5, "always_switch")) %>% 
  table() %>% 
  prop.table()

replicate(1000, play_monty(ndoors = 5, "never_switch")) %>% 
  table() %>% 
  prop.table()

# Can you change the code above to simulate playing 1000 times with 100 doors, 
# instead of 5?

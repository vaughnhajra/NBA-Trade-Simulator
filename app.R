library(shiny)
library(dplyr)
library(googlesheets4)
library(formattable)

# Method 1: Works locally but not when published to web
#statsOLD <- read_sheet("https://docs.google.com/spreadsheets/d/1LPWzaTDMT5d4UVEdxfBvsYsv0f4ydONkH_HqEIBL4xA/edit#gid=0")
#head(statsOLD)

# Method 2: Doesn't work locally OR on the web (getting error)
stats <- as_tibble(read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSaYeKES6eE4i4AirnT5R93JAWo2_RQ7MIv-oVsuQXynASZkQbf9piqv0y7HbNeGQPBvscJ7QCOqMyv/pub?gid=0&single=true&output=csv"))
#head(stats)
#stats_tibble <- as_tibble(stats)
stats$Salary <- as.numeric(gsub("[^0-9.]", "", stats$Salary))
names(stats) <- c("Rk", "Player", "Pos", "Salary", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FG%", "3P", "3PA", "3P%", "2P", "2PA", "2P%", "eFG%", "FT", "FTA", "FT%", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "2Pa", "0-3a", "3-10a", "10-16a", "16-3Pa", "3Pa", "2Ppct", "0-3pct", "3-10pct", "10-16pct", "16-3Ppct", "3Ppct", "BPM", "OBPM", "DBPM", "USG%")
#head(stats)

# https://stackoverflow.com/questions/24237399/how-to-select-the-rows-with-maximum-values-in-each-group-with-dplyr
stats_unique <- stats %>%
  group_by(Player) %>%
  slice(which.max(G)) %>%
  ungroup()

overview_content <- "
<h2>Overview</h2>
<p>This project is an NBA Team Simulator that allows users to create and manage rosters, simulate games, and analyze player statistics.</p>
<h2>Box Score Creation</h2>
<p>The box score is created using a Monte Carlo simulation on a per-possession basis. Each possession is simulated to determine shot outcomes, turnovers, and other statistics.</p>
<h2>App Functionality</h2>
<p>The app consists of three main tabs:</p>
<ul>
  <li><strong>Create Roster:</strong> Allows users to select a team and add or remove players to create a roster. Users can also adjust player minutes and simulate player swaps.</li>
  <li><strong>Simulate Games:</strong> Enables users to simulate multiple games based on the created roster. The simulation calculates game statistics and updates the box score accordingly.</li>
  <li><strong>Player Finder:</strong> Provides tools to filter and search for players based on various statistics such as points, assists, rebounds, etc. Users can filter players by position, salary range, age, minutes per game, and games played.</li>
</ul>
"

ui <- function(request) {
  navbarPage(
    title = "NBA Team Simulator",
    id = "nav",
    tabPanel("Executive Summary", icon = icon("map"),
             fluidPage(
               tags$div(HTML(overview_content))
             )
    ),
    tabPanel("Create Roster", icon = icon("users"),
             sidebarLayout(
               sidebarPanel(
                 # Sidebar content (stays the same)
                 selectInput("team", "Select Team:", choices = unique(stats_unique$Tm)),
                 selectInput("add_players", "Add to Roster:",
                             choices = stats_unique$Player,
                             multiple = TRUE),
                 uiOutput("remove_players_selector"),
                 actionButton("process_changes", "Update Roster"),
                 sliderInput("maxMinutes", "Max Minutes for Star Player:",
                             min = 30, max = 48, value = 36),
                 uiOutput("add_minutes_selector"),
                 uiOutput("remove_minutes_selector"),
                 sliderInput("minutesSwap", "Minutes to Swap:",
                             min = 0, max = 48, value = 0),
                 actionButton("changeMins", "Swap Minutes")
               ),
               mainPanel(
                 dataTableOutput("roster_table")
               )
             )
    ),
    tabPanel("Simulate Games", icon = icon("basketball"),
             sidebarLayout(
               sidebarPanel(
                 # Sidebar content
                 helpText("Games are simulated on a per-possession basis using Monte-Carlo simulation, and averaged to create per game box scores."),
                 sliderInput("gameNum", "Games to Simulate:",
                             min = 1, max = 500, value = 50),
                 actionButton("simulateGames", "Start Simulation")
               ),
               mainPanel(
                 # Main content for page 1
                 dataTableOutput("box_score")
               )
             )
    ),
    tabPanel("Player Finder", icon = icon("magnifying-glass"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("filterStats", "Statistics to Display:", choices = c("PTS", "AST", "ORB","DRB","TRB", "TOV", "FT%", "STL", "BLK", "3P%", "2P%","MP", "BPM", "OBPM", "DBPM"), multiple = TRUE,
                             selected = c("MP", "OBPM", "DBPM")),
                 checkboxGroupInput("positions", label = ("Position(s):"), 
                                    choices = list("PG" = "PG", "SG" = "SG", "SF" = "SF", "PF" = "PF", "C" = "C"),
                                    selected = c("PG", "SG", "SF", "PF", "C")),
                 sliderInput("salaryRange", label = ("Salary Range (millions of $):"), min = 0, 
                             max = 50, value = c(0, 50)),
                 sliderInput("ageRange", label = ("Age Range:"), min = 17, 
                             max = 45, value = c(17, 32)),
                 sliderInput("MinutesSlider", label = ("Minutes per Game:"), min = 0, 
                             max = 48, value = c(5,48)),
                 sliderInput("gamesSlider", label = ("Games Played:"), min = 1, 
                             max = 83, value = c(10,83))
               ),
               mainPanel(
                 # Main content for page 2
                 tableOutput("filteredPlayersTable")
               )
             )
    )
  )
}

# Define server logic
server <- function(input, output) {

  output$filteredPlayersTable <- renderTable({
    
    filtered_data <- stats_unique %>%
      filter(Pos %in% input$positions) %>%
      filter(Salary >= input$salaryRange[1]*1000000 & Salary <= input$salaryRange[2]*1000000) %>%
      filter(Age >= input$ageRange[1] & Age <= input$ageRange[2]) %>%
      filter(MP >= input$MinutesSlider[1] & MP <= input$MinutesSlider[2]) %>%
      filter(G >= input$gamesSlider[1] & G <= input$gamesSlider[2]) %>%
      arrange(desc(BPM))
    
    # Selecting columns based on user-selected statistics
    selected_stats <- c("Player", "Pos", "Tm", "Age", "BPM") # Always include these columns
    selected_stats <- c(selected_stats, input$filterStats) # Add user-selected statistics
    
    filtered_data <- filtered_data %>%
      select(all_of(selected_stats)) %>%
      mutate(Age = floor(Age))
    
    filtered_data
  })
  
  
  
  # Define the function to create roster
  create_roster <- function(stats_data, team, total_minutes) {
    roster <<- stats_data %>%
      filter(Tm == input$team) %>%
      filter(MP > 3) %>%
      filter(G > 5) %>%
      arrange(desc(BPM)) %>% 
      slice(1:15) %>%
      mutate(TOVPercentage = TOV / sum(TOV))%>%
      mutate(ORBPercentage = ORB / sum(ORB))%>%
      mutate(ASTPercentage = AST / sum(AST))
    
    # Normalize BPM values to a range from 0 to 10
    min_bpm <- min(roster$BPM)
    max_bpm <- max(roster$BPM)
    roster$normalized_bpm <- (roster$BPM - min_bpm) / (max_bpm - min_bpm)
    
    # Calculate minutes allocation based on normalized BPM values
    roster$minutes_allocation <- 240 * roster$normalized_bpm / sum(roster$normalized_bpm)
    
    # Ensure total minutes constraint
    if (sum(roster$minutes_allocation) > 240) {
      excess_minutes <- sum(roster$minutes_allocation) - 240
      roster$minutes_allocation <- roster$minutes_allocation - (excess_minutes * (roster$minutes_allocation / sum(roster$minutes_allocation)))
    }
    
    # Cap minutes for top players
    max_minutes <<- input$maxMinutes
    exceeding_players <- which(roster$minutes_allocation > max_minutes)
    #print(exceeding_players)
    
    while (length(exceeding_players) > 0) {
      # Set minutes for exceeding players to max_minutes
      roster$minutes_allocation[exceeding_players] <- max_minutes
      
      #Get non-exceeding players
      non_exceeding_players <- setdiff(1:nrow(roster), exceeding_players)
      
      
      # Calculate total minutes after capping exceeding players
      total_minutes_left <- 240 - sum(roster$minutes_allocation)
      #print(total_minutes_left)
      
      # Calculate minimum and maximum BPM of non-exceeding players
      min_bpm <- min(roster$BPM[non_exceeding_players])
      max_bpm <- max(roster$BPM[non_exceeding_players])
      
      roster$normalized_bpm <- (roster$BPM - min_bpm) / (max_bpm - min_bpm)
      
      # Calculate minutes allocation based on normalized BPM values
      roster$minutes_allocation <- roster$minutes_allocation + total_minutes_left * roster$normalized_bpm / sum(roster$normalized_bpm)
      
      # Ensure total minutes constraint
      if (sum(roster$minutes_allocation) > 240) {
        excess_minutes <- sum(roster$minutes_allocation) - 240
        roster$minutes_allocation <- roster$minutes_allocation - (excess_minutes * (roster$minutes_allocation / sum(roster$minutes_allocation)))
      }
      
      exceeding_players <- which(roster$minutes_allocation > max_minutes)
      
    }
    
    # Add allocated minutes to the roster dataframe
    roster <<- roster %>%
      mutate(allocated_minutes = minutes_allocation)
    
    return(roster)
  }
  
  # Example usage of the function
  output$roster_table <- renderDataTable({
    # Assuming you have loaded your stats data into 'stats_data'
    team <- input$team
    total_minutes <- 240
    
    roster <<- create_roster(stats_unique, team, total_minutes)
    
    roster2 <- roster
    
    # Rounding minutes_allocation to the nearest tenth
    roster2$minutes_allocation <- round(roster$minutes_allocation, 1)
    
    # Formatting Salary to display in millions with a dollar sign
    roster2$Salary <- paste0("$", format(roster$Salary / 1000000, digits = 1), "m")
    roster2%>% select(Player, Pos, Age, Salary, minutes_allocation)
  }, options = list(pageLength = 15, lengthMenu = c(15, 15), paging = FALSE, searching = FALSE))
  
  # Example usage of the function
  output$box_score <- renderDataTable({
    
  }, options = list(pageLength = 15, lengthMenu = c(15, 15), paging = FALSE, searching = FALSE))
  
  #Custom remove players selector
  output$remove_players_selector <- renderUI({
    team_players <- stats_unique[stats_unique$Tm == input$team, "Player"]
    selectInput("remove_players", "Remove from Roster:", choices = team_players, multiple = TRUE)
  })
  
  #Custom add minutes selector
  output$add_minutes_selector <- renderUI({
    selectInput("addMinutes", "Player to Give Minutes:", choices = stats_unique$Player, multiple = FALSE)
  })
  
  #Custom remove minutes selector
  output$remove_minutes_selector <- renderUI({
    selectInput("removeMinutes", "Player to Take Minutes:", choices = stats_unique$Player, multiple = FALSE)
  })
  
  observeEvent(input$process_changes, {
    
    #Add Players
    stats_unique$Tm[stats_unique$Player %in% input$add_players] <<- input$team
    
    #Remove Players
    stats_unique <<- stats_unique[!stats_unique$Player %in% input$remove_players, ]
    
    # Print the updated dataframe
    #print(stats_unique)
    #print(input$add_players)
    #print(input$remove_players)
    
    # Create Roster
    roster <<- create_roster(stats_unique, input$team, total_minutes)
    
    # Print the updated roster
    # print(roster)
    
    # Display the updated roster table
    output$roster_table <- renderDataTable({
      # Assuming you have loaded your stats data into 'stats_data'
      team <- input$team
      total_minutes <- 240
      
      roster <<- create_roster(stats_unique, team, total_minutes)
      
      roster2 <- roster
      
      # Rounding minutes_allocation to the nearest tenth
      roster2$minutes_allocation <- round(roster$minutes_allocation, 1)
      
      # Formatting Salary to display in millions with a dollar sign
      roster2$Salary <- paste0("$", format(roster$Salary / 1000000, digits = 1), "m")
      roster2%>% select(Player, Pos, Age, Salary, minutes_allocation)
    }, options = list(pageLength = 15, lengthMenu = c(15, 15), paging = FALSE, searching = FALSE))
    
    output$box_score <- renderDataTable({
      box_score <<- data.frame(
        x2p = numeric(15),
        x2pa = numeric(15),
        x3p = numeric(15),
        x3pa = numeric(15),
        minutes = numeric(15)
      ) %>% mutate(Player = roster$Player)
      box_score
    })
    
  })
  
  observeEvent(input$changeMins, {
    
    # Number of minutes to change
    
    player1 <- input$addMinutes
    player2 <- input$removeMinutes
    
    #print(roster$Player)
    #print(player1)
    #print(player2)
    
    # Check if player1 exists in the roster
    if (player1 %in% roster$Player) {
      
      
      # Check if player2 exists in the roster
      if (player2 %in% roster$Player) {
        
        #Add Player 1 minutes
        original_mins_1 <- roster[roster$Player == player1, "minutes_allocation"]
        roster[roster$Player == player1, "minutes_allocation"] <<- original_mins_1 + input$minutesSwap
        
        #Add Player 2 minutes
        original_mins_2 <- roster[roster$Player == player2, "minutes_allocation"]
        roster[roster$Player == player2, "minutes_allocation"] <<- original_mins_2 - input$minutesSwap
        
        #Correct negative minutes
        if (roster[roster$Player == player2, "minutes_allocation"] < 0){
          roster[roster$Player == player1, "minutes_allocation"] <<- roster[roster$Player == player1, "minutes_allocation"] - roster[roster$Player == player2, "minutes_allocation"]
          roster[roster$Player == player2, "minutes_allocation"] <<- 0
        }
        
        #Correct negative minutes
        if (roster[roster$Player == player1, "minutes_allocation"] > 48){
          roster[roster$Player == player2, "minutes_allocation"] <<- roster[roster$Player == player2, "minutes_allocation"] + (roster[roster$Player == player1, "minutes_allocation"] - 48)
          roster[roster$Player == player1, "minutes_allocation"] <<- 48
        }
        
        
      } else {
        # Handle the case when player2 is not found in the roster
        showModal(modalDialog(paste("Player", player2, "not found in the roster.")))
      }
      
    } else {
      # Handle the case when player1 is not found in the roster
      showModal(modalDialog(paste("Player", player1, "not found in the roster.")))
    }
    
    
    
    
    
    # Display the updated roster table
    output$roster_table <- renderDataTable({
      roster2 <- roster
      
      # Rounding minutes_allocation to the nearest tenth
      roster2$minutes_allocation <- round(roster$minutes_allocation, 1)
      
      # Formatting Salary to display in millions with a dollar sign
      roster2$Salary <- paste0("$", format(roster$Salary / 1000000, digits = 1), "m")
      roster2%>% select(Player, Pos, Age, Salary, minutes_allocation) %>% 
        arrange(desc(minutes_allocation))
    }, options = list(pageLength = 15, lengthMenu = c(15, 15), paging = FALSE, searching = FALSE))
    
  })
  
  observeEvent(input$simulateGames, {
    season(input$gameNum)
    
    output$box_score <- renderDataTable({
      # Exclude 'Player' column from rounding
      box_score_rounded <- box_score[, -which(names(box_score) == "Player")]
      
      # Round all numeric columns except for percentages to the nearest tenth
      numeric_cols <- sapply(box_score_rounded, is.numeric)
      box_score_rounded[, numeric_cols] <- round(box_score_rounded[, numeric_cols], digits = 2)
      
      # Add the 'Player' column back to the front
      box_score_rounded <- cbind(box_score$Player, box_score_rounded)
      
      # Rename the first column to "Player"
      names(box_score_rounded)[1] <- "Player"
      
      # Printing the rounded data frame
      return(box_score_rounded)
    })
  })
  
  
  
  
  shot<- function(){
    
    #Decide who takes shot
    taker <- sample(onFloor$Player, size = 1, prob = onFloor$usage_weights)
    
    #Decide on shot distance
    distance<- c("0-3", "3-10","10-16","16-3p","3p")
    shot_distance<- sample(distance,size=1, 
                           prob=c(stats_unique[stats_unique$Player == taker, "0-3a"], 
                                  stats_unique[stats_unique$Player == taker, "3-10a"],
                                  stats_unique[stats_unique$Player == taker, "10-16a"],
                                  stats_unique[stats_unique$Player == taker, "16-3Pa"],
                                  stats_unique[stats_unique$Player == taker, "3Pa"]))
    
    # Test print for debugging if necessary
    # print(taker)
    # print(shot_distance)
    return(shotResult(taker, shot_distance))
    
  }
  
  shotResult <- function(player_name, distance){
    # Switch-case structure
    switch(distance,
           "0-3"= {
             # Code for 0-3 distance
             percentage <- stats_unique[stats_unique$Player == player_name, "0-3pct"]
           },
           "3-10" = {
             # Code for 3-10 distance
             percentage <- stats_unique[stats_unique$Player == player_name, "3-10pct"]
             
           },
           "10-16" = {
             # Code for 10-16 distance
             percentage <- stats_unique[stats_unique$Player == player_name, "10-16pct"]
           },
           "16-3p" = {
             # Code for 16-3p distance
             percentage <- stats_unique[stats_unique$Player == player_name, "16-3Ppct"]
           },
           "3p" = {
             # Code for 3p distance
             percentage <- stats_unique[stats_unique$Player == player_name, "3Ppct"]
           }
    )
    
    makeMiss <- c("make", "miss")
    make <- sample(makeMiss, prob = c(percentage, (1-percentage)), size = 1)
    
    if(make == "make" & distance != "3p"){
      box_score[box_score$Player == player_name, "x2p"]<<- box_score[box_score$Player == player_name, "x2p"]+1
      box_score[box_score$Player == player_name, "x2pa"]<<- box_score[box_score$Player == player_name, "x2pa"]+1
      assist_result <- c("AST", "nAST")
      assist<- sample(assist_result, prob = c(0.67, (1-.67)), size = 1)
      if(assist == "AST"){
        ASTer <- sample(onFloor$Player, size = 1, prob = onFloor$AST_weights)
        for(i in onFloor$Player){
          if(ASTer == i){
            box_score[box_score$Player == i, "AST"]<<- box_score[box_score$Player == i, "AST"]+1 
          }
        }
      }
      return(2)
    } else if(make == "make"){
      box_score[box_score$Player == player_name, "x3p"]<<- box_score[box_score$Player == player_name, "x3p"]+1
      box_score[box_score$Player == player_name, "x3pa"]<<- box_score[box_score$Player == player_name, "x3pa"]+1
      assist_result <- c("AST", "nAST")
      assist<- sample(assist_result, prob = c(.67, (1-.67)), size = 1)
      if(assist == "AST"){
        ASTer <- sample(onFloor$Player, size = 1, prob = onFloor$AST_weights)
        for(i in onFloor$Player){
          if(ASTer == i){
            box_score[box_score$Player == i, "AST"]<<- box_score[box_score$Player == i, "AST"]+1 
          }
        }
      }
      return(3)
    }else if(make =="miss" & distance != "3p"){
      box_score[box_score$Player == player_name, "x2pa"]<<- box_score[box_score$Player == player_name, "x2pa"]+1
      board_result <- c("ORB", "nORB")
      board<- sample(board_result, prob = c(.17, (1-.17)), size = 1)
      if(board == "ORB"){
        ORBer <- sample(onFloor$Player, size = 1, prob = onFloor$ORB_weights)
        for(i in onFloor$Player){
          if(ORBer == i){
            box_score[box_score$Player == i, "ORB"]<<- box_score[box_score$Player == i, "ORB"]+1 
          }
        }
        shot()
      }
      return(0)
    }else {
      box_score[box_score$Player == player_name, "x3pa"]<<- box_score[box_score$Player == player_name, "x3pa"]+1
      board_result <- c("ORB", "nORB")
      board<- sample(board_result, prob = c(.17, (1-.17)), size = 1)
      if(board == "ORB"){
        ORBer <- sample(onFloor$Player, size = 1, prob = onFloor$ORB_weights)
        for(i in onFloor$Player){
          if(ORBer == i){
            box_score[box_score$Player == i, "ORB"]<<- box_score[box_score$Player == i, "ORB"]+1 
          }
        }
        shot()
      }
      return(0)
    }
    
  }
  
  decide_sub <- function(onFloor) {
    
    for (player in onFloor$Player) {
      
      cap = 60 * as.numeric(onFloor[onFloor$Player == player, "allocated_minutes"])
      playing_time <- as.numeric(onFloor[onFloor$Player == player, "playing_time"])
      # print(cap)
      # print(playing_time)
      
      if ( playing_time >= cap) {
        roster[roster$Player == player, "minutes_exhausted"] <<- TRUE
        box_score[box_score$Player == player, "minutes"]<<- playing_time / 60
        substitutePlayer(as.character(onFloor[onFloor$Player == player, "Player"]))
      }
    }
  }
  
  
  substitutePlayer <- function(sub_out){
    
    
    #Remove original player from floor # CHECK THIS!!!
    onFloor <<- onFloor[!(onFloor$Player %in% sub_out), ]# Remove the player we're subbing out from the roster 
    
    
    #go through roster (should be sorted by minutes already)
    for (player in roster$Player){
      
      #Check if player is eligible to sub in
      if(roster[roster$Player == player, "minutes_exhausted"] == FALSE){
        
        # Create new row to add to floor
        new_row <- tibble(
          Player = player,
          `USG%` = as.numeric(roster[roster$Player == player, "USG%"]),
          TOVPercentage = as.numeric(roster[roster$Player == player, "TOVPercentage"]),
          ORBPercentage = as.numeric(roster[roster$Player == player, "ORBPercentage"]),
          ASTPercentage = as.numeric(roster[roster$Player == player, "ASTPercentage"]),
          allocated_minutes = as.numeric(roster[roster$Player == player, "allocated_minutes"]),
          playing_time = 0,
          usage_weights = 0,
          TOV_weights = 0,
          ORB_weights= 0,
          AST_weights = 0
        )
        
        #add row to floor
        onFloor <<- rbind(onFloor, new_row)
        
        roster[roster$Player == player, "minutes_exhausted"] <<- TRUE
        
        calculate_usage_weights()
        onFloor$allocated_minutes <<- as.numeric(onFloor$allocated_minutes)
        
        
        # end loop
        break()
      }
    }
  }
  
  calculate_usage_weights <- function(){
    
    onFloor$usage_weights <<- (onFloor$`USG%` / sum(onFloor$`USG%`))
    onFloor$TOV_weights <<- (onFloor$TOVPercentage / sum(onFloor$TOVPercentage))
    onFloor$ORB_weights <<- (onFloor$ORBPercentage / sum(onFloor$ORBPercentage))
    onFloor$AST_weights <<- (onFloor$ASTPercentage / sum(onFloor$ASTPercentage))
  }
  
  possession <- function() {
    possessions <<- possessions + 1 
    possession_length <- rnorm(1, 14.4, 0.42895221179054)
    gameclock <<- gameclock - possession_length
    #Once we get the players as an input we can just subtract possesion_length from MP*60 for playes on the court#
    onFloor$playing_time <<- onFloor$playing_time + possession_length
    
    possible_outcomes<- c("shot", "no shot")
    outcome<- sample(possible_outcomes,size=1,prob=c(0.5, 1-0.5))
    
    if(outcome == "shot"){
      possible_outcomes_turnover<- c("attempt", "turnover")
      outcome_turnover<- sample(possible_outcomes_turnover,size=1,prob=c(1-.14, .14))
      if(outcome_turnover == "attempt"){
        shot()
      }
      else{
        #Decide who tunrover ball
        TOVer <- sample(onFloor$Player, size = 1, prob = onFloor$TOV_weights)
        for(i in onFloor$Player){
          if(TOVer == i){
            box_score[box_score$Player == i, "TOV"]<<- box_score[box_score$Player == i, "TOV"]+1
          }
        }
      }
    }
    
    
  }
  
  initialize_game <- function(){
    # reset floor
    # reset minutes
    # reset game clock
    # set starting five exhausted minutes = true
    # add to game counter
    # (future) reset game scores
    # Set up roster
    
    # Create floor
    onFloor <<- roster %>% 
      arrange(desc(minutes_allocation)) %>% 
      slice(1:5) %>% 
      select(Player, `USG%`,TOVPercentage, ORBPercentage, ASTPercentage, minutes_allocation) %>%
      mutate(playing_time = 0)
    
    #Create Box Score
    
    box_score <<- data.frame(
      x2p = numeric(15),
      x2pa = numeric(15),
      x3p = numeric(15),
      x3pa = numeric(15),
      TOV = numeric(15),
      ORB = numeric(15),
      AST = numeric(15),
      minutes = numeric(15)
    ) %>% mutate(Player = roster$Player)
    
    
    #Calculate usage weights (later turned into function)
    #total_usage<-sum(onFloor$`USG%`)
    #onFloor$usage_weights <<-(onFloor$`USG%` / total_usage)
    calculate_usage_weights()
    
  }
  
  game <- function(){
    possessions <<- 0
    
    gameclock <<- 60*48
    
    while(gameclock > 0){
      possession()
      
      if(possessions %% 10 == 0){
        decide_sub(onFloor)
      }
      
    }
    
    box_score$x2pPct = box_score$x2p / box_score$x2pa
    box_score$x3pPct = box_score$x3p / box_score$x3pa
    box_score$usg = (box_score$x2pa + box_score$x3pa) / (sum(box_score$x2pa) + sum(box_score$x3pa))
    
    final_score <<- sum(box_score$x2p)*2 + sum(box_score$x3p)*3
    
    # Fill in remaining minutes to box score:
    for(player in onFloor$Player){
      box_score[box_score$Player == player, "minutes"]<<- onFloor[onFloor$Player == player, "playing_time"] / 60
      #print(player)
    }
    #FOR TESTING REMOVE LATER
    #new_score <- tibble(
    #Score = final_score,
    #)
    #final_scores_df <<- rbind(final_scores_df, new_score)
    
    paste("final score:", as.character(final_score))
  }
  
  resetMinutes <- function(){
    #FOR TESTING REMOVE LATER
    #box_score <<- data.frame(
    #x2p = numeric(14),
    #x2pa = numeric(14),
    #x3p = numeric(14),
    #x3pa = numeric(14),
    #minutes = numeric(14)
    #) %>% mutate(Player = roster$Player)
    
    #Reset roster
    roster <<- roster %>%
      mutate(allocated_minutes = minutes_allocation, minutes_exhausted = FALSE)%>%
      arrange(desc(allocated_minutes)) %>%
      mutate(minutes_exhausted = ifelse(row_number() <= 5, TRUE, minutes_exhausted))
    
    # Create floor
    onFloor <<- roster %>% 
      arrange(desc(allocated_minutes)) %>% 
      slice(1:5) %>% 
      select(Player, `USG%`,TOVPercentage, ORBPercentage, ASTPercentage, allocated_minutes) %>%
      mutate(playing_time = 0)
    
    # Calculate weights
    calculate_usage_weights()
  }
  
  season <- function(num_games){
    initialize_game()
    
    for(i in 1:num_games){
      resetMinutes()
      game()
    }
    
    #get season per game averages
    box_score$x2p <<- box_score$x2p / num_games
    box_score$x2pa <<- box_score$x2pa / num_games
    
    #get season per game averages
    box_score$x3p <<- box_score$x3p / num_games
    box_score$x3pa <<- box_score$x3pa / num_games
    
    #add percentages to box score
    box_score$x2pPct <<- box_score$x2p / box_score$x2pa
    box_score$x3pPct <<-box_score$x3p / box_score$x3pa
    
    #add season per game to box score
    box_score$TOV <<- box_score$TOV / num_games
    
    #add season per game to box score
    box_score$ORB <<- box_score$ORB / num_games
    
    #add season per game to box score
    box_score$AST <<- box_score$AST / num_games
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)
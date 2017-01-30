```{r, echo=FALSE}

gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" , "Baltic Avenue" , "Income Tax" , "Reading Railroad" , "Oriental Avenue" , "Chance" , "Vermont Avenue" , "Connecticut Avenue" , "Jail" , "St. Charles Place" , "Electric Company" , "States Avenue" , "Virginia Avenue" , "Pennsylvania Railroad" , "St. James Place" , "Community Chest" , "Tennessee Avenue" , "New York Avenue" , "Free Parking" , "Kentucky Avenue" , "Chance" , "Indiana Avenue" , "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" , "Ventnor Avenue" , "Water Works" , "Marvin Gardens" , "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" , "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" , "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"))

chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." , "Advance to St. Charles Place" , "Advance token to nearest Utility" , "Advance token to the nearest Railroad" , "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" , "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" , "Get out of Jail Free" , "Make general repairs on all your property" , "Pay poor tax of $15" , "You have been elected Chairman of the Board" , "Your building loan matures"))

communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor ??? Collect $200" , "Doctor's fees Pay $50" , "From sale of stock you get $45" , "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" , "Income tax refund" , "Life insurance matures ??? Collect $100" , "Pay hospital fees of $100" , "Pay school tax of $150" , "Receive for services $25" , "You are assessed for street repairs" , "You have won second prize in a beauty contest" , "You inherit $100"))
##create vectors for special spaces
chanceSpot <- c(8,23,37)
communityChestSpot <- c(3,18,34)

##create referrence class that will store the current location of our token. It automatically resets after passing 40 (passing Go)
TokenTracker = setRefClass("TokenTracker", 
  fields = list("Location" = "numeric", "Movement" = "numeric", "PrisonSentence" = "numeric", "JailBreak" = "logical", "HouseArrest" = "logical"),
  methods = list(
    initialize = function() {
      Location <<- 1
      Movement <<- 0
      PrisonSentence <<- 0
      JailBreak <<- F
      HouseArrest <<- F
    },
    walk = function() {
      
      if(PrisonSentence == 0){
          
          if((Location + Movement) > 40){
            Location <<- Location + Movement - 40
          }
          
          else{
            Location <<- Location + Movement
            if((Location) == 31){
              Location <<- 11
              PrisonSentence <<- 4
          }
          }

      }
      
      if(PrisonSentence > 0){
        if(PrisonSentence >1){
        if(JailBreak == F){
          PrisonSentence <<- PrisonSentence - 1
        }
        
        if(JailBreak == T){
          PrisonSentence <<- 0
          HouseArrest <<- T
          Location <<- Location + Movement
        }
        }
      else{
        if(JailBreak == F){
          PrisonSentence <<- 0
          HouseArrest <<- T
          Location <<- Location + Movement
        }
        if(JailBreak == T){
          PrisonSentence <<- 0
          HouseArrest <<- T
          Location <<- Location + Movement
        }
      }
      }
    }
))

token <- TokenTracker$new()
token$initialize()

##Create index to tally landing on spaces
gameboard$tally <- 0

##dice function
dice <- function(){
    faces <- sample(1:6, 2, replace=TRUE)
    if(faces[1] == faces[2]) doubles = TRUE
    else doubles = FALSE
    movement = sum(faces)
    return(list(faces=faces, doubles=doubles, movement=movement))
}

##chanceCard function
drawChance <- function(){

  carddrawn <<- sample(chancedeck[[1]],1)
  
  if(carddrawn == 1){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
    token$Location <<- 1
  }
  
  if(carddrawn == 2){
      gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      token$Location <<- 25
  }
  
  if(carddrawn == 3){
      gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      token$Location <<- 12
  }
  
  if(carddrawn == 4){
      gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      if(token$Location < 13 | token$Location > 29){
        token$Location <<- 13
      }
      else{
        token$Location <<- 29
      }
  }
  
  if(carddrawn == 5){
      gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      if(token$Location < 6 | token$Location > 36){
        token$Location <<- 6
      }
    
      if(token$Location > 6 & token$Location < 16){
         gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
         token$Location <<- 16
      }
    
      if(token$Location > 16 & token$Location < 26){
        gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
         token$Location <<- 26
      }
    
      if(token$Location > 26 & token$Location < 36){
        gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
         token$Location <<- 36
      }
      }
     
  if(carddrawn == 6){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      token$Location <<- 6
    }
  
  if(carddrawn == 7){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      token$Location <<- 40
  }
  
  if(carddrawn == 8){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      token$Location <<- 11
      token$PrisonSentence <<- 4
  }
  
  if(carddrawn == 9){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      token$Location <<- token$Location - 3
      
        if(token$Location == 34){
          chestDraw()
        }
  }
         
  cat("You drew chance card", carddrawn, "and today is your lucky day: ", as.character(chancedeck[[2]][carddrawn]))
  }

#community chest functon

chestDraw <- function(){
  communityCard <- sample(communitydeck[[1]],1)
  
  if(communityCard == 1){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
    token$Location <<- 1
  }
  
  if(communityCard == 2){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
      token$Location <<- 11
      token$PrisonSentence <<- 4
  }
  
  
  cat("You drew community card", communityCard, "and today is your lucky day: ", as.character(communitydeck[[2]][communityCard]))
  }
  
#game function, where it resets the token back to Go
game <- function(x){
  token <<- TokenTracker$new()
  token$initialize()
  replicate(x, turn())
}

#reset function, sets tallies to zero
reset <- function(){
  gameboard$tally <<- 0
  token <<- TokenTracker$new()
  token$initialize()
}
  
  
#turn function
turn <- function(){
  
 cat("Start at:", as.character(gameboard[[2]][token$Location]),"\nAddress Number", gameboard[[1]][token$Location],"\n")
  
  token$JailBreak <- FALSE
  token$HouseArrest <- FALSE
  
  roll <- dice()
  token$Movement <- roll$movement
  
    if(roll[2] == TRUE & token$PrisonSentence > 0){
    token$JailBreak <<- TRUE
    }
  
  token$walk()
  
  cat("You rolled a", token$Movement, "and landed on",
  (as.character(gameboard[[2]][token$Location])), "\n")

   if(any(chanceSpot == token$Location)){
      drawChance()
   }
   if(any(communityChestSpot == token$Location)){
     chestDraw()
   }
   

  
  if(roll[2] == TRUE & token$HouseArrest == F){
    gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
    roll <- dice()
    
    token$Movement <-roll$movement
    token$walk()
    
    cat("You rolled doubles and got an extra roll of", token$Movement, "and then landed at", as.character(gameboard[[2]][token$Location]), "\n")
    
    if(any(chanceSpot == token$Location)){
      drawChance()
    }
    
    if(any(communityChestSpot == token$Location)){
      chestDraw()
    }
    

      
    if(roll[2] == TRUE){
       
        roll <- dice()
        token$Movement <- roll$movement
        
         if(roll[2] == TRUE){
            token$Location <<- 11
            token$PrisonSentence <<- 3
            cat("You rolled doubles three times in a row and got sent at Jail, address number 11\n")
         }
          else{
            gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
            token$walk()
            cat("You rolled doubles twice in a row and rolled a", token$Movement, "on your third roll, landing on", token$Location)
            if(any(chanceSpot == token$Location)){
                drawChance()
            }
    
            if(any(communityChestSpot == token$Location)){
                chestDraw()
            } 
          }
    }

  }
cat("\nEnd at:", as.character(gameboard[[2]][token$Location]),"\nAddress Number", gameboard[[1]][token$Location], "\n")
gameboard[[3]][token$Location] <<- gameboard[[3]][token$Location] + 1
    token$HouseArrest <<- F
  }

reset()
```

```{r}
for(i in 1:20){
  game(100)
}

library(dplyr)

gameboard <- gameboard %>% arrange(desc(gameboard[[3]]))
gameboard

xx <- barplot(gameboard$tally, xlab = '', cex.names = 0.5, col = 1:40, space = 0,width = 0.2)
axis(1, at = xx, labels=gameboard$title, tick=FALSE, las=2, line=-1, cex.axis=.6)
```

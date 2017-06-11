library(stats)
library(magrittr)
library(fdrtool)

#' Crossroad simulation.
#' 
#' @param parameters Vector of green to red light ratios for N/S direction for every step.
#' @param trafficDists List containing vectors of mean traffic density values for direction for every day phase.
#' @param distEscape List containing theta parameter describing cars' escape time distribution.
#' @return All cars waiting time sum.
simulate <- function(parameters, trafficDists, distEscape) {
  if (parameters %>% length %>% mod(24) > 0) stop('parameters number must be dividable by 24')
  
  dayPhaseSteps <- length(parameters) / 24 * c(4, 5, 4, 11)
  waitingCars <- c(0, 0, 0, 0)
  waitTime <- 0
  step <- 0
  phaseEnd <- 0
  currentMoves <- c(0, 0, 0, 0)
  
  for (phase in 1:length(dayPhaseSteps)) {
    phaseEnd <- phaseEnd + dayPhaseSteps[[phase]]
    phaseDists <- trafficDists[[phase]]
    
    while (step < phaseEnd) {
      # Update waiting time with cars left
      waitTime <- waitTime + sum(waitingCars)
      
      # Simulate arriving cars
      waitingCars <- waitingCars + rpois(length(phaseDists), phaseDists)
      
      cycles <- list(c(1, 2), c(3, 4))
      cycleEnd <- floor(step) + parameters[floor(step) + 1]
      
      for (cc in cycles) {
        while (step < cycleEnd && any(waitingCars[cc] > 0)) {
          # Update current moves
          startingDirections <- waitingCars[cc] > 0 & currentMoves[cc] <= step
          currentMoves[cc][startingDirections] <- step + rhalfnorm(sum(startingDirections), distEscape$theta)
          
          waitTime <- waitTime + (mod(step, 1) * sum(startingDirections))
          
          step <- min(currentMoves[currentMoves > step])
          direction <- currentMoves == step
          
          # Handle current car's move
          waitingCars[direction] <- waitingCars[direction] - 1
        }
        
        if (step < cycleEnd) {
          step <- cycleEnd
          
        } else if (any(currentMoves > step)) {
          # Let last car leave the crossroad
          direction <- currentMoves > step
          step <- currentMoves[direction]
          waitingCars[direction] <- waitingCars[direction] - 1
        }
        
        cycleEnd <- floor(step + 1)
      }
    }
  }
  
  waitTime
}

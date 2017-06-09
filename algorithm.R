library(plotrix)
library(polynom)

#' Mutation evolutionary algorithm.
#' 
#' @param bounds List of ranges for every dimension.
#' @param mu Parent population count.
#' @param heuristic Optimized function - must accept a list of points and return list of heuristic scores.
#' @param maxSteps steps 
#' @return TODO
mea <- function(bounds, mu, heuristic, maxSteps = 10) {
  P <- initPopulation(bounds, mu)
  hP <- sapply(P, heuristic)
  
  best <- list(point = P[which.min(hP)], h = min(hP))
  
  for (t in 1:maxSteps) { # TODO end conditions
    # for testing
    points(unlist(P), hP)
    invisible(readline(prompt="Press [enter] to continue"))
    
    R <- reproduce(P)
    O <- mutate(R, bounds) # TODO distribution
    hO <- sapply(O, heuristic)
    
    msk <- select(c(P, O), c(hP, hO))
    P <- c(P, O)[msk]
    hP <- c(hP, hO)[msk]
  }
  P
}

initPopulation <- function(bounds, count) replicate(count, randomPoint(bounds), simplify = FALSE)

randomPoint <- function(bounds) sapply(bounds, function(range) runif(1, min = range[1], max = range[2]))

reproduce <- function(population) population # TODO

mutate <- function(population, bounds, mean = 0.5, sd = mean) {
  lapply(population, function(point) (point + rnorm(length(point), mean = mean, sd = sd)) %>% normalize(bounds))
}

normalize <- function(point, bounds) {
  apply(rbind(point, bounds), 2, function(pair) normalizeValue(pair[[1]], unlist(pair[[2]])))
}
  
normalizeValue <- function(value, range) {
  min <- range[[1]]
  max <- range[[2]]
  mod(value - min, max - min) + min
}

select <- function(population, scores) rank(scores) <= length(population)/2 # TODO

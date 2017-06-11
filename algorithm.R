library(plotrix)
library(polynom)
library(magrittr)

#' Mutation evolutionary algorithm.
#' 
#' @param heuristic Optimized function - must accept a list of points and return list of heuristic scores.
#' @param min Explored space lower bound.
#' @param max Explored space upper bound.
#' @param mu Parent population count.
#' @param maxSteps steps 
#' @param mutationMean Mutation normal distribution mean parameter.
#' @param mutationSd Mutation normal distribution standard deviation parameter.
#' @return Best point in log.
mea <- function(heuristic, min, max, mu, maxSteps = 10, mutationMean = 1, mutationSd = mutationMean) {
  if (any(max < min)) stop('All values in min must be lower than corresponding max values.') 
  P <- initPopulation(min, max, mu)
  hP <- sapply(P, heuristic)
  
  best <- list(point = P[[which.min(hP)]], h = min(hP))
  
  for (t in 1:maxSteps) {
    message('generation ', t)
    R <- reproduce(P, hP, mu)
    
    O <- mutate(R, min, max, mutationMean, mutationSd)
    hO <- sapply(O, heuristic)
    
    if (min(hO) < best$h) {
      best <- list(point = O[[which.min(hO)]], h = min(hO))
      message('new best found h = ', best$h)
    }
    
    P <- O
    hP <- hO
  }
  best
}

#' Generate random points in given space.
#' 
#' @param min Space lower bound.
#' @param max Space upper bound.
#' @param count Count of generated points.
#' @return List of random points in given space.
initPopulation <- function(min, max, count) replicate(count, runif(length(min), min = min, max = max), simplify = FALSE)

#' Select and crossover lambda pairs from population with respect to score.
#' 
#' @param population Population of points to select.
#' @param scores Population scores in the same order as in population argument.
#' @param lambda Count of individuals to create.
#' @return List reproduced points.
reproduce <- function(population, scores, lambda) {
  R <- list()
  for (i in 1:lambda) {
    msk <- select(population, scores, 2)
    R <- c(R, list(crossover(population[msk])))
  }
  R
}

#' Crossover two points in given pair.
#' 
#' @param pair Pair of points.
#' @return Child - new point, product of crossover.
crossover <- function(pair) {
  ratio <- runif(1)
  pair[[1]] * ratio + pair[[2]] * (1 - ratio)
}

#' Mutate population using distribution described by mean and sd arguments
#' with respect to min/max bounds.
#' 
#' @param population List of points to mutate.
#' @param min Space lower bound.
#' @param max Space upper bound.
#' @param mean Mean of mutation distribution.
#' @param sd Standard distribution of mutation.
#' @return Mutated population.
mutate <- function(population, min, max, mean, sd) {
  lapply(population, function(point) (point + rnorm(length(point), mean = mean, sd = sd)) %>% normalize(min, max))
}

#' Project point into given bounds.
#' 
#' @param point Vector - point to project.
#' @param min Space lower bound.
#' @param max Space upper bound.
#' @return Projected point.
normalize <- function(point, min, max) mod(point - min, max - min) + min

#' Select count points from population with respect to their scores.
#' 
#' @param population List of points.
#' @param scores Vector with population scores.
#' @param count Number - count of points to select.
#' @return List of selected points.
select <- function(population, scores, count) {
  scores <- -scores # minimize
  normalizedScores <- scores - min(scores)
  sample(1:length(population), size = count, replace = TRUE, prob = normalizedScores)
}


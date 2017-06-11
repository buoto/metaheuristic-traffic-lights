source('algorithm.R')

randomPoint(list(c(1, 2), c(4,5), c(1,6)))
initPopulation(list(c(1, 2), c(4,5), c(1,6)), 4)

normalize(c(-1.1, 1, 0), list(c(-1, 1), c(0.12, 0.20), c(1,2)))

mutate(list(c(0.5, 0.5, 0.5), rep(1, 3)), list(c(0,1), c(0.5, 2), c(0, 1)), 0.1, 0.1)

f <- polynomial(c(-1, -1, -2, -1, 0.2))
plot(f, xlim = c(-10, 10))

mea(list(c(-10, 10)), 10, as.function(f))

source('simulation.R')

a <- replicate(100, simulate(rep(1/2, 24), list(c(1, 0, 0, 1), c(1, 0, 0, 1), c(1, 0, 0, 1), c(1, 0, 0, 1)), list(theta = 5)))

thetas <- 1:20
results <- list()
for (theta in thetas) {
  print(theta)
  a <- replicate(1000, simulate(rep(1/2, 720),
                               list(morning = c(2, 2, 1, 0.5), noon = c(0.5, 0.5, 0.2, 0.1), afternoon = c(2, 2, 0.8, 0.8), night = c(0.3, 0.2, 0.05, 0.1)),
                               list(theta = theta)))
  results <- c(results, list(a))
}
mean <- results %>% sapply(mean)
sd <- results %>% sapply(sd)
df <- data.frame(theta = thetas, t =1/thetas * 120, mean = mean, sd = sd, x = sd/mean)
View(df)

ref <- rep(.5, 720)

# very low max(dist)/theta ratio - losing!
h.low <- function (parameters) simulate(parameters,
         list(morning = c(2, 2, 1, 0.5), noon = c(0.5, 0.5, 0.2, 0.1), afternoon = c(2, 2, 0.8, 0.8), night = c(0.3, 0.2, 0.05, 0.1)),
         list(theta = 15))
solution.low <- mea(replicate(720, c(0, 1), simplify = FALSE), 10, h, maxSteps = 1000, mutationMean = 0.01)
soln.low <- replicate(100, h.low(solution.low$point))
refn.low <- replicate(100, h.low(ref))

# max(dist)/theta ~ 1 - winning
h.1 <- function (parameters) simulate(parameters,
         list(morning = c(2, 3, 1, 0.5), noon = c(0.5, 0.5, 0.2, 0.1), afternoon = c(4, 5, 2, 1), night = c(0.3, 0.2, 0.05, 0.1)),
         list(theta = 4))
refn.1 <- replicate(100, h.1(ref))
solution.1 <- mea(replicate(720, c(0, 1), simplify = FALSE), 10, h.1, maxSteps = 100, mutationMean = 0.01)
soln.1 <- replicate(100, h.1(solution.1$point))

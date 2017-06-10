source('algorithm.R')

randomPoint(list(c(1, 2), c(4,5), c(1,6)))
initPopulation(list(c(1, 2), c(4,5), c(1,6)), 4)

normalize(c(-1.1, 1, 0), list(c(-1, 1), c(0.12, 0.20), c(1,2)))

mutate(list(c(0.5, 0.5, 0.5), rep(1, 3)), list(c(0,1), c(0.5, 2), c(0, 1)), 0.1, 0.1)

f <- polynomial(c(-1, -1, -2, -1, 0.2))
plot(f, xlim=c(-10, 10))

mea(list(c(-10, 10)), 10, as.function(f))

source('simulation.R')
simulate(rep(1/2, 24), rep(1, 4), rep(0, 4), rep(0, 4), rep(1, 4), list(theta = 5))

a <- replicate(100, simulate(rep(1/2, 24), list(c(1, 0, 0, 1), c(1, 0, 0, 1), c(1, 0, 0, 1), c(1, 0, 0, 1)), list(theta = 5)))

results <- c()
for (i in 1:10) {
  a <- replicate(100, simulate(rep(1/2, 720),
                               list(morning = c(2, 2, 1, 0.5), noon = c(0.5, 0.5, 0.2, 0.1), afternoon = c(2, 2, 0.8, 0.8), night = c(0.3, 0.2, 0.05, 0.1)),
                               list(theta = i)))
  results <- c(results, list(mean = mean(a), sd = sd(a)))
}

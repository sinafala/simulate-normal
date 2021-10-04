
# function to simulate one run and return the end position
simRun <- function(
  start=0
  ,stepSize=1
  ,stepProb=0.5
  ,steps=100) 
{ 
  stepRun <- rbinom(steps,1,stepProb)
  runEnd <- 0
  for (step in 1:steps) {
    runEnd <- runEnd + ifelse(stepRun[step]==0,-1*stepSize,stepSize)
  }
  return(runEnd)
}

# function to simulate a bunch of runs, return the end positions, and plot the histogram
simRuns <- function(
  runs=100
  ,start=0
  ,stepSize=1
  ,stepProb=0.5
  ,steps=100
  ,binSize=10)
{
  runEnds <- rep(0,runs)
  for (run in 1:runs) {
    runEnds[run] <- simRun(start,stepSize,stepProb,steps)
  }
  breaks <- seq((min(runEnds)-1.5*binSize),(max(runEnds)+1.5*binSize),binSize)
  hist(runEnds,breaks)
  return(runEnds)
}

# simulate a bunch of runs
runEnds <- simRuns(runs=10000,start=0,stepSiz=1,stepProb=0.5,steps=2000,binSize=10)

# have a look and fit the simulated distributions
library(fitdistrplus)
fitNorm <- fitdist(runEnds, "norm")    
plot(fitNorm)   

library(MASS)
fit <- fitdistr(runEnds, "normal")
para <- fit$estimate
hist(runEnds,prob = TRUE)
curve(dnorm(x, para[1], para[2]), col = 2, add = TRUE)

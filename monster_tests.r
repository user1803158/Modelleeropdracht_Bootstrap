
# import the monster function
source("monster_functie.r")

# test code
mean0norm <- function (sd){
  function(N){
    rnorm(N, mean=0, sd=sd)
  }
}

results = monster(mean0norm, 1, function(x) sd(x), n=100, N=1000, M=300)

hist(results, xlab="estimated variance")

normdist <- function (sd){
  function(N){
    rnorm(N, mean=0, sd=sd)
  }
}

results = monster(mean0norm, 1, function(x) sd(x), n=100, N=1000, M=300)

hist(results, xlab="estimated variance")
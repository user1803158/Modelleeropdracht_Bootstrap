# maak een functie aan die als input een parameter neemt en als output
# een functie produceert die gegeven een getal N, N samples returned
distr <- function(theta){
  function(N){
    rnorm(N, mean=0, sd=theta)
  }
}

f = distr(1)
print(f(10))
print(f(20))
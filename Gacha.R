library(ggplot2)
library(tidyverse)

##Real data

#Dec 12 2023

counts <- c(2876,2881,2939,2813,2894,2756,2722,2798,2798,2629,
2618,2594,2625,2593,2568,2573,2474,2504,2470,2449,2417,2446,2362,
2341,2363,2383,2300,2341,2207,2312,2276,2215,2256,2218,2187,2197,2123,
2139,2072,2070,2213,2060,2112,2093,2037,2087,1995,1996,1981,1985,2040,1923,
1965,1969,1779,1893,1880,1837,1935,1900,1825,1844,1807,1835,1818,1774,1795,1802,
1793,1811,1782,1640,1781,16909,30157,39159,41889,39230,32881,24123,15872,
9230,4620,2060,747,238,66,19,14,3)

n <- 1:90

paimon_moe <- data.frame(n=n, counts=counts)


prob <- round(counts/sum(counts), digits = 5)
head(prob)

##
counts <- c(n1, n2, ..., n90)
names(counts) <- 1:90

# Create a new vector based on counts
new_vector <- unlist(lapply(names(counts), function(number) rep(as.numeric(number), counts[number])))



#plot
plot(N, counts, type="h",
     main = "Real data from paimon.moe Nov 07-23")





##theoretical results based on rule 1 and 2

p <- 0.006 #base prob
N <- 1:90  #possible range of N

# PMF
pmf_values <- c(dgeom(1:89 - 1, prob = p), 1- sum(dgeom(1:89 - 1, prob = p)))

# Plot

par(mfrow=c(2,1))

plot(N, pmf_values, type = "h", lwd = 2, col = "black",
     xlab = "Number of Rolls", ylab = "Probability",
     main = "Theoretical Dist of N",
     xlim =c(0,90),
     ylim = c(0, max(pmf_values)))
axis(side = 1, at = 90, labels = 90, col.axis = "red", lwd.axis = 2)

plot(N, counts, type="h",
     xlab = "Number of Rolls",
     ylab = "counts",
     main = "Real data from paimon.moe Nov 07-23")
axis(side = 1, at = 90, labels = 90, col.axis = "red", lwd.axis = 2)


#Simulation


##Using function to find a
roll <- function(a=0.06){
  nsim <- 100000 #how many wins we want to get
  r <- 1 #no.of rows
  k <- 0 #dummy variable to keep track of ith wins
  N <- vector("numeric", nsim) #results
  
  for (i in 1:nsim) {
    while (r <= 90) {
      p <- ifelse(r < 74, 
                  0.006, 
                  0.006 + a * (r - 73))
      X <- rbinom(n=1, size = 1, prob = p)
      
      if (X == 0) { 
        r <- r + 1 #if loss cont. rolling
      } else {
        N[k + 1] <- r #record the counts
        k <- k + 1 #add 1 to winning no.
        r <- 1 #reset rolls after win
        break
      }
    }
  }
  result <- c(nsim/sum(N),mean(N)) 
  result <- nsim/sum(N) - 0.016
  return(result)
}


set.seed(789687)
roll(a=0.0525)

# optimize

set.seed(789687)

roll <- function(a=0.06){
  nsim <- 10000 #how many wins we want to get
  r <- 1 #no.of rows
  k <- 0 #dummy variable to keep track of ith wins
  N <- vector("numeric", nsim) #results
  
  for (i in 1:nsim) {
    while (r <= 90) {
      p <- ifelse(r < 74, 
                  0.006, 
                  0.006 + a * (r - 73))
      X <- rbinom(n=1, size = 1, prob = p)
      
      if (X == 0) { 
        r <- r + 1 #if loss cont. rolling
      } else {
        N[k + 1] <- r #record the counts
        k <- k + 1 #add 1 to winning no.
        r <- 1 #reset rolls after win
        break
      }
    }
  }
  #result <- c(nsim/sum(N),mean(N)) 
  result <- nsim/sum(N) 
  return(result)
}

##Optimize

#optimize(roll, interval = c(0.05,0.06))

nsim <- 100
M <- vector(length = nsim)
O <- vector(length = nsim)

for(i in 1:nsim){
  Optim <- optimize(roll, interval = c(0.05,0.06))
  M[i] <- Optim$minimum
  O[i] <- Optim$objective
}

mean(M)
sd(M)
mean(O)
sd(O)

#95% CI
mean(M) + c(-1,1)*1.96*sd(M)


#M[i,] <- c(Optim$minimum, Optim$objective)

#######
##KS test

#for loop
set.seed(789687)
nsim <- sum(counts) #how many wins we want to get
r <- 1 #no.of rows
k <- 0 #dummy variable to keep track of ith wins
N <- vector(length = nsim) #results
a <- 0.058 #proposed coefficient

for (i in 1:nsim) {
  while (r <= 90) {
    p <- ifelse(r < 74, 
                0.006, 
                0.006 + a * (r - 73))
    X <- rbinom(n=1, size = 1, prob = p)
    
    if (X == 0) {
      r <- r + 1
    } else {
      k <- k + 1
      N[k] <- r
      r <- 1
      break
    }
  }
}

nsim/sum(N)
mean(N)

##KS test 
sim <- table(N)
SIM <- as.data.frame(sim)
ks.test(SIM$Freq, paimon_moe$counts)

##result 
#plot
ggplot() +
  geom_histogram(aes(x=N),bins=90, col="black")+
  scale_x_continuous(breaks = seq(0, 90, length.out = 10)) +
  labs(title="Distribution of Simulations (a=0.0525)")

table(N)
table(counts)

#consolidated prob
nsim/sum(N)
#mean
mean(N)



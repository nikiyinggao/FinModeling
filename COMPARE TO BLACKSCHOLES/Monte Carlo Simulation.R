# Monte Calso Simulation of Call and Put with convergence

MCS_Call <- function(S,K,r,sigma,T,N){ 
  
  # Initialize call value to zero
  C0 <- 0
  
  # N iterations for Monte Carlo simulation f.
 
  for(n in 1:N) {
  C0 <- C0 + max(S*exp((r-0.5*sigma^2)*T+sigma*sqrt(T)* + rnorm(1,0,1))-K,0)
  }
  
  N <- N
  C0 <- exp(-r*T)*C0/N

  # Analytical solution via Black-Scholes
  d1 <- (log(S/K) + (r+sigma^2/2)*T)/(sigma*sqrt(T)) 
  d2 <- d1 - sigma*sqrt(T)
  
  C0_A <- S*pnorm(d1) - exp(-r*T)*K*pnorm(d2)
    
return(cbind(N,C0,C0_A))
  
}

# Calls to function with increasing N values
# Request 𝑛𝑠𝑖𝑚𝑢𝑙𝑎𝑡𝑖𝑜𝑛𝑠=100,1,000,5,000,10,000,20,000,50,000.
N100 <- MCS_Call(100,100,0.05,0.2,1,100)
N1000 <- MCS_Call(100,100,0.05,0.2,1,1000)
N5000 <- MCS_Call(100,100,0.05,0.2,1,5000)
N10000 <- MCS_Call(100,100,0.05,0.2,1,10000)
N20000 <- MCS_Call(100,100,0.05,0.2,1,20000)
N50000 <- MCS_Call(100,100,0.05,0.2,1,50000)

MCS_Call_data <- rbind(N100,N1000,N5000,N10000,N20000,N50000)
MCS_Call_data
MCS_Call_data[,1]

plot(MCS_Call_data[,1], MCS_Call_data[,2], type="o", col="blue", pch="o", lty=1, lwd=2,
     main="MCS for European Call versus Black-Scholes for Increasing N",
     xlab="N (# of Steps)",ylab="Option Value")
points(MCS_Call_data[,1], MCS_Call_data[,3], col="red", pch="*")
lines(MCS_Call_data[,1], MCS_Call_data[,3], col="red",lty=2, lwd=2)

MCS_Put <- function(S,K,r,sigma,T,N){ 
  
  # Initialize put value to zero
  P0 <- 0
  
  # N iterations for Monte Carlo simulation f.
  
  for(n in 1:N){
    P0 <- P0 + max(K- S*exp((r-0.5*sigma^2)*T+sigma*sqrt(T)* + rnorm(1,0,1)),0)
  }
  
  N <- N
  P0 <- exp(-r*T)*P0/N
  
  # Analytical solution via Black-Scholes 

  d1 <- (log(S/K) + (r+sigma^2/2)*T)/(sigma*sqrt(T)) 
  d2 <- d1 - sigma*sqrt(T)

  P0_A <-  exp(-r*T)*K*pnorm(-d2) - S*pnorm(-d1)
  
  return(cbind(N,P0,P0_A))
  
}

# Calls to function with increasing N values
# Request 𝑛𝑠𝑖𝑚𝑢𝑙𝑎𝑡𝑖𝑜𝑛𝑠=100,1,000,5,000,10,000,20,000,50,000.
N100 <- MCS_Call(100,100,0.05,0.2,1,100)
N1000 <- MCS_Call(100,100,0.05,0.2,1,1000)
N5000 <- MCS_Call(100,100,0.05,0.2,1,5000)
N10000 <- MCS_Call(100,100,0.05,0.2,1,10000)
N20000 <- MCS_Call(100,100,0.05,0.2,1,20000)
N50000 <- MCS_Call(100,100,0.05,0.2,1,50000)

MCS_Put_data <- rbind(N100,N1000,N5000,N10000,N20000,N50000)
MCS_Put_data
MCS_Put_data[,1]

plot(MCS_Put_data[,1], MCS_Put_data[,2], type="o", col="blue", pch="o", lty=1, lwd=2,
     main="MCS for European Put versus Black-Scholes for Increasing N",
     xlab="N (# of Steps)",ylab="Option Value")
points(MCS_Put_data[,1], MCS_Put_data[,3], col="red", pch="*")
lines(MCS_Put_data[,1], MCS_Put_data[,3], col="red",lty=2, lwd=2)

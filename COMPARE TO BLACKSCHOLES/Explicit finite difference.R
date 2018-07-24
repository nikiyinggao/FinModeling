

EXFD_Call <- function(S,K,r,sigma,T,M,N){
  
  Smax <- 20*S 
  xmax <- log(Smax)  
  
  dt <- T/M
  dx <- (xmax-log(S))/N 

  xmin <- log(S)-N*dx 
  x <- seq(xmin,xmax,by=dx) 
  
  NN <- 2*N 
  dd <- dt/(dx^2) 
  
  f <- pmax(exp(x)-K,0)  
  g <- f 
  g[1] <- 0
  
  a <- (dd/2)*(sigma^2-dx*(r-sigma^2/2)) 
  b <- (1-dd*sigma^2)
  c <- (dd/2)*(sigma^2+dx*(r-sigma^2/2)) 
  
  for(j in 1:M){
   
    g[NN+1] <- exp(r*j*dt)*(Smax-K) 
    
    for(i in 2:NN){ 
        f[i] <- a*g[i-1]+b*g[i]+c*g[i+1]
    } 
    g <- f 
  } 
  
  C0 <- exp(-r*T)*f[N+1]
  
  # Analytical solution via Black-Scholes
  d1 <- (log(S/K) + (r+sigma^2/2)*T)/(sigma*sqrt(T)) 
  d2 <- d1 - sigma*sqrt(T)

  C0_A <- S*pnorm(d1) - exp(-r*T)*K*pnorm(d2) 
  stability_ratio <- dt/((dx)^2/(2*sigma^2))
  
  return(c(C0,C0_A,stability_ratio)) 
  
}

# With 300 for M and 100 for N the result is stable and reasonably accurate
EXFD_Call(100,100,0.05,0.2,1,300,100)

# With 1000 for M and 100 for N the result is stable and reasonably accurate
EXFD_Call(100,100,0.05,0.2,1,1000,100)

# With 100 for M and 1000 for N the result is not stable
EXFD_Call(100,100,0.05,0.2,1,100,1000)

# Explicit FD Method for Put Option

EXFD_put <- function(S,K,r,sigma,T,M,N){
  
  Smax <- 20*S 
  xmax <- log(Smax)  
  
  dt <- T/M
  dx <- (xmax-log(S))/N 
  
  xmin <- log(S)-N*dx
  x <- seq(xmin,xmax,by=dx) 
  
  NN <- 2*N 
  dd <- dt/(dx^2) 
  
  f <- pmax(K-exp(x),0)  
  g <- f 
  g[1] <- 0
  

  a <- (dd/2)*(sigma^2-dx*(r-sigma^2/2)) 
  b <- (1-dd*sigma^2)
  c <- (dd/2)*(sigma^2+dx*(r-sigma^2/2)) 
  
  for(j in 1:M){
   
    g[NN+1] <- exp(r*j*dt)*(K-Smax) 

    for(i in 2:NN){ 
      f[i] <- a*g[i-1]+b*g[i]+c*g[i+1]
    } 
    g <- f 
  } 
  
  P0 <- exp(-r*T)*f[N+1]
  
  # Analytical solution via Black-Scholes 
  d1 <- (log(S/K) + (r+sigma^2/2)*T)/(sigma*sqrt(T)) 
  d2 <- d1 - sigma*sqrt(T)
 
  P0_A <- exp(-r*T)*K*pnorm(-d2)- S*pnorm(-d1) 
  
  stability_ratio <- dt/((dx)^2/(2*sigma^2))
  
  return(c(P0,P0_A,stability_ratio)) 
  
}


# With 300 for M and 100 for N the result is stable and reasonably accurate
EXFD_put(100,100,0.05,0.2,1,300,100)

# With 1000 for M and 100 for N the result is stable and reasonably accurate
EXFD_put(100,100,0.05,0.2,1,1000,100)

# With 100 for M and 1000 for N the result is not stable
EXFD_put(100,100,0.05,0.2,1,100,1000)




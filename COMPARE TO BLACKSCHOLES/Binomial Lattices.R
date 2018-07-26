EuropeanCallBE <- function (S,X,r,sigma,tau,M)
 
  {
# compute constants
    dt <- tau / M
    u <- exp ( sigma * sqrt (dt))
    d <- 1 /u
    p <- ( exp (r * dt) - d) / (u - d)
 
 # initialise asset prices at maturity ( period M)
    C <- pmax (S * d^(M :0) * u ^(0: M) - X, 0)

 # log / cumsum version
    csl <- cumsum ( log (c(1 ,1: M)))
    tmp <- csl [M +1] - csl - csl [(M +1) :1] + log (p) *(0: M) + log (1-p)*(M :0)
    C0 <- exp (-r* tau )* sum ( exp ( tmp )*C)
    
    d1 <- (log(S/X) + (r+sigma^2/2)*tau)/(sigma*sqrt(tau)) 
    d2 <- d1 - sigma*sqrt(tau)
    C0_A <- S*pnorm(d1) - exp(-r*tau)*X*pnorm(d2)
    
    return (cbind(M, C0, C0_A))
}

nsteps <- rbind(10,20,50,100,150,200,500,1000)

results <- matrix(nrow=length(nsteps),ncol=3)

for(i in 1:length(nsteps)){
  
  results[i,] <- binomial_tree_EOPT(100,100,0.05,1,0.2,nsteps[i],callput="call",return_tree=FALSE) 

}

results

plot(EuropeanCallBE_data[,1], EuropeanCallBE_data[,2], type="o", col="blue", pch="o", lty=1, lwd=2,
     main="Binomial lattice for European Call versus Black-Scholes for Increasing N",
     xlab="N (# of Steps)",ylab="Option Value")
points(EuropeanCallBE_data[,1], EuropeanCallBE_data[,3], col="red", pch="*")
lines(EuropeanCallBE_data[,1], EuropeanCallBE_data[,3], col="red",lty=2, lwd=2)

EuropeanPutBE <- function (S ,X,r,sigma,tau,M)
  
{
  dt <- tau / M
  u <- exp ( sigma * sqrt (dt))
  d <- 1 /u
  p <- ( exp (r * dt) - d) / (u - d)
  
  
  # initialise asset prices at maturity (period M)
  P <- pmax (X - S * d^(M :0) * u ^(0: M), 0)
  
  # log / cumsum version
  csl <- cumsum ( log (c(1 ,1: M)))
  tmp <- csl [M +1] - csl - csl [(M +1) :1] + log (p) *(0: M) + log (1-p)*(M :0)
  P0 <- exp (-r* tau )* sum ( exp ( tmp )*P)
  
  d1 <- (log(S/X) + (r+sigma^2/2)*tau)/(sigma*sqrt(tau)) 
  d2 <- d1 - sigma*sqrt(tau)
  P0_A <- exp(-r*tau)*X*pnorm(-d2) - S*pnorm(-d1)
  
  return (cbind(M, P0, P0_A))
}

#request nstep= 10,20,50,100,200,500,1,000.
N10 <- EuropeanPutBE(100,100,0.05,0.2,1,10)
N20 <- EuropeanPutBE(100,100,0.05,0.2,1,20)
N50 <- EuropeanPutBE(100,100,0.05,0.2,1,50)
N100 <- EuropeanPutBE(100,100,0.05,0.2,1,100)
N200 <- EuropeanPutBE(100,100,0.05,0.2,1,200)
N500 <- EuropeanPutBE(100,100,0.05,0.2,1,500)
N1000 <- EuropeanPutBE(100,100,0.05,0.2,1,1000)

EuropeanPutBE_data <- rbind(N10,N20,N50,N100,N200,N500,N1000)
EuropeanPutBE_data
EuropeanPutBE_data[,1]

plot(EuropeanPutBE_data[,1], EuropeanPutBE_data[,2], type="o", col="blue", pch="o", lty=1, lwd=2,
     main="Binomial lattice for European Put versus Black-Scholes for Increasing N",
     xlab="N (# of Steps)",ylab="Option Value")
points(EuropeanPutBE_data[,1], EuropeanPutBE_data[,3], col="red", pch="*")
lines(EuropeanPutBE_data[,1], EuropeanPutBE_data[,3], col="red",lty=2, lwd=2)

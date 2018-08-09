
install.packages("quadprog")
library(quadprog)

#copy the expected return vector and the covariance matrixto the clipboard
read.excel.mu <- function(header=FALSE,...) {
  read.table(pipe("pbpaste"),sep="\t")
}
excel.data.mu <- read.excel.mu()
excel.data.mu


read.excel.V <- function(header=FALSE,...) {
  read.table(pipe("pbpaste"),sep="\t")
}
excel.data.V <- read.excel.V()
excel.data.V


mu <- c(excel.data.mu$V1, excel.data.mu$V2, excel.data.mu$V3, excel.data.mu$V4, excel.data.mu$V5,
        excel.data.mu$V6, excel.data.mu$V7, excel.data.mu$V8, excel.data.mu$V9, excel.data.mu$V10,
        excel.data.mu$V11, excel.data.mu$V12)
V <- cbind(excel.data.V$V1, excel.data.V$V2, excel.data.V$V3, excel.data.V$V4, excel.data.V$V5,
       excel.data.V$V6, excel.data.V$V7, excel.data.V$V8, excel.data.V$V9, excel.data.V$V10,
       excel.data.V$V11, excel.data.V$V12)
mu
V


# Function for the Efficient Frontier (need quadprog library) 
  Frontier <- function(n_assets,mu_vector,V_matrix) {

  mu_frontier <- seq(min(mu_vector)+.001, max(mu_vector)-.001, by=0.001)

  m <- length(mu_frontier)
  sigma_vector <- rep(0,m)
  weights_matrix <- matrix(0,nrow=m,ncol=n_assets)

  d <- rep(0,n_assets)

  tmp <- matrix(0,nrow=n_assets,ncol=n_assets)
  diag(tmp) <- 1
 
  #the problem is: min(-d'b + 1/2*b'Db), s.t. A'b>=b0
  A <- cbind(rep(1,n_assets),mu_vector,tmp)

  #Create Dataset
  set.seed(1)
  x <- runif(100)
  y <- runif(100)
  z <- y*x

  #assign color using rainbow function
  range01 <- function(x)(x-min(x))/diff(range(x))
  rainbow(12)
  cRamp <- function(x){
    cols <- colorRamp(rainbow(12))(range01(x))
    apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
  }  
  
  
  # For loop to solve for weights on frontier for array of mu values
  for(i in 1:m){
    
    # b0 vector is so sum of weights is one, portfolio return is mu_frontier
    # each weight is larger than zero - no short sales
    b0 <- c(1,mu_frontier[i],rep(0,n_assets))
    
    Z <- solve.QP(V_matrix,d,A,b0,meq=2)
    sigma_vector[i] <- sqrt(2*Z$value)
    weights_matrix[i,] <- Z$solution
    
  }
  
  # Plot the frontier
  plot(sigma_vector, mu_frontier, xlim=c(0,max(sqrt(diag(V_matrix)))),ylim=c(min(mu_vector),max(mu_vector)),"l",
       xlab="Standard Deviation of the Portfolio", ylab="Mean of the Portfolio",
       main="Efficient Frontier with No-Short Sales Constraint")
  list <- paste("weights_matrix", 1:n_assets)
  barplot(t(weights_matrix), names.arg=mu_frontier,xlab="Mean of the Portfolio",ylab="Weights for Assets",
          col = cRamp(z), border=0,legend=list("SPY","QQQ","IWD","IWF","MDY","IWM","IWN","IWO","IYR","EWJ","EFA","EEM"),
          args.legend=list(x="topright"))
  
  
  return(cbind(weights_matrix, mu_frontier, sigma_vector))
  
  }
  
  # Call to function using prior mu and V values
  Frontier(12,mu,V)
  
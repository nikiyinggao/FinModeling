# Option Pricing Example

# Include package to connect to Excel
install.packages("openxlsx")
library(openxlsx)

# Path, file, sheet names
Excel_file_path <- "//ncccnsf701z1.wellsfargo.net/C_WBGWCM_Users/A323898/My Documents/Steve Young/"
Excel_file_name <- "Black_Scholes.xlsx"
Excel_full_file_name <- paste(Excel_file_path,Excel_file_name,sep="")
Excel_data_sheet_name <- "Output Data"
Excel_graphs_sheet_name <- "Graphs"

# Load the workbook object once to prevent plots showing up as blanks
Excel_workbook <- openxlsx::loadWorkbook(Excel_full_file_name)

# Black-Scholes Option 
BlackScholes <- function(S, X, rf, d, T, sigma, flag) {
  
  # d1 and d2 values
  F <- S * exp((rf - d)*T)
  H <- exp(-d)*T
  d1 <- (log(F/X) + (.5*(sigma^2) * T))/(sigma * sqrt(T))
  d2 <- (log(F/X) - (.5*(sigma^2) * T))/(sigma * sqrt(T))
  
  # European call and put values and Greeks
  call <- exp(-rf * T) * (F * pnorm(d1) - X * pnorm(d2))
  put <- exp(-rf * T) * (X * pnorm(-d2) - F * pnorm(-d1))
  
  calldelta <- H * pnorm(d1)
  putdelta <- H *(pnorm(d1)-1)
  callgamma <-putgamma <- H*dnorm(d1)/S*sigma*sqrt(T)
  callvega <-putvega <- S*H*dnorm(d1)*sqrt(T)
  calltheta <-d*S*H*pnorm(d1)-S*H*dnorm(d1)*sigma/2*sqrt(T)-rf*X*exp(-rf*T)*pnorm(d2)
  puttheta <- -d*S*H*pnorm(-d1)-S*H*dnorm(d1)*sigma/2*sqrt(T)+rf*X*exp(-rf*T)*pnorm(-d2)
  callpho <-X*T*exp(-rf*T)*pnorm(d2)
  putho <- -X*T*exp(-rf*T)*pnorm(-d2)
  if(flag=='call'){
    
    return(call)
  } else if(flag=='put') {
    
    return(put)
  } else if(flag=='calldelta'){
    
    return(calldelta)
    
  }else if(flag=='putdelta'){
    
    return(putdelta)
  } else if(flag=='callgamma') {
    
    return(callgamma)
  } else if(flag=='putgamma') {
    
    return(putgamma)
  } else if(flag=='callvega') {
    
    return(callvega)
  } else if(flag=='putvega') {
    
    return(putvega)
  } else if(flag=='calltheta') {
    
    return(calltheta)
  } else if(flag=='puttheta') {
    
    return(puttheta)
  } else if(flag=='callrho') {
    
    return(callpho)
  } else if(flag=='putrho') {
    
    return(putpho)
  }}

# Sequence of S values to call function followed by inputs and function call
S_sequence <- seq(from = 50, to = 150, by = 1)
X <- 100
rf <- .05
d <- 0
T_one_year <- 1
T_half_year <- 0.5
T_maturity <- .0001
sigma <- .20
cflag <- 'call'

Calls_one_year <- round(BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, cflag),digits=4)
Calls_half_year <- round(BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, cflag),digits=4)
Calls_maturity <- round(BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, cflag),digits=4)

pflag <- 'put'

Puts_one_year <- round(BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, pflag),digits=4)
Puts_half_year <- round(BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, pflag),digits=4)
Puts_maturity <- round(BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, pflag),digits=4)

# Write results to spreadsheet
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Calls_one_year, startCol=2, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Calls_half_year, startCol=3, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Calls_maturity, startCol=4, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
#put
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Puts_one_year, startCol=5, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Puts_half_year, startCol=6, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Puts_maturity, startCol=7, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

par(mfrow=c(1,1))

# Create plot which we will save to spreadsheet 
Calls <- plot(x = S_sequence, y = Calls_one_year, main = "Black-Scholes Call Values", xlab = "Spot Price",
              ylab = "Option Value", xlim = c(50,150), ylim = c(0, max(Calls_one_year)), type = "l", col = "blue", lwd = 2)
lines(x = S_sequence, y = Calls_half_year, type = "l", col = "red", lwd = 2, lty = 2)
lines(x = S_sequence, y = Calls_maturity, type = "l", col = "green", lwd = 2, lty = 2)
legend(x="topleft", legend=c("1 Yr Call Values","1/2 Yr Call Values", "Maturity Call Values"),
       bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))

# Print pic to view and insertPlot into Excel
print(Calls)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

par(mfrow=c(1,1))

#Put
Puts <- plot(x = S_sequence, y = Puts_one_year, main = "Black-Scholes Put Values", xlab = "Spot Price",
              ylab = "Option Value", xlim = c(50,150), ylim = c(0, max(Puts_one_year)), type = "l", col = "blue", lwd = 2)
lines(x = S_sequence, y =Puts_half_year, type = "l", col = "red", lwd = 2, lty = 2)
lines(x = S_sequence, y = Puts_maturity, type = "l", col = "green", lwd = 2, lty = 2)
legend(x="topright", legend=c("1 Yr Put Values","1/2 Yr Put Values", "Maturity Put Values"),
       bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("yellow","black","blue"))

# Put
print(Puts)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=22,startCol=2,fileType="bmp",units="in",dpi=600)

# Save the workbook object exactly once to prevent plots showing up as blanks
openxlsx::saveWorkbook(Excel_workbook,Excel_full_file_name,overwrite=TRUE)
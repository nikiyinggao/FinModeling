# Include package to connect to Excel
install.packages("openxlsx")
library(openxlsx)

# Path, file, sheet names
Excel_file_path <- "/Users/niki/Downloads/12_July_2018/"
Excel_file_name <- "Black_Scholes.xlsx"
Excel_full_file_name <- paste(Excel_file_path,Excel_file_name,sep="")
Excel_data_sheet_name <- "Output Data"
Excel_graphs_sheet_name <- "Graphs"

# Load the workbook object once to prevent plots showing up as blanks
Excel_workbook <- openxlsx::loadWorkbook(Excel_full_file_name)


# Number of digits for results
options(digits=8)

# Black-Scholes Option 
BlackScholes <- function(S, X, rf, d, T, sigma, flag) {
  
  # d1 and d2 values
  F <- S * exp((rf - d)*T)
  d1 <- (log(F/X) + (.5*(sigma^2) * T))/(sigma * sqrt(T))
  d2 <- (log(F/X) - (.5*(sigma^2) * T))/(sigma * sqrt(T))
  
  # European call and put values and Greeks
  call <- exp(-rf * T) * (F * pnorm(d1) - X * pnorm(d2))
  put <- exp(-rf * T) * (X * pnorm(-d2) - F * pnorm(-d1))
  
  calldelta <- exp(-d * T) * pnorm(d1)
  putdelta <- exp(-d * T) * (pnorm(d1)-1)
  callgamma = putgamma = exp(-d * T) * dnorm(d1)/ (S * sqrt(T) * sigma)
  callvega = putvega = S * exp(-d * T) * dnorm(d1) *sqrt (T)
  callTheta = d*S*exp(-d * T)*pnorm(d1) - (S*exp(-d * T)*dnorm(d1)*sigma/(2*sqrt(T))) - rf*X*exp(-rf * T)*pnorm(d2)
  putTheta = -d*S*exp(-d * T)*pnorm(-d1) - (S*exp(-d * T)*dnorm(d1)*sigma/(2*sqrt(T))) + rf*X*exp(-rf * T)*pnorm(-d2)
  callRho = X*T*exp(-rf * T)*pnorm(d2)
  putRho = X*T*exp(-rf * T)*pnorm(-d2)
  if(flag=='call'){
    
    return(call)
  } else if(flag=='put') {
    
    return(put)
  } else if(flag=='calldelta'){
    
    return(calldelta)
    
  } else if(flag=='putdelta'){
    
    return(putdelta)
    
  } else if(flag=='callgamma'){
    
    return(callgamma)
    
  } else if(flag=='putgamma'){
    
    return(putgamma)
    
  } else if(flag=='callvega'){
    
    return(callvega)
    
  } else if(flag=='putvega'){
    
    return(putvega)
    
  } else if(flag=='callTheta'){
    
    return(callTheta)
    
  } else if(flag=='putTheta'){
    
    return(putTheta)
    
  } else if(flag=='callRho'){
    
    return(callRho)
    
  } else if(flag=='putRho'){
    
    return(putRho)
    
  }
  
}

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
Calls_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, cflag)
Calls_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, cflag)
Calls_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, cflag)

pflag <- 'put'
Puts_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, pflag)
Puts_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, pflag)
Puts_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, pflag)

cdeltaflag <- 'calldelta'
cdelta_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, cdeltaflag)
cdelta_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, cdeltaflag)
cdelta_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, cdeltaflag)

pdeltaflag <- 'putdelta'
pdelta_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, pdeltaflag)
pdelta_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, pdeltaflag)
pdelta_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, pdeltaflag)

cgammaflag <- 'callgamma'
cgamma_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, cgammaflag)
cgamma_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, cgammaflag)
cgamma_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, cgammaflag)

pgammaflag <- 'putgamma'
pgamma_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, pgammaflag)
pgamma_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, pgammaflag)
pgamma_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, pgammaflag)

cvegaflag <- 'callvega'
cvega_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, cvegaflag)
cvega_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, cvegaflag)
cvega_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, cvegaflag)

pvegaflag <- 'putvega'
pvega_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, pvegaflag)
pvega_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, pvegaflag)
pvega_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, pvegaflag)

cThetaflag <- 'callTheta'
cTheta_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, cThetaflag)
cTheta_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, cThetaflag)
cTheta_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, cThetaflag)

pThetaflag <- 'putTheta'
pTheta_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, pThetaflag)
pTheta_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, pThetaflag)
pTheta_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, pThetaflag)

cRhoflag <- 'callRho'
cRho_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, cRhoflag)
cRho_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, cRhoflag)
cRho_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, cRhoflag)

pRhoflag <- 'putRho'
pRho_one_year <- BlackScholes(S_sequence, X, rf, d, T_one_year, sigma, pRhoflag)
pRho_half_year <- BlackScholes(S_sequence, X, rf, d, T_half_year, sigma, pRhoflag)
pRho_maturity <- BlackScholes(S_sequence, X, rf, d, T_maturity, sigma, pRhoflag)

# Write results to spreadsheet
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Calls_one_year, startCol=2, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Calls_half_year, startCol=3, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Calls_maturity, startCol=4, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Puts_one_year, startCol=5, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Puts_half_year, startCol=6, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, Puts_maturity, startCol=7, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cdelta_one_year, startCol=8, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cdelta_half_year, startCol=9, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cdelta_maturity, startCol=10, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pdelta_one_year, startCol=11, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pdelta_half_year, startCol=12, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pdelta_maturity, startCol=13, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cgamma_one_year, startCol=14, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cgamma_half_year, startCol=15, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cgamma_maturity, startCol=16, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pgamma_one_year, startCol=17, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pgamma_half_year, startCol=18, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pgamma_maturity, startCol=19, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cvega_one_year, startCol=20, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cvega_half_year, startCol=21, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cvega_maturity, startCol=22, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pvega_one_year, startCol=23, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pvega_half_year, startCol=24, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pvega_maturity, startCol=25, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cTheta_one_year, startCol=26, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cTheta_half_year, startCol=27, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cTheta_maturity, startCol=28, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pTheta_one_year, startCol=29, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pTheta_half_year, startCol=30, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pTheta_maturity, startCol=31, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cRho_one_year, startCol=32, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cRho_half_year, startCol=33, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, cRho_maturity, startCol=34, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pRho_one_year, startCol=35, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pRho_half_year, startCol=36, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)
openxlsx::writeData(Excel_workbook, Excel_data_sheet_name, pRho_maturity, startCol=37, startRow = 2, colNames=FALSE,
                    rowNames=FALSE, keepNA = TRUE)

# Create plot which we will save to spreadsheet 
Calls <- plot(x = S_sequence, y = Calls_one_year, main = "Black-Scholes Call Values", xlab = "Spot Price",
          ylab = "Option Value", xlim = c(50,150), ylim = c(0, max(Calls_one_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = Calls_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = Calls_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr Call Values","1/2 Yr Call Values", "Maturity Call Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
Puts <- plot(x = S_sequence, y = Puts_one_year, main = "Black-Scholes Put Values", xlab = "Spot Price",
          ylab = "Option Value", xlim = c(50,150), ylim = c(0, max(Puts_one_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = Puts_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = Puts_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr Put Values","1/2 Yr Put Values", "Maturity Put Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          cdelta <- plot(x = S_sequence, y = cdelta_one_year, main = "Black-Scholes calldelta Values", xlab = "Spot Price",
                        ylab = "Call delta", xlim = c(50,150), ylim = c(0, max(cdelta_one_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = cdelta_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = cdelta_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr calldelta Values","1/2 Yr calldelta Values", "Maturity calldelta Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          pdelta <- plot(x = S_sequence, y = pdelta_one_year, main = "Black-Scholes putdelta Values", xlab = "Spot Price",
                       ylab = "Put delta", xlim = c(50,150),  type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = pdelta_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = pdelta_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr putdelta Values","1/2 Yr putdelta Values", "Maturity putdelta Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          cgamma <- plot(x = S_sequence, y = cgamma_one_year, main = "Black-Scholes callgamma Values", xlab = "Spot Price",
                         ylab = "call gamma", xlim = c(50,150),ylim = c(0, max(cgamma_half_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = cgamma_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = cgamma_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr callgamma Values","1/2 Yr callgamma Values", "Maturity callgamma Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          pgamma <- plot(x = S_sequence, y = pgamma_one_year, main = "Black-Scholes putgamma Values", xlab = "Spot Price",
                         ylab = "put gamma", xlim = c(50,150), ylim = c(0, max(cgamma_half_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = pgamma_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = pgamma_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr putgamma Values","1/2 Yr putgamma Values", "Maturity putgamma Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          cvega <- plot(x = S_sequence, y = cvega_one_year, main = "Black-Scholes callvega Values", xlab = "Spot Price",
                         ylab = "call vega", xlim = c(50,150),ylim = c(0, max(cvega_one_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = cvega_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = cvega_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr callvega Values","1/2 Yr callvega Values", "Maturity callvega Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          pvega <- plot(x = S_sequence, y = pvega_one_year, main = "Black-Scholes putvega Values", xlab = "Spot Price",
                         ylab = "put vega", xlim = c(50,150), ylim = c(0, max(pvega_one_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = pvega_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = pvega_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr putvega Values","1/2 Yr putvega Values", "Maturity putvega Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          cTheta <- plot(x = S_sequence, y = cTheta_one_year, main = "Black-Scholes callTheta Values", xlab = "Spot Price",
                        ylab = "call Theta", xlim = c(50,150),type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = cTheta_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = cTheta_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr callTheta Values","1/2 Yr callTheta Values", "Maturity callTheta Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          pTheta <- plot(x = S_sequence, y = pTheta_one_year, main = "Black-Scholes putTheta Values", xlab = "Spot Price",
                        ylab = "put Theta", xlim = c(50,150),  type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = pTheta_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = pTheta_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr putTheta Values","1/2 Yr putTheta Values", "Maturity putTheta Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          cRho <- plot(x = S_sequence, y = cRho_one_year, main = "Black-Scholes callRho Values", xlab = "Spot Price",
                         ylab = "Call Rho", xlim = c(50,150), ylim = c(0, max(cRho_one_year)), type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = cRho_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = cRho_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr callRho Values","1/2 Yr callRho Values", "Maturity callRho Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
          
          pRho <- plot(x = S_sequence, y = pRho_one_year, main = "Black-Scholes putRho Values", xlab = "Spot Price",
                         ylab = "Put Rho", xlim = c(50,150),  type = "l", col = "blue", lwd = 2)
          lines(x = S_sequence, y = pRho_half_year, type = "l", col = "red", lwd = 2, lty = 2)
          lines(x = S_sequence, y = pRho_maturity, type = "l", col = "green", lwd = 2, lty = 2)
          legend(x="topleft", legend=c("1 Yr putRho Values","1/2 Yr putRho Values", "Maturity putRho Values"),
                 bty="n",lty=c(1,2,2), lwd=c(2,2,2), cex=0.75, col=c("blue","red","green"))
# Print pic to view and insertPlot into Excel
print(Calls)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(Puts)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(cdelta)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(pdelta)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(cgamma)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(pgamma)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(cvega)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(pvega)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(cTheta)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(pTheta)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(cRho)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

print(pRho)
openxlsx::insertPlot(Excel_workbook,sheet=Excel_graphs_sheet_name,width=7,height=5,startRow=2,startCol=2,fileType="bmp",units="in",dpi=600)

# Save the workbook object exactly once to prevent plots showing up as blanks
openxlsx::saveWorkbook(Excel_workbook,Excel_full_file_name,overwrite=TRUE)

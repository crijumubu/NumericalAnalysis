### INITIAL INFORMATION

xi <- 2.5
xip1 <- 2.9
presetEror <- 3
f_xi <- sin(4*(xi)+6)
f_xip1 <- sin(4*(xip1)+6)
percentageRelativeError <- 1000 ### BY DEFAULT I SET PERCENTAGE RELATIVE ERROR VALUE TO 1000, THIS VALUE CAN BE WHATEVER NUMBER GREATER THAN PRESET ERROR VALUE
contIteraion <- 1
result <- f_xi
operationCos <- cos(4*(xi)+6)
operationSen <- sin(4*(xi)+6)
constAddByDerivative <- 4
actualSign <- 1

while(presetEror <= percentageRelativeError){
  if (contIteraion %% 2 != 0){
    result <-  result + (((constAddByDerivative**contIteraion)*actualSign)*operationCos)*((xip1-xi)**contIteraion)/(factorial(contIteraion))
  }else{
    actualSign <- actualSign * -1
    result <- result + (((constAddByDerivative**contIteraion)*actualSign)*operationSen)*((xip1-xi)**contIteraion)/(factorial(contIteraion))
  }
  percentageRelativeError <- abs((f_xip1-result)/f_xip1)*100
  contIteraion <- contIteraion + 1
}

### OUTPUT

cat("The final result obtained with the Taylor's serie is", result, "\n")
cat("The number of derivatives needed was", contIteraion - 1, "\n")
cat("The percentage relative error was", percentageRelativeError, "%")

### STUDENT: CRISTIAN JULIAN MUNOZ BUENAHORA - 000430876
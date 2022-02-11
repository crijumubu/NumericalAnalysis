### INITIAL INFORMATION FOR LABORATORY SOLUTION

minLimit <- -2
maxLimit <- 2
step <- 0.3
xLower <- minLimit
xUpper <- xLower + step
roots <- c()
contRoots <- 0

### FUNCTIION CREATED TO EVALUATE THE PROPOSED FUNCTION GIVEN A X VALUE
evaluateFunction <- function(xValue) {
  return (-5*(xValue**3) + 2*(xValue**2) +3*xValue)
}

### FIRST WHILE LOOP, THIS HELP ME TO DETERMINATE IF IN THE XLOWER AND XUPPER RANGE IS A ROOT
while(xUpper <= maxLimit){
  xLower <- xUpper
  xUpper <- xLower + step
  
  if (evaluateFunction(xLower) * evaluateFunction(xUpper) < 0){
    presetError <- 0.5
    percentageRelativeError <- 1000
    
    bisectionxLower <- xLower
    bisectionxUpper <- xUpper
    bisectionxRMinusOne <- 0
    evaluatebisectionxUpper <- evaluateFunction(bisectionxUpper)
    
    ### SECOND WHILE LOOP, THIS HELP ME TO FIND THE ROOT VALUE, KNOWING THAT IN THE XLOWER AND XUPPER RANGE IS A ROOT
    while (percentageRelativeError >= presetError){
      evaluatebisectionxLower <- evaluateFunction(bisectionxLower)
      
      bisectionxR <- (bisectionxLower + bisectionxUpper)/2
      evaluatebisectionxR <- evaluateFunction(bisectionxR)
      
      percentageRelativeError <- abs((bisectionxR - bisectionxRMinusOne)/bisectionxR)*100
      
      if(evaluatebisectionxLower * evaluatebisectionxR > 0){
        bisectionxLower <- bisectionxR
      }
      else{
        bisectionxUpper <- bisectionxR
      }
      
      bisectionxRMinusOne <- bisectionxR
      
    }
    contRoots <- contRoots + 1
    roots[contRoots] <- bisectionxR
    
  }
}

### SOLUTION OUTPUT

cat("The program have found", contRoots, "roots for the proposed function, these are:\n")
for (i in roots){
  cat("->", i,"\n")
}

### STUDENT: CRISTIAN JULIAN MUNOZ BUENAHORA - 000430876
### DEFAULT VECTOR

x <- c(5,7,10,9,80,31,2,80)
xlength<-length(x)

### MEAN


sum <- 0
for (i in 1:xlength){
  sum<-sum+x[i]
}

mean<-sum/length(x)

cat("The mean of the vector values is", mean, "\n")
 
### MEDIAN


for (i in 1:xlength){
  for (j in i:xlength){
    if (x[i] > x[j]){
      temp<-x[i]
      x[i]<-x[j]
      x[j]<-temp
    }
  }
}

median<-NULL
if (xlength/2 != xlength%/%2){ ### IF THIS IS TRUE IT MEANS THAT THE NUMBER OF ELEMENTS OF THE VECTOR IS ODD (IT SHOULD BE CLARIFIED THAT IT COULD BE DONE WITH THE MODULUS OPERATOR BUT IN THIS CASE WE WERE NOT ALLOWED, THEN WE RESORTED TO USING THE INTEGER DIVISION OPERATOR AS AN AID)
  median<-x[(xlength%/%2)+1]
} else{
  median<-(x[xlength/2]+x[(xlength/2)+1])/2
}

cat("The median of the vector values is", median, "\n")

### MODE


dictionaryPositionsNumPos <- c() # DICTIONARY THAT GIVEN A NUMBER OF THE VECTOR X TELLS ME THE POSITION THAT WILL HAVE ASSIGNED THAT VALUE IN THE VECTOR REPETITIONSVALUE IN WHICH IS THE NUMBER OF REPETITIONS (NEXT LINE)
repetitionsValue <- c() # VECTOR WHICH CONTAINS THE NUMBER OF TIMES EACH VALUE OF VECTOR X IS REPEATED

for (i in 1:length(x)){
  dictionaryPositionsNumPos[x[i]]<-i
  repetitionsValue[i]<- 0 # BY DEFAULT, THE NUMBER OF REPEATS THAT EACH ELEMENT HAS IN THE VECTOR IS SET TO 0
}

dictionaryPositionsPosNum <- c()# DICTIONARY THAT GIVEN THE POSITION ASSIGNED IN THE VECTOR REPETITIONSVALUE TELLS ME TO WHICH VALUE OF THE VECTOR X CORRESPONDS TO
for (i in 1:length(x)){
  repetitionsValue[dictionaryPositionsNumPos[x[i]]]<-repetitionsValue[dictionaryPositionsNumPos[x[i]]]+1
  dictionaryPositionsPosNum[i]<-x[i]
}

greatestNumbers <- c()
greaterNumber <- repetitionsValue[1]
greatestNumbers[1] <- dictionaryPositionsPosNum[1]
for (i in 2:length(repetitionsValue)){
  if (greaterNumber < repetitionsValue[i]){
    greaterNumber <- repetitionsValue[i]
    greatestNumbers <- c()
    greatestNumbers[1] <- dictionaryPositionsPosNum[i]
  } else if (greaterNumber == repetitionsValue[i]){
    greatestNumbers[length(greatestNumbers)+1]<-dictionaryPositionsPosNum[i]
  }
}

cat("The mode of the vector values is", greatestNumbers, "\n")

### STUDENT: CRISTIAN JULIAN MUNOZ BUENAHORA - 000430876
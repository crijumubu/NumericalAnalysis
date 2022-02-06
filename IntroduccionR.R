### PART 1

x <- 10
y <- 25
z <- sum(x,y)
cat("x + y = ", z)

### PART 2

i<-c(5,3,1,3,8,10)
cat(i)
j<-c(15,13,11,13,18,110)
cat(j)
k<-c(i,j)
cat("This is the union of vectors -> ", k, "\n")
cat(paste("The concatenated vector is -> "), i, j, "\n")

### PART 3

if (x < y){
    print("Y is greater than X")
}else if(y < x){
    print("X is lower than Y")
}else{
    print("X and Y aren't equals")
}

### PART 4

for (i in 1:6){
    cat("Iteration ", i, "\n")
    ###if (i>=3){
        ###break
    ###}
}

i = 0
while (i<6){
    cat(i, " is lower than 6\n")
    i=i+1
}

### PART 5

cat("The elements of the vector k are -> ", length(k), "\n")
cat("The third element of the vector k is -> ", k[3])
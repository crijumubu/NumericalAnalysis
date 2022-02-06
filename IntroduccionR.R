### PARTE 1

x <- 10
y <- 25
z <- sum(x,y)
cat("x + y = ", z)

### PARTE 2

i<-c(5,3,1,3,8,10)
cat(i)
j<-c(15,13,11,13,18,110)
cat(j)
k<-c(i,j)
cat("Esta es la union de los vectores -> ", k, "\n")
cat(paste("El vector concatenado es -> "), i, j, "\n")

### PARTE 3

if (x < y){
    print("Y es mayor que X")
}else if(y < x){
    print("X es mayor que Y")
}else{
    print("X y Y no son iguales")
}

### PARTE 4

for (i in 1:6){
    cat("Iteracion ", i, "\n")
    ###if (i>=3){
        ###break
    ###}
}

i = 0
while (i<6){
    cat(i, " es menor a 6\n")
    i=i+1
}

### PARTE 5

cat("Los elementos del vector k son -> ", length(k), "\n")
cat("El tercero elemento del vector k es -> ", k[3])
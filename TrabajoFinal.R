
#####Ejercicio 2####
#Literal a
library(readxl)
data1 <- read_excel("poblacion1.xlsx",sheet = 1,col_names = TRUE,na = "")
str(data1)
View(data2)
ncol(data1)
nrow(data1)
data2 <- read_excel("poblacion2.xlsx",sheet = 1,col_names = TRUE,na = "")
str(data2)
ncol(data2)
m<-nrow(data2)
colMeans(data[,-1])
m
class(m)
#Literal b
poblacion<-merge(data1,data2)
ncol(poblacion)
quantile(poblacion[,10], probs = seq(0, 1, 0.25), na.rm = TRUE)

v<-frequency(poblacion[,10])
v
class(poblacion[,10])
v<-as.logical(poblacion[,10])
v
a<-numeric(nrow(poblacion))
a
for (i in 1:nrow(poblacion)){
  if(poblacion[i,10]=="SI"){
    a[i]<-1 
  }
    
}
a
frequency(a)
barplot(a)
literalc<-function(data){
  data1<-data[-1,]
  v<-character(ncol(data1))
  
  for (i in 1:ncol(data1)){
    v[i]<-class(data1[,i])
    if(v[i]=="numeric"){
      boxplot(data1[,i])
  }if(v[i]=="character")
    vec <- as.logica;(data1[,i])
    barplot(data1[,i])
  }
  }
  v
}
literalc(poblacion)




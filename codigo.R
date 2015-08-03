
#########ESCUELA POLITECNICA NACIONAL#######

###Regresion Lineal Multiple###
#Nombre: Katherine Morales

#1.- Realice un Fork del repositorio Regresion-Lineal-Multiple.

#2.-  Genere un archivo rlm.R, de tal forma que le permita responder las siguientes preguntas:
#2.1.- Leer los archivos poblacion1.xlsx y poblacion2.xlsx , y analizar sus dimensiones.
library(readxl)
data1<-read_excel("poblacion1.xlsx", sheet=1, na=" ")
str(data1)
data22<-read_excel("poblacion2.xlsx", sheet=1, na=" ")
str(data2)

#Dimensiones

#Cada data(i) tiene
#observaciones:
nrow(data1)
nrow(data2)
#variables
ncol(data2)
ncol(data2)

#2.2.- Una los archivos leídos en un mismo objeto llamado población.
#Uso la funcion merge para unir dos data frames
poblacion <- merge(data1,data2,by="identificador",suffixes=c("",""))
View(poblacion)
#2.3.- Cree un código que identiﬁque la clase de cada variable y genere diagramas
#de cajas para variables continuas y diagramas de barras para variables discretas.

#Creo una funcion para una data en general

#la clase de cada variable de nuestra data
clase<-function(data){
  v<-character(ncol(data))
  
  for (i in 1:ncol(data)){
    v[i]<-class(data[,i])
  }
  cl<-data.frame(names(data),v)
  cl
} 
#los diagramas para cada variable de nuestra data
diagramas<-function(data){
  
  for(i in 2:ncol(data)){
    if(is.numeric(data[,i])==TRUE){
      boxplot(data[,i])
      title(names(data)[i])
    }
  }
  
  for(i in 2:ncol(data)){
    if(is.numeric(data[,i])!=TRUE){
      barplot(table(data[,i]))
      title(names(data)[i])
    }
  }
  
}
clase(poblacion)
diagramas(poblacion)

#2.3.- Cree un código que calcule automáticamente el mínimo, media,
#máximo, desviación estándar, primer cuartil de cada variable 
#numérica y la frecuencia en el caso de variables categóricas.

info<-function(data){
  maximo <-numeric(ncol(data))
  minimo <-numeric(ncol(data))
  media <-numeric(ncol(data))
  desv_est <-numeric(ncol(data))
  primer_cuartil <-numeric(ncol(data))
  
  for(i in  2:ncol(data)){
    if(is.numeric(data[,i])==TRUE){
      maximo[i] <- max(data[,i])
      minimo[i] <- min(data[,i])
      media[i] <- mean(data[,i])
      desv_est[i] <- sd(data[,i])
      primer_cuartil[i] <- quantile(data[,i],probs=seq(0,1,0.25),na.rm = FALSE)[2]
    }
  }
informacion<-data.frame(names(data),maximo,minimo,media,desv_est,primer_cuartil)
informacion 
}
info(poblacion)
frec1  <- table(data[,9])/ncol(data)
frec2 <- table(data[,10])/ncol(data)
frecuencia<-data.frame(frec1,frec2)
frecuencia

#2.3.- Calcule la correlación entre la variable dependiente poblacion
#  y cada una de las variables explicativas (numéricas).

correlaciones<-function(data){
  
  correlacion <- numeric(ncol(data))
  
  for(j in 2:ncol(data)){
    if(is.numeric(data[,j])==TRUE){
      correlacion[j] <- cor(data[,2], data[,j])
  }

}
informacion<-data.frame(names(poblacion),correlacion)
informacion
}
correlaciones(poblacion)
#Notemos que en los valores iguales a cero es porque son variables no numericas
#y en el otro caso es la variable identificacion


#2.5.- Considere la variable categórica serv.bas.compl con una conﬁabilidad 
# del 90%, ¿Puede asumirse que la media de la variable poblacion en el 
# grupo serv.bas.compl: SI es distinta a la media del grupo 
#serv.bas.compl: NO ?


#Transformo las variables no numericas en factores
#para poder usar el test t(student)
names(poblacion)
serv_fact <- factor(poblacion[,"serv.bas.compl"],levels=c("SI","NO"),labels=c("si","no"))
region_fact <- factor(poblacion[,"region"],levels=c("A","B"),labels=c("a","b"))
poblacion_fact <- data.frame(poblacion[,1:8],region_fac,serv_fac)
poblacion_fact
ncol(poblacion_fact)
#Analizo el diagrama de cajas entre la variable
#poblacion y serv.bas.compl
plot(poblacion ~ serv_fact , data = poblacion_fact) 

#Prueba de hipotesis: 
t <- t.test(poblacion ~ serv_fact , data=poblacion_fact, conf.level=0.9)
t
# Por lo tanto se acepta que la diferencia de medias es igual a cero, pues se encuantra dentro
#de nuestro intervalo de confianza


#2.6.- Considerando los cálculos anteriores genere el modelo de regresión 
# lineal múltiple que mejor se ajuste a los datos. 
# Para ello tomaremos en cuenta el criterio de la correlacion
#que existe entre nuestras variables explicativas y la poblacion.
# es decir "menores.18", "tasa.crimen" pues presentaron mayor correlacion 

reg1 <- lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen, data=poblacion_fact)
summary(reg1)

qf(0.90,df1=3,df2=36)
#F-statistic: 2.173<F(0.5)=2.242605
#Por lo tanto no se rechaza la hipotesis nula
#2.7.- Interprete el R2
R2 <- summary(reg1)[["r.squared"]]
#R2=0.1533
#alrededor de; 15.33 de nuestros datos son explicados por nuestra regresion


#Esto quiere decir que la regresion explica el: 
porcentaje <- 100*summary(reg1)[["r.squared"]] 
#porcentaje=15.33%
#de la variabilidad.

#2.8.-  Analice la signiﬁcancia de la regresión y de cada uno de los 
# parámetros individuales. 

anova <- aov(reg1)
summary(anova)

#2.9.-  Realice un análisis detallado de los residuos.

residuos<-1:40  
for(i in 1:40){
  residuos[i]<-summary(reg1)[["residuals"]][i]
}

#poblacion vs residuos
plot(poblacion[,"poblacion"],residuos)

#histograma 
hist(residuos)

qqnorm(residuos)
qqline(residuos,col="red")
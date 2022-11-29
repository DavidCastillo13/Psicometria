

library(psych)
library(polycor)
library(ggcorrplot)
library(readxl)



base = Cuestionario_estilo_cognitivo_respuestas_2_

colnames(base)

base = as.data.frame(base[,10:42])
base

unique(base$P1)

ifelse(base == "Parcialmente de acuerdo",1,0)

base[base == "Totalmente en desacuerdo"] = 1
base[base == "Parcialmente en desacuerdo"] = 2
base[base == "Ni en desacuerdo ni deacuerdo"] = 3
base[base == "Parcialmente de acuerdo"] = 4
base[base == "Totalmente de acuerdo"] = 5
base[base == "Totalmente deacuerdo"] = 5

base1 = base[-1,]

uwu = base1[base1$P11 == "3",]

uwu = uwu[uwu$P24 == "1",]


## Preguntas analitico

analitico =uwu[,c(1,2,3,4,5,6,7,8,9,12,14,18,20,21,22,25,26,28,29)]

int = uwu[,c(10,13,15,16,17,19,23,27,30,31,32,33)]

colnames(analitico) = c("Ana-1","Ana-2","Ana-3","Ana-4","Ana-5","Ana-6","Ana-7","Ana-8",
                        "Ana-9","Ana-12","Ana- 14","Ana-18" ,"Ana-20" ,"Ana-21",
                        "Ana-22" ,"Ana-25" ,"Ana-26" ,"Ana-28", "Ana-29")

colnames(analitico)

colnames(int) =  c("Int-10", "Int-13" ,"Int-15", "Int-16" ,"Int-17", 
                   "Int-19", "Int-23", 
                   "Int-27" ,"Int-30", "Int-31" ,"Int-32", "Int-33") 

colnames(int)


for (i in 1:ncol(analitico)){
  analitico[,i] = as.ordered(as.factor(analitico[,i]))
}



for (i in 1:ncol(int)){
  int[,i] = as.factor(int[,i])
}

mat_cor1 <- hetcor(analitico)$correlations #matriz de correlación policorica
ggcorrplot(mat_cor1,type="lower",hc.order = T)

cortest.bartlett(mat_cor1, n=nrow(analitico))->p_esf
p_esf$p

KMO(mat_cor1)


mat_cor2 <- hetcor(int)$correlations #matriz de correlación policorica
ggcorrplot(mat_cor2,type="lower",hc.order = T)

cortest.bartlett(mat_cor2, nrow(int))->p_esf
p_esf$p

KMO(mat_cor2)

completo = data.frame(analitico,int)

mat_cor <- hetcor(completo)$correlations
ggcorrplot(mat_cor,type="lower",hc.order = T)

for (i in 1:ncol(int)) {
  int[,i] = as.integer(int[,i])
}

psych::alpha(int)
?alpha

for (i in 1:ncol(analitico)) {
  analitico[,i] = as.integer(analitico[,i])
}

psych::alpha(analitico)

a=uwu[,-c(11,24)]

for (i in 1:ncol(a)) {
  a[,i] = as.integer(a[,i])
}

cortest.bartlett(a, n = ncol(a))

KMO(a)



mat_cor2<- hetcor(a)$correlations
par(mfrow = c(1, 2))

ggcorrplot(mat_cor,type="lower",hc.order = T)

### Analisis factorial

library(psych)

h= as.numeric(rownames(uwu))
Cuestionario_estilo_cognitivo_respuestas_2_[h,]

base2 = Cuestionario_estilo_cognitivo_respuestas_2_[h,]

base2 = as.data.frame(base2)

colnames(base)

for (i in 1:ncol(uwu)) {
  base2[,i+9] = uwu[,i]
}

base2 = base2[,-c(1,2,20,33)]


colnames(base2) = c("Edad","Sexo","Universidad","Estrato","Programa_Actual",
                    "Semestre_Actual","Promedio_Actual_Acum","Ana-1","Ana-2","Ana-3","Ana-4","Ana-5","Ana-6","Ana-7","Ana-8",
                    "Ana-9","Int-10","Ana-12","Int-13","Ana-14","Int-15", "Int-16" ,"Int-17","Ana-18","Int-19" ,"Ana-20" ,"Ana-21",
                    "Ana-22" ,"Int-23","Ana-25" ,"Ana-26","Int-27" ,"Ana-28", "Ana-29","Int-30", "Int-31" ,"Int-32", "Int-33")

summary(base2)


for (i in 1:ncol(base2[8:38])){
  base2[,i+7] = as.numeric(base2[,i+7])
}

owo = cor(base2[8:38], method = "spearman")

p=fa(owo, nfactors = 2, rotate = "varimax")$loadings

### Explicar las cargas, maso menos si se representan bien en cada dimencion, que hay algunas que estan en dos dimenciones, etc
### Revisar por que con la policorica, vota un error, tal vez la vieja confiable de los psicologos sea la mejor al no saber el error

y=fa(mat_cor,nfactors = 2, rotate = "varimax");y
y$loadings 

for (u in 1:ncol(analitico)) {
  analitico[,u] = as.numeric(analitico[,u])
}

for (u in 1:ncol(int)) {
  int[,u] = as.numeric(int[,u])
}

psych::alpha(analitico)
psych::alpha(int)

## ACP por encima

require(FactoMineR)
require(factoextra)

g = PCA(base2[c(2,3,4,6,8:38)],quali.sup = c(1,2,3,4), graph = F)
g$eig[,2]

plot(g$eig[,2],1:31,type = "h")

hist(g$eig[,2], ylab = "Varianza explicada")
?hist
fviz_pca_var(g, repel = T)

### Baremos

library(stringr)
require(dplyr)


ana = base2 %>% select(starts_with("Ana"))

for (i in 1:ncol(ana)) {
  ana[,i] = as.integer(ana[,i])
}

puntuaciones_ana = apply(ana, 1, sum)

base2$puntuacionAna = puntuaciones_ana

int1 = base2 %>% select(starts_with("Int"))

for (i in 1:ncol(int1)) {
  int1[,i] = as.integer(int1[,i])
}

puntuaciones_int = apply(int1, 1, sum)

base2$puntuacionInt = puntuaciones_int

base2

base2$Sexo

Hombre <- subset(base2, Sexo=='Masculino')
Mujer <- subset(base2, Sexo=='Femenino')

#### Hombres

## Analitica

score <- Hombre$puntuacionAna      # Total score 
tosc <- sort(unique(Hombre$puntuacionAna))             # Levels of total score 
perc <- 100 * (cumsum(prop.table(table(score)))) # Percentiles 
sura <- 100 * (tosc / max(score))        # Success rate 
zsco <- sort(unique(scale(score)))       # Z-score 
tsco <- 50 + 10 * zsco                   # T-score
baremos<-cbind(tosc,perc,tsco,sura,zsco)
baremos<-as.data.frame(baremos)
baremos

scale(c(0,5,2))

## Intuitivo

score1 <- Hombre$puntuacionInt      # Total score 
tosc <- sort(unique(Hombre$puntuacionInt))             # Levels of total score 
perc <- 100 * (cumsum(prop.table(table(score1)))) # Percentiles 
sura1 <- 100 * (tosc / max(score1))        # Success rate 
zsco1 <- sort(unique(scale(score1)))       # Z-score 
tsco1<- 50 + 10 * zsco1                   # T-score
baremos1<-cbind(tosc,perc,tsco1,sura1,zsco1)
baremos1<-as.data.frame(baremos1)
baremos1

baremos1$tosc1[1]

tosc

p = function(i,a){
  if((i-mean(score))/(sd(score))>(a-mean(score1))/(sd(score1))){
    baremo = baremos
    puntuacion = i
    cualidad = "analitico"
  }else{
    baremo = baremos1
    puntuacion = a
    cualidad = "intuitivo"
  }
  percentil = c()
  for (h in 1:nrow(baremo)) {
   
    if(baremo$tosc[h] < puntuacion & puntuacion < baremo$tosc[h+1]){
      percentil=  c(percentil, baremo$perc[h])
      percentil = c(percentil, baremo$perc[h+1])
    }else if (baremo$tosc[h] == puntuacion){
      percentil = c(percentil, baremo$perc[h])
    }
  }
  if(length(percentil>1)){
    percentil = mean(percentil)
  }
  
  if(percentil <=35){
    print(paste0("Adaptativo"))
  }else if(percentil >35 & percentil <=70){
    print(paste0("Quasi:", cualidad))
  } else if(percentil > 70){
    print(paste0(cualidad))
  }
 
}

estilo_cognitivo=c()
for (i in 1:nrow(Hombre)) {
  analitic = Hombre[,39][i]
  intu = Hombre[,40][i]
  estilo_cognitivo = c(estilo_cognitivo,p(analitic,intu))
}

unique(estilo_cognitivo)

estilo_analitico= sum(estilo_cognitivo == "analitico")
estilo_quasi_analitico =sum(estilo_cognitivo == "Quasi:analitico")
estilo_intuitivo = sum(estilo_cognitivo == "intuitivo")
estilo_adaptativo = sum(estilo_cognitivo == "Adaptativo")
estilo_quasi_ituitivo = sum(estilo_cognitivo == "Quasi:intuitivo")

data.frame(estilo_analitico,
           estilo_quasi_analitico ,
           estilo_intuitivo,
           estilo_adaptativo ,
           estilo_quasi_ituitivo)


apply(data.frame(estilo_analitico,
                 estilo_quasi_analitico ,
                 estilo_intuitivo,
                 estilo_adaptativo ,
                 estilo_quasi_ituitivo),1,sum)

baremos1

12*5

#### evaluar en base al mayor puntaje Z

# Hombres de 19 - 68 Adaptativos
# Hombres de 69 - 78 Quasi analiticos
# Hombres entre 78 - 95 Analiticos

# Hombres entre 12 - 32 Adaptativos
# Hombres entre 33 - 41 Quasi intuitivos
# Hombres entre 41 - 60 Intuitivos

### Para las mujeres

## Analitica

score <- Mujer$puntuacionAna      # Total score 
tosc <- sort(unique(Mujer$puntuacionAna))             # Levels of total score 
perc <- 100 * (cumsum(prop.table(table(score)))) # Percentiles 
sura <- 100 * (tosc / max(score))        # Success rate 
zsco <- sort(unique(scale(score)))       # Z-score 
tsco <- 50 + 10 * zsco                   # T-score
baremos<-cbind(tosc,perc,tsco,sura,zsco)
baremos<-as.data.frame(baremos)
baremos

scale(c(0,5,2))

## Intuitivo

score1 <- Mujer$puntuacionInt      # Total score 
tosc <- sort(unique(Mujer$puntuacionInt))             # Levels of total score 
perc <- 100 * (cumsum(prop.table(table(score1)))) # Percentiles 
sura1 <- 100 * (tosc / max(score1))        # Success rate 
zsco1 <- sort(unique(scale(score1)))       # Z-score 
tsco1<- 50 + 10 * zsco1                   # T-score
baremos1<-cbind(tosc,perc,tsco1,sura1,zsco1)
baremos1<-as.data.frame(baremos1)
baremos1

sort((unique(score1) - mean(score1))/(sd(score1)))

sd(score1)

p = function(i,a){
  
  if((i-mean(score))/(sd(score))>(a-mean(score1))/(sd(score1))){
    baremo = baremos
    puntuacion = i
    cualidad = "analitico"
  }else{
    baremo = baremos1
    puntuacion = a
    cualidad = "intuitivo"
  }
  percentil = c()
  for (h in 1:nrow(baremo)) {
    
    if(baremo$tosc[h] < puntuacion & puntuacion < baremo$tosc[h+1]){
      percentil=  c(percentil, baremo$perc[h])
      percentil = c(percentil, baremo$perc[h+1])
    }else if (baremo$tosc[h] == puntuacion){
      percentil = c(percentil, baremo$perc[h])
    }
  }
  if(length(percentil>1)){
    percentil = mean(percentil)
  }
  
  if(percentil <=35){
    print(paste0("Adaptativo"))
  }else if(percentil >35 & percentil <=70){
    print(paste0("Quasi:", cualidad))
  } else if(percentil > 70){
    print(paste0(cualidad))
  }
  
}

estilo_cognitivo=c()
for (i in 1:nrow(Mujer)) {
  analitic = Mujer[,39][i]
  intu = Mujer[,40][i]
  estilo_cognitivo = c(estilo_cognitivo,p(analitic,intu))
}


unique(estilo_cognitivo)

estilo_analitico= sum(estilo_cognitivo == "analitico")
estilo_quasi_analitico =sum(estilo_cognitivo == "Quasi:analitico")
estilo_intuitivo = sum(estilo_cognitivo == "intuitivo")
estilo_adaptativo = sum(estilo_cognitivo == "Adaptativo")
estilo_quasi_ituitivo = sum(estilo_cognitivo == "Quasi:intuitivo")

data.frame(estilo_analitico,
           estilo_quasi_analitico ,
           estilo_intuitivo,
           estilo_adaptativo ,
           estilo_quasi_ituitivo)

apply(data.frame(estilo_analitico,
                 estilo_quasi_analitico ,
                 estilo_intuitivo,
                 estilo_adaptativo ,
                 estilo_quasi_ituitivo),1,sum)

###  Se evalua en base al mayor  

# Mujeres de 19 - 70 Adaptativos
# Mujeres de 71 - 78 Quasi analiticos
# Mujeres entre 78 - 95 Analiticos

# Mujeres entre 12 - 33 Adaptativos
# Mujeres entre 34 - 40 Quasi intuitivos
# Mujeres entre 41 - 60 Intuitivos


##########

p = function(i,a){
  baremos$perc
  
  if(i>a){
    baremo = baremos
    puntuacion = i
    cualidad = "analitico"
  }else{
    baremo = baremos1
    puntuacion = a
    cualidad = "intuitivo"
  }
  percentil = c()
  for (h in 1:nrow(baremo)) {
    
    if(baremo$tosc[h] < puntuacion & puntuacion < baremo$tosc[h+1]){
      percentil=  c(percentil, baremo$perc[h])
      percentil = c(percentil, baremo$perc[h+1])
    }else if (baremo$tosc[h] == puntuacion){
      percentil = c(percentil, baremo$perc[h])
    }
  }
  if(length(percentil>1)){
    percentil = mean(percentil)
  }
  
  if(percentil <=35){
    print(paste0("Adaptativo"))
  }else if(percentil >35 & percentil <=70){
    print(paste0("Quasi:", cualidad))
  } else if(percentil > 70){
    print(paste0(cualidad))
  }
}



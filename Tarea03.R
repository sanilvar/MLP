#  title: "Tarea"
#author: "Ilvar Dario Sanabria"
#date: "29/8/2020"

library(ggplot2)
library(data.table)
library(ggcorrplot)
library(dplyr)

setwd('c:\\tmp\\')
saber = fread('s11_2019II.txt',sep='|')
head(saber)

summary(saber[,c("PUNT_MATEMATICAS", "PUNT_INGLES",'PUNT_C_NATURALES',
                 'PUNT_LECTURA_CRITICA','COLE_NATURALEZA')])

summary(saber[,c('FAMI_ESTRATOVIVIENDA','FAMI_TIENEINTERNET','FAMI_TIENECOMPUTADOR',
                 'FAMI_SITUACIONECONOMICA','ESTU_HORASSEMANATRABAJA')])

summary(saber[,c('DESEMP_INGLES','COLE_CALENDARIO','COLE_AREA_UBICACION','COLE_JORNADA',
                 'FAMI_SITUACIONECONOMICA','ESTU_HORASSEMANATRABAJA','FAMI_NUMLIBROS')])

s11 <- saber[!is.na(saber$PUNT_INGLES),]
s11 <- s11[!is.na(s11$ESTU_HORASSEMANATRABAJA),]
s11 <- s11[!is.na(s11$FAMI_TIENEINTERNET),]
s11 <- s11[!is.na(s11$FAMI_PERSONASHOGAR),]
s11 <- s11[!is.na(s11$COLE_JORNADA),]
s11 <- s11[!is.na(s11$FAMI_ESTRATOVIVIENDA),]
s11 <- s11[!is.na(s11$FAMI_SITUACIONECONOMICA),]
saberN <- s11[,c('PUNT_MATEMATICAS','PUNT_C_NATURALES','PUNT_LECTURA_CRITICA', 
                  'PUNT_SOCIALES_CIUDADANAS','PUNT_INGLES')]


plot(saberN$PUNT_INGLES,saberN$PUNT_MATEMATICAS, pch=20)
plot(saberN$PUNT_C_NATURALES,saberN$PUNT_MATEMATICAS, pch=20)
plot(saberN$PUNT_LECTURA_CRITICA,saberN$PUNT_MATEMATICAS, pch=20)
plot(factor(s11$COLE_JORNADA), s11$PUNT_MATEMATICAS,  pch = 20)# Modelo lineal
plot(factor(s11$FAMI_NUMLIBROS),s11$PUNT_MATEMATICAS,  pch = 20)# Modelo lineal
plot(s11$PUNT_MATEMATICAS, factor(s11$FAMI_TIENECOMPUTADOR), pch = 10)# Modelo lineal
plot(s11$PUNT_MATEMATICAS, factor(s11$ESTU_DEDICACIONLECTURADIARIA), pch = 10)# Modelo lineal

abline(M_2 , col = "red") # A-


mse_m2 <- mean((saberN$PUNT_MATEMATICAS - M_2$fitted.values) ^2)
mse_m2

#hacer un model para pronosticar la nota de matematicas

n_train <- round(0.7 * nrow(saberN))
n_test <-  round(0.3 * nrow(saberN))
head(saberN)

#correlación

corr <- round(cor(saberN),2)
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 2,
           method="circle",
           colors = c("orange","white", "green"),
           title="Correlograma de Boston",
           ggtheme=theme_bw)


set.seed(290820)
indica_train <- sample(nrow(s11), n_train)
set.seed(290820)
indica_test <- sample(nrow(s11), n_test)


muestra_train = s11[indica_train,]
head(muestra_train)
muestra_test = s11[indica_test,]

m1 = lm(PUNT_MATEMATICAS ~ PUNT_INGLES + PUNT_C_NATURALES, data = muestra_train)
#m1 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_INGLES +  PUNT_LECTURA_CRITICA+COLE_AREA_UBICACION,data = muestra_train)
m1 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
          PUNT_SOCIALES_CIUDADANAS+PUNT_INGLES,data = muestra_train) #43.72365
summary(m1)


###################### La metrica se calcula sobre la muestra de prueba ####
y_pron <- predict(m1, muestra_test)
mse_test <- mean((muestra_test$PUNT_MATEMATICAS - y_pron)^2)
mse_test


#Calculamos el modelo introduciendo variables y revisando el resultado

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES,data = muestra_train) #51.31982

m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES~PUNT_LECTURA_CRITICA,data = muestra_train) #45.40393

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES+PUNT_LECTURA_CRITICA +
#         PUNT_INGLES,data = muestra_train) #44.16519

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
#          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
#          COLE_JORNADA + COLE_AREA_UBICACION,data = muestra_train) #43.31526

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
#          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
#          COLE_JORNADA + COLE_AREA_UBICACION + COLE_NATURALEZA,data = muestra_train) #43.29828

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
#          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
#          COLE_JORNADA + COLE_AREA_UBICACION + COLE_NATURALEZA + 
#          ESTU_HORASSEMANATRABAJA,data = muestra_train) #43.24402

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
#          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
#          COLE_JORNADA + COLE_AREA_UBICACION + COLE_NATURALEZA + 
#          ESTU_HORASSEMANATRABAJA + COLE_GENERO,data = muestra_train) #43.23852

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
#          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
#          COLE_JORNADA + COLE_AREA_UBICACION + COLE_NATURALEZA + 
#          ESTU_HORASSEMANATRABAJA + COLE_GENERO + ESTU_GENERO ,data = muestra_train) #42.18585

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
#          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
#          COLE_JORNADA + COLE_AREA_UBICACION + COLE_NATURALEZA + 
#          ESTU_HORASSEMANATRABAJA + COLE_GENERO + ESTU_GENERO ,data = muestra_train) #42.17278

#m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES + PUNT_LECTURA_CRITICA + 
#          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
#          COLE_JORNADA + COLE_AREA_UBICACION + COLE_NATURALEZA + 
#          ESTU_HORASSEMANATRABAJA + COLE_GENERO + ESTU_GENERO + 
#          COLE_CALENDARIO,data = muestra_train) #42.17184

#Se introduce inteeracción entre las variables para ver si mejora el resultado
m2 = lm(PUNT_MATEMATICAS ~ PUNT_C_NATURALES * PUNT_LECTURA_CRITICA * 
          PUNT_SOCIALES_CIUDADANAS + PUNT_INGLES + 
          COLE_JORNADA + COLE_AREA_UBICACION + COLE_NATURALEZA + 
          ESTU_HORASSEMANATRABAJA + COLE_GENERO + ESTU_GENERO + 
          COLE_CALENDARIO ,data = muestra_train) #42.01123

summary(m2)
step(m2, direction = "both", trace = 0)

###################### La metrica se calcula sobre la muestra de prueba ####
y_pron <- predict(m2, muestra_test)
head(y_pron)
mse_test <- mean((muestra_test$PUNT_MATEMATICAS - y_pron)^2)
mse_test

muestra_test$PRONO=y_pron

resultado = data.frame(muestra_test$ESTU_CONSECUTIVO,muestra_test$PRONO)
write.csv(resultado,'c://tmp//Tarea03MLP.txt')

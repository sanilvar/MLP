library(ggplot2)

data(iris)

iris$y <- ifelse(iris$Species == "setosa", "setosa", "otras")
table(iris$y)

ggplot(data = iris, aes(x = y, y = Sepal.Length)) + geom_boxplot()

iris$y <- ifelse(iris$Species == "setosa", 1, 0)
modelo <- glm(y ~ Sepal.Length, data = iris, family = "binomial")
summary(modelo)
coefficients(modelo)

exp(-0.4)
1 / exp(-0.4)


iris$y_otros <- ifelse(iris$Species == "setosa", 0, 1)
modelo <- glm(y_otros ~ Sepal.Length, data = iris, family = "binomial")
summary(modelo)
coefficients(modelo)

exp( 0.4097812 )
# Por cada cm de aumento en la longitud del sepalo aumenta en un 50% más
# el chance de ser de la especie otros (versicolor o virginica) con respecto
# a setosa

exp(-1.7278217)
# Hay mayor probabilidad de ser del evento que no es de inter´res (setosa) con
# respecto a ser del evento de interés (virginica o versicolor), más de cinco veces 
1 /5.62838

invodds <- function(x){
  1 / (1 + x^-1)
}
invodds(1.1)
# odds de 1.1
#p = 0.52, 1-p = 0.48

plot(invodds, xlim = c(0,4), xlab = "odds", ylab = "Prob de exito")



iris$y <- ifelse(iris$Species == "setosa", 1, 0)
modelo <- glm(y ~ Sepal.Length, data = iris, family = "binomial")
summary(modelo)
coefficients(modelo)

# Probabilidades 
iris$p_i <- modelo$fitted.values
iris$p_i_2 <- predict(modelo, data.frame(Sepal.Length = iris$Sepal.Length), type = "response")
iris$p_i_3 <- exp( 27.828521  + -5.175698 * iris$Sepal.Length) / (1 + exp(  27.828521 + -5.175698 *
                                                                              iris$Sepal.Length))
plot(iris$Sepal.Length, iris$p_i, xlab = "x", ylab = "p_i")
abline(h = 0.5, col = "red")


# Probabilidades 

iris$y <- ifelse(iris$Species == "setosa", 0, 1)


modelo <- glm(y ~ Sepal.Length, data = iris, family = "binomial")
coefficients(modelo)

iris$p_i <- modelo$fitted.values
iris$p_i_2 <- predict(modelo, data.frame(Sepal.Length = iris$Sepal.Length), type = "response")
iris$p_i_3 <- exp( -27.828521  + 5.175698 * iris$Sepal.Length) /
  (1 + exp( - 27.828521 + 5.175698 * iris$Sepal.Length))

View(iris)
plot(iris$Sepal.Length, iris$p_i, xlab = "x", ylab = "p_i")
abline(h = 0.5, col = "red")

iris$yhat = ifelse(iris$p_i >= 0.5,1,0)

#Pronostico
data("iris")

iris$y <- ifelse(iris$Species == "setosa", 0, 1)


indica_mue = sample(nrow(iris),round(0.7*nrow(iris)))

training = iris[indica_mue,]
test = iris[-indica_mue,]


m1 <- glm(y ~ Sepal.Length, data = training, family = "binomial")
summary(m1)


test$probs = predict(m1,test, type='response')

#Clasifico de acuerdo a la probabildiad
test$yhat = ifelse(test$probs >= 0.5,1,0)

#Matriz de confusion

table(test$y, test$yhat)
mc = table(test$y, test$yhat)

sum(diag(mc)) / sum(mc)

# Sensibilidad y la especificidad

#Sensibilidad
mc[2,2]/sum(mc[2,])

#error tipo 1
mc[2,1]/sum(mc[2,])

#Especificidad
mc[1,1]/sum(mc[1,])

#Error tipo 2
mc[1,2]/sum(mc[1,])


#Estudio de caso perdidad de clientes celulares
load("C:/Users/User/Documents/EspAnalitica/2do Semestre/Machine learning Probabilistico/Clase06/churn_celulares.RData")

#Revisamos la variable calidad del producto
ggplot(data= insumo, aes(y=calidad_produc, x=factor(target)))+
  geom_boxplot()

ggplot(data= insumo, aes(x=calidad_produc, color=factor(target)))+
  geom_density()

#Revisamos la variable antiguedad
ggplot(data= insumo, aes(fill=antigued, x=factor(target)))+
  geom_bar()

table(insumo$antigued) #casi todos los clientes sos antiguos

#Por region
ggplot(data= insumo, aes(x=region, fill=factor(target)))+
  geom_bar()

prop.table(table(insumo$region,insumo$target),1)*100
barplot(prop.table(table(insumo$region,insumo$target),1)*100)

set.seed(17092020)
train_cel = insumo[sample(280,196),]
set.seed(17092020)
test_cel = insumo[-sample(280,196),]

M1 = glm(target~ calidad_produc + cant_cargas_m1, data=train_cel, family='binomial')

test_cel$probs = predict(M1, test_cel)
test_cel$pron = ifelse(test_cel$probs >= 0.5, 1, 0)

mc_churn = table(test_cel$target,test_cel$pron)
mc_churn

accuracy_test <- sum(diag(mc_churn)) / sum(mc_churn)
accuracy_test  # 100%

#Analisis datos insumo
#Por grupo de edad
ggplot(data= insumo, aes(x=target, fill=factor(grupo_edad)))+
  geom_bar()

#Por Genero
ggplot(data= insumo, aes(x=target, fill=factor(gener)))+
  geom_bar()

#Por Esatdo civil
ggplot(data= insumo, aes(x=target, fill=factor(estado_civil)))+
  geom_bar()
#LOs solteros se retiran mas facil

#Por Uso Swerv Cliente
ggplot(data= insumo, aes(x=target, fill=factor(uso_serv_cliente)))+
  geom_bar()

#Por Uso Dtos Usuario
ggplot(data= insumo, aes(x=target, fill=factor(data_usur)))+
  geom_bar()

#Por Uso Tecnologia
ggplot(data= insumo, aes(x=target, fill=factor(technology)))+
  geom_bar()

#Por Uso band_7
ggplot(data= insumo, aes(x=target, fill=factor(band_7)))+
  geom_bar()

#Por Max_Netowrk_voice
ggplot(data= insumo, aes(x=target, fill=factor(max_network_voice)))+
  geom_bar()

#Por calls_drop_s
ggplot(data= insumo, aes(x=target, y=calls_drop_s, fill=factor(calls_drop_s)))+
  geom_point()

#Por calls_drop_s
ggplot(data= insumo, aes(x=target, fill=factor(consumo_granel_m1)))+
  geom_bar()

#Cantidad cargas
ggplot(insumo, aes(x = factor(target), y = cant_cargas_m1)) +
    geom_dotplot(binwidth =0.5, binaxis = "y", stackdir = "center")

#Cantidad Call_drop_S
ggplot(insumo, aes(x = factor(target), y = calls_drop_s)) +
  geom_dotplot(binwidth =0.6, binaxis = "y", stackdir = "center")

#Cantidad Call_failure_S
ggplot(insumo, aes(x = factor(target), y = calls_failure_s)) +
  geom_dotplot(binwidth =calls_failure_s, binaxis = "y", stackdir = "center")

#Cantidad call_out_month_p
ggplot(insumo, aes(x = factor(target), y = calls_out_month_p)) +
  geom_dotplot(binwidth =0.6, binaxis = "y", stackdir = "center")

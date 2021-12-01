#Creo archivo de datos de ejemplo
d <- data.frame(state=rep(c('NY', 'CA'), c(10, 10)),
                year=rep(1:10, 2),
                response=c(rnorm(10), rnorm(10)))

#Hago la regresión deseada
modell<-by(d, d$state, function(d) lm(d$response ~ d$year))

#sentencia para mostrar los coeficientes de regresión que la transpongo para una mejor edición a positeriori
t(sapply(modell,coef))

#acá hice una prueba solo filtrando los datos de "NY" e hice la regresión para comprobar que estan bien hechos los cálculos
d1 <- subset(d, state=='NY')

lm(response ~ year,d1)

#Creo archivo de datos de ejemplo
d <- data.frame(state=rep(c('NY', 'CA'), c(10, 10)),
                year=rep(1:10, 2),
                response=c(rnorm(10), rnorm(10)))

#Hago la regresi�n deseada
modell<-by(d, d$state, function(d) lm(d$response ~ d$year))

#sentencia para mostrar los coeficientes de regresi�n que la transpongo para una mejor edici�n a positeriori
t(sapply(modell,coef))

#ac� hice una prueba solo filtrando los datos de "NY" e hice la regresi�n para comprobar que estan bien hechos los c�lculos
d1 <- subset(d, state=='NY')

lm(response ~ year,d1)
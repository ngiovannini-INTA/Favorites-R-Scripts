#Forma abreviada

library(lme4)
#formato de datos de GPV.csv (Anio;carav;TP;Sexo;PESO;Edad;Pesodia)
gpv <- read.csv('E:/GPV.csv',sep=';')
fits <- lmList(PESO ~ Edad | carav, data=gpv)
fits2 <- coef(fits)

write.table(fits2, file = 'E:/gpv_out.csv', sep=";", row.names = T, col.names = T, quote=F, na="", dec='.')

#forma larga y complicada
library(tidyr)

datos <- read.csv("C:/Users/giovannini.nicolas/Desktop/ADG.csv", sep=";")

#wide to long
dat_long <- pivot_longer(datos,
                         cols = dia1:dia84,
                         names_to = "dia", 
                         values_to = "peso")

#Genero la variable de tiempo (numerica para la regresion)
dat_long$dia_n <- ifelse(dat_long$dia=='dia1',1,0)
dat_long$dia_n <- ifelse(dat_long$dia=='dia14',14,dat_long$dia_n)
dat_long$dia_n <- ifelse(dat_long$dia=='dia27',27,dat_long$dia_n)
dat_long$dia_n <- ifelse(dat_long$dia=='dia42',42,dat_long$dia_n)
dat_long$dia_n <- ifelse(dat_long$dia=='dia54',54,dat_long$dia_n)
dat_long$dia_n <- ifelse(dat_long$dia=='dia70',70,dat_long$dia_n)
dat_long$dia_n <- ifelse(dat_long$dia=='dia77',77,dat_long$dia_n)
dat_long$dia_n <- ifelse(dat_long$dia=='dia84',84,dat_long$dia_n)


#Hago la regresión deseada
reg <- by(dat_long, dat_long$id, function(dat_long) lm(dat_long$peso ~ dat_long$dia_n))

#sentencia para mostrar los coeficientes de regresión que la transpongo para una mejor edición a posteriori
dat_adg <- data.frame(t(sapply(reg,coef)))

#renombro las columnas
colnames(dat_adg) <- c('int','ADG')
dat_adg$id <- rownames(dat_adg)
dat_adg <- dat_adg[,c('id','ADG')]

#acá hice una prueba solo filtrando los datos de "id1" e hice la regresión para comprobar que estan bien hechos los cálculos
id1 <- subset(dat_long, id==1)
lm(peso ~ dia_n,id1)

#Uno los archivos 
datos <- merge(datos,dat_adg,by='id')

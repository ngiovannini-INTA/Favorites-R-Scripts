
# Update ------------------------------------------------------------------

What's the best way to upgrade?
That's a matter of taste. For most people the best thing to do is to uninstall R 
(see the previous Q), install the new version, copy any installed packages to the library folder 
in the new installation, run update.packages(checkBuilt=TRUE, ask=FALSE) in the new R and then 
delete anything left of the old installation. Different versions of R are quite deliberately 
installed in parallel folders so you can keep old versions around if you wish.

For those with a personal library (folder R\win-library\x.y of your home directory, R\win64-library\x.y on 64-bit builds), you will need to update that too when the minor version of R changes (e.g. from 3.0.2 to 3.1.0). A simple way to do so is to copy (say) R\win-library\3.0 to R\win-library\3.1 before running update.packages(checkBuilt=TRUE, ask=FALSE).

library(installr)
installr::updateR()

# Remover Objetos ---------------------------------------------------------
rm(datos)


# Crear y setear directorio -----------------------------------------------
dir.create("c:/Dropbox/DATA/TEXEL/POB/2018/")                 
setwd("c:/Dropbox/DATA/TEXEL/POB/2018/")                      
POB <- ("c:/Dropbox/DATA/TEXEL/POB/2018/")


# Importar CSV ----------------------------------------------------------------
datos <- read.csv("C:/Users/giovannini.nicolas/Desktop/dat.csv", sep=";")

datosV <-read.csv("C:/Dropbox/DATA/PruebaVale.csv", sep=";", skip=2, header=TRUE)


# Exportar ----------------------------------------------------------------
write.table(wombat1, file = "TX_PC.dat", sep="\t", row.names = F, col.names = F, quote=F, na="0")


# Leer datos pequeños pegados ---------------------------------------------
data <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
       1   M     7.9  12.3  10.7
       2   F     6.3  10.6  11.1
       3   F     9.5  13.1  13.8
       4   M    11.5  13.4  12.9
')



# Subsets -----------------------------------------------------------------
subset(datosV,datosV$hora %in% c("2:00","1:00"))
subset(datosV,datosV$hora=="1:00" & datosV$t="A.M.")
subset(datosV,datosV$hora=="1:00" | datosV$hora=="2:00")


# Rename valores/factor names ---------------------------------------------
levels(data_long$nCC)[levels(data_long$nCC)=="CC32"] <- "M1"
wombat1$CZA[wombat1$CZA == 0] <- 3
levels(datos$C) <- list(A="+", B=".", C="-")



# Rename Columnas ---------------------------------------------------------
colnames(newped)[colnames(newped)=="ID"] <- "IDUI"
names(VC_PC)=c("ID_WOMBAT","SeqID","TRAIT","EBV","SEP","ACC","INB")


# Concatenar --------------------------------------------------------------
data_long$momento<-paste(data_long$ID, data_long$nCC, sep="")



# Merge -------------------------------------------------------------------
DL<-merge(data_long, data_long2, by = "momento")



# Datos wide a Long -------------------------------------------------------
data_long <- melt(datos, id.vars=c("ID", "Edad"), 
                    measure.vars = c("CC32","CC0","CC35"), 
                    variable.name="nCC",
                    value.name="CC")

reshape(angora, direction='long', 
                   varying=c('PV.1','PV.2','PV.3','PV.4','PV.5','PV.6','CC.1','CC.2','CC.3','CC.4','CC.5','CC.6'), 
                   timevar='var',
                   times=c('1','2','3','4','5','6'),
                   v.names=c('PV', 'CC'),
                   idvar='caravana')

reshape(VC_PC, idvar = "ID_WOMBAT", timevar = "TRAIT", direction = "wide")

# Tablas cruzadas ---------------------------------------------------------
  #1 factor
tapply(R$LG,R$provincia, mean, na.rm=T)

  #2 factores
tapply(R$LG,list(R$provincia,R$edad), mean, na.rm=T)


# proporciones/frecuencias ------------------------------------------------
prop.table(table(R$ored,R$provincia),2)



# if/else -----------------------------------------------------------------
for ( i in 1:nrow(nuevo)) { 
  if (!is.na(nuevo$ED_PC50[i]) & nuevo$ED_PC50[i] > 75 & nuevo$ED_PC50[i] < 25) 
  {
    nuevo$PC50a[i] <- NA
    nuevo$ED_PC50[i] <- NA
    
  }  else  
  {    nuevo$PC50a[i] <- nuevo$PC50[i]
  } 
}

ifelse( q4 == 5, 1,0 )

# Remover filas -----------------------------------------------------------
data = data[!is.na(data$TP),]
data = data[data$SX != '',]


# Editar datos ------------------------------------------------------------
edit(x)


# Estadisticos descriptivos -----------------------------------------------
vars.tab4 <- data1[,c("PCNa","PC50a","PC100a","PC240a","POB240","EGD240","RINF")] #Me quedo con las variables de interes

n <- colSums(!is.na(vars.tab4))
mean <- apply(vars.tab4, 2, mean, na.rm=TRUE)
sd <- apply(vars.tab4, 2, sd, na.rm=TRUE)
tabla4 <- rbind(n,mean,sd)



# Extraer caracteres ------------------------------------------------------
substr(ped1$IDUI, 1, 4)


# Ordenar filas -----------------------------------------------------------
iris[order(iris$Petal.Length),]


# Ordenar columnas --------------------------------------------------------
data_wide <- data_wide[, c(1,2,5,3,4)]


# Muestra 10 primeros registros -------------------------------------------
head(data)


# #Exportar png -----------------------------------------------------------

png(file="ScatterCluster.png", width=800, heigh=600 )
ggplot(res.hcpc$call$X, aes(x=Dim.1, y=Dim.2, colour=clust, ellipses=clust)) 
dev.off()


# Caluculadora tamaño de muestra para anova con 2 grupos ------------------
#calculo de tamaño muestral: usar potencia de 80% o mas. El calculo no considera los bloques, 
#para hacerlo mas conservador calcular el número todal de repeticiones (trat x bloque) y restarle 
#algunas observaciones a cuenta de los grados de libertad que se come modelar los bloques.

# http://powerandsamplesize.com/Calculators/Compare-2-Means/2-Sample-Equality

muA=18
muB=21.5
kappa=1 #nA/nB
sd=3.6
alpha=0.05
beta=0.20 
(nB=(1+1/kappa)*(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(muA-muB))^2)
ceiling(nB) # Este es el tamaño de muestra mínimo
z=(muA-muB)/(sd*sqrt((1+1/kappa)/nB))
(Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2)))

# Calculadora del tamaño de muestra para comparar k medias
muA=35
muB=30
sd=10
tau=3
alpha=0.01
beta=0.20
(n=2*(sd*(qnorm(1-alpha/(2/tau))+qnorm(1-beta))/(muA-muB))^2)
ceiling(n) # 63
z=(muA-muB)/(sd*sqrt(2/n))
(Power=pnorm(z-qnorm(1-alpha/(2/tau)))+pnorm(-z-qnorm(1-alpha/(2/tau))))


# Dar formato de fecha ----------------------------------------------------
REF$fNacimiento <- strptime(REF$fNacimiento,format="%d/%m/%Y")

#Extraer partes de fecha
library(lubridate)
year(FN)

#Paquete Lubridate para manipular fechas e intervalos de tiempo
#https://rpubs.com/Maugnetic/282491


# Eliminar duplicados -----------------------------------------------------
uno <- todo.ped[!duplicated(todo.ped$rimmd),]



# Hacer Matriz positiva definida ------------------------------------------

prueba <- matrix(data=c(30.859,1.230,0.040,10.191,
                        1.230,0.851,0.003,1.368,
                        0.040,0.003,0.0004,0.016,
                        10.191,1.368,0.016,15.234),nrow=4,ncol=4)

makPD = function(A){
  D = eigen(A)
  sr =0
  nneg=0
  V = D$values
  U = D$vectors
  N=nrow(A)
  for(k in 1:N){
    if(V[k] < 0){
      nneg=nneg+1
      sr=sr+V[k]+V[k] }
  }
  wr=(sr*sr*100)+1
  p=V[N - nneg]
  for(m in 1:N){
    if(V[m] < 0 ){
      c = V[m]
      V[m] = p*(sr-c)*(sr-c)/wr
    } }
  A = U %*% diag(V) %*% t(U)
  return(A)}

makPD(prueba)


#Correr BLUPF90 desde R ----------------------------------------------

setwd('c:/Drive/DOC/Tesis/Soft/Simulacion/AlphaSimR/blup3/')
shell("renumf90 renum.par")
shell("blupf90 renf90.par")

#Objetivo:  mostrar ganancia de predicados simples
#Primero se debe bajar a la PC local el archivo paquete_premium_201905.csv que esta en la carpeta datasetsOri


#No se trabajan con los dataframes de R Base
#Se trabaja con el absolutamente superador  data.table
require("data.table")


#cargo los datos
#"dataset" es el nombre de la variable, podria haberse llamado con cualquier nombre
#se debe cambiar la ruta
dataset  <- fread("G:\\Documents\\ITBA\\Modulo3\\datasetsOri\\paquete_premium_202011.csv")
dataset
#El universo agrupado por  clase_ternaria
ftable(dataset$clase_ternaria)

#calculo la ganancia
dataset[  , ganancia:= -1250 ]
dataset[ clase_ternaria=='BAJA+2' ,   ganancia:= 48750]
sum( dataset$ganancia )

dataset[  ,  pos:= as.integer(clase_ternaria=='BAJA+2') ]
dataset[  ,  neg:= as.integer(clase_ternaria!='BAJA+2') ]

#clase
dataset[  , clase:= 0 ]
dataset[ clase_ternaria=='BAJA+2' ,   clase:= 1]
sum( dataset$clase )


#Otra forma de ver el universo
ftable(dataset[  ,clase_ternaria])

#Basico
summary( dataset)

by(dataset, dataset$clase_ternaria, summary)


#Corte por edad
hist(dataset[ ,cliente_edad] )
boxplot(cliente_edad  ~ clase_ternaria,  data=dataset)

ftable(dataset[ cliente_edad <=33, clase_ternaria])
ftable(dataset[ cliente_edad  >33, clase_ternaria])

#Ganancia
sum( dataset[ cliente_edad <=33, ganancia] )
#Lift
(sum( dataset[ cliente_edad <=33, clase]) / nrow( dataset[ cliente_edad <=33, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))

#Ganancia
sum( dataset[ cliente_edad >33, ganancia] )
#Lift
(sum( dataset[ cliente_edad >33, clase]) / nrow( dataset[ cliente_edad >33, ] )) /  (sum( dataset[, clase]) / nrow( dataset ))


#------------------------------------------------------------------------------
#Acertando a una variable buena

sum( dataset[ cliente_edad<=33, ganancia] )

dataset[  ctrx_quarter<=20,  .N,  clase_ternaria ]

dataset[  cliente_edad<=33,  list( "neg"=sum(neg),  "pos"=sum(pos) ) ]
#-----------------------------------------------------------------------------
setorder( dataset, ctrx_quarter ) #ordeno el dataset por valor ctrx
dataset[ , ganancia_acumulada := cumsum( ganancia ) ]
dataset[ , max(ganancia_acumulada) ]
which.max( dataset$ganancia_acumulada )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Acertando a otra variable buena


setorder( dataset, ctrx_quarter ) #ordeno el dataset por valor ctrx
dataset[ , ganancia_acumulada := cumsum( ganancia ) ]
dataset[ , max(ganancia_acumulada) ]
which.max( dataset$ganancia_acumulada )

#------------------------------------------------------------------------------

#Corte por mcuentas_saldo
hist(dataset[ ,mcuentas_saldo] )
boxplot(mcuentas_saldo  ~ clase_ternaria, data=dataset)
boxplot(mcuentas_saldo  ~ clase_ternaria, data=dataset, outline=FALSE)

ftable(dataset[ mcuentas_saldo <= -120000, clase_ternaria])
ftable(dataset[ mcuentas_saldo >  -120000, clase_ternaria])


sum( dataset[ mcuentas_saldo <= -120000, ganancia] )
sum( dataset[ mcuentas_saldo > -120000, ganancia] )

#Corte por Visa_mconsumototal
hist(dataset[ ,Visa_mconsumototal] )
boxplot(Visa_mconsumototal  ~ clase_ternaria, data=dataset, outline=FALSE)


vganancias <- c( -1250, 48750, -1250 )
ftable(dataset[ Visa_mconsumototal <= 20000, clase_ternaria])
ftable(dataset[ Visa_mconsumototal >  20000, clase_ternaria])
ftable(dataset[ is.na(Visa_mconsumototal)  , clase_ternaria])

#multiplico vectores para calcular la ganancia
vganancias <- c( -1250, 48750, -1250 )
sum( ftable(dataset[ Visa_mconsumototal <= 20000, clase_ternaria]) * vganancias )
sum( ftable(dataset[ Visa_mconsumototal > 20000, clase_ternaria]) * vganancias )
sum( ftable(dataset[ is.na(Visa_mconsumototal), clase_ternaria]) * vganancias )



#Tabla de contingencia , ya que  ttarjeta_visa  es una variable DISCRETA
table( dataset$ctarjeta_visa,  dataset$clase_ternaria, useNA = 'ifany')

#la funcion t() traspone una matriz
t( table( dataset$ctarjeta_visa,  dataset$clase_ternaria, useNA = 'ifany') )* vganancias


colSums(t( table( dataset$ctarjeta_visa,  dataset$clase_ternaria, useNA = 'ifany') )* vganancias)



#Tabla de contingencia , ya que  Visa_status  es una variable DISCRETA
table( dataset$Visa_status,  dataset$clase_ternaria, useNA = 'ifany')
colSums(t( table( dataset$Visa_status,  dataset$clase_ternaria, useNA = 'ifany') )* vganancias)


#Tabla de contingencia , ya que  Master_status  es una variable DISCRETA
table( dataset$Master_status,  dataset$clase_ternaria, useNA = 'ifany')
colSums(t( table( dataset$Master_status,  dataset$clase_ternaria, useNA = 'ifany') )* vganancias)

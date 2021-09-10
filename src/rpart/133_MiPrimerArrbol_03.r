#Lo mismo que el arbol anterior, pero ahora calculo en forma automática la ganancia en los datos que entreno
#limpio la memoria
rm(list=ls())   #remove all objects borra todos los objetas
gc()            #garbage collection libera toda la memoria para poder usar y almacenar


#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")
require("rpart.plot")

setwd("G:\\Documents\\ITBA\\Modulo3\\")  #Establezco el Working Directory

#cargo los datos
dataset <- fread("./datasetsOri/paquete_premium_202011.csv")

# generacion del modelo e impresion del arbol, maxdepth=5
modeloA  <-  rpart("clase_ternaria ~ .", data = dataset, xval=0, cp=-1, maxdepth=5 )
jpeg(file ="./work/MiPrimerArbol_03A.jpg",  width = 6, height = 4, units = 'in', res = 300)
prp(modeloA, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

# generacion del modelo e impresion del arbol, ahora con  maxdepth=20
modeloB  <-  rpart("clase_ternaria ~ .", data = dataset, xval=0, cp=-1, maxdepth=20 )  #maxdepth vale 20 !!!
jpeg(file ="./work/MiPrimerArbol_03B.jpg",  width = 32, height = 16, units = 'in', res = 300)
prp(modeloB, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()


#calculo la ganancia del arbol   A
tabla_arbolA <-  as.data.table( modeloA$frame )
tabla_arbolA[ , ganancia :=   -1250*yval2.V2  +  48750*yval2.V3 - 1250*yval2.V4 ]
#sumo los nodos que son hojas y tienen ganancia positiva en los datos donde entrene
tabla_arbolA[ var=="<leaf>" & ganancia>0,  sum( ganancia ) ]


#calculo la ganancia del arbol  B
tabla_arbolB <-  as.data.table( modeloB$frame )
tabla_arbolB[ , ganancia :=   -1250*yval2.V2  + 48750*yval2.V3 - 1250*yval2.V4 ]
#sumo los nodos que son hojas y tienen ganancia positiva en los datos donde entrene
tabla_arbolB[ var=="<leaf>" & ganancia>0,  sum( ganancia ) ]


#En los datos que entreno me conviene un arbol de profundidad 20 al de profunidad 5
#¿Esto se cumplira si aplico el modelo a los datos del futuro?



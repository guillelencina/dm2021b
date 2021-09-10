#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("G:\\Documents\\ITBA\\Modulo3\\")  #Establezco el Working Directory

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202011.csv")

#cargar aqui los parametros
parametros  <-  list( "cp"=-1, "minsplit"=2,  "minbucket"=3, "maxdepth"=8 )

modelo  <- rpart("clase_ternaria ~ .",
                 data= dtrain,
                 xval= 0,
                 control= parametros )

#cargo los datos donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")

prediccion  <- predict( modelo, dapply , type= "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[  , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= paste0( "./kaggle/arbol_aplicado6.csv"), 
        sep= "," )

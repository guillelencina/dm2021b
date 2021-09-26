#Necesita para correr en Google Cloud  "exp 12 continua ==0 
#16 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU
#LightGBM  min_data_in_leaf= 4000   quitando    "mpasivos_margen", "mactivos_margen"

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("G:/Documents/ITBA/Modulo3")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/paquete_premium_202011_ext.csv")

#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01 := ifelse( clase_ternaria=="CONTINUA", 0, 1) ]

campos_malos  <- c("mpasivos_margen", "mactivos_margen")

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos) )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- lgb.train( data= dtrain,
                      param=  list( objective= "binary", min_data_in_leaf= 2850,learning_rate=0.015,feature_fraction=0.5,
                                    num_leaves=26,num_iterations= 844)
                    )


#aplico el modelo a los datos sin clase, 202101
dapply  <- fread("./datasets/paquete_premium_202101_ext.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > 0.05)  ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lightgbm_613_exp12.csv", 
        sep= "," )

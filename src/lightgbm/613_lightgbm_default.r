#LightGBM  min_data_in_leaf= 4000   quitando    "mpasivos_margen", "mactivos_margen"

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("~/buckets/b1/")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasetsOri/paquete_premium_202011.csv")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "mpasivos_margen","mactivos_margen") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- lgb.train( data= dtrain,
                      param=  list( objective= "binary", min_data_in_leaf= 4000 )
                    )


#aplico el modelo a los datos sin clase, 202101
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > 0.025)  ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lightgbm_613.csv", 
        sep= "," )

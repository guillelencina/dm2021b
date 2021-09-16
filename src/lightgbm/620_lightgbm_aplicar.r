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
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", 
                           "mpasivos_margen","mactivos_margen", "mrentabilidad_annual") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#Aqui se deben cargar los parametros
param_buenos  <- list( objective= "binary",
                       num_iterations= 540,
                       learning_rate=  0.02,
                       min_data_in_leaf=  2500,
                       num_leaves= 700,
                       feature_fraction= 0.24,
                       prob_corte= 0.027
                       )

#genero el modelo
modelo  <- lgb.train( data= dtrain,
                      param= param_buenos
                    )


#aplico el modelo a los datos sin clase, 202101
dapply  <- fread("./datasetsOri/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > param_buenos$prob_corte)  ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lgb_aplicar_101.csv", 
        sep= "," )

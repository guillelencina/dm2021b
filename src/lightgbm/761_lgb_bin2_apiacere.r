#Necesita para correr en Google Cloud  guille sin cpos malos
#96 GB de memoria RAM
#300 GB de espacio en el disco local
#8 vCPU

#clase_binaria2   1={BAJA+2,BAJA+1}    0={CONTINUA}
#Entrena en a union de ONCE meses de [202001, 202011]
#No usa variables historicas

#Optimizacion Bayesiana de hiperparametros de  lightgbm
#usa el interminable  5-fold cross validation
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

# WARNING  usted debe cambiar este script si lo corre en su propio Linux

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
       )
#defino la carpeta donde trabajo
setwd( directory.root )



kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript         <- "761_lgb_bin2_apiacere"

karch_dataset    <- "./datasets/paquete_premium_corregido_ext.csv.gz"
kmes_apply       <- 202101  #El mes donde debo aplicar el modelo
kmes_train_hasta <- 202011  #Obvimente, solo puedo entrenar hasta 202011

kmes_train_desde <- 202001  #Entreno desde Enero-2020

kcanaritos  <-  10
kBO_iter    <-  100   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower=    0.02 , upper=    0.06),
         makeNumericParam("feature_fraction", lower=    0.1  , upper=    0.4),
         makeIntegerParam("min_data_in_leaf", lower= 1000L   , upper= 8000L),
         makeIntegerParam("num_leaves",       lower=  100L   , upper= 1024L),
         makeNumericParam("prob_corte",       lower=    0.040, upper=    0.055)
        )

campos_malos  <- c("mextraccion_autoservicio_lag1",
                   "ccaja_ahorro_delta1",
                   "Visa_msaldototal_lag1",
                   "mv_mconsumototal",
                   "Visa_mpagominimo_lag1",
                   "cextraccion_autoservicio_lag1",
                   "mvr_consumototal2_lag1",
                   "ccaja_ahorro_lag1",
                   "tmobile_app_lag1",
                   "mv_status03",
                   "ctransferencias_recibidas_lag1",
                   "mvr_msaldopesos2",
                   "cmobile_app_trx_lag1",
                   "tcallcenter_delta1",
                   "ctransferencias_emitidas",
                   "mv_mfinanciacion_limite",
                   "ccallcenter_transacciones_delta1",
                   "Visa_mpagominimo_delta1",
                   "Visa_cadelantosefectivo_lag1",
                   "glr_prompay_delta1",
                   "Visa_msaldototal",
                   "Visa_mpagado",
                   "Visa_fultimo_cierre",
                   "mv_status01_lag1",
                   "mv_mfinanciacion_limite_lag1",
                   "cmobile_app_trx",
                   "Master_msaldototal_lag1",
                   "mvr_mconsumospesos_lag1",
                   "Master_mconsumototal_delta1",
                   "Master_fultimo_cierre",
                   "mv_status02",
                   "ccallcenter_transacciones",
                   "mtransferencias_emitidas_lag1",
                   "mplazo_fijo_dolares",
                   "ctransferencias_emitidas_delta1",
                   "ccomisiones_mantenimiento",
                   "mv_cconsumos_delta1",
                   "glr_mdebitos1_delta1",
                   "mvr_Master_mlimitecompra",
                   "Visa_cconsumos_delta1",
                   "thomebanking_delta1",
                   "mextraccion_autoservicio",
                   "mtransferencias_recibidas_delta1",
                   "Visa_Finiciomora_delta1",
                   "mvr_saldototal2_delta1",
                   "Visa_fechaalta_delta1",
                   "Master_mconsumospesos",
                   "internet_lag1",
                   "Visa_mfinanciacion_limite_delta1",
                   "mv_msaldopesos_lag1",
                   "mv_pagado_minimo",
                   "mv_mconsumosdolares_delta1",
                   "mcuenta_debitos_automaticos_delta1",
                   "mv_mlimitecompra",
                   "Visa_cconsumos_lag1",
                   "mvr_mpagospesos_delta1",
                   "Visa_mpagospesos_delta1",
                   "Master_fultimo_cierre_lag1",
                   "mvr_Master_mlimitecompra_lag1",
                   "mv_msaldopesos_delta1",
                   "mvr_saldototal2",
                   "cplazo_fijo_lag1",
                   "Visa_mconsumosdolares_lag1",
                   "glr_mpaymsaldo_lag1",
                   "glr_mpaymsaldo_delta1",
                   "Visa_mlimitecompra_delta1",
                   "mautoservicio_delta1",
                   "cpagomiscuentas",
                   "cmobile_app_trx_delta1",
                   "ctarjeta_master_transacciones",
                   "mv_status03_delta1",
                   "ctarjeta_master_transacciones_lag1",
                   "matm_lag1",
                   "Visa_fultimo_cierre_lag1",
                   "ctarjeta_visa_transacciones_delta1",
                   "mv_mconsumosdolares_lag1",
                   "ccajas_consultas",
                   "ccajas_otras_delta1",
                   "mvr_msaldopesos2_delta1",
                   "mvr_consumototal2_delta1",
                   "mv_pagado_minimo_lag1",
                   "Visa_msaldototal_delta1",
                   "mv_Fvencimiento_delta1",
                   "cextraccion_autoservicio_delta1",
                   "mvr_msaldototal_lag1",
                   "mplazo_fijo_dolares_delta1",
                   "mvr_msaldototal",
                   "glr_mdebitos2_delta1",
                   "mvr_mpagospesos_lag1",
                   "Master_mconsumototal_lag1",
                   "cpayroll_trx_lag1",
                   "matm_other_lag1",
                   "Visa_mpagado_lag1",
                   "ctarjeta_visa",
                   "thomebanking_lag1",
                   "ctarjeta_visa_debitos_automaticos_delta1",
                   "Master_fultimo_cierre_delta1",
                   "Master_mpagospesos_lag1",
                   "mv_mconsumospesos",
                   "Master_msaldototal",
                   "glr_mdebitos2_lag1",
                   "Master_mpagominimo_lag1",
                   "mvr_mpagado",
                   "mprestamos_prendarios",
                   "catm_trx",
                   "ctarjeta_visa_debitos_automaticos_lag1",
                   "mv_mconsumospesos_lag1",
                   "ccajas_transacciones_delta1",
                   "ccajas_otras",
                   "cprestamos_hipotecarios",
                   "mvr_msaldototal_delta1",
                   "cliente_vip_delta1",
                   "mvr_mpagado_delta1",
                   "glr_mdebitos2",
                   "Visa_mpagosdolares_delta1",
                   "glr_mpaymquarter_lag1",
                   "mv_status05_lag1",
                   "Visa_delinquency_lag1",
                   "tpaquete3_delta1",
                   "ctarjeta_debito",
                   "mv_status06_delta1",
                   "Master_mpagospesos",
                   "Master_Finiciomora",
                   "mv_status02_delta1",
                   "mv_mconsumototal_lag1",
                   "glr_mpaymquarter_delta1",
                   "cpagomiscuentas_lag1",
                   "mv_msaldodolares_lag1",
                   "ccomisiones_otras_delta1",
                   "Master_mpagominimo",
                   "ccheques_emitidos",
                   "Visa_mconsumosdolares",
                   "ccajas_consultas_delta1",
                   "ccuenta_debitos_automaticos",
                   "gl_ageprompay_delta1",
                   "ccajas_transacciones_lag1",
                   "Master_mconsumospesos_delta1",
                   "Visa_mconsumosdolares_delta1",
                   "Master_delinquency_lag1",
                   "ccajas_transacciones",
                   "Visa_mpagado_delta1",
                   "active_quarter_lag1",
                   "mvr_mconsumototal_lag1",
                   "mvr_mconsumosdolares_lag1",
                   "mvr_mconsumosdolares",
                   "mtarjeta_master_consumo",
                   "Master_msaldototal_delta1",
                   "mextraccion_autoservicio_delta1",
                   "mv_mfinanciacion_limite_delta1",
                   "Master_Finiciomora_lag1",
                   "Visa_fultimo_cierre_delta1",
                   "mv_Finiciomora_delta1",
                   "tcallcenter_lag1",
                   "Master_mpagospesos_delta1",
                   "mvr_msaldodolares_lag1",
                   "Master_fechaalta_delta1",
                   "mttarjeta_master_debitos_automaticos",
                   "Visa_mpagosdolares_lag1",
                   "cseguro_vida_lag1",
                   "mv_mconsumospesos_delta1",
                   "ctarjeta_debito_lag1",
                   "mtarjeta_master_consumo_lag1",
                   "ctarjeta_master",
                   "Master_mpagominimo_delta1",
                   "mvr_mconsumototal_delta1",
                   "Master_mpagado_delta1",
                   "catm_trx_lag1",
                   "mprestamos_prendarios_lag1",
                   "cforex_sell_lag1",
                   "glr_cpaytotalage_lag1",
                   "Master_mpagosdolares_lag1",
                   "mv_mconsumototal_delta1",
                   "ctarjeta_visa_debitos_automaticos",
                   "gl_mpaytotal_delta1",
                   "Master_mpagado",
                   "mttarjeta_master_debitos_automaticos_lag1",
                   "Master_cconsumos_lag1",
                   "mvr_mpagosdolares_lag1",
                   "mvr_mconsumosdolares_delta1",
                   "mtarjeta_master_consumo_delta1",
                   "cprestamos_prendarios",
                   "mv_status07_lag1",
                   "mv_msaldodolares",
                   "mprestamos_hipotecarios_lag1",
                   "Master_cconsumos",
                   "tpaquete3_lag1",
                   "matm_other_delta1",
                   "ccheques_emitidos_lag1",
                   "cseguro_accidentes_personales_lag1",
                   "Master_madelantodolares_delta1",
                   "mvr_msaldodolares",
                   "Visa_msaldodolares_delta1",
                   "Master_delinquency",
                   "ccheques_depositados_lag1",
                   "cinversion2",
                   "minversion1_pesos_lag1",
                   "mvr_msaldodolares_delta1",
                   "mvr_mpagado_lag1",
                   "ctarjeta_master_transacciones_delta1",
                   "mv_status06_lag1",
                   "mv_mlimitecompra_delta1",
                   "mplazo_fijo_dolares_lag1",
                   "Visa_Fvencimiento_delta1",
                   "mvr_Visa_mlimitecompra_delta1",
                   "gl_ageprompay_lag1",
                   "mforex_buy",
                   "ctarjeta_visa_delta1",
                   "matm_other",
                   "ccuenta_debitos_automaticos_lag1",
                   "ccajas_extracciones_lag1",
                   "cforex_sell",
                   "ccheques_depositados",
                   "cforex_lag1",
                   "catm_trx_delta1",
                   "cplazo_fijo_delta1",
                   "cprestamos_prendarios_lag1",
                   "gl_cpaytotal_delta1",
                   "mforex_sell",
                   "mcheques_emitidos",
                   "Master_status_lag1",
                   "mttarjeta_master_debitos_automaticos_delta1",
                   "cseguro_accidentes_personales",
                   "ctarjeta_debito_delta1",
                   "mvr_msaldodolares2",
                   "gl_mpaytotal_lag1",
                   "ccajas_extracciones",
                   "ccajas_depositos_lag1",
                   "ccajas_depositos",
                   "ctarjeta_master_lag1",
                   "mtarjeta_visa_descuentos_delta1",
                   "cseguro_vivienda",
                   "ccheques_emitidos_delta1",
                   "Master_cconsumos_delta1",
                   "minversion2",
                   "ctarjeta_visa_lag1",
                   "mv_msaldodolares_delta1",
                   "tcuentas_lag1",
                   "mforex_sell_lag1",
                   "cprestamos_hipotecarios_lag1",
                   "mv_mpagado_lag1",
                   "cseguro_vivienda_lag1",
                   "Master_delinquency_delta1",
                   "mcaja_ahorro_adicional",
                   "mv_mpagado",
                   "cpagomiscuentas_delta1",
                   "mvr_msaldodolares2_lag1",
                   "mvr_msaldodolares2_delta1",
                   "Master_cadelantosefectivo_lag1",
                   "Master_Fvencimiento_delta1",
                   "Visa_delinquency_delta1",
                   "Master_mpagosdolares",
                   "ccajas_extracciones_delta1",
                   "mcheques_depositados",
                   "catm_trx_other_lag1",
                   "mprestamos_hipotecarios",
                   "Master_mconsumosdolares_lag1",
                   "mcheques_emitidos_delta1",
                   "Visa_msaldodolares",
                   "mcheques_emitidos_lag1",
                   "mtarjeta_visa_descuentos",
                   "mcheques_depositados_delta1",
                   "Master_mpagado_lag1",
                   "cliente_vip_lag1",
                   "ccajas_depositos_delta1",
                   "ccajas_consultas_lag1",
                   "cforex_buy_delta1",
                   "mcheques_depositados_lag1",
                   "ctarjeta_master_delta1",
                   "cseguro_vida",
                   "minversion1_pesos",
                   "ccuenta_debitos_automaticos_delta1",
                   "cinversion1",
                   "Master_mconsumosdolares_delta1",
                   "Visa_msaldodolares_lag1",
                   "mcaja_ahorro_adicional_lag1",
                   "Master_mpagosdolares_delta1",
                   "Master_mconsumosdolares",
                   "active_quarter_delta1",
                   "active_quarter",
                   "Visa_status_lag1",
                   "Master_msaldodolares_delta1",
                   "ccallcenter_transacciones_lag1",
                   "ccajas_otras_lag1",
                   "mforex_sell_delta1",
                   "catm_trx_other",
                   "Visa_cadelantosefectivo_delta1",
                   "glr_cpaytotalage_delta1",
                   "mv_status03_lag1",
                   "cpayroll_trx_delta1",
                   "mv_mpagosdolares_lag1",
                   "mv_status02_lag1",
                   "Visa_madelantodolares_delta1",
                   "mvr_mpagosdolares",
                   "catm_trx_other_delta1",
                   "mv_mpagado_delta1",
                   "cforex_sell_delta1",
                   "cforex",
                   "Master_msaldodolares",
                   "mforex_buy_lag1",
                   "cseguro_auto",
                   "mtarjeta_visa_descuentos_lag1",
                   "tpaquete4_delta1",
                   "Master_msaldodolares_lag1",
                   "Visa_madelantopesos",
                   "mvr_mpagosdolares_delta1",
                   "mtarjeta_master_descuentos_delta1",
                   "Master_cadelantosefectivo",
                   "cforex_delta1",
                   "cinversion2_lag1",
                   "Master_madelantodolares_lag1",
                   "Visa_madelantodolares_lag1",
                   "Master_madelantopesos",
                   "cforex_buy",
                   "mforex_buy_delta1",
                   "Master_madelantodolares",
                   "ccheques_depositados_delta1",
                   "cinversion1_lag1",
                   "mcaja_ahorro_adicional_delta1",
                   "minversion2_lag1",
                   "ctarjeta_master_debitos_automaticos",
                   "mvr_Master_mlimitecompra_delta1",
                   "Master_cadelantosefectivo_delta1",
                   "Master_madelantopesos_delta1",
                   "minversion2_delta1",
                   "ctarjeta_master_debitos_automaticos_lag1",
                   "Master_Finiciomora_delta1",
                   "minversion1_pesos_delta1",
                   "tcuentas",
                   "mprestamos_hipotecarios_delta1",
                   "mv_mpagosdolares",
                   "Visa_madelantopesos_lag1",
                   "mcajeros_propios_descuentos",
                   "mcheques_emitidos_rechazados_lag1",
                   "cseguro_vivienda_delta1",
                   "cforex_buy_lag1",
                   "Master_madelantopesos_lag1",
                   "Visa_madelantopesos_delta1",
                   "mv_mpagosdolares_delta1",
                   "cliente_edad_delta1",
                   "minversion1_dolares_lag1",
                   "ctarjeta_visa_descuentos",
                   "ccheques_emitidos_rechazados_delta1",
                   "ccheques_emitidos_rechazados",
                   "tpaquete1",
                   "ctarjeta_master_debitos_automaticos_delta1",
                   "ccheques_depositados_rechazados_lag1",
                   "cprestamos_prendarios_delta1",
                   "ccheques_depositados_rechazados_delta1",
                   "cseguro_accidentes_personales_delta1",
                   "ctarjeta_master_descuentos_lag1",
                   "mcajeros_propios_descuentos_delta1",
                   "mcheques_depositados_rechazados_delta1",
                   "cseguro_vida_delta1",
                   "mprestamos_prendarios_delta1",
                   "mpagodeservicios_lag1",
                   "ctarjeta_visa_descuentos_delta1",
                   "mcheques_emitidos_rechazados",
                   "minversion1_dolares_delta1",
                   "mpagodeservicios_delta1",
                   "ccaja_seguridad_delta1",
                   "mtarjeta_master_descuentos_lag1",
                   "mtarjeta_master_descuentos",
                   "mcheques_emitidos_rechazados_delta1")   #aqui se deben cargar todos los campos culpables del Data Drifting

ksemilla_azar  <- 100103  #Aqui poner la propia semilla
#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )

  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento

  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly

  return( experimento_actual )
}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------

PROB_CORTE  <- 0.025

fganancia_logistic_lightgbm   <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #aqui esta el inmoral uso de los pesos para calcular la ganancia correcta
  gan  <- sum( (probs > PROB_CORTE  ) *
               ifelse( vlabels== 1 & vpesos > 1, 48750, -1250 ) )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  gc()
  PROB_CORTE <<- x$prob_corte   #asigno la variable global

  kfolds  <- 5   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= 999983,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 1/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( 999983 )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )


  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds  
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL

   #si tengo una ganancia superadora, genero el archivo para Kaggle
   if(  ganancia > GLOBAL_ganancia_max )
   {
     GLOBAL_ganancia_max  <<- ganancia  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-

     set.seed(ksemilla_azar)

     modelo  <- lightgbm( data= dtrain,
                          param= param_completo,
                          verbose= -100
                        )

    #calculo la importancia de variables
    tb_importancia  <- lgb.importance( model= modelo )
    fwrite( tb_importancia, 
            file= paste0(kimp, "imp_", GLOBAL_iteracion, ".txt"),
            sep="\t" )

     prediccion  <- predict( modelo, data.matrix( dapply[  , campos_buenos, with=FALSE]) )

     Predicted  <- as.integer( prediccion > x$prob_corte )

     entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                      "Predicted"= Predicted)  )

     #genero el archivo para Kaggle
     fwrite( entrega, 
             file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
             sep= "," )
   }

   #logueo 
   xx  <- param_completo
   xx$iteracion_bayesiana  <- GLOBAL_iteracion
   xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
   loguear( xx,  arch= klog )

   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
dir.create( paste0( "./work/E",  kexperimento, "/" ) )
kbayesiana  <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, ".RDATA" )
klog        <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_BOlog.txt" )
kimp        <- paste0("./work/E",  kexperimento, "/E",  kexperimento, "_", kscript, "_" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_", kscript, "_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
 tabla_log  <- fread( klog)
 GLOBAL_iteracion  <- nrow( tabla_log ) -1
 GLOBAL_ganancia_max  <- tabla_log[ , max(ganancia) ]
}


#cargo el dataset que tiene los 36 meses
dataset  <- fread(karch_dataset)


campos_lags  <- setdiff(  colnames(dataset) ,  c("clase_ternaria","clase01", "numero_de_cliente","foto_mes", campos_malos) )

#Hago feature Engineering en este mismo script
#agreglo los lags de orden 1
setorderv( dataset, c("numero_de_cliente","foto_mes") )
dataset[ , paste0( campos_lags, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= campos_lags]

#agrego los deltas de los lags, con un "for" nada elegante
for( vcol in campos_lags )
{
   dataset[,  paste0(vcol, "_delta1") := get( vcol)  - get(paste0( vcol, "_lag1"))]
}


#agrego canaritos
if( kcanaritos > 0 )
{
  for( i  in 1:kcanaritos)  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]
}


#cargo los datos donde voy a aplicar el modelo
dapply  <- copy( dataset[  foto_mes==kmes_apply ] )


#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]



#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
#uso el weight como un truco ESPANTOSO para saber la clase real
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[  foto_mes>=kmes_train_desde & foto_mes<=kmes_train_hasta , campos_buenos, with=FALSE]),
                        label=  dataset[ foto_mes>=kmes_train_desde & foto_mes<=kmes_train_hasta, clase01],
                        weight=  dataset[ foto_mes>=kmes_train_desde & foto_mes<=kmes_train_hasta , ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] ,
                        free_raw_data= TRUE
                      )

#elimino el dataset para liberar memoria RAM
rm( dataset )
gc()

#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}



#apagado de la maquina virtual, pero NO se borra
system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE)

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


quit( save="no" )



#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

for( q in c(0.3) )
for( x1 in c( 40) )
  x2 =400 
{

Estrategia_A  <- function(x1,q)
{
  #Estrategia
  #En la primer ronda se hace tirar x1 tiros libres a cada uno de los 100 jugadores ( se gastan x1*100 tiros )
  #Se eligen a la mejor mitad de primer ronda( se descarta a la otra mitad)
  #En la segunda ronda, a la mejor mitad de la primera se los hace tirar 400 tiros a cada uno
  #Se elige el mejor jugador de la segunda ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   x1  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

  planilla_cazatalentos[ ids_juegan1,  tiros1 := x1 ]  #registro en la planilla que tiran x1 tiros

  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, x1)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  
  
  
  #Ronda 2 -------------------------------------------------------
  #Al mejor quantil lo hago tirar x2 tiros cada uno
  #La quantil  parte a un conjunto en dos partes 
  quantil  <- planilla_cazatalentos[ ids_juegan1, quantile(aciertos1,probs = q) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 >= quantil, id ]

 
  print(length(ids_juegan2))
  
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := x2 ]  #registro en la planilla que tiran x2 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, x2)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos2) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_A()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
cat("x1=",x1,"x2=",x2,"q=",q,"tiros_total=",tiros_total,"tasa_eleccion_correcta=",tasa_eleccion_correcta)

cat("\n")
#Esta estrategia elije al verdadero_mejor el 99% de las veces
#pero lamentablemente necesita de un total de 36600   tiros libres
}

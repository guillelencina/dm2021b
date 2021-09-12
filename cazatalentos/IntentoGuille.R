#Intento de Solucion del desafio  LENCINA GUILLERMO
#la idea es para cada ronda calcular la fraccion de aciertos con los tiros de esa
#ronda más los tiros de las rondas previas (como el promedio usado para el descenso en el fútbol)


#Para buscar la mejor tasa de elección se pueden variar 
# los valores de los cuantiles a superar en cada ronda
# la cantidad de tiros en cada ronda (que es similar a lo anterior)
# llego casi a la solución pero ya no se me ocurren más cosas que probar
# quizás falta probar como deberia variar la cantidad de tiros de una ronda a la siguiente

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
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
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

Estrategia_B  <- function()
{
  #Estrategia
  #Se juegan 6 rondas
  #En cada ronda, los jugadores que participan, tiran x tiros y se suman los tiros de las etapas previas
  #De una ronda a la otra, sólo pasan los que tuvieron igual o mayor aciertos a cierto valor de cuantil
  #Se elige el mejor jugador de la sexta ronda
  #los tiros en cada ronda pueden no ser iguales(falta estudiar esta dependencia de cant de tiros con cada ronda)
  
  gimnasio_init()
  
  #planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )
  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   x  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := 55 ]  #registro en la planilla que tiran x tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, 55)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  
  
  #Ronda 2 -------------------------------------------------------
  
  cuantil  <- planilla_cazatalentos[ ids_juegan1, mean(aciertos1) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 >= cuantil, id ]
  
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := 65 ]  #registro en la planilla que tiran x tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, 65)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  
  #promedio con los tiros de la ronda anteriores
  
  planilla_cazatalentos$aciertos2=(planilla_cazatalentos$aciertos2*planilla_cazatalentos$tiros2+
                                     planilla_cazatalentos$aciertos1*planilla_cazatalentos$tiros1)/
    (planilla_cazatalentos$tiros2+planilla_cazatalentos$tiros1)
  #-------------------------------------------------------------------------------
  #Ronda 3 -------------------------------------------------------
  
  cuantil  <- planilla_cazatalentos[ ids_juegan2, mean(aciertos2) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ aciertos2 >= cuantil, id ]
  
  
  
  planilla_cazatalentos[ ids_juegan3,  tiros3 := 70]  #registro en la planilla que tiran x tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, 70)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla
  
  #promedio con los tiros de las rondas anteriores
  
  
  
  planilla_cazatalentos$aciertos3=(planilla_cazatalentos$aciertos3*planilla_cazatalentos$tiros3+
                                     planilla_cazatalentos$aciertos2*
                                     (planilla_cazatalentos$tiros1
                                      +planilla_cazatalentos$tiros2))/
    
    
    (planilla_cazatalentos$tiros1
     +planilla_cazatalentos$tiros2
     +planilla_cazatalentos$tiros3)
  
  #-------------------------------------------------------------------------------
  
  
  
  #Ronda 4 -------------------------------------------------------
  
  cuantil  <- planilla_cazatalentos[ ids_juegan3, mean(aciertos3) ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ aciertos3 >= cuantil, id ]
  
  
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := 75 ]  #registro en la planilla que tiran x tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, 75)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla
  
  
  #promedio con los tiros de las rondas anteriores
  
  planilla_cazatalentos$aciertos4=(planilla_cazatalentos$aciertos4*planilla_cazatalentos$tiros4+
                                     planilla_cazatalentos$aciertos3*
                                     (planilla_cazatalentos$tiros1
                                      +planilla_cazatalentos$tiros2
                                      +planilla_cazatalentos$tiros3))/
    
    
    (planilla_cazatalentos$tiros1
     +planilla_cazatalentos$tiros2
     +planilla_cazatalentos$tiros3
     +planilla_cazatalentos$tiros4)
  #-------------------------------------------------------------------------------
  
  
  #Ronda 5 -------------------------------------------------------
  
  cuantil  <- planilla_cazatalentos[ ids_juegan4, mean(aciertos4) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ aciertos4 >= cuantil, id ]
  
  
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := 90 ]  #registro en la planilla que tiran x tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, 90)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla
  
  
  #promedio con los tiros de las rondas anteriores
  
  planilla_cazatalentos$aciertos5=(planilla_cazatalentos$aciertos5*planilla_cazatalentos$tiros5+
                                     planilla_cazatalentos$aciertos4*
                                     (planilla_cazatalentos$tiros1
                                      +planilla_cazatalentos$tiros2
                                      +planilla_cazatalentos$tiros3
                                      +planilla_cazatalentos$tiros4))/
    
    
    (planilla_cazatalentos$tiros1
     +planilla_cazatalentos$tiros2
     +planilla_cazatalentos$tiros3
     +planilla_cazatalentos$tiros4
     +planilla_cazatalentos$tiros5)
  #-------------------------------------------------------------------------------
  
  #Ronda 6 -------------------------------------------------------
  
  cuantil  <- planilla_cazatalentos[ ids_juegan5, mean(aciertos5) ]
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][ aciertos5 >= cuantil, id ]
  
  
  
  planilla_cazatalentos[ ids_juegan6,  tiros6 := 90 ]  #registro en la planilla que tiran x tiros
  resultado6  <- gimnasio_tirar( ids_juegan6, 90)
  planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]  #registro en la planilla
  
  #promedio con los tiros de las rondas anteriores
  
  planilla_cazatalentos$aciertos6=(planilla_cazatalentos$aciertos6*planilla_cazatalentos$tiros6+
                                     planilla_cazatalentos$aciertos5*
                                     (planilla_cazatalentos$tiros1
                                      +planilla_cazatalentos$tiros2
                                      +planilla_cazatalentos$tiros3
                                      +planilla_cazatalentos$tiros4
                                      +planilla_cazatalentos$tiros5))/
    
    
    (planilla_cazatalentos$tiros1
     +planilla_cazatalentos$tiros2
     +planilla_cazatalentos$tiros3
     +planilla_cazatalentos$tiros4
     +planilla_cazatalentos$tiros5
     +planilla_cazatalentos$tiros6)
  #-------------------------------------------------------------------------------
  
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos6) ]
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
  
  veredicto  <- Estrategia_B()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total 
tasa_eleccion_correcta

#Es una sábana corta ...
#-------Pruebas  ------------------------------------------------------

#Hay tela para cortar

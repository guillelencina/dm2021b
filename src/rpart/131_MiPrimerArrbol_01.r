#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")
require("rpart.plot")

#cargo los datos
dataset  <- fread("G:\\Documents\\ITBA\\Modulo3\\datasetsOri\\paquete_premium_202011.csv")

#Hipotesis Dolores. Crear solo dos clase Buena y mala

dataset[ , clase_Dolores := "MALO"]
dataset[ clase_ternaria=="BAJA+2" , clase_Dolores := "BUENO"]

#generacion del modelo
modelo <- rpart("clase_Dolores ~ . -clase_ternaria",
                data = dataset,
                cp= -1,
                maxdepth= 8
)

#impresion elaborada del arbol
jpeg(file ="G:\\Documents\\ITBA\\Modulo3\\work\\Arbol_Dolores_02_depth8.jpg", width = 20, height = 10, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()


#Hipotesis dolores








# generacion del modelo
modelo  <- rpart("clase_ternaria ~ .",  
                  data = dataset, 
                  cp= -1,
                  maxdepth= 3
                  )

#impresion elaborada del arbol
jpeg(file ="G:\\Documents\\ITBA\\Modulo3\\work\\MiPrimerArbol_01.jpg",  width = 6, height = 4, units = 'in', res = 300)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()

# Es util el arbol que salió ?
# Que ganancia tiene cada hoja de ese arbol ?
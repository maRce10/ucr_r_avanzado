# ejercicio 2 #####

t1 <- Sys.time() 

cc_vector <- NULL

repeat
{
  v1 <- rnorm(n = 20, mean = 100, sd = 20)
  
  v2 <- rnorm(n = 20, mean = 100, sd = 20)
  
  corr_coef <- cor(v1, v2)
  
  cc_vector[length(cc_vector) + 1] <- corr_coef

  t2 <- Sys.time() 
    
  if (corr_coef > 0.9 | difftime(t2, t1) > 10)   break
  
}

### ejercicio 3 #####

# 3.1 Haga un bucle `for` que devuelva el número de columnas para cada juego de datos de ejemplo

# primero extraer solo los q son data frames
dfs <- df[df$is_df, ]

clms <- NULL

for(i in dfs$dt_sets) clms <- append(clms, ncol(get(i)))


# 3.2 Haga un bucle `for` que devuelva el número de filas para cada juego de datos de ejemplo

fls <- NULL

for(i in dfs$dt_sets) fls <- append(fls, nrow(get(i)))


# 3.3 Haga un bucle `for` que devuelva el número de filas y columnas para cada juego de datos de ejemplo

# opcion 1
fls <- NULL
clms <- NULL

for(i in dfs$dt_sets) {
  fls <- append(fls, nrow(get(i)))
  clms <- append(clms, ncol(get(i)))
  }

fls.clms <- cbind(fls, clms)


# opcion 2 usando cbind y rbind internamente
fls.clms <- NULL

for(i in dfs$dt_sets) {
  fls <- append(fls, nrow(get(i)))
  clms <- append(clms, ncol(get(i)))

  fc <- cbind(fls, clms)
  fls.clms <- rbind(fls.clms, fc)
  }



# 3.4 Usando los datos de ChickWeight, calcule la correlación entre el peso y la edad de cada pollito (consejo: (1) use unique (ChickWeight $ Chick) dentro del inicio del bucle for y (2) cree subconjuntos usando indexación dentro del cuerpo del bucle)

cor.chks <- NULL

for(i in unique(ChickWeight$Chick)) {
  
  wg <- ChickWeight$weight[ChickWeight$Chick == i]
  ag <- ChickWeight$Time[ChickWeight$Chick == i]

  cr <- cor(wg, ag)
  cor.chks <- append(cor.chks, cr)
  
  }


### ejercicio 4 #####

# 4.1 Haga un bucle lapply equivalente al bucle for en el ejercicio 3.4 (*utilizando los datos ‘ChickWeight’ calcule la correlación entre peso y tiempo para cada Chick*)

cor.chks <- lapply(unique(ChickWeight$Chick), function(i){
  
  wg <- ChickWeight$weight[ChickWeight$Chick == i]
  ag <- ChickWeight$Time[ChickWeight$Chick == i]
  
  cr <- cor(wg, ag)
  return(cr)
  }
)

# 4.2  Haga un bucle `sapply` para calcular el mayor peso registrado para cada tipo de dieta (pista: `unique(ChickWeight$Diet)`, deberia devolver un valor por tipo de dieta). Nombre el vector resultante para que contenga el identificador de cada dieta.

max.wg <- sapply(unique(ChickWeight$Diet), function(i) max(ChickWeight$weight[ChickWeight$Diet == i]))

names(max.wg) <- unique(ChickWeight$Diet)

# 4.3 Haga un bucle apply para calcular el promedio de cada variable numérica en el conjunto de datos ‘iris’.
data(iris)

# opcion 1
apply(X = iris[, 1:4], MARGIN = 2, FUN = mean)

# opcion 2
apply(X = iris[, sapply(iris, is.numeric)], MARGIN = 2, FUN = mean)




# Ejercicios extra


# E.1 Reúna los resultados del ejercicio 3.4 en un nuevo marco de datos con columnas para ‘chick’ y ‘correlation’ usando un bucle for (consejo: use rbind)

cor.df <- NULL

for(i in unique(ChickWeight$Chick)) {
  
  wg <- ChickWeight$weight[ChickWeight$Chick == i]
  ag <- ChickWeight$Time[ChickWeight$Chick == i]
  
  cr <- cor(wg, ag)
  df <- data.frame(chick = i, corr = cr)
  cor.df <- rbind(cor.df, df)  
}


# E.2 ¿Cuántos de los ejemplos de conjuntos de datos del marco de datos contenían una columna de factor?

# Respuesta
# para revisar si en un solo data frame, cada una de las columnas es o noun factor, se puede usar un sapply:

sapply(get(dfs$dt_sets[[1]]), is.factor)

# o directamente ver si hay algun TRUE
any(sapply(get(dfs$dt_sets[[1]]), is.factor))

# ahora podemos poner esto dentro de un loop para q nos diga para cada juego de datos si hay o no un factor

fctrs <- sapply(dfs$dt_sets, function(x)
  any(sapply(get(x), is.factor))
  )

# este es el numero de juegos de datos con al menos un factor
sum(fctrs)




# E.3 Calcule el coeficiente de variación de cada variable numérica por especie en el conjunto de datos ‘iris’ usando ‘tapply’.

# primero hacer una funcion para el CV
cv <- function(x) sd(x) / mean(x)

# y ahora usar la funcion con un sapply en las variables numericas

# opcion 1
sapply(X = iris[, 1:4], FUN = cv)

# opcion 2
sapply(iris[, sapply(iris, is.numeric)], cv)




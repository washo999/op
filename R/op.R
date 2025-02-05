crear_matriz <- function(nf, nc) {
  M1 <- matrix(0, nrow = nf, ncol = nc)  # Inicializa la matriz
  return(M1)
}

# Para ingresar columnas
Ingreso_columnas <- function() {
  cat("Numero de filas: ")
  nf <- as.integer(readline())
  cat("Numero de columnas: ")
  nc <- as.integer(readline())
  return(list(nf = nf, nc = nc))
}

val_matriz <- function(M1, nf, nc) {
  for (i in 1:nf) {
    for (j in 1:nc) {
      cat("Ingrese el elemento [", i, ",", j, "]: ")
      M1[i, j] <- as.numeric(readline())
    }
  }
  return(M1)
}

cant_par_impar <- function(M1, nf, nc) {
  p = 0
  imp = 0
  for (i in 1:nf) {
    for (j in 1:nc) {
      if (M1[i, j] %% 2 == 0) {
        p <- p + 1
      } else {
        imp <- imp + 1
      }
    }
  }
  return(list(p = p, imp = imp))
}

###### Pasar a vectores los valores de los pares e impares
val_par_vec <- function(M1, nf, nc) {
  v1 <- c()
  v2 <- c()
  k <- 1
  q <- 1
  for (i in 1:nf) {
    for (j in 1:nc) {
      if (M1[i, j] %% 2 == 0) {
        v1[k] <- M1[i, j]
        k <- k + 1
      } else {
        v2[q] <- M1[i, j]
        q <- q + 1
      }
    }
  }
  return(list(v1 = v1, v2 = v2))
}

############## Crear el menú
menu <- function() {
  cat("Menu principal\n")
  cat("1. Ingresar la matriz\n")
  cat("2. Cuantos pares e impares tiene la matriz\n")
  cat("3. Valores pares e impares\n")
  cat("4. Salir\n")
  cat("Escoja una opción: ")
  op1 <- as.numeric(readline())
  return(op1)
}

menu3 <- function() {
  F <- Ingreso_columnas()
  nf1 <- F$nf
  nc1 <- F$nc
  M <- crear_matriz(nf1, nc1)
  Mat1 <- val_matriz(M, nf1, nc1)
  #### Cambio a numérico los valores de la matriz
  Mat2 <- matrix(as.numeric(Mat1), nrow = nrow(Mat1), ncol = ncol(Mat1))

  ban = 0
  while (ban == 0) {
    op1 <- menu()

    switch(as.character(op1),
           "1" = {
             cat("Visualizar la matriz ingresada\n")
             print(Mat1)
             print(Mat2)
           },
           "2" = {
             cat("Existen en la matriz\n")
             c1 <- cant_par_impar(Mat2, nf1, nc1)
             cat("Pares =", c1$p, "\n")
             cat("Impares =", c1$imp, "\n")
           },
           "3" = {
             cat("Valores pares e impares de la matriz \n")
             l1vect <- val_par_vec(Mat2, nf1, nc1)
             cat("Vector 1 con pares\n")
             print(l1vect$v1)
             cat("Vector 2 con impares\n")
             print(l1vect$v2)
           },
           "4" = {
             cat("Saliendo del sistema....\n")
             ban <- 1
           },
           {
             cat("Error, dijite una opción correcta\n")
           }
    ) # cierre del switch
  } # cierre del while
}

menu3()





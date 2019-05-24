# This package contains useful function for statistical calculation

#' @title Informations statistique univarié
#' @description Permet d'avoir toutes les informations statistique univarié d'un vecteur.
#'
#' @seealso \code{\link{mean}}
#' @seealso \code{\link{stat.var_nc}}
#' @seealso \code{\link{stat.var}}
#' @seealso \code{\link{stat.sd_nc}}
#' @seealso \code{\link{stat.sd}}
#' @seealso \code{\link{median}}
#' @seealso \code{\link{stat.mode}}
#'
#' @param x le vecteur.
stat.description <- function(x) {
  cat("Valeurs:\n")
  cat(x);
  cat("\n\n");

  cat(sprintf("Moyenne = %.5f\n", mean(x)))
  cat(sprintf("Ecart-type NC = %.5f\n", stat.sd_nc(x)))
  cat(sprintf("Ecart-type = %.5f\n", sd(x)))
  cat(sprintf("Variance NC = %.5f\n", stat.var_nc(x)))
  cat(sprintf("Variance = %.5f\n", var(x)))
  cat(sprintf("Mediane = %.5f\n", median(x)))
  cat(sprintf("Mode = %.5f\n", stat.mode(x)))
}

#' @title Variance non corrigé
#' @description Donne la variance empirique non corrigé d'un vecteur.\cr
#'  Equation 1.25 page 22
#'
#' @param x le vecteur
stat.var_nc <- function(x) {
  mean_x <- mean(x)
  return ((sum((x-mean_x)^2)) / length(x))
}

#' @title Variance corrigé
#' @description onne la variance empirique corrigé d'un vecteur.\cr
#'  Equation 1.26 page 22
#'
#' @param x le vecteur
stat.var <- function(x) {
  return (var(x))
}

#' @title Ecart-type non corrigé
#' @description Donne l'écart-type empirique non corrigé d'un vecteur.\cr
#'  Equation 1.27 page 22
#'
#' @param x le vecteur
stat.sd_nc <- function(x) {
  return (sqrt(stat.var_nc(x)))
}

#' @title Ecart-type corrigé
#' @description Donne l'écart-type empirique corrigé d'un vecteur.\cr
#'  Equation 1.28 page 22
#'
#' @param x le vecteur
stat.sd <- function(x) {
  return (sqrt(sd(x)))
}

#' @title Mode
#' @description Donne le mode d'un vecteur.
#' @details Le mode de la série statistique discrète (X) = (X(i))i=1,...,n,
#' notée Mo(X) est la valeur xi dont la fréquence est maximale.\cr
#' Dès lors, on distinguera\cr
#'  — les distributions unimodales avec un seul mode,\cr
#'  — des distributions plurimodales avec plusieurs modes.\cr
#'
#' @param x le vecteur
stat.mode <- function(x) {
  unique_x <- unique(x)
  return (unique_x[which.max(tabulate(match(x, unique_x)))])
}

#' @title Covariance
#' @description Donne la covariance.\cr
#'  Definition 2.6 page 44
#' @details Notre objectif est d’obtenir une valeur numerique exprimant le degre
#'  de liaison qui peut exister entre deux variables X et Y.\cr\cr
#'
#' Cov(X, Y) = sum((x - mean(x)) * (y - mean(y)))
#'
#' @param x le vecteur X
#' @param y le vecteur Y
#' @return la covariance entre X et Y
stat.covariance <- function(x, y) {
  mean_x = mean(x)
  mean_y = mean(y)

  return (sum((x - mean_x) * (y - mean_y)))
}

#' @title Droite de régression
#' @description Donne la droite de régression d'un vecteur Y en fonction d'un vecteur X.\cr
#'  Theoreme 2.1 page 48
#' @details Nous travaillons avec deux séries statistiques X et Y, mesurées sur un
#' échantillon de n individus. Si on suppose que le comportement théorique de Y en
#' fonction de X peut s’expliquer de la manière suivante\cr
#'
#' La droite de régression de Y par rapport à X est la droite\cr
#' Y = a * X + b\cr\cr
#' a = Cov(X, Y) / S^2(X)\cr
#' b = Y - a * X
#'
#' @seealso \code{\link{stat.covariance}}
#'
#' @param x le vecteur X
#' @param y le vecteur Y
#'
#' @examples
#' Les cotes de 9 étudiants en Janvier et en fin d’année sont les suivantes.
#' Janvier <- c(77, 50, 71, 72, 81, 94, 96, 99, 67)
#' Juin <- c(82, 66, 78, 34, 47, 85, 99, 99, 68)
#'
#' - Déterminez la droite de régression de Y en fonction X
#' stat.linear_regression(Janvier, Juin)
stat.linear_regression <- function(x, y) {
  cat("Regression line\n---------------\n")

  cat("Y = a * X + b\n")
  cat("a = Cov(X, Y) / S^2(X)\n")
  cat("b = mean(Y) - a * mean(X)\n\n")

  mean_x = mean(x)
  mean_y = mean(y)
  cat(sprintf("mean(X) = %.5f\n", mean_x))
  cat(sprintf("mean(Y) = %.5f\n\n", mean_y))

  cov_x_y = stat.covariance(x, y)

  cat(sprintf("Cov(X, Y) = %.5f\n\n", cov_x_y))

  s2_x = sum((x - mean_x)^2)
  cat(sprintf("S^2(X) = %.5f\n\n", s2_x))

  a = cov_x_y / s2_x
  b = mean_y - a * mean_x

  cat(sprintf("a = %.5f\n", a))
  cat(sprintf("b = %.5f\n\n", b))

  cat(sprintf("Y = %.5f + X * %.5f\n\n", b, a))
}

#' @title Valeur de Y par rapport à X pour une droite de régression
#' @description Donne une valeur de Y par rapport à X pour une droite de régression basé sur un
#' vecteur X et un vecteur Y.
#'
#' @param x le vecteur X
#' @param y le vecteur Y
#' @param x_valeur la valeur de X
#'
#' @return la valeur de Y
#'
#' @examples
#' Les cotes de 9 étudiants en Janvier et en fin d'année sont les suivantes.
#' Janvier <- c(77, 50, 71, 72, 81, 94, 96, 99, 67)
#' Juin <- c(82, 66, 78, 34, 47, 85, 99, 99, 68)
#'
#' - En utilisant le modèle de liaison entre la note de janvier et celle de
#' juin, estimez la cote de juin d'un étudiant ayant eu 85 en janvier.
#' stat.linear_regression.y(Janvier, Juin, 85)
stat.linear_regression.y <- function(x, y, x_value) {
  mean_x = mean(x)
  mean_y = mean(y)

  cov_x_y = sum((x - mean_x) * (y - mean_y))

  s2_x = sum((x - mean_x)^2)

  a = cov_x_y / s2_x
  b = mean_y - a * mean_x

  return(b + x_value * a)
}

#' @title Valeur de X par rapport à Y pour une droite de régression
#' @description Donne une valeur de X par rapport à Y pour une droite de régression basé sur un
#' vecteur X et un vecteur Y.
#'
#' @param x le vecteur X
#' @param y le vecteur Y
#' @param y_valeur la valeur de Y
#'
#' @return la valeur de X
#'
#' @examples
#' Les cotes de 9 étudiants en Janvier et en fin d'année sont les suivantes.
#' Janvier <- c(77, 50, 71, 72, 81, 94, 96, 99, 67)
#' Juin <- c(82, 66, 78, 34, 47, 85, 99, 99, 68)
#'
#' - En utilisant le modèle de liaison entre la note de janvier et celle de
#' juin, estimez la cote de janvier d'un étudiant ayant eu 78 en juin
#' stat.linear_regression.x(Janvier, Juin, 78)
stat.linear_regression.x <- function(x, y, y_value) {
  mean_x = mean(x)
  mean_y = mean(y)

  cov_x_y = sum((x - mean_x) * (y - mean_y))

  s2_x = sum((x - mean_x)^2)

  a = cov_x_y / s2_x
  b = mean_y - a * mean_x

  return((y_value - b) / a)
}

#' @title Droite de regression pour une matrice
#' @description Donne la droite de regression d'une matrice à deux dimension. Les noms des colonnes et des
#' lignes doivent contenir la valeur moyenne pour la ligne/colonne (example: si l'interval est {65,70}, le titre
#' doit être 67.5)
#'
#' @param reg_matrix la matrice de régression
#' @param row_name le nom 'user-friendly' utilisé pour les lignes (ex. Poids)
#' @param col_name le nom 'user-friendly' utilisé pour les colonnes (ex. Taille)
stat.linear_regression.matrix <- function(reg_matrix, row_name = 'Row', col_name = 'Column') {
  length_title <- nchar(row_name) + 3 + nchar(col_name)

  row <- c()
  col <- c()

  for (i in 1:length(rownames(reg_matrix))) {
    row <- c(row, rep(as.numeric(rownames(reg_matrix)[i]),
                      sum(reg_matrix[i, ])))
  }

  for (i in 1:length(colnames(reg_matrix))) {
    col <- c(col, rep(as.numeric(colnames(reg_matrix)[i]),
                      sum(reg_matrix[, i])))
  }

  a_rw = cov(row, col) / var(row)
  b_rw = mean(col) - a_rw * mean(row)

  cat(sprintf("%s - %s\n%s\n", row_name, col_name,
              paste(rep('-', length_title), collapse = '')))
  cat(sprintf("a = %.5f\n", a_rw))
  cat(sprintf("b = %.5f\n", b_rw))

  cat("\n")

  a_col = cov(row, col) / var(col)
  b_col = mean(row) - a_col * mean(col)

  cat(sprintf("%s - %s\n%s\n", col_name, row_name,
              paste(rep('-', length_title), collapse = '')))
  cat(sprintf("a = %.5f\n", a_col))
  cat(sprintf("b = %.5f\n", b_col))
}

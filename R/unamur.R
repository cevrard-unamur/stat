# This package contains useful function for the UNamur statistics lecture

#' Permet d'avoir toutes les informations statistique univarié d'un vecteur.
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

#' Donne la variance empirique non corrigé d'un vecteur.
#'  Equation 1.25 page 22
#'
#' @param x le vecteur
stat.var_nc <- function(x) {
  mean_x <- mean(x)
  return ((sum((x-mean_x)^2)) / length(x))
}

#' Donne la variance empirique corrigé d'un vecteur.
#'  Equation 1.26 page 22
#'
#' @param x le vecteur
stat.var <- function(x) {
  return (var(x))
}

#' Donne l'écart-type empirique non corrigé d'un vecteur
#'  Equation 1.27 page 22
#'
#' @param x le vecteur
stat.sd_nc <- function(x) {
  return (sqrt(stat.var_nc(x)))
}

#' Donne l'écart-type empirique corrigé d'un vecteur
#'  Equation 1.28 page 22
#'
#' @param x le vecteur
stat.sd <- function(x) {
  return (sqrt(sd(x)))
}

#' Donne le mode d'un vecteur
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

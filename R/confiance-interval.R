# This package contains useful function to calculate confiance interval

qz <- function(q, df=NULL) {
  return(qnorm(q, 0, 1))
}

pz <- function(p, df=NULL, lower.tail=TRUE) {
  return(pnorm(p, 0, 1, lower.tail=lower.tail))
}

#' @title Interval de confiance pour la moyenne
#' @description Calcul l'interval de confiance à partir d'un vecteur.
#'
#' @param x le vecteur
#' @param sd l'écart-type (Défaut: NULL, il est calculé à partir du vecteur s'il n'est pas fourni)
#' @param conf.level paramètre de confiance (Défaut: 0.95)
#'
#' @return Un vecteur contenant l'interval de confiance
#'
#' @examples
#' On veut estimer le rendement d’un engrais pour la culture du blé.
#' Sur douze parcelles expérimentales, on a trouvé les rendements suivants en tonnes par hectare:
#'
#' rendements <- c(7.7, 8.4, 7.8, 8.2, 7.9, 8.5, 8.4, 8.2, 7.6, 7.8, 8.4, 8.3)
#'
#' CI.mean(rendements)
CI.mean <- function(x, sd=NULL, conf.level=.95)
{
  mu <- mean(x)
  n <- length(x)
  empirical_sd <- FALSE

  if (is.null(sd)) {
    sd <- sd(x)
    empirical_sd <- TRUE
  }

  return(CI.mean.base(n, mu, sd, empirical_sd, conf.level))
}

#' @title Interval de confiance pour la moyenne
#' @description Calcul l'interval de confiance sans echantillon.
#'
#' @param n la taille de l'échantillon
#' @param mu la moyenne de l'échantillon
#' @param sd l'écart-type
#' @param empirical_sd indique si l'écart-type est connu ou alors calculé à partir de l'échantillon
#' @param conf.level paramètre de confiance (Défaut: 0.95)
#'
#' @return Un vecteur contenant l'interval de confiance
#'
#' @examples
#' Pour  évaluer le nombre de mots d’un livre, on tire 20 pages au hasard et on y compte le nombre de mots.
#' On trouve, pour les 20 valeurs, une moyenne de 614 mots et un  écart-type de 26 mots.
#' Donner un intervalle de confiance à 95% pour le nombre total de mots du livre sachant qu’il a 158 pages
#' (on admettra que l’approximation normale est satisfaisante).
#'
#' CI.mean.base(158, 614, 26, empirical_sd = FALSE)
CI.mean.base <- function(n, mu, sd, empirical_sd, conf.level=.95) {
  cat(sprintf("Taille de l'échantillon: %.5f\n", n))
  cat(sprintf("Moyenne: %.5f\n", mu))
  cat(sprintf("Ecart-type: %.5f\n", sd))

  if (empirical_sd) {
    cat("L'écart-type est calculé à partir de l'échantillon\n")
    cat("Utilisation de la loi Student\n")
    e = qt((1+conf.level)/2, n-1) * sd / sqrt(n)
    cat(sprintf("e = Pr[t(%.2f) ≤ x] = %.4f * %.5f / sqrt(%.5f)\n", n-1, (1+conf.level)/2, sd, n))
  } else {
    cat("L'écart-type est connu\n")
    cat("Utilisation de la loi Normale\n")
    e = qz((1+conf.level)/2) * (sd / sqrt(n))
    cat(sprintf("e = Pr[N(0, 1) ≤ x] = %.4f * (%.2f / sqrt(%.2f))\n", (1+conf.level)/2, sd, n))
  }

  cat(sprintf("\nL'interval de confiance avec %.2f est compris entre %.5f et %.5f\n", conf.level, mu-e, mu+e))

  return(c(mu-e, mu+e))
}

#' @title Interval de confiance pour la moyenne connaissant la variance
#' @description Calcul l'interval de confiance pour la moyenne à partir de la taille
#' d'un échantillon, d'une moyenne et de la variance
#'
#' @param n la taille de l'échantillon
#' @param mu la moyenne de l'échantillon
#' @param var la variance
#' @param conf.level paramètre de confiance (Défaut: 0.95)
#'
#' @return Un vecteur contenant l'interval de confiance
#'
#' @examples
#' Considérons une population admettant une distribution normale.
#' On y prélève un échantillon aléatoire simple d’effectif 16, qui donne une moyenne de 80.2.
#' Déterminez un intervalle de confiance à 95% pour la moyenne :
#'
#' 1. si σ2 = 0.09
#'
#' CI.mean.var(16, 80.2, .09)
CI.mean.var = function(n, mu, var, conf.level = 0.95) {
  return (CI.mean.base(n, mu, sqrt(var), empirical_sd = FALSE, conf.level))
}

#' @title Interval de confiance pour la variance
#' @description Calcul l'interval de confiance pour la variance à partir de la taille
#' d'un échantillon, d'une moyenne et d'un écart-type
#'
#' @param n la taille de l'échantillon
#' @param mu la moyenne de l'échantillon
#' @param sd l'écart-type
#'
#' @examples
#' Considérons une population admettant une distribution normale.
#' On y prélève un échantillon aléatoire simple d’effectif 16, qui donne une moyenne de 80.2.
#' Déterminez un intervalle de confiance à 95% pour la moyenne :
#'
#' 2. si σ2 est inconnu, et l'écart-type de l'échantillon vaut 0.3.
#' Dans ce dernier cas, déterminez un intervalle de confiance pour la variance, au même niveau de confiance.
#'
#' CI.var.sd(16, 80.2, .3)
CI.var.sd = function(n, mu, sd, conf.level = 0.95) {
  sighat = sd * (n / (n - 1))
  norm = (1 + conf.level) / 2

  interval1 = ((n - 1) * sighat^2) / qchisq(norm, n - 1)
  interval2 = ((n - 1) * sighat^2) / qchisq(1 - norm, n - 1)

  cat(sprintf("[%.5f, %.5f]", interval1, interval2))
}

CI.mean.effectif = function(pi_u, ecart, alpha = .95) {
  norm = (1 + alpha) / 2
  val_norm = qnorm(norm)

  effectif = (val_norm / ecart)^2 * pi_u * (1 - pi_u)

  cat(sprintf("Effectif >= %.5f", effectif))
}

CI.var.base <- function(n, v, conf.level=.95) {
  return(c((n-1)*v/qchisq((1+conf.level)/2, n-1), (n-1)*v/qchisq((1-conf.level)/2, n-1)))
}

CI.var <- function(x, conf.level=.95)
{
  v <- var(x)
  n <- length(x)

  return(CI.var.base(n, v, conf.level))
}

CI.prop.base <- function(n, prop.char, conf.level=.95) {
  e = qnorm((1+conf.level)/2, 0, 1) * sqrt((prop.char * (1 - prop.char)) / n)
  return(c(prop.char-e, prop.char+e))
}

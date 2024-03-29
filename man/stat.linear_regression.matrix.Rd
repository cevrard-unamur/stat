% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat.R
\name{stat.linear_regression.matrix}
\alias{stat.linear_regression.matrix}
\title{Droite de regression pour une matrice}
\usage{
stat.linear_regression.matrix(reg_matrix, row_name = "Row",
  col_name = "Column")
}
\arguments{
\item{reg_matrix}{la matrice de régression}

\item{row_name}{le nom 'user-friendly' utilisé pour les lignes (ex. Poids)}

\item{col_name}{le nom 'user-friendly' utilisé pour les colonnes (ex. Taille)}
}
\description{
Donne la droite de regression d'une matrice à deux dimension. Les noms des colonnes et des
lignes doivent contenir la valeur moyenne pour la ligne/colonne (example: si l'interval est {65,70}, le titre
doit être 67.5)
}

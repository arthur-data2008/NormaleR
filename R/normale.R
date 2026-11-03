# Calcul de la densite de la loi normale centree reduite

#' Calculer la densite de la loi normale centree reduite
#'
#' Calcule la valeur de la fonction de densite
#' pour une valeur donnee.
#'
#' @param x Valeur numerique a evaluer.
#'
#' @return La valeur de la densite normale.
#'
#' @examples
#' densite_normale(0)
#'
#' @export

densite_normale <- function(x) {

  resultat <- (1 / sqrt(2 * pi)) * exp(-(x^2) / 2)

  return(resultat)
}#' Calculer une probabilite sur un intervalle
#'
#' Calcule P(a <= Z <= b) pour une loi normale standard.
#'
#' @param a Borne inferieure.
#' @param b Borne superieure.
#'
#' @return La probabilite comprise entre a et b.
#'
#' @examples
#' probabilite_intervalle(-1, 1)
#'
#' @export
probabilite_intervalle <- function(a, b) {

  if (a > b) {
    stop("La borne a doit etre inferieure ou egale a b")
  }

  resultat <- probabilite_normale(b) -
    probabilite_normale(a)

  return(resultat)
}#' Calculer un quantile de la loi normale standard
#'
#' @param p Probabilite comprise entre 0 et 1.
#'
#' @return Le quantile correspondant.
#'
#' @examples
#' quantile_normale(0.975)
#'
#' @export
quantile_normale <- function(p) {

  if (p < 0 || p > 1) {
    stop("La probabilite doit etre entre 0 et 1")
  }

  resultat <- qnorm(p)

  return(resultat)
}#' Calculer la probabilite cumulee de la loi normale centree reduite
#'
#' Calcule P(Z <= x), ou Z suit une loi normale standard.
#'
#' @param x Valeur numerique.
#'
#' @return La probabilite cumulee jusqu'a x.
#'
#' @examples
#' probabilite_normale(0)
#'
#' @export
probabilite_normale <- function(x) {

  resultat <- integrate(
    densite_normale,
    lower = -Inf,
    upper = x
  )$value

  return(resultat)
}#' Simuler des valeurs suivant une loi normale centree reduite
#'
#' Genere un echantillon aleatoire de taille n.
#'
#' @param n Nombre de valeurs a generer.
#'
#' @return Un vecteur de valeurs aleatoires.
#'
#' @examples
#' simuler_normale(100)
#'
#' @export
simuler_normale <- function(n) {

  if (n <= 0 || n != as.integer(n)) {
    stop("n doit etre un entier strictement positif")
  }

  resultat <- rnorm(n, mean = 0, sd = 1)

  return(resultat)
}#' Tracer une distribution normale simulee
#'
#' Affiche un histogramme et la densite theorique.
#'
#' @param n Nombre de valeurs a simuler.
#'
#' @return Un graphique de la distribution simulee.
#'
#' @examples
#' tracer_normale(1000)
#'
#' @export
tracer_normale <- function(n) {

  echantillon <- simuler_normale(n)

  hist(echantillon,
       probability = TRUE,
       main = "Distribution normale centree reduite",
       xlab = "Valeurs de Z",
       ylab = "Densite",
       col = "lightblue",
       border = "white")

  curve(dnorm(x),
        add = TRUE,
        col = "red",
        lwd = 2)
}

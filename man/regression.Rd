% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regression.R
\name{regression}
\alias{regression}
\title{Creer un dataframe qui exclut les outliers d'une base de donnees}
\usage{
regression(VD, pred, alpha)
}
\arguments{
\item{VD}{la VD de la base de donnees}

\item{pred}{VI1 de la bdd a nettoyer}

\item{alpha}{risque alpha}

\item{data}{dataframe}

\item{nb_fact}{nombre de facteurs: 0, 1 ou 2}

\item{effect}{VI1, VI2 or VI1*VI2}
}
\value{
resultats du test ANOVA, comme dans SPSS, aux normes APA
}
\description{
Creer un dataframe qui exclut les outliers d'une base de donnees
}
\keyword{APA}
\keyword{homoscedasticite,}

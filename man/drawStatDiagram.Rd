% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statisticalDiagram.R
\name{drawStatDiagram}
\alias{drawStatDiagram}
\title{draw StatDiagram}
\usage{
drawStatDiagram(
  no,
  arrows,
  nodes,
  labels,
  nodeslabels = list(),
  xmargin,
  radx,
  rady,
  fit = NULL,
  addprime = TRUE,
  box.col = "white",
  xlim = c(0, 1),
  ylim = c(0, 1)
)
}
\arguments{
\item{no}{process macro model number}

\item{arrows}{A data.frame}

\item{nodes}{A data.frame}

\item{labels}{A list}

\item{nodeslabels}{A list}

\item{xmargin}{horizontal margin of plot}

\item{radx}{horizontal radius of the box.}

\item{rady}{vertical radius of the box.}

\item{fit}{An object of class lavaan. Result of lavaan::sem()}

\item{addprime}{logical Whether add prime to label "c"}

\item{box.col}{fill color of the box}

\item{xlim}{the x limits (min,max) of the plot}

\item{ylim}{the y limits (min,max) of the plot}
}
\description{
draw StatDiagram
}

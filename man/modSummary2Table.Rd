% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modSummary2.R
\name{modSummary2Table}
\alias{modSummary2Table}
\title{Make flextable summarizing moderation effect}
\usage{
modSummary2Table(x, vanilla = TRUE, ...)
}
\arguments{
\item{x}{An object}

\item{vanilla}{logical}

\item{...}{Further argument to be passed to modSummary3}
}
\description{
Make flextable summarizing moderation effect
}
\examples{
fit=lm(govact~negemot*sex+negemot*age+posemot+ideology,data=glbwarm)
modSummary2Table(fit)
}

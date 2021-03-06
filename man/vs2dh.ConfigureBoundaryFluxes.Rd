% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs2dhConfigure.R
\name{vs2dh.ConfigureBoundaryFluxes}
\alias{vs2dh.ConfigureBoundaryFluxes}
\title{Configure Boundary fluxes}
\usage{
vs2dh.ConfigureBoundaryFluxes(nodes = NULL)
}
\arguments{
\item{nodes}{data.frame with columns "idbf" (id of boundary face), "bf_j" (grid row)
and "bf_n" (grid column)}
}
\value{
Boundary fluxes parameterisation
}
\description{
Configure Boundary fluxes
}
\examples{
nodes <- data.frame(idbf = c(rep(1,5), rep(2,6)), 
                    bf_j = 1:11, 
                    bf_n = rep(1,11))
vs2dh.ConfigureBoundaryFluxes(nodes = nodes) 
}

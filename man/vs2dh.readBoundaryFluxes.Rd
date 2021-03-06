% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs2dhReadOutput.R
\name{vs2dh.readBoundaryFluxes}
\alias{vs2dh.readBoundaryFluxes}
\title{Read VS2dh model output file with fluxes at boundary faces}
\usage{
vs2dh.readBoundaryFluxes(model.path, fileName = "boundaryFluxes.out",
  dbg = TRUE)
}
\arguments{
\item{model.path}{full path to folder containing model output files}

\item{fileName}{name of file containing fluxes at boundary faces
(default: "boundaryFluxes.out")}

\item{dbg}{if true text output}
}
\value{
read model results of boundary faces are imported in a R object
}
\description{
Read VS2dh model output file with fluxes at boundary faces
}
\examples{
### Location of example vs2dh model contained in "kwb.vs2dh package"
model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
vs2dh.readBoundaryFluxes(model.path)
}

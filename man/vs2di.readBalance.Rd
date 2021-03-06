% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs2dhReadOutput.R
\name{vs2di.readBalance}
\alias{vs2di.readBalance}
\title{Read VS2dh model output file with (energy, fluid) mass balance time series}
\usage{
vs2di.readBalance(model.path, engine, fileName = "balance.out",
  dbg = TRUE)
}
\arguments{
\item{model.path}{full path to folder containing model output files}

\item{engine}{model engine 'vs2dh' (for flow & heat modelling) or 'vs2dt' (for 
flow & solute transport) (Default: no default)}

\item{fileName}{name of file containing (energy and fluid) balance results 
(default: "balance.out")}

\item{dbg}{if true text output}
}
\value{
balance model results are imported in a R object
}
\description{
Read VS2dh model output file with (energy, fluid) mass balance time series
}
\examples{
### Location of kwb.vs2dh package on your computer
wDir <- system.file(package = "kwb.vs2dh")
model.path <- file.path(wDir, "extdata/vs2dh_example/tutorial2")
res <- vs2di.run(model.path = model.path) 
vs2di.readBalance(model.path = model.path, engine = "vs2dh")

}

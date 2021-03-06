% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs2dhReadOutput.R
\name{vs2di.readMain}
\alias{vs2di.readMain}
\title{Read main VS2dh model output file "vs2dh.out"}
\usage{
vs2di.readMain(model.path, engine = "vs2dh")
}
\arguments{
\item{model.path}{full path to folder containing model output files}

\item{engine}{model engine 'vs2dh' (for flow & heat modelling) or 'vs2dt' (for 
flow & solute transport) (Default: "vs2dh")}
}
\value{
main model results are imported in a R object
}
\description{
Read main VS2dh model output file "vs2dh.out"
}
\examples{
### Location of example vs2dh model contained in "kwb.vs2dh package"
model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
res <- vs2di.run(model.path = model.path)  
vs2di.readMain(model.path = model.path, engine = "vs2dh")
}

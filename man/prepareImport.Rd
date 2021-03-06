% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs2dhReadConfig.R
\name{prepareImport}
\alias{prepareImport}
\title{Helper function: prepareImport (imports and prepares input file)}
\usage{
prepareImport(model.path, mdb.path = system.file("extdata",
  "InputDescription.xls", package = "kwb.vs2dh"), dbg = TRUE)
}
\arguments{
\item{model.path}{full path to folder containing vs2dh.dat}

\item{mdb.path}{full path to the folder containing the InputDescription.xls
which is used as lookup table for converting FORTRAN to R parameter names 
(Default: system.file("extdata", "InputDescription.xls", package = "kwb.vs2dh"))}

\item{dbg}{if TRUE text output is printed on the screen}
}
\value{
Read vs2dhi.dat and does some preparations steps which are stores in a list
which contains the sublists "input" (raw imported vs2dh.dat file), "inpDescription" 
(lookup table defined in file "mdb.path"), "res" (prepared table with parameters names 
and values) and  "rechargePeriods" (summary of start/endlines of file "input" for 
each recharge period)
}
\description{
Helper function: prepareImport (imports and prepares input file)
}
\examples{
\dontrun{
model.path <- system.file("extdata", "vs2dh_example/tutorial2", package = "kwb.vs2dh")
prepareImport(model.path)
}

}

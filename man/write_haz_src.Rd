% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_haz_src.R
\name{write_haz_src}
\alias{write_haz_src}
\title{Table to Fortran}
\usage{
write_haz_src(icoor = 1, faults, fault_out.src)
}
\arguments{
\item{icoor}{numeric: The unit of the source coordinate, 0 is km, 1 is latitude and longitude. Default is 1.}

\item{faults}{character: Table of 'fault.src'}

\item{fault_out.src}{character: Set the new output name}
}
\value{
fortran file of \code{faults}
}
\description{
Table to Fortran
}

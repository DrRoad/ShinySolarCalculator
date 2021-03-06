% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suncalculator.R
\name{EA}
\alias{EA}
\title{Elevation Angle}
\usage{
EA(latitude, DA, HRA)
}
\arguments{
\item{latitude}{Location's latitude in degree}

\item{DA}{Declination Angle in degree}

\item{HRA}{Hour Angle (HRA) in degree}
}
\value{
Elevation Angle in degree
}
\description{
The elevation angle (used interchangeably with altitude angle) is 
the angular height of the sun in the sky measured from the horizontal.
}
\examples{
EA(51.93,2.936844,-163.3115)
}


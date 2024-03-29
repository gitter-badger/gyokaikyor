% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fmtcatch_kumamoto_saga.R
\name{fmtcatch.saga}
\alias{fmtcatch.saga}
\title{Load catch data of saga and tidy it up}
\usage{
\method{fmtcatch}{saga}(path, spcs, type)
}
\arguments{
\item{path}{File path to process}

\item{spcs}{Romaji spcs name one of
\itemize{
  \item{"maaji"}
  \item{"maiwashi"}
  \item{"masaba"}
  \item{"katakuchi"}
  \item{"urume"}
}}

\item{type}{Character value one of
\itemize{
  \item{"karatsu"}
  \item{"kennai"}
  \item{"teichi"}
  \item{"chumaki"}
}
  Data processing algorithm is controled by this parameter.}
}
\description{
Load catch data of saga and tidy it up
}

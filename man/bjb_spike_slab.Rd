% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spikeslab.R
\name{bjb_spike_slab}
\alias{bjb_spike_slab}
\title{spike&slab}
\usage{
bjb_spike_slab()
}
\arguments{
\item{W}{predictor matrix}

\item{z}{response variance}

\item{iteration}{Number of iterations}

\item{a, b}{rejection sampler parameter}

\item{s}{xi's hyperparameter}

\item{time_check}{Output run time}

\item{iteration_check}{Output iteration}

\item{beta}{the initial value of beta}

\item{xi}{the initial value of Global shrinkage parameter}

\item{Sigma}{the inial value of Sigma}

\item{w}{Sigma's hyperparameter}
}
\value{
beta posterior samples
}
\description{
Run linear model algorithm with spike&slab prior
}

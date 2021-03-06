% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mycltu.R
\name{mycltu}
\alias{mycltu}
\title{Uniform Distribution based on the Central Limit Theorem}
\usage{
mycltu(n, iter, a = 0, b = 10)
}
\arguments{
\item{n}{The sample size of the population}

\item{iter}{The number of iterations taken}

\item{a}{The lower limit of the distribution}

\item{b}{The upper limit of the distribution}
}
\value{
A histogram of the density v. sample mean
with density curves for the population and sample
}
\description{
Uniform Distribution based on the Central Limit Theorem
}
\examples{
mycltu(n=20,iter=10000)
}

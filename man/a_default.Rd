% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wf_basic.R
\name{a_default}
\alias{a_default}
\alias{a_default.data.frame}
\alias{a_default.list}
\alias{k_default}
\alias{k_default.data.frame}
\alias{k_default.list}
\alias{r_default}
\alias{r_default.data.frame}
\alias{r_default.list}
\title{Calculate Default LoCoH Parameters}
\usage{
a_default(dat, ...)

\method{a_default}{data.frame}(dat, ...)

\method{a_default}{list}(dat, FUN = max, ...)

k_default(dat, ...)

\method{k_default}{data.frame}(dat, ...)

\method{k_default}{list}(dat, FUN = mean, ...)

r_default(dat, ...)

\method{r_default}{data.frame}(dat, ...)

\method{r_default}{list}(dat, FUN = mean, ...)
}
\arguments{
\item{dat}{\verb{[data.frame | list]} The location data used for fitting the
LoCoH. Can be a single \code{data.frame} or a list of \code{data.frame}s with the
following columns:
\itemize{
\item \verb{$x} -- The x-coordinate of the animal's location
\item \verb{$y} -- The y-coordinate of the animal's location
\item \verb{$t} -- The date and time (as a \code{POSIXct} object) of the location
}}

\item{...}{Extra arguments passed to methods (none currently implemented).}

\item{FUN}{\verb{[function]} A function to summarize across list elements (the
\code{data.frame}s) if \code{dat} is a \code{list}. Ignored if \code{dat} is a \code{data.frame}.
Defaults to \code{max} for \code{a_default()} and to \code{mean} for \code{k_default()} and
\code{r_default()}.}
}
\value{
A numeric vector of length 1 with the value of \code{a}, \code{k}, or \code{r}
}
\description{
Calculates suggested starting value for \code{a}, \code{k}, or \code{r} for constructing
local convex hulls.
}
\details{
\itemize{
\item \code{a_default()} Calculates \code{a} as the maximum distance between any two
points in the dataset.
\item \code{k_default()} Calculates \code{k} as the square root of the number of
points in the dataset.
\item \code{r_default()} Calculates \code{r} as half the maximum nearest neighbor
distance between points in the dataset.
}

\emph{Note}: computation times can get large with a large dataset.

\code{r_default()} is the most time-consuming algorithm and has the least
desirable properties (see Getz et al. 2007).

\code{a_default()} also requires calculating a distance matrix -- so can also be
somewhat time consuming -- but Getz et al. (2007) found the resulting LoCoH
is robust to the value of \code{a}.

\code{k_default()} is the easiest to compute as it depends only on the
number of points and does not require a distance matrix.
}
\examples{

\dontrun{
data(tracks)

# data.frame
winter01 <- tracks[which(tracks$id == "ID01" & tracks$season == "Winter"), ]

a_default(winter01)
k_default(winter01)
r_default(winter01)

# list
winter <- tracks[which(tracks$season == "Winter"), ]

winter_list <- split(winter, winter$id)

a_default(winter_list)
k_default(winter_list)
r_default(winter_list)
}

}
\seealso{
These starting algorithms are described by Getz et al. (2007).
\emph{insert full citation here}
}

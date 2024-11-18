\name{nsec}
\alias{nsec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NSEC - The No Significant Effect Concentration
}
\description{
This function computes a No Significant Effect Concentration (NSEC) from a C-R model fitted using the drc package. Optionally, the user may specify additional drc-type model structures to the nsec function which are then used to construct a model-averaged NSEC (maNSEC),
}
\usage{
nsec(fit,mods,control.conc=0,sig=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{the fitted model object from function drm in package drc}
  \item{mods}{a list of alternative models for computing a maNSEC. These must be recognised model specifications from the drc package e.g LL.3()}
  \item{control.conc}{the concentration which is regarded as the 'control'. Defaults to zero if not specified.}
  \item{sig}{the signicance level (one-sided), Defaults to 0.05 if not specified.}
}
\details{
The NSEC is very similar in spirit to the NOEC in that we wish to determine the largest concentration for which the expected response is statistically insignificant to the expected control response.However, unlike the NSEC, the resulting concentration is not constrained to be one of the test concentrations. Further, and unlike a NOEC which treats concentration values as levels of a factor, the NSEC derives it's estimate from a concentration-response model fitted to the experimental data.

Using the nsec() functin is a two-step process: 1) the drc package is used to fit a chosen C-R model via the drm() function.
}

\value{
A single numeric value which is the NSEC for the supplied model if no alternative models are specified or the model-averaged NSEC (maNSEC) based on the supplied model and the additional models specified in mod. In addition, the model(s) are printed out together with their individual NSEC values as well as the model weight.
}


\references{
Fisher, R. and Fox, D.R. (2023). Introducing the No-Significant-Effect Concentration. Environmental Toxicology and Chemistry.
\url{ https://doi.org/10.1002/etc.5610}

Fisher, R.,Fox, DR.,Negri, AP.,van Dam, J.,Flores, F.,Koppel, D. (2023) Methods for estimating no-effect toxicity concentrations in ecotoxicology. Integrated Environmental Assessment and Management.
\url{ https://doi.org/10.1002/ieam.4809}
}

\note{
A good interactive tutorial explaining the NSEC can be found here
\url{https://z7izxm-david-fox.shinyapps.io/nsec-web/#section-the-nsec-explained}
}

\seealso{
\code{\link[=drm]{drc::drm}}
%% ~~objects to See Also as \code{\link{help}}, ~~~

}

\author{Prof. David Fox and Dr. Rebecca Fisher}


\examples{
\dontrun{
#  load drc package
library(drc)
# fit nominal model - three-parameter log-logistic function
fit<-drm(response/n ~ concentration, weights = n, data = dat,fct = LL.3(),
    type = "binomial")
# examine the model fit
plot(fit)
# print summary of fitted model
fit
# use default options to obtain 5 percent NSEC from the fitted log-logistic model
nsec(fit)
# obtain 1 percent NSEC where the 'control' concentration is 10.
nsec(fit,control.conc=10,sig=0.01)
# obtain model-averaged NSEC based on fitted model plus additional 4 models.
nsec(fit,mods=list(LL.4(), LN.4(), W1.4(),W1.3()))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

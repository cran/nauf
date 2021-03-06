% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nauf-pmmeans-class-generic.R
\name{nauf.pmm.list}
\alias{nauf.pmm.list}
\title{List of predicted marginal means objects for \code{nauf} models.}
\description{
The \code{\link{nauf_pmmeans}} function returns an object of class
\code{nauf.pmm.list}.
}
\details{
The \code{nauf.pmm.list} object contains a first element \code{pmmeans}
which contains the predicted marginal means, and possibly additional
\code{contrasts} elements depending on whether the call to 
\code{\link{nauf_pmmeans}} indicated that pairwise comparisons should be made.  
If the \code{by} argument was specified, there will be a numbered 
\code{contrasts} element for each level of the full interaction of the 
factors listed in the \code{by} argument.  The object also has a \code{specs}
attribute which contains information about the variables and subsets from the 
call to the function.

The object has \code{summary} and \code{print} methods which first print 
information from the \code{specs} attribute, and then pass addtional function
arguments along to each element in the list.  For frequentist regressions, 
the elements in the \code{nauf.pmm.list} are 
\code{\link[lsmeans]{lsmobj-class}} objects. In this case, the \code{summary} 
and \code{print} methods for the \code{nauf.pmm.list} take arguments listed in 
\code{\link[lsmeans]{summary.ref.grid}} (e.g. \code{infer}, \code{type}, 
\code{adjust}, etc.). For Bayesian regressions, the elements in the 
\code{nauf.pmm.list} are \code{\link{nauf.pmm.stan}} objects. In this case, 
the \code{summary} and \code{print} methods for the \code{nauf.pmm.list} take 
arguments which are listed in \code{\link{nauf.pmm.stan}}.

For posterior marginal means from a Bayesian \code{nauf} model, there is
a method for \code{\link[=as.shinystan,nauf.pmm.list-method]{as.shinystan}} 
which allows \code{\link[shinystan]{launch_shinystan}} to be used.
}


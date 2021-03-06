% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nauf_trms_fr_x_z.R
\name{nauf_contrasts}
\alias{nauf_contrasts}
\title{Not applicable unordered factor contrasts.}
\usage{
nauf_contrasts(object, inc_ordered = FALSE)
}
\arguments{
\item{object}{A \code{\link{nauf.terms}} object, a model frame made with
\code{\link{nauf_model.frame}}, a \code{nauf.glm} model (see
\code{\link{nauf_glm}}), or a \code{\linkS4class{nauf.lmerMod}} or
\code{\linkS4class{nauf.glmerMod}} model.}

\item{inc_ordered}{A logical indicating whether or not ordered factor
contrasts should also be returned (default \code{FALSE}).}
}
\value{
A named list of contrasts for all unordered factors in \code{object},
  and also optionally contrasts for ordered factors in \code{object}.  See
  'Details'.
}
\description{
The \code{nauf_contrasts} function returns a list of contrasts applied to
factors in an object created using a function in the \code{nauf} package.
See 'Details'.
}
\details{
In the \code{nauf} package, \code{NA} values are used to encode when an
unordered factor is truly \emph{not applicable}.  This is different than
"not available" or "missing at random".  The concept applies only to
unordered factors, and indicates that the factor is simply not meaningful
for an observation, or that while the observation may technically be
definable by one of the factor levels, the interpretation of its belonging to
that level isn't the same as for other observations.  For imbalanced
observational data, coding unordered factors as \code{NA} may also be used to
control for a factor that is only contrastive within a subset of the data due
to the sampling scheme.  To understand the output of the
\code{nauf_contrasts} function, the treatment of unordered factor contrasts
in the \code{nauf} package will first be discussed, using the
\code{\link{plosives}} dataset included in the package as an example.

In the \code{\link{plosives}} dataset, the factor \code{ling} is coded as
either \code{Monolingual}, indicating the observation is from a monolingual
speaker of Spanish, or \code{Bilingual}, indicating the observation is from a
Spanish-Quechua bilingual speaker.  The \code{dialect} factor indicates the
city the speaker is from (one of \code{Cuzco}, \code{Lima}, or
\code{Valladolid}).  The Cuzco dialect has both monolingual and bilingual
speakers, but the Lima and Valladolid dialects have only monolingual
speakers.  In the case of Valladolid, the dialect is not in contact with
Quechua, and so being monolingual in Valladolid does not mean the same
thing as it does in Cuzco, where it indicates
\emph{monolingual as opposed to bilingual}.  Lima has Spanish-Quechua
bilingual speakers, but the research questions the dataset serves to answer
are specific to monolingual speakers of Spanish in Lima.  If we leave the
\code{ling} factor coded as is in the dataset and use
\code{\link[standardize]{named_contr_sum}} to create the contrasts, we obtain
the following:

\tabular{llrrr}{
dialect    \tab ling        \tab dialectCuzco \tab dialectLima \tab lingBilingual \cr
Cuzco      \tab Bilingual   \tab            1 \tab           0 \tab             1 \cr
Cuzco      \tab Monolingual \tab            1 \tab           0 \tab            -1 \cr
Lima       \tab Monolingual \tab            0 \tab           1 \tab            -1 \cr
Valladolid \tab Monolingual \tab           -1 \tab          -1 \tab            -1
}

With these contrasts, the regression coefficient \code{dialectLima} would not
represent the difference between the intercept and the mean of the Lima
dialect; the mean of the Lima dialect would be the
\code{(Intercept) + dialectLima - lingBilingual}.  The interpretation of the
\code{lingBilingual} coefficient is similarly affected, and the intercept
term averages over the predicted value for the non-existent groups of Lima
bilingual speakers and Valladolid bilingual speakers, losing the
interpretation as the corrected mean (insofar as there can be a corrected
mean in this type of imbalanced data).  With the \code{nauf} package, we can
instead code non-Cuzco speakers' observations as \code{NA} for the
\code{ling} factor (i.e. execute
\code{plosives$ling[plosives$dialect != "Cuzco"] <- NA}).  These \code{NA}
values are allowed to pass into the regression's model matrix, and are then
set to \code{0}, effectively creating the following contrasts:

\tabular{llrrr}{
dialect    \tab ling        \tab dialectCuzco \tab dialectLima \tab lingBilingual \cr
Cuzco      \tab Bilingual   \tab            1 \tab           0 \tab             1 \cr
Cuzco      \tab Monolingual \tab            1 \tab           0 \tab            -1 \cr
Lima       \tab NA          \tab            0 \tab           1 \tab             0 \cr
Valladolid \tab NA          \tab           -1 \tab          -1 \tab             0
}

Because sum contrasts are used, a value of \code{0} for a dummy variable
averages over the effect of the factor, and the coefficient
\code{lingBilingual} only affects the predicted value for observations where
\code{dialect = Cuzco}.  In a regression fit with these contrasts, the
coefficient \code{dialectLima} represents what it should, namely the
difference between the intercept and the mean of the Lima dialect, and the
intercept is again the corrected mean.  The \code{lingBilingual} coefficient
is now the difference between Cuzco bilingual speakers and the corrected mean
\emph{of the Cuzco dialect}, which is \code{(Intercept) + dialectCuzco}.
These \code{nauf} contrasts thus allow us to model all of the data in a
single model without sacrificing the interpretability of the results. In
sociolinguistics, this method is called \emph{slashing} due to the use of a
forward slash in GoldVarb to indicate that a factor is not applicable.

This same methodology can be applied to other parts of the
\code{\link{plosives}} dataset where a factor's interpretation is the same
for all observations, but is only contrastive within a subset of the data due
to the sampling scheme.  The \code{age} and \code{ed} factors (speaker age
group and education level, respectively) are factors which can apply to
speakers regardless of their dialect, but in the dataset they are only
contrastive within the Cuzco dialect; all the Lima and Valladolid speakers
are 40 years old or younger with a university education (in the case of
Valladolid, the data come from an already-existing corpus; and in the case of
Lima, the data were collected as part of the same dataset as the Cuzco data,
but as a smaller control group).  These factors can be treated just as the
\code{ling} factor by setting them to \code{NA} for observations from Lima
and Valladolid speakers.  Similarly, there is no read speech data for the
Valladolid speakers, and so \code{spont} could be coded as \code{NA} for
observations from Valladolid speakers.

Using \code{NA} values can also allow the inclusion of a random effects
structure which only applies to a subset of the data.  The
\code{\link{plosives}} dataset has data from both read (\code{spont = FALSE};
only Cuzco and Lima) and spontaneous (\code{spont = TRUE}; all three
dialects) speech.  For the read speech, there are exactly repeated measures
on 54 items, as indicated by the \code{item} factor.  For the
spontaneous speech, there are not exactly repeated measures, and so in this
subset, \code{item} is coded as \code{NA}.  In a regression fit using
\code{nauf_lmer}, \code{nauf_glmer}, or \code{nauf_glmer.nb} with \code{item}
as a grouping factor, the random effects model matrix is created for the read
speech just as it normally is, and for spontaneous speech observations all of
the columns are set to \code{0} so that the \code{item} effects only affect
the fitted values for read speech observations.  In this way, the noise
introduced by the read speech items can be accounted for while still
including all of the data in one model, and the same random effects for
\code{speaker} can apply to all observations (both read and spontaneous),
which will lead to a more accurate estimation of the fixed, speaker, and item
effects since more information is available than if the read and spontaneous
speech were analyzed in separate models.

There are two situations in which unordered factors will need more than one set
of contrasts: (1) when an unordered factor with \code{NA} values interacts
with another unordered factor, and some levels are collinear with \code{NA};
and (2) when an unordered factor is included as a slope for a random effects
grouping factor that has \code{NA} values, but only a subset of the levels
for the slope factor occur when the grouping factor is not \code{NA}.  As an
example of an interaction requiring new contrasts, consider the interaction
\code{dialect * spont} (that is, suppose we are interested in whether the
effect of \code{spont} is different for Cuzco and Lima).  We code
\code{spont} as \code{NA} when \code{dialect = Valladolid}, as mentioned
above.  This gives the following contrasts for the main effects:

\tabular{llrrr}{
dialect    \tab spont \tab dialectCuzco \tab dialectLima \tab spontTRUE \cr
Cuzco      \tab TRUE  \tab            1 \tab           0 \tab         1 \cr
Cuzco      \tab FALSE \tab            1 \tab           0 \tab        -1 \cr
Lima       \tab TRUE  \tab            0 \tab           1 \tab         1 \cr
Lima       \tab FALSE \tab            0 \tab           1 \tab        -1 \cr
Valladolid \tab NA    \tab           -1 \tab          -1 \tab         0
}

If we simply multiply these \code{dialect} and \code{spont} main effect
contrasts together to obtain the contrasts for the interaction (which is what
is done in the default \code{\link[stats]{model.matrix}} method), we get
following contrasts:

\tabular{llrr}{
dialect    \tab spont \tab dialectCuzco:spontTRUE \tab dialectLima:spontTRUE \cr
Cuzco      \tab TRUE  \tab                      1 \tab                     0 \cr
Cuzco      \tab FALSE \tab                     -1 \tab                     0 \cr
Lima       \tab TRUE  \tab                      0 \tab                     1 \cr
Lima       \tab FALSE \tab                      0 \tab                    -1 \cr
Valladolid \tab NA    \tab                      0 \tab                     0
}

However, these contrasts introduce an unnecessary parameter to the model
which causes collinearity with the main effects since
\code{spontTRUE = dialectCuzco:spontTRUE + dialectLima:spontTRUE} in all
cases.  The functions in the \code{nauf} package automatically recognize when
this occurs, and create a second set of contrasts for \code{dialect} in which
the \code{Valladolid} level is treated as if it were \code{NA} (through and
additional call to \code{\link[standardize]{named_contr_sum}}):

\tabular{lr}{
dialect    \tab dialect.c2.Cuzco \cr
Cuzco      \tab                1 \cr
Lima       \tab               -1 \cr
Valladolid \tab                0
}

This second set of \code{dialect} contrasts is only used when it needs to be.
That is, in this case, these contrasts would be used in the creation of the
model matrix columns for the interaction term \code{dialect:spont} term,
but not in the creation of the model matrix columns for the main effect terms
\code{dialect} and \code{spont}, and when the second set of contrasts is
used, \code{.c2.} will appear between the name of the factor and the level so
it can be easily identified:

\tabular{llrrrr}{
dialect    \tab spont \tab dialectCuzco \tab dialectLima \tab spontTRUE \tab dialect.c2.Cuzco:spontTRUE \cr
Cuzco      \tab TRUE  \tab            1 \tab           0 \tab         1 \tab                          1 \cr
Cuzco      \tab FALSE \tab            1 \tab           0 \tab        -1 \tab                         -1 \cr
Lima       \tab TRUE  \tab            0 \tab           1 \tab         1 \tab                         -1 \cr
Lima       \tab FALSE \tab            0 \tab           1 \tab        -1 \tab                          1 \cr
Valladolid \tab NA    \tab           -1 \tab          -1 \tab         0 \tab                          0
}

Turning now to an example of when a random slope requires new contrasts,
consider a random \code{item} slope for \code{dialect}.  Because
\code{dialect = Valladolid} only when \code{item} is \code{NA}, using the
main effect contrasts for \code{dialect} for the \code{item} slope would
result in collinearity with the \code{item} intercept in the random effects
model matrix:

\tabular{llrrr}{
dialect    \tab item \tab i01:(Intercept) \tab i01:dialectCuzco \tab i01:dialectLima \cr
Cuzco      \tab i01  \tab               1 \tab                1 \tab               0 \cr
Cuzco      \tab i02  \tab               0 \tab                0 \tab               0 \cr
Cuzco      \tab NA   \tab               0 \tab                0 \tab               0 \cr
Lima       \tab i01  \tab               1 \tab                0 \tab               1 \cr
Lima       \tab i02  \tab               0 \tab                0 \tab               0 \cr
Lima       \tab NA   \tab               0 \tab                0 \tab               0 \cr
Valladolid \tab NA   \tab               0 \tab                0 \tab               0
}

This table shows the random effects model matrix for \code{item i01} for all
possible scenarios, with the rows corresponding to (in order): a Cuzco
speaker producing the read speech plosive in \code{item i01}, a Cuzco speaker
producing a read speech plosive in another \code{item}, a Cuzco speaker
producing a spontaneous speech plosive, a Lima speaker producing the read
speech plosive in \code{item i01}, a Lima speaker producing a read speech
plosive in another \code{item}, a Lima speaker producing a spontaneous speech
plosive, and a Valladolid speaker producing a spontaneous speech plosive.
With the main effect contrasts for \code{dialect},
\code{i01:(Intercept) = i01:dialectCuzco + i01:dialectLima} in all cases,
causing collinearity.  Because this collinearity exists for all read speech
item random effects model matrices, the model is unidentifiable.  The
functions in the \code{nauf} package automatically detect that this is the
case, and remedy the situation by creating a new set of contrasts used for
the \code{item} slope for \code{dialect}:

\tabular{llrr}{
dialect    \tab item \tab i01:(Intercept) \tab i01:dialect.c2.Cuzco \cr
Cuzco      \tab i01  \tab               1 \tab                    1 \cr
Cuzco      \tab i02  \tab               0 \tab                    0 \cr
Cuzco      \tab NA   \tab               0 \tab                    0 \cr
Lima       \tab i01  \tab               1 \tab                   -1 \cr
Lima       \tab i02  \tab               0 \tab                    0 \cr
Lima       \tab NA   \tab               0 \tab                    0 \cr
Valladolid \tab NA   \tab               0 \tab                    0
}

If we were to, say, fit the model
\code{intdiff ~ dialect * spont + (1 + dialect | item)}, then \code{nauf} would
additionally recognize that the same set of altered contrasts for
\code{dialect} are required in the fixed effects interaction term
\code{dialect:spont} and the \code{item} slope for \code{dialect}, and both
would be labeled with \code{.c2.}.  In other (rare) cases, more than two sets
of contrasts may be required for a factor, in which case they would have
\code{.c3.}, \code{.c4.} and so on.

In this way, users only need to code unordered factors as \code{NA} in the
subsets of the data where they are not contrastive, and \code{nauf} handles
the rest.  Having described in detail what \code{nauf} contrasts are, we now
return to the \code{nauf_contrasts} function.  The function can be used on
objects of any \code{nauf} model, a \code{\link{nauf.terms}} object, or a
model frame made by \code{\link{nauf_model.frame}}. It returns a named list
with a matrix for each
unordered factor in \code{object} which contains all contrasts associated the
factor.  For the model \code{intdiff ~ dialect * spont + (1 + dialect | item)},
the result would be a list with elements \code{dialect} and \code{spont} that
contain the following matrices (see the 'Examples' section for code to
generate this list):

\tabular{lrrr}{
dialect    \tab Cuzco \tab Lima \tab .c2.Cuzco \cr
Cuzco      \tab     1 \tab    0 \tab         1 \cr
Lima       \tab     0 \tab    1 \tab        -1 \cr
Valladolid \tab    -1 \tab   -1 \tab         0
}

\tabular{lr}{
spont \tab TRUE \cr
TRUE  \tab    1 \cr
FALSE \tab   -1 \cr
NA    \tab    0
}

The default is for the list of contrasts to only contain information about
unordered factors.  If \code{inc_ordered = TRUE}, then the contrast matrices
for any ordered factors in \code{object} are also included.
}
\section{Note}{
 The argument \code{ncs_scale} changes what value is used for
  the sum contrast deviations.  The default value of \code{1} would give the
  contrast matrices in 'Details'.  A value of \code{ncs_scale = 0.5}, for example,
  would result in replacing \code{1} with \code{0.5} and \code{-1} with
  \code{-0.5} in all of the contrast matrices.
}
\examples{
dat <- plosives
dat$spont[dat$dialect == "Valladolid"] <- NA

mf <- nauf_model.frame(intdiff ~ dialect * spont + (1 + dialect | item), dat)
nauf_contrasts(mf)

mf <- nauf_model.frame(intdiff ~ dialect * spont + (1 + dialect | item),
  dat, ncs_scale = 0.5)
nauf_contrasts(mf)

}
\seealso{
\code{\link{nauf_model.frame}}, \code{\link{nauf_model.matrix}},
  \code{\link{nauf_glFormula}}, \code{\link{nauf_glm}}, and
  \code{\link{nauf_glmer}}.
}


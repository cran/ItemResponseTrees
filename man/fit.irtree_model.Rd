% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{fit.irtree_model}
\alias{fit.irtree_model}
\title{Fit an ItemResponseTrees model}
\usage{
\method{fit}{irtree_model}(
  object = NULL,
  data = NULL,
  engine = c("mirt", "mplus", "tam"),
  ...,
  link = c("logit", "probit"),
  verbose = interactive(),
  control = NULL,
  improper_okay = FALSE
)
}
\arguments{
\item{object}{Object of class \code{irtree_model}. See \link{irtree_model} for more
information.}

\item{data}{Data frame containing containing one row per respondent and one
column per variable. The variable names must correspond to those used in
\code{object}.}

\item{engine}{String specifying whether to use mirt, Mplus, or TAM for
estimation.}

\item{...}{Not currently used. Use \code{control} instead.}

\item{link}{String specifying the link function. May be either logit, or (in
case of Mplus), probit.}

\item{verbose}{Logical indicating whether output should be printed to the
console.}

\item{control}{List. The allowed elements of this list depend on the
\code{engine}. Use \code{\link[=control_mirt]{control_mirt()}}, \code{\link[=control_mplus]{control_mplus()}}, or \code{\link[=control_tam]{control_tam()}} for
convenience. Note that the \code{fit()} function does not use \code{...}, but that
you can use the \verb{control_*()} functions to pass additional arguments.}

\item{improper_okay}{Logical indicating whether the model should also be fit
if it is not a proper IR-tree model. Set this only to \code{TRUE} if you really
know what you are doing.}
}
\value{
Returns a list of class \code{irtree_fit}. The first list element is the
return value of either \code{\link[mirt:mirt]{mirt::mirt()}}, \code{\link[MplusAutomation:readModels]{MplusAutomation::readModels()}}, or
\code{\link[TAM:tam.mml]{TAM::tam.mml()}}. Further information is provided in the element
\code{spec}.
}
\description{
This function takes a \code{data} frame and an \code{object} of class \link{irtree_model}
and runs the model in either mirt, Mplus, or TAM.
}
\section{Methods}{
 The methods \code{coef()}, \code{summary()}, and \code{print()} are
implemented for objects of class \code{irtree_fit}, and those wrap the
respective functions of \link[mirt:mirt-package]{mirt},
\link[MplusAutomation:MplusAutomation-package]{MplusAutomation}, or
\link[TAM:TAM-package]{TAM}. However, \code{\link[=glance.irtree_fit]{glance()}},
\code{\link[=tidy.irtree_fit]{tidy()}}, and \code{\link[=augment.irtree_fit]{augment()}} may be
more helpful.
}

\examples{
\donttest{
# Running these examples may take a while

data("jackson")
df1 <- jackson[1:456, paste0("C", 1:5)]
df2 <- jackson[1:456, c(paste0("C", 1:5), paste0("E", 1:5))]

irtree_create_template(df1)

# Graded Response Model ---------------------------------------------------

m1 <- "
IRT:
t BY C1@1, C2@1, C3@1, C4@1, C5@1;

Class:
GRM
"

model1 <- irtree_model(m1)

fit1 <- fit(model1, data = df1)

glance(fit1)
tidy(fit1, par_type = "difficulty")
augment(fit1)

# IR-Tree Models ----------------------------------------------------------

##### IR-tree model for 1 target trait #####

m2 <- "
Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

IRT:
t BY C1@1, C2@1, C3@1, C4@1, C5@1;
e BY C1@1, C2@1, C3@1, C4@1, C5@1;
m BY C1@1, C2@1, C3@1, C4@1, C5@1;

Class:
Tree
"

model2 <- irtree_model(m2)

# See ?mirt::mirt for details on method argument
fit2 <- fit(model2, data = df1, control = control_mirt(method = "MHRM"))

##### IR-tree model for 2 target traits #####

m3 <- "
Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

IRT:
t1 BY C1@1, C2@1, C3@1, C4@1, C5@1;
t2 BY E1@1, E2@1, E3@1, E4@1, E5@1;
e  BY C1@1, C2@1, C3@1, C4@1, C5@1, E1@1, E2@1, E3@1, E4@1, E5@1;
m  BY C1@1, C2@1, C3@1, C4@1, C5@1, E1@1, E2@1, E3@1, E4@1, E5@1;

Class:
Tree

Constraints:
t = t1 | t2
"

model3 <- irtree_model(m3)

fit3 <- fit(model3, data = df2, control = control_mirt(method = "MHRM"))

##### IR-tree model constrained to Steps Model #####

m4 <- "
Equations:
1 = (1-a1)
2 = a1*(1-a2)
3 = a1*a2*(1-a3)
4 = a1*a2*a3*(1-a4)
5 = a1*a2*a3*a4

IRT:
a1 BY C1@1, C2@1, C3@1, C4@1, C5@1;
a2 BY C1@1, C2@1, C3@1, C4@1, C5@1;
a3 BY C1@1, C2@1, C3@1, C4@1, C5@1;
a4 BY C1@1, C2@1, C3@1, C4@1, C5@1;

Class:
Tree

Constraints:
a1 = a2
a1 = a3
a1 = a4
"

model4 <- irtree_model(m4)

fit4 <- fit(model4, data = df1)

# Partial Credit Model ----------------------------------------------------

##### Ordinary PCM #####

m5 <- "
IRT:
t BY C1@1, C2@1, C3@1, C4@1, C5@1;

Weights:
t = c(0, 1, 2, 3, 4)

Class:
PCM
"

model5 <- irtree_model(m5)

fit5 <- fit(model5, data = df1 - 1, engine = "tam")

##### Multidimensional PCM with constraints #####

m6 <- "
IRT:
t1 BY C1@1, C2@1, C3@1, C4@1, C5@1;
t2 BY E1@1, E2@1, E3@1, E4@1, E5@1;
e  BY C1@1, C2@1, C3@1, C4@1, C5@1, E1@1, E2@1, E3@1, E4@1, E5@1;
m  BY C1@1, C2@1, C3@1, C4@1, C5@1, E1@1, E2@1, E3@1, E4@1, E5@1;

Weights:
t = c(0, 1, 2, 3, 4)
e = c(1, 0, 0, 0, 1)
m = c(0, 0, 1, 0, 0)

Class:
PCM

Constraints:
t = t1 | t2
"

model6 <- irtree_model(m6)

fit6 <- fit(model6, data = df2 - 1, engine = "tam",
            control = control_tam(control = list(snodes = 1234)))
}
}

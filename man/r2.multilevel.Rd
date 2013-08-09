\name{r2.multilevel}
\alias{r2.multilevel}
\title{Calculate marginal and conditional R squared (R2) for a multilevel/hierarchical model following Nakagawa & Schielzeth (2013)}
\usage{
  r2.multilevel(distrib, fixed.params, predictors,
    random.vars, resid.var, intercept)
}
\arguments{
  \item{distrib}{Distribution type as character (either
  "normal", "logit", "logitprop", poisson"). Choose
  "logitprop" if analysing proportions rather than binary
  responses. "poisson" assumes log link.}

  \item{fixed.params}{A numeric vector giving the mean
  estimate for all fixed effect parameters. It is critical
  to keep the same order as the order of predictors in
  \code{predictors}.}

  \item{predictors}{Matrix with predictor values (only
  fixed effects!). Columns order must match the order of
  parameters in \code{fixed.params}}

  \item{random.vars}{A numeric vector with the random
  effects variances.}

  \item{resid.var}{Residual variance (0 when distrib ==
  "logit").}

  \item{intercept}{Estimate for the intercept of the model.
  Only required for distrib == "poisson".}
}
\value{
  A numeric matrix with two values: the marginal and
  conditional r-squared
}
\description{
  This function calculates the margil (fixed effects only)
  and conditional (including both fixed and random effects)
  R-squared of a multilevel/hierarchical model following
  Nakagawa & Schielzeth (2013) (see \code{References}).
  Unlike \code{\link{r.squaredGLMM}} in package
  \code{MuMIn}, this function can be applied to the output
  of Bayesian hierarchical models as fitted e.g. in JAGS or
  STAN. Parameter estimates (variances) need then to be
  provided instead of being extracted directly from the
  model.
}
\examples{
library(arm)


####################################################
# Data generation (Taken from Nakagawa & Schielzeth (2013))
###################################################

# 1. Design matrices
#---------------------------------------------------

# 12 different populations n = 960
Population <- gl(12, 80, 960)

# 120 containers (8 individuals in each container)
Container <- gl(120, 8, 960)

# Sex of the individuals. Uni-sex within each container (individuals are sorted at the pupa stage)
Sex <- factor(rep(rep(c("Female", "Male"), each = 8), 60))

# Habitat at the collection site: dry or wet soil (four indiviudal from each Habitat in each container)
Habitat <- factor(rep(rep(c("dry", "wet"), each = 4), 120))

# Food treatment at the larval stage: special food ('Exp') or standard food ('Cont')
Treatment <- factor(rep(c("Cont", "Exp"), 480))

# Data combined in a dataframe
Data <- data.frame(Population = Population, Container = Container, Sex = Sex, Habitat = Habitat, Treatment = Treatment)


# 2. Gaussian response: body length (both sexes)
#---------------------------------------------------

# simulation of the underlying random effects (Population and Container with variance of 1.3 and 0.3, respectively)
PopulationE <- rnorm(12, 0, sqrt(1.3))
ContainerE <- rnorm(120, 0, sqrt(0.3))

# data generation based on fixed effects, random effects and random residuals errors
Data$BodyL <- 15 - 3 * (as.numeric(Sex) - 1) + 0.4 * (as.numeric(Treatment) - 1) + 0.15 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] +
  rnorm(960, 0, sqrt(1.2))


  # 3. Binomial response: colour morph (males only)
  #---------------------------------------------------

  # Subset the design matrix (only males express colour morphs)
  DataM <- subset(Data, Sex == "Male")

  # simulation of the underlying random effects (Population and Container with variance of 1.2 and 0.2, respectively)
  PopulationE <- rnorm(12, 0, sqrt(1.2))
  ContainerE <- rnorm(120, 0, sqrt(0.2))

  # generation of response values on link scale (!) based on fixed effects and random effects
  ColourLink <- with(DataM, 0.8 * (-1) + 0.8 * (as.numeric(Treatment) - 1) + 0.5 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])

  # data generation (on data scale!) based on negative binomial distribution
  DataM$Colour <- rbinom(length(ColourLink), 1, invlogit(ColourLink))


  # 4. Poisson response: fecundity (females only)
  #---------------------------------------------------

  # Subset the design matrix (only females express colour morphs)
  DataF <- Data[Data$Sex == "Female", ]

  # random effects
  PopulationE <- rnorm(12, 0, sqrt(0.4))
  ContainerE <- rnorm(120, 0, sqrt(0.05))

  # generation of response values on link scale (!) based on fixed effects, random effects and residual errors
  EggLink <- with(DataF, 1.1 + 0.5 * (as.numeric(Treatment) - 1) + 0.1 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] + rnorm(480, 0, sqrt(0.1)))

  # data generation (on data scale!) based on Poisson distribution
  DataF$Egg <- rpois(length(EggLink), exp(EggLink))



############################################
# RUNNING MODELS
############################################

## BINOMIAL EXAMPLE (logit)

Data <- DataM

model <- lmer(Colour ~ Treatment + Habitat + (1 | Population) + (1 | Container), family = "binomial", data = Data)

fixed.p <- fixef(model)[-1]   # intercept removed
random.p <- c(VarCorr(model)$Container[1], VarCorr(model)$Population[1])

r2 <- r2.multilevel(distrib="logit", fixed.params=fixed.p, predictors=cbind(Data$Treatment, Data$Habitat), random.vars=random.p)
}
\author{
  Paco, based on code by Nakagawa & Schielzeth (2013) (see
  \code{references}).
}
\references{
  Nakagawa, S, Schielzeth, H. (2012). A general and simple
  method for obtaining R2 from Generalized Linear
  Mixed-effects Models. Methods in Ecology and Evolution:
  (online) doi:10.1111/j.2041-210x.2012.00261.x
}
\seealso{
  \code{\link{r2.gelman}} in this same package for Gelman &
  Pardoe's (2006) approach.

  \code{\link{r.squaredGLMM}} in \code{MuMIn} package and
  \link{http://jslefche.wordpress.com/2013/03/13/r2-for-linear-mixed-effects-models/}
}


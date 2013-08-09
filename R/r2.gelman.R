<<<<<<< HEAD
#' TODO:
#' examples
#'
#'
#'
=======
>>>>>>> 80c14f0033ce77e6358ba2ed83bdc7321eca62f6
#' Calculate R squared (R2) for a multilevel/hierarchical model following Gelman & Pardoe (2006)
#' 
#' This function calculates the R-squared of a multilevel/hierarchical model following Gelman & Pardoe (2006) (see \code{References}). 
#' 
#' 
#' @export
<<<<<<< HEAD
#' @return A numeric value: the r-squared
=======
#' @return A numeric vector with two values: the marginal and conditional r-squared
>>>>>>> 80c14f0033ce77e6358ba2ed83bdc7321eca62f6
#' @param residuals A numeric matrix containing the residual values obtained through MCMC.
#' @param data.level Logical. If TRUE, calculates the R2 at the data level (level 1) and the denominator is the variance of the data
#' instead of variance of the parameters/linear predictors (see function definition for more details).
#' @param responsevar A numeric vector. The response variable used to fit the model. Only required for data-level calculations.
#' @param param.sims A numeric matrix containing the posterior samples for the random effects parameters.
#' @author Paco, based on code by Gelman & Hill (2007) (see \code{references}).
#' @references Gelman & Pardoe (2006) Technometrics.
#' Gelman & Hill (2007) book
#' @seealso \code{\link{r2.multilevel}} in this same package.
#' @seealso \code{\link{r.squaredGLMM}} in \code{MuMIn} package and \link{http://jslefche.wordpress.com/2013/03/13/r2-for-linear-mixed-effects-models/}
#' 
<<<<<<< HEAD
=======
#' @examples
#' 
#'  
#' ####################################################
#' Data generation (Taken from Nakagawa & Schielzeth (2013))
#' ###################################################
#' 
#' # 1. Design matrices 
#' #---------------------------------------------------
#'
#' # 12 different populations n = 960
#' Population <- gl(12, 80, 960)
#' 
#' # 120 containers (8 individuals in each container)
#' Container <- gl(120, 8, 960)
#' 
#' # Sex of the individuals. Uni-sex within each container (individuals are sorted at the pupa stage)
#' Sex <- factor(rep(rep(c("Female", "Male"), each = 8), 60))
#' 
#' # Habitat at the collection site: dry or wet soil (four indiviudal from each Habitat in each container)
#' Habitat <- factor(rep(rep(c("dry", "wet"), each = 4), 120))
#' 
#' # Food treatment at the larval stage: special food ('Exp') or standard food ('Cont')
#' Treatment <- factor(rep(c("Cont", "Exp"), 480))
#' 
#' # Data combined in a dataframe
#' Data <- data.frame(Population = Population, Container = Container, Sex = Sex, Habitat = Habitat, Treatment = Treatment)
#' 
#' 
#' # 2. Gaussian response: body length (both sexes)
#' #---------------------------------------------------
#' 
#' # simulation of the underlying random effects (Population and Container with variance of 1.3 and 0.3, respectively)
#' PopulationE <- rnorm(12, 0, sqrt(1.3))
#' ContainerE <- rnorm(120, 0, sqrt(0.3))
#' 
#' # data generation based on fixed effects, random effects and random residuals errors
#' Data$BodyL <- 15 - 3 * (as.numeric(Sex) - 1) + 0.4 * (as.numeric(Treatment) - 1) + 0.15 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] + 
#'   rnorm(960, 0, sqrt(1.2))
#'   
#'   
#'   # 3. Binomial response: colour morph (males only)
#'   #---------------------------------------------------
#'   
#'   # Subset the design matrix (only males express colour morphs)
#'   DataM <- subset(Data, Sex == "Male")
#'   
#'   # simulation of the underlying random effects (Population and Container with variance of 1.2 and 0.2, respectively)
#'   PopulationE <- rnorm(12, 0, sqrt(1.2))
#'   ContainerE <- rnorm(120, 0, sqrt(0.2))
#'   
#'   # generation of response values on link scale (!) based on fixed effects and random effects
#'   ColourLink <- with(DataM, 0.8 * (-1) + 0.8 * (as.numeric(Treatment) - 1) + 0.5 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container])
#'   
#'   # data generation (on data scale!) based on negative binomial distribution
#'   DataM$Colour <- rbinom(length(ColourLink), 1, invlogit(ColourLink))
#'   
#'   
#'   # 4. Poisson response: fecundity (females only)
#'   #---------------------------------------------------
#'   
#'   # Subset the design matrix (only females express colour morphs)
#'   DataF <- Data[Data$Sex == "Female", ]
#'   
#'   # random effects
#'   PopulationE <- rnorm(12, 0, sqrt(0.4))
#'   ContainerE <- rnorm(120, 0, sqrt(0.05))
#'   
#'   # generation of response values on link scale (!) based on fixed effects, random effects and residual errors
#'   EggLink <- with(DataF, 1.1 + 0.5 * (as.numeric(Treatment) - 1) + 0.1 * (as.numeric(Habitat) - 1) + PopulationE[Population] + ContainerE[Container] + rnorm(480, 0, sqrt(0.1)))
#'   
#'   # data generation (on data scale!) based on Poisson distribution
#'   DataF$Egg <- rpois(length(EggLink), exp(EggLink))
#'   
#'   
#'   
#' ############################################
#' RUNNING MODELS
#' ############################################
#' 
#' ## BINOMIAL EXAMPLE (logit)
#' 
#' Data <- DataM
#' 
#' model <- lmer(Colour ~ Treatment + Habitat + (1 | Population) + (1 | Container), family = "binomial", data = Data)
#'   
#' fixed.p <- fixef(model)[-1]   # intercept removed
#' random.p <- c(VarCorr(model)$Container[1], VarCorr(model)$Population[1])
#' 
#' r2 <- r2.multilevel(distrib="logit", fixed.params=fixed.p, predictors=cbind(Data$Treatment, Data$Habitat),
#' random.vars=random.p)
#' 
#' # Compare with r2.gelman:
#'  
#' r2.gelm <- r2.gelman(residuals=resid(model), data.level=T, responsevar=Data$Colour)

>>>>>>> 80c14f0033ce77e6358ba2ed83bdc7321eca62f6



r2.gelman <- function(residuals, data.level, responsevar, param.sims){
  
  if (data.level == TRUE)
<<<<<<< HEAD
    r2 <- 1 - mean(apply(residuals, 1, var)) / var(responsevar)
=======
    r2 <- 1 - mean(apply(residuals, 1, var))/var(responsevar)
>>>>>>> 80c14f0033ce77e6358ba2ed83bdc7321eca62f6
  
  else
    r2 <- 1 - mean(apply(residuals, 1, var)) / mean(apply(param.sims, 1, var))
  
  return(r2)
  
}
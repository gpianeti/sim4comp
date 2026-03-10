library(MASS)
library(EnvStats)
library(checkmate)

##############   ###############################################################
### inputs ###   ###############################################################
##############   ###############################################################

# p
# number of predictors (integer)

# c
# number of different classes (integer)

# n
# total sample size (integer)

# distrib
# the desidered p marginal distributions (vector).
# to be chosen among the following available distributions: "Beta", "Chisq", 
# "Exp", "F", "Gamma", "Logistic", "Pareto", "Lognormal", "t", "Triangular", 
# "Uniform", "Weibull", "Normal", "Binomial". N.B. EVERY DISTRIBUTION IS 
# PARAMETRIZED AS SPECIFIED IN ITS OWN PACKAGE EXCEPT FOR t-DISTRIBUTION WHERE 
# THE ncp PARAMETER ONLY AFFECTS ITS MEAN (i.e. it's not embedded in the 
# function but simply added on the simulated values)

# param
# parameters of the distributions (list):
# it has to contain c lists (one for each class), 
# each one containing p vectors of parameters (one for each distrib).

# corr
# correlation matrices reporting the desidered correlation structure (list):
# it can be one (assuming omoskedasticity(?) between the classes) otherwise 
# c matrices must be specified, the default value is diag(1, p, p).

# priors 
# (vector), the default value is rep(c, 1/c).

#######################   ######################################################
### data generation ###   ######################################################
#######################   ######################################################

# We simulate c joint distributions of the p variables
gendata <- function(n, c, p, distrib, param, corr = NULL, priors = NULL) {
  
  ## priors check
  if (is.null(priors)) {
    priors <- rep(1/c, c)
  } else {
    assert_numeric(priors, len = c, any.missing = FALSE, lower = 0, finite = TRUE)
  }
  
  ## distrib check
  assert(
    check_character(distrib, len = 1),
    check_character(distrib, len = p),
    combine = "or",
    .var.name = "distrib"
  )
  assert_subset(distrib, choices = c("Beta", "Chisq", "Exp", "F",
                                     "Gamma", "Logistic", "Pareto", "Lognormal",
                                     "t", "Triangular", "Uniform", "Weibull", "Normal", "Binomial"))
  if (length(distrib) == 1) {
    distrib <- rep(distrib, p)
  }
  
  ## param check
  assert_list(param, len = c, types = "list", any.missing = FALSE)
  for (sublist in param) {
    assert_list(sublist, len = p, types = "numeric", any.missing = FALSE)
  }
  
  ## correlation check
  if (is.null(corr)) {
    corr <- replicate(c, diag(1, p, p), simplify = FALSE)
  }
  if (test_matrix(corr)) {
    corr <- list(corr)
  }
  if (length(corr) == 1 && c>1) {
    corr <- replicate(c, corr[[1]], simplify = FALSE)
  }
  assert_list(corr, len = c)
  for (i in 1:c) {
    mat <- corr[[i]]
    assert_matrix(mat, 
                  nrows = p, ncols = p, 
                  mode = "numeric",
                  any.missing = FALSE,
                  .var.name = paste0("corr[[", i, "]]"))
    if (!all(mat==t(mat))) {
      stop(sprintf("Matrix %d is not symmetric", i))
    }
    ev <- eigen(mat)$values
    if (!all(ev >= 0)) {
      stop(sprintf("Matrix %d is not Positive Semi-Definite", i))
    }
    if (!all(abs(diag(mat) - 1) == 0)) {
      stop(sprintf("Matrix %d is not a valid Correlation Matrix (diagonal is not 1)", i))
    }
  }
  
  # sample priors from a multinomial distribution
  n <- as.vector(rmultinom(1, n, priors))

  X <- c()
  unif <- c()
  # we work class by class
  for (k in 1:c) {
    varsk <- corr[[k]]
    nk <- n[k]
    margin <- mvrnorm(nk, mu = rep(0, p), Sigma = varsk)
    U <- pnorm(margin, 0, 1)
    Xaus <- c()
    # we simulate the p variable in every class
    for (j in 1:p) {
      
      # extract the parameters for the kth class
      parameters <- param[[k]][[j]]
      
      # and simulate
      if (distrib[j] == "Normal") {
        mu <- parameters[1]
        sd <- parameters[2]
        a <- qnorm(U[, j], mean = mu, sd = sd)
      } 
      
      else if (distrib[j] == "t") {
        df <- parameters[1]
        ncp <- ifelse(is.na(parameters[2]), 0, parameters[2])
        a <- qt(U[, j], df = df) + ncp
      } 
      
      else if (distrib[j] == "Beta") {
        shape1 <- parameters[1]
        shape2 <- parameters[2]
        ncp <- ifelse(is.na(parameters[3]), 0, parameters[3])
        a <- qbeta(U[, j], shape1, shape2, ncp)
      }
      
      else if (distrib[j] == "Gamma") {
        shape <- parameters[1]
        rate <- parameters[2]
        a <- qgamma(U[, j], shape, rate)
      }
      
      else if (distrib[j] == "Chisq") {
        df <- parameters[1]
        ncp <- ifelse(is.na(parameters[2]), 0, parameters[2])
        a <- qchisq(U[, j], df, ncp)
      }
      
      else if (distrib[j] == "Exp") {
        rate <- parameters[1]
        a <- qexp(U[, j], rate)
      }
      
      else if (distrib[j] == "F") {
        df1 <- parameters[1]
        df2 <- parameters[2]
        ncp <- ifelse(is.na(parameters[3]), 0, parameters[3])
        a <- qf(U[, j], df1, df2, ncp)
      }
      
      else if (distrib[j] == "Logistic") {
        location <- parameters[1]
        scale <- parameters[2]
        a <- qlogis(U[, j], location, scale)
      }
      
      else if (distrib[j] == "Lognormal") {
        logmu <- parameters[1]
        logsd <- parameters[2]
        a <- qlnorm(U[, j], logmu, logsd)
      }
      
      else if (distrib[j] == "Pareto") {
        location <- parameters[1]
        shape <- parameters[2]
        a <- qpareto(U[, j], location, shape)
      }
      
      else if (distrib[j] == "Triangular") {
        min <- parameters[1]
        max <- parameters[2]
        mode <- ifelse(is.na(parameters[3]), (max-min)/2, parameters[3])
        a <- qtri(U[, j], min, max, mode)
      }
      
      else if (distrib[j] == "Uniform") {
        min <- parameters[1]
        max <- parameters[2]
        a <- qunif(U[, j], min, max)
      }
      
      else if (distrib[j] == "Weibull") {
        shape <- parameters[1]
        scale <- parameters[2]
        a <- qweibull(U[, j], shape, scale)
      }
      
      else if (distrib[j] == "Binomial") {
        size <- parameters[1]
        prob <- parameters[2]
        a <- qbinom(U[, j], size, prob)
      }
      
      # store the results relative to the pth distribution
      Xaus <- cbind(Xaus, a)
    }
    
    # add the class of membership and store the results relative to the cth class
    Xaus <- cbind(Xaus, rep(k, nk))
    X <- rbind(X, Xaus)
  }
  X <- as.data.frame(X)
  names(X) <- c(paste0(distrib, as.character(1:p)), "Y")
  X$Y <- as.factor(X$Y)
  return(X)
}

################################################################################


### EXAMPLE for p = 2 ###
#n = 1000
#p = 2
#c = 3
#distrib = c("t", "Normal")
#var 1  #var 2               
#param <- list(list(c(4, 2),  c(7, 3)),      #k=1  
#              list(c(11), c(5.5, 0.5)),  #k=1  
#              list(c(32, 3), c(5, 4)))      #k=2

#corr <- list(matrix(c(1, 0.2, 0.2, 1), 2, 2))

# data generation
#set.seed(1)
#data <- gendata(n, c, p, distrib = distrib, param = param, corr = corr)


# plot and correlations
#name1 <- paste0(distrib, as.character(1:p))[1]
#name2 <- paste0(distrib, as.character(1:p))[2]
#ggplot(data = data, aes(x = .data[[name1]], y = .data[[name2]])) +
#  geom_point(aes(colour = Y))
#cor(data[data$Y == 1, 1], data[data$Y == 1, 2])
#cor(data[data$Y == 2, 1], data[data$Y == 2, 2])
#cor(data[data$Y == 3, 1], data[data$Y == 3, 2])
#cor(data[, 1], data[, 2])



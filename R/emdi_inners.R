# This script contains the checks of arguments that have be done for the
# ebp function.


# Function called in ebp
ebp_check1 <- function(fixed, pop_data, pop_domains, smp_data, smp_domains, L){



  if (is.null(fixed)  || !inherits(fixed, "formula")) {
    stop('Fixed must be a formula object. See also help(ebp).')
  }
  if (!is.data.frame(pop_data)) {
    stop('Pop_data must be a data frame containing population data.
         See also help(ebp).')
  }
  if (!is.character(pop_domains) || length(pop_domains) != 1) {
    stop('Pop_domains must be a vector of lenght 1 and of class character
         specifying the variable name of a numeric or factor variable
         indicating domains in the population data. See also help(ebp).')
  }
  if (!is.data.frame(smp_data)) {
    stop('Smp_data must be a data frame containing sample data.
         See also help(ebp).')
  }
  if (!is.character(smp_domains) || length(smp_domains) != 1) {
    stop('Smp_domains must be a vector of lenght 1 and of class character
         specifying the variable (name)  of a numeric or factor variable
         indicating domains in the sample data. See also help(ebp).')
  }

  if (!is.numeric(L) || length(L) != 1 || L < 1) {
    stop('L needs to be a single value, interpreted as an integer, determining
         the number of Monte-Carlo simulations. The value must be at least
         1. See also help(ebp).')
  }
  if (!all(unique(as.character(smp_data[[smp_domains]])) %in%
           unique(as.character(pop_data[[pop_domains]])))) {
    stop('The sample data contains domains that are
         not contained in the population data.')
  }
  }

ebp_check2 <- function(threshold, transformation, interval, MSE, boot_type, B,
                       custom_indicator, cpus, seed, na.rm, smp_weight, pop_weight){
  if (!is.null(threshold) && !(is.numeric(threshold) && length(threshold) == 1)
      && !inherits(threshold, "function")) {
    stop("threshold needs to be a single numeric value or a function of y.
         If it is NULL 60% of the median is selected as threshold.
         See also help(ebp).")
  }
  if (inherits(threshold, "function") && !all(attributes(formals(threshold))$names == c("y"))) {
    stop('If threshold is a function the argument needs to be y and only y. Also
         a single numeric value is possible as threshold. If it is
         NULL 60% of the median of the target variable is selected as threshold.
         See also help(ebp).')
  }
  if (is.null(transformation) || !(transformation == "box.cox"
                                   || transformation == "log" || transformation == "dual" || transformation == "log.shift"
                                   || transformation == "no")) {
    stop("The five options for transformation are ''no'', ''log'', ''box.cox'',
         ''dual'' or ''log.shift''." )
  }
  if (any(interval != 'default') & (!is.vector(interval, mode = "numeric") ||
                                    length(interval) != 2 || !(interval[1] < interval[2]))) {
    stop("interval needs to be a numeric vector of length 2
         defining a lower and upper limit for the estimation of the optimal
         transformation parameter. The value of the lower limit needs to be
         smaller than the upper limit. You can also choose 'default'. See also help(ebp).")
  }
  if (transformation == 'dual' & any(interval < 0)) {
    stop("For the dual transformation, lambda needs to be positive, so the lower
         limit of the interval cannot be negative. See also help(ebp).")
  }
  if (!is.logical(MSE) || length(MSE) != 1) {
    stop("MSE must be a logical value. Set MSE to TRUE or FALSE. See also
         help(ebp).")
  }
  if (is.null(boot_type) || !(length(boot_type) == 1 && (boot_type == "parametric"
                                                         || boot_type == "wild"))) {
    stop("The two bootstrap procedures are ''parametric'' or ''wild''." )
  }
  if (MSE == TRUE && !(is.numeric(B) && length(B) == 1  && B > 1)) {
    stop('If MSE is set to TRUE, a single numeric value for the number of bootstrap
         sample needs to be chosen that is greater than 1. See also help(ebp).')
  }
  if (!is.numeric(cpus) || !(is.numeric(cpus) && length(cpus) == 1)) {
    stop("Cpus must be a single number determining the number of kernels for the
         parallelization.")
  }
  if (!is.null(seed) && (!is.numeric(seed) || !(is.numeric(seed) && length(seed) == 1))) {
    stop("The seed must be a single value, interpreted as an integer, or NULL
         See also help(ebp).")
  }
  if (!is.null(custom_indicator)) {

    if (!inherits(custom_indicator, "list")) {
      stop("Additional indicators need to be added in argument custom_indicator
           as a list of functions. For help see Example 2 in help(ebp).")
    }

    N_custom <- length(custom_indicator)
    for (i in seq_len(N_custom)) {
      if (!inherits(custom_indicator[[i]], "function")) {
        stop("The elements of the list need to be functions. These Functions
             for custom indicators need to have exactly the following
             two arguments: y, threshold; even though a threshold might not
             included in the indicator. For help see Example 2 in help(ebp).")
      }
      else if (inherits(custom_indicator[[i]], "function")
               && !all(names(formals(custom_indicator[[i]])) == c("y", "threshold"))) {
        stop("Functions for custom indicators need to have exactly the following
             two arguments: y, threshold; even though a threshold might not
             included in the indicator. For help see Example 2 in help(ebp).")
      }
      }
      }
  if (!(inherits(na.rm, "logical") && length(na.rm) == 1)) {
    stop("na.rm needs to be a logical value. Set na.rm to TRUE or FALSE. See
         also help(ebp).")
  }
  if(is.character(smp_weight) && length(smp_weight) != 1 || !is.character(smp_weight) && !is.null(smp_weight)) {
    stop('Sample Weights must be a vector of length 1 and of class character
         specifying the variable name of a numeric variable
         indicating weights in the sample data. See also help(ebp).')
  }
  if(is.character(pop_weight) && length(pop_weight) != 1 || !is.character(pop_weight) && !is.null(pop_weight)) {
    stop('Popuation Weights must be a vector of length 1 and of class character
         specifying the variable name of a numeric variable
         indicating weights in the sample data. See also help(ebp).')
  }

  if(!is.null(smp_weight) && !(transformation == "log"|| transformation == "no")) {
    stop("Weighted ebp can only be used without transformation or the log-
         transformation")
  }
  if(!is.null(smp_weight) && isTRUE(MSE) && boot_type == "wild") {
    stop("The weighted version of ebp is only available with the ''parametric''
         bootstrap.")
  }

  }


# Functions called in notation
fw_check1 <- function(pop_data, mod_vars, pop_domains, smp_data,
                      fixed, smp_domains, threshold, smp_weight, pop_weight) {
  if (!all(mod_vars %in% colnames(pop_data))) {
    stop(paste0("Variable ", mod_vars[which(!(mod_vars %in% colnames(smp_data)))], " is not contained in pop_data.
                Please provide valid variable names for the explanatory variables."))
  }
  if (!(pop_domains %in% colnames(pop_data))) {
    stop(paste0("The domain variable ", pop_domains, " is not contained in pop_data.
                Please provide valid variable name for pop_domains."))
  }
  if (!all(mod_vars %in% colnames(smp_data))) {
    stop(paste0("Variable ", mod_vars[which(!(mod_vars %in% colnames(smp_data)))], " is not contained in smp_data.
                Please provide valid variable names for the explanatory variables."))
  }
  if (!(smp_domains %in% colnames(smp_data))) {
    stop(paste0("The domain variable ", smp_domains, " is not contained in smp_data.
                Please provide valid variable name for smp_domains."))
  }
  if (!((as.character(fixed[2])) %in% colnames(smp_data))) {
    stop(paste0("Variable ", as.character(fixed[2]), " is not contained in smp_data.
                Please provide valid variable name for the dependent variable."))
  }

  if (!is.numeric(smp_data[[paste(fixed[2])]])) {
    stop(paste0(as.character(fixed[2])," must be the name of a variable that
                is a numeric vector."))
  }
  if (is.character(smp_weight)) {
    if(!(smp_weight %in% colnames(smp_data)))
      stop(paste0("The weights variable ", smp_weight, " is not contained in smp_data.
                  Please provide a valid variable name for the weights variable."))
  }
  if (is.character(smp_weight)) {
    if (!is.numeric(smp_data[[smp_weight]]))
      stop(paste0("The variable ", smp_weight, " must be the name of a variable that
                  is a numeric vector."))
  }
  if(is.character(smp_weight)) {
    if(!all(smp_data[[smp_weight]] > 0))
      stop(paste0("Negativ or zero weights are included in ", smp_weight, " Please remove
                  obersvations with negative or zero values."))
  }

  if (is.character(pop_weight)) {
    if(!(pop_weight %in% colnames(pop_data)))
      stop(paste0("The weights variable ", pop_weight, " is not contained in pop_data.
                  Please provide a valid variable name for the population weights variable."))
  }
  if (is.character(pop_weight)) {
    if (!is.numeric(pop_data[[pop_weight]]))
      stop(paste0("The variable ", pop_weight, " must be the name of a variable that
                  is a numeric vector."))
  }
  if(is.character(pop_weight)) {
    if(!all(pop_data[[pop_weight]] > 0))
      stop(paste0("Negative or zero weights are included in ", pop_weight, " Please remove
                  obersvations with negative or zero values."))
  }

  if (dim(pop_data)[1] < dim(smp_data)[1]) {
    stop("The population data set cannot have less observations than the
         sample data set.")
  }

  if (inherits(threshold, "function") && (!is.numeric(threshold(smp_data[[paste(fixed[2])]]))
                                          || length(threshold(smp_data[[paste(fixed[2])]])) != 1)) {
    stop("The threshold function must return a single numeric value when evaluated
         with the dependent variable.")
  }

  }




fw_check2 <- function(pop_domains, pop_domains_vec, smp_domains, smp_domains_vec){
  if (!(is.numeric(pop_domains_vec) || any(inherits(pop_domains_vec, "factor")))) {
    stop(paste0(pop_domains, " needs to be the name of a variable that is numeric or
                a (ordered) factor."))
  }
  if (!(is.numeric(smp_domains_vec) || any(inherits(smp_domains_vec, "factor")))) {
    stop(paste0(smp_domains, " needs to be the name of a variable that is numeric or
                a (ordered) factor."))
  }
  if ((is.numeric(pop_domains_vec) && any(inherits(smp_domains_vec, "factor"))) ||
      (is.numeric(smp_domains_vec) && any(inherits(pop_domains_vec, "factor"))) ) {
    stop(paste0(pop_domains, " and ", smp_domains," need to be names of variables that are
                of the same class (factor and ordered factor are considered to be
                the same class). See also help(ebp)."))
  }
  }

fw_check3 <- function(obs_dom, dist_obs_dom, pop_domains, smp_domains){
  if (sum(obs_dom) == 0 || sum(dist_obs_dom) == 0) {
    #stop('Pop_domains and smp_domains do not have any value in common. Do really
    #     both variables indicate the same domains in population data and sample
    #     data, respectively?')
    stop(paste0(pop_domains, " and ", smp_domains, " do not have any value in common.
                Do really both variables indicate the same domains in population data
                and sample data, respectively?"))
  }

  }

writeexcel_check <- function(object, file, split){
  if (!inherits(object, "emdi")) {
    stop('First object needs to be of class emdi.')
  }
  if (!(inherits(split, "logical") && length(split) == 1)) {
    stop("split must to be a logical value. Set CV to TRUE or FALSE.")
  }
  if (!inherits(file, "character") || (length(grep(".xlsx", file)) == 0)) {
    stop("file must to be a character string that determines a path and
         filename. It should end on .xlsx")
  }
}

writeods_check <- function(object, file, split){
  if (!inherits(object, "emdi")) {
    stop('First object needs to be of class emdi.')
  }
  if (!(inherits(split, "logical") && length(split) == 1)) {
    stop("split must to be a logical value. Set CV to TRUE or FALSE.")
  }
  if (!inherits(file, "character") || (length(grep(".ods", file)) == 0)) {
    stop("file must to be a character string that determines a path and
         filename. It should end on .ods")
  }
  testZIP <- 1
  try(testZIP <- shell("zip"), silent = TRUE)
  if (testZIP == 1) {
    stop("No zipping application was found, see details in help(write.ods) for details.")
  }
}

# Auxiliary functions

makeXY <- function(formula, data){
  mf <- model.frame(formula=formula, data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)

  list(y = y,
       x = x)
}

throw_class_error <- function(object, subclass){
  if(!inherits(object, "emdi")){
    error_string <- paste0(subclass, " object has to be created by the emdi package for emdi methods to work.")
    stop(error_string)
  }
}



data_transformation <- function(fixed,
                                smp_data,
                                transformation,
                                lambda) {

  y_vector <- as.vector(smp_data[paste(fixed[2])])

  transformed <- if (transformation == "no") {
    no_transform(y = y_vector, shift = NULL)
  } else if (transformation == "log") {
    log_transform(y = y_vector, shift = 0)
  } else if (transformation == "box.cox") {
    box_cox(y = y_vector, lambda = lambda, shift = 0)
  } else if (transformation == "dual") {
    dual(y = y_vector, lambda = lambda, shift = 0)
  } else if (transformation == "log.shift") {
    log_shift_opt(y = y_vector, lambda = lambda, shift = NULL)
  }

  smp_data[paste(fixed[2])] <- transformed$y

  return(list(transformed_data = smp_data, shift = transformed$shift))
} # End data_transformation


# Following functions are only internal ----------------------------------------

# Function std_data_transformation only returns a data frame with transformed
# dependent variable.

std_data_transformation <- function(fixed=fixed,
                                    smp_data,
                                    transformation,
                                    lambda) {

  y_vector <- as.matrix(smp_data[paste(fixed[2])])

  std_transformed <- if (transformation == "box.cox"){
    as.data.frame(box_cox_std(y = y_vector, lambda = lambda))
  } else if (transformation == "dual") {
    as.data.frame(dual_std(y = y_vector, lambda = lambda))
  } else if (transformation == 'log.shift') {
    as.data.frame(log_shift_opt_std(y = y_vector, lambda = lambda))
  } else if (transformation == "log") {
    smp_data[paste(fixed[2])]
  } else if (transformation == "no") {
    smp_data[paste(fixed[2])]
  }

  smp_data[paste(fixed[2])] <- std_transformed
  return(transformed_data = smp_data)
} # End std_data_transformation


# Back transformation function -------------------------------------------------

back_transformation <- function(y, transformation, lambda, shift) {
  back_transformed <- if (transformation == "no") {
    no_transform_back(y = y)
  } else if (transformation == "log") {
    log_transform_back(y = y, shift = shift)
  } else if (transformation == "box.cox") {
    box_cox_back(y = y, lambda = lambda, shift = shift)
  } else if (transformation == "dual") {
    dual_back(y = y, lambda = lambda, shift = shift)
  } else if (transformation == "log.shift") {
    log_shift_opt_back(y = y, lambda = lambda)
  }

  return(y = back_transformed)
} # End back_transform


# Transformation types ---------------------------------------------------------

# No transformation ------------------------------------------------------------

# Transformation: no transformation
no_transform <- function(y, shift = NULL) {
  return(list(y = y, shift = NULL))
} # End no-transform


# Back transformation: no transformation
no_transform_back <- function(y) {
  return(y = y)
}

# Log transformation -----------------------------------------------------------

# Transformation: log
log_transform <- function(y, shift = 0) {
  min <- min(y)
  if (min <= 0) {
    shift <- abs(min) + 1
    y <- y + shift
  }
  y <- log(y)
  return(list(y = y, shift = shift))
} # End log_transform


# Back transformation: log
log_transform_back <- function(y, shift = 0) {
  y <- exp(y) - shift
  return(y = y)
} # End log_transfom_back


# Box Cox ----------------------------------------------------------------------

# Transformation: Box Cox
box_cox <- function(y, lambda = lambda, shift = 0) {
  with_shift <- function(y, shift) {
    min <- min(y)
    if (min <= 0) {
      shift <- shift + abs(min(y)) +1
    } else {
      shift <- shift
    }
    return(shift)
  }
  # Shift parameter
  shift <- with_shift(y = y, shift = shift)

  lambda_cases <- function(y, lambda = lambda) {
    lambda_absolute <- abs(lambda)
    if (lambda_absolute <= 1e-12) {  #case lambda=0
      y <- log(y + shift)
    } else {
      y <- ((y + shift)^lambda - 1) / lambda
    }
    return(y)
  }
  y <- lambda_cases(y = y, lambda = lambda)

  return(list(y = y, shift = shift))
} # End box_cox



# Standardized transformation: Box Cox

geometric.mean <- function(x) { #for RMLE in the parameter estimation

  exp(mean(log(x)))
}

box_cox_std <- function(y, lambda) {
  min <- min(y)
  if (min <= 0) {
    y <- y - min + 1
  }

  gm <- geometric.mean(y)
  y <- if (abs(lambda) > 1e-12) {
    y <- (y^lambda - 1) / (lambda * ((gm)^(lambda - 1)))
  } else {
    y <- gm * log(y)
  }
  return(y)
}


# Back transformation: Box Cox
box_cox_back <- function(y, lambda, shift = 0) {

  lambda_cases_back <- function(y, lambda = lambda, shift){
    if (abs(lambda) <= 1e-12) {   #case lambda=0
      y <-  exp(y) - shift
    } else {
      y <- (lambda * y + 1)^(1 / lambda) - shift
    }
    return(y = y)
  }
  y <- lambda_cases_back(y = y, lambda = lambda, shift = shift)

  return(y = y)
} #  End box_cox_back


# The dual transformation ------------------------------------------------------

# Transformation: dual
dual <-  function(y, lambda = lambda, shift = 0) {

  with_shift <- function(y, shift) {
    min <- min(y)
    if (min <= 0) {
      shift <- shift + abs(min(y)) +1
    } else {
      shift <- shift
    }
    return(shift)
  }

  shift <- with_shift(y = y, shift = shift)

  lambda_absolute <- abs(lambda)

  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  log(y + shift)
  } else if (lambda > 1e-12){
    yt <- ((y + shift)^(lambda) - (y + shift)^(-lambda))/(2 * lambda)
  } else {
    stop("lambda cannot be negative for the dual transformation")
  }
  return(list(y = yt, shift = shift))
  #return(y = yt)
}

# Standardized transformation: dual
dual_std <- function(y, lambda) {

  min <- min(y)
  if (min <= 0) {
    y <- y - min + 1
  }

  yt <- dual(y, lambda)$y

  zt <- if (abs(lambda) > 1e-12) {
    geo <- geometric.mean(y^(lambda -1) + y^(-lambda -1))
    zt <- yt * 2 / geo
  } else {
    zt <- geometric.mean(y) * log(y)
  }

  y <- zt

  return(y)
}

# Back transformation: dual
dual_back <- function(y, lambda = lambda, shift) {
  lambda_absolute <- abs(lambda)
  if(lambda_absolute <= 1e-12)
  {
    y <- exp(y) - shift
  }
  else
  {
    y <- (lambda * y + sqrt(lambda^2 * y^2 + 1))^(1/lambda) - shift
  }

  return(y = y)
}


# The log-shift transformation -------------------------------------------------

#  Transformation: log_shift_opt

log_shift_opt <- function(y, lambda = lambda, shift = NULL) {

  with_shift <-  function(y, lambda) {

    min <- min(y + lambda)
    if (min <= 0) {
      lambda <- lambda + abs(min) + 1
    } else {
      lambda <- lambda
    }
    return(lambda)
  }

  # Shift parameter
  lambda <- with_shift(y = y, lambda = lambda )

  log_trafo <- function(y, lambda = lambda) {
    y <- log(y + lambda)
    return(y)
  }
  yt <- log_trafo(y = y, lambda = lambda)
  #return(y)
  return(list(y = yt, shift = NULL))
} # End log_shift



# Standardized transformation: Log_shift_opt

geometric.mean <- function(x) { #for RMLE in the parameter estimation
  exp(mean(log(x)))
}

log_shift_opt_std <- function(y, lambda) {

  with_shift <-  function(y, lambda) {
    min <- min(y + lambda)
    if (min <= 0) {
      lambda <- lambda + abs(min(y)) + 1
    } else {
      lambda <- lambda
    }
    return(lambda)
  }

  # Shift parameter
  lambda <- with_shift(y = y, lambda = lambda )

  log_trafo_std <- function(y, lambda = lambda) {
    gm <- geometric.mean(y + lambda)
    y <- gm * log(y + lambda)
    return(y)
  }
  y <- log_trafo_std(y = y, lambda = lambda)
  return(y)
}

# Back transformation: log_shift_opt
log_shift_opt_back <- function(y, lambda) {
  log_shift_opt_back <- function(y, lambda = lambda){
    y <-  exp(y) - lambda
    return(y = y)
  }
  y <- log_shift_opt_back(y = y, lambda = lambda)
  return(y = y)
} #  End log_shift_opt

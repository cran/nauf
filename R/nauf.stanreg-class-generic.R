

#' Class for fitted Bayesian models with \code{nauf} contrasts.
#'
#' Models fit with \code{\link{nauf_stan_lm}}, \code{\link{nauf_stan_glm}}, 
#' \code{\link{nauf_stan_glm.nb}}, \code{\link{nauf_stan_lmer}}, 
#' \code{\link{nauf_stan_glmer}}, and \code{\link{nauf_stan_glmer.nb}} have 
#' class \code{nauf.stanreg} and inherit from class \code{stanreg} (see 
#' \code{\link[rstanarm]{stanreg-objects}} for details on the elements contained 
#' in the fitted model object).  The \code{stanreg} methods for the generic 
#' functions listed in the \code{\link[rstanarm]{stanreg-methods}} page 
#' (including those linked to in the 'See Also' section) work for 
#' \code{nauf.stanreg} models, with the same restrictions on the \code{re.form} 
#' argument described in the \code{\link{predict.nauf.merMod}} page when using 
#' the \code{posterior_predict} and \code{predict} functions.  The only 
#' exception is that the \code{\link[rstanarm]{kfold}} function from the 
#' \code{rstanarm} package cannot be used on 
#' \code{nauf.stanreg} objects; instead, \code{\link{nauf_kfold}} should be used.
#' The \code{\link{nauf_ref.grid}} and \code{\link{nauf_pmmeans}} functions also 
#' work with \code{nauf.stanreg} objects, as do
#' \code{\link[=as.shinystan,nauf.stanreg-method]{as.shinystan}} and
#' \code{\link[shinystan]{launch_shinystan}}.
#'
#' @seealso \code{\link{nauf_stan_glm}}, \code{\link{nauf_stan_glmer}},
#'   \code{\link{nauf_contrasts}}, \code{\link[rstanarm]{stanreg-objects}},
#'   and \code{\link[rstanarm]{stanreg-methods}}.
#'
#' @importFrom shinystan as.shinystan launch_shinystan
#' @importMethodsFrom shinystan as.shinystan
#'
#' @name nauf.stanreg
setOldClass(c("nauf.stanreg", "stanreg"))


#' @export
print.nauf.stanreg <- function(x, digits = 4, ...) {
  drop_class(x) <- "nauf.stanreg"
  print(x, digits = digits, ...)
  cat("\n\n")
  first_class(x) <- "nauf.stanreg"
  invisible(x)
}


#' @export
formula.nauf.stanreg <- function(x, fixed.only = FALSE, random.only = FALSE,
                                 ...) {
  form <- x$formula
  
  if (is.nauf.stanmer(x)) {
    if (missing(fixed.only) && random.only) fixed.only <- FALSE
    if (fixed.only && random.only) {
      stop("can't specify 'only fixed' and 'only random' terms")
    }
    if (fixed.only) form <- lme4_getFixedFormula(form)
    if (random.only) form <- lme4_reOnly(form, response = TRUE)
  }
  
  first_class(form) <- "nauf.formula"
  attr(form, "standardized.scale") <- attr(x$formula, "standardized.scale")
  return(form)
}


#' @export
terms.nauf.stanreg <- function(x, fixed.only = TRUE, random.only = FALSE, ...) {
  if (is.nauf.stanfer(x)) return(x$terms)
  
  if (missing(fixed.only) && random.only) fixed.only <- FALSE
  if (fixed.only && random.only) {
    stop("can't specify 'only fixed' and 'only random' terms")
  }

  tt <- attr(x$glmod$fr, "terms")

  if (fixed.only) {
    tt <- terms.formula(formula(x, fixed.only = TRUE))
    attr(tt, "predvars") <- attr(terms(x$glmod$fr), "predvars.fixed")
  }
  if (random.only) {
    tt <- terms.formula(lme4::subbars(formula(x, random.only = TRUE)))
    attr(tt, "predvars") <- attr(terms(x$glmod$fr), "predvars.random")
  }

  first_class(tt) <- "nauf.terms"
  nauf.info(tt) <- nauf.info(x)
  return(tt)
}


#' @export
predict.nauf.stanreg <- function(object, ..., newdata = NULL,
                                 type = c("link", "response"), se.fit = FALSE) {
  if (is.nauf.stanmer(object)) 
    stop("'predict' is not available for models fit with ", 
      object$call[[1]], ". Please use the 'posterior_predict' function instead.", 
      call. = FALSE)
      
  type <- match.arg(type)
  if (!se.fit && is.null(newdata)) {
    if (type == "link") return(object$linear.predictors)
    return(object$fitted.values)
  }
  
  if (type == "response") {
    stop("type='response' should not be used for models estimated by MCMC", 
      "\nor variational approximation. Instead, use the 'posterior_predict' ", 
      "function to draw \nfrom the posterior predictive distribution.", 
      call. = FALSE)
  }
  
  dat <- nauf_pp_data(object, newdata)
  stanmat <- as.matrix(object)
  beta <- stanmat[, seq_len(ncol(dat$x)), drop = FALSE]
  eta <- rsa_linear_predictor.matrix(beta, dat$x, dat$offset)
  if (type == "response") eta <- object$family$linkinv(eta)
  fit <- colMeans(eta)
  
  if (!se.fit) return(fit)
  se.fit <- apply(eta, 2L, sd)
  return(rsa_nlist(fit, se.fit))
}



###### rstantools methods ######

## These functions are not different from the stanreg versions in what they
## produce, except for the treatment of re.form and new group levels in predict 

#' @export
posterior_predict.nauf.stanreg <- function(object, newdata = NULL, draws = NULL,
                                           re.form = NULL, fun = NULL,
                                           seed = NULL, offset = NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  if (!is.null(fun)) fun <- match.fun(fun)
  if (!is.null(newdata) && !is.data.frame(newdata)) {
    stop("'newdata', if specified, must be a data.frame.")
  }
  
  dat <- nauf_pp_data(object, newdata = newdata, re.form = re.form,
    offset = offset, ...)
    
  ppargs <- nauf_pp_args(object, data = nauf_pp_eta(object, dat, draws))
  if (object$family$family == "binomial") {
    ppargs$trials <- rep(1, nrow(dat$x))
  }
  
  ppfun <- rsa_pp_fun(object)
  ytilde <- do.call(ppfun, ppargs)
  
  if (!is.null(newdata) && nrow(newdata) == 1L) ytilde <- t(ytilde)
  if (!is.null(fun)) ytilde <- do.call(fun, list(ytilde))
  if (is.null(newdata)) {
    colnames(ytilde) <- rownames(model.frame(object))
  } else {
    colnames(ytilde) <- rownames(newdata)
  }
  
  return(structure(ytilde, class = c("ppd", class(ytilde))))
}


#' @export
log_lik.nauf.stanreg <- function(object, newdata = NULL, offset = NULL, ...) {
  if (!is.null(newdata) && !is.data.frame(newdata)) {
    stop("'newdata', if specified, must be a data.frame.")
  }
  
  calling_fun <- as.character(sys.call(-1))[1]
  args <- nauf_ll_args(object, newdata = newdata, offset = offset, 
    reloo_or_kfold = calling_fun %in% c("kfold", "reloo"), ...)
    
  fun <- rsa_ll_fun(object)
  
  out <- vapply(seq_len(args$N), FUN = function(i) {
    as.vector(fun(i = i, data = args$data[i, , drop = FALSE],
      draws = args$draws))
  }, FUN.VALUE = numeric(length = args$S))
  
  if (is.null(newdata)) {
    colnames(out) <- rownames(model.frame(object))
  } else {
    colnames(out) <- rownames(newdata)
  }
  
  return(out)
}


#' @export
posterior_linpred.nauf.stanreg <- function(object, transform = FALSE,
                                           newdata = NULL, re.form = NULL, 
                                           offset = NULL, XZ = FALSE, ...) {
  if (!is.null(newdata) && !is.data.frame(newdata)) {
    stop("'newdata', if specified, must be a data.frame.")
  }
  
  dat <- nauf_pp_data(object, newdata = newdata, re.form = re.form, 
    offset = offset)
    
  if (XZ) {
    XZ <- dat[["x"]]
    if (is.nauf.stanmer(object)) XZ <- cbind(XZ, t(dat[["Zt"]]))
    return(XZ)
  }
  
  eta <- nauf_pp_eta(object, data = dat, draws = NULL)[["eta"]]
  
  if (is.null(newdata)) {
    colnames(eta) <- rownames(model.frame(object))
  } else {
    colnames(eta) <- rownames(newdata)
  }
  
  if (!transform) return(eta)
  
  return(object$family$linkinv(eta))
}


#' @importFrom loo E_loo
#'
#' @export
loo_linpred.nauf.stanreg <- function(object, type = c("mean", "var", "quantile"),
                                     probs = 0.5, transform = FALSE, ..., lw) {
  type <- match.arg(type)
  lwts <- nauf_loo_weights(object, lw, log = TRUE, ...)
  linpreds <- posterior_linpred(object, transform = transform)
  return(loo::E_loo(x = linpreds, lw = lwts, type = type, probs = probs))
}


#' @export
loo_predict.nauf.stanreg <- function(object, type = c("mean", "var", "quantile"),
                                     probs = 0.5, ..., lw) {
  type <- match.arg(type)
  lwts <- nauf_loo_weights(object, lw, log = TRUE, ...)
  preds <- posterior_predict(object)
  return(loo::E_loo(x = preds, lw = lwts, type = type, probs = probs))
}


#' @export
loo_predictive_interval.nauf.stanreg <- function(object, prob = 0.9, ..., lw) {
  stopifnot(length(prob) == 1)
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  labs <- paste0(100 * probs, "%")
  intervals <- loo_predict(object, type = "quantile", probs = probs,
    lw = lw, ...)
  rownames(intervals) <- labs
  return(t(intervals))
}


#' @importFrom loo loo pareto_k_ids
#'
#' @export
loo.nauf.stanreg <- function(x, ..., k_threshold = NULL) {
  if (rsa_model_has_weights(x)) {
    rsa_recommend_exact_loo(reason = "model has weights")
  }
  user_threshold <- !is.null(k_threshold)
  if (user_threshold) {
    rsa_validate_k_threshold(k_threshold)
  } else {
    k_threshold <- 0.7
  }
  
  loo_x <- suppressWarnings(loo::loo(rsa_ll_fun(x), args = nauf_ll_args(x), ...))
  bad_obs <- loo::pareto_k_ids(loo_x, k_threshold)
  n_bad <- length(bad_obs)
  out <- structure(loo_x, name = deparse(substitute(x)),
    discrete = rsa_is_discrete(x), yhash = rsa_hash_y(x))
    
  if (!length(bad_obs)) {
    if (user_threshold) {
      message("All pareto_k estimates below user-specified threshold of ", 
        k_threshold, ". \nReturning loo object.")
    }
    return(out)
  }
  
  if (!user_threshold) {
    if (n_bad > 10) {
      rsa_recommend_kfold(n_bad)
    } else {
      rsa_recommend_reloo(n_bad)
    }
    return(out)
  }
  
  reloo_out <- rsa_reloo(x, loo_x, obs = bad_obs)
  
  return(structure(reloo_out, name = attr(out, "name"), discrete = attr(out, 
    "discrete"), yhash = attr(out, "yhash")))
}


#' @export
waic.nauf.stanreg <- function(x, ...) {
  out <- waic(rsa_ll_fun(x), args = nauf_ll_args(x))
  return(structure(out, class = c("loo", "waic"), name = deparse(substitute(x)), 
      discrete = rsa_is_discrete(x), yhash = rsa_hash_y(x)))
}


#' Cross-validation for \code{nauf.stanreg} models.
#'
#' The same as \code{\link[rstanarm]{kfold}}, but ensuring the use of
#' \code{\link{nauf_contrasts}}.
#'
#' @param x,K,save_fits See \code{\link[rstanarm]{kfold}}.
#'
#' @return An object with classes \code{kfold} and \code{loo}.
#'
#' @export
nauf_kfold <- function(x, K = 10, save_fits = FALSE) {
  stopifnot(is.nauf.stanreg(x))
  stopifnot(K > 1, K <= nobs(x))
  if (rsa_model_has_weights(x)) {
    stop("kfold is not currently available for models fit using weights.")
  }
  
  x$call["open_progress"] <- NULL
  d <- nauf_kfold_and_reloo_data(x)
  N <- nrow(d)
  perm <- sample.int(N)
  idx <- ceiling(seq(from = 1, to = N, length.out = K + 1))
  
  bin <- .bincode(perm, breaks = idx, right = FALSE, include.lowest = TRUE)
  lppds <- list()
  fits <- array(list(), c(K, 2), list(NULL, c("fit", "omitted")))
  
  for (k in 1:K) {
    message("Fitting model ", k, " out of ", K)
    omitted <- which(bin == k)
    fit_k <- update(object = x, data = d[-omitted, , drop = FALSE],
      weights = NULL, refresh = 0)
    lppds[[k]] <- log_lik(fit_k, newdata = d[omitted, , drop = FALSE],
      newx = rstanarm::get_x(x)[omitted, , drop = FALSE], 
      stanmat = as.matrix(x))
    if (save_fits) fits[k, ] <- list(fit = fit_k, omitted = omitted)
  }
  
  elpds <- unlist(lapply(lppds, function(x) {
    apply(x, 2, rsa_log_mean_exp)
  }))
  
  out <- list(elpd_kfold = sum(elpds), se_elpd_kfold = sqrt(N * var(elpds)),
    pointwise = cbind(elpd_kfold = elpds))
  if (save_fits) out$fits <- fits
  
  return(structure(out, class = c("kfold", "loo"), K = K,
    name = deparse(substitute(x)), discrete = rsa_is_discrete(x),
    yhash = rsa_hash_y(x)))
}


#' @importFrom bayesplot pp_check
#'
#' @export
pp_check.nauf.stanreg <- function(object, plotfun = "dens_overlay", nreps = NULL,
                                  seed = NULL, ...) {
  drop_class(object) <- "nauf.stanreg"
  pp_check(object = object, plotfun = plotfun, nreps = nreps, seed = seed, ...)
}


#' Create a shinystan object from a nauf.stanreg model.
#'
#' This is a simple wrapper function that calls the \code{\link[shinystan]{as.shinystan}}
#' method for \code{stanreg} objects.
#'
#' @param X A \code{\link{nauf.stanreg}} model.
#' @param ... See \code{\link[shinystan]{as.shinystan}}.
#'
#' @return See \code{\link[shinystan]{as.shinystan}}.
#'
#' @importFrom stringr str_replace
#'
#' @export
setMethod("as.shinystan", signature(X = "nauf.stanreg"), function(X, ...) {
  class(X) <- class(X)[-1]
  out <- shinystan::as.shinystan(X, ...)
  out@model_name <- stringr::str_replace(out@model_name, "rstanarm", "nauf")
  return(out)
})


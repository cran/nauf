

#' Fit a mixed effects regression using \code{nauf} contrasts.
#'
#' The mixed effects regression functions \code{nauf_lmer}, \code{nauf_glmer.nb},
#' and \code{nauf_glmer} fit linear, negative binomial, and other generalized
#' linear mixed effects models, respectively, impelementing
#' \code{\link{nauf_contrasts}}.
#'
#' \code{nauf_lmer}, \code{nauf_glmer}, and \code{nauf_glmer.nb} are based on
#' the \code{lme4} functions \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}},
#' and \code{\link[lme4]{glmer.nb}}, respectively, but implement
#' \code{\link{nauf_contrasts}}.  The \code{nauf} functions have all the same
#' arguments as the functions they are based on, but additionally
#' \code{ncs_scale}, which is passed to \code{\link{nauf_model.frame}}.  Other
#' than \code{ncs_scale}, the arguments have the same functions as they do in
#' the functions they are based on.  The default values for \code{na.action} and
#' \code{contrasts} cannot be changed (see \code{\link{nauf_model.frame}}).
#'
#' @param formula,data,subset,weights,offset,control,start,devFunOnly,verbose,REML,family,etastart,mustart,nAGQ,interval,tol,nb.control,initCtrl,... See
#'   \code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}}, and
#'   \code{\link[lme4]{glmer.nb}}.  Note that many arguments that are passed to
#'   \code{nauf_glmer} are passed to \code{nauf_glmer.nb} through \code{...},
#'   including \code{ncs_scale}.
#' @param na.action,contrasts Changes to the default values for these arguments
#'   are ignored with a warning.  See \code{\link{nauf_model.frame}}.
#' @param ncs_scale A positive number to be passed as the \code{scale} argument
#'   to \code{\link[standardize]{named_contr_sum}} for all unordered factors.
#'   See \code{\link{nauf_model.frame}}.  For \code{nauf_glmer.nb},
#'   \code{ncs_scale} is passed through \code{...}.
#'
#' @return A fitted model of class \code{\linkS4class{nauf.lmerMod}}
#'   (\code{nauf_lmer}) or \code{\linkS4class{nauf.glmerMod}} (\code{nauf_glmer}
#'   and \code{nauf_glmer.nb}).
#'
#' @examples
#' \dontrun{
#' dat <- plosives
#' dat$spont[dat$dialect == "Valladolid"] <- NA
#' sobj <- standardize(intdiff ~ voicing * dialect * spont +
#'   (1 + voicing * spont | speaker) + (1 + dialect | item), dat)
#' mod <- nauf_lmer(sobj$formula, sobj$data)
#' }
#'
#' @seealso \code{\link{nauf_contrasts}} for a description of the contrasts
#'   applied to unordered factors; and \code{\link[lme4]{lmer}},
#'   \code{\link[lme4]{glmer}}, and \code{\link[lme4]{glmer.nb}} for argument
#'   definitions.
#'
#' @export
nauf_glmer <- function(formula, data = NULL, family = gaussian,
                       control = lme4::glmerControl(), start = NULL,
                       verbose = 0L, nAGQ = 1L, subset, weights,
                       na.action = na.pass, offset, contrasts = NULL, mustart,
                       etastart, devFunOnly = FALSE,
                       ncs_scale = attr(formula, "standardized.scale"), ...) {
  # based on lme4::glmer
  if (!inherits(control, "glmerControl")) {
    if (!is.list(control)) {
      stop("'control' is not a list; use glmerControl()")
    }
    msg <- "Use control=glmerControl(..) instead of passing a list"
    if (length(cl <- class(control))) {
      msg <- paste(msg, "of class", dQuote(cl[1]))
    }
    warning(msg, immediate. = TRUE)
    control <- do.call(lme4::glmerControl, control)
  }

  mc <- mcout <- match.call()

  if (!is.null(contrasts)) warning("Ignoring 'contrasts'; must be NULL")
  if (!isTRUE(all.equal(na.action, na.pass))) {
    warning("Ignoring 'na.action'; must be na.pass")
  }

  mc$contrasts <- NULL
  mc$na.action <- na.pass

  if (is.linear(family <- get_family(family))) {
    warning("calling nauf_glmer with family=gaussian (identity link) as a ",
      "shortcut to lmer() is deprecated; please call nauf_lmer directly")
    mc[[1]] <- quote(nauf::nauf_lmer)
    mc["family"] <- NULL
    return(eval(mc, parent.frame()))
  } else if (is.character(family) && family == "negbin") {
    stop("To fit a negative binomial model, use nauf_glmer.nb")
  }

  mc[[1]] <- quote(nauf::nauf_glFormula)
  glmod <- eval(mc, parent.frame(1L))
  mcout$formula <- glmod$formula
  glmod$formula <- NULL

  if (control$nAGQ0initStep) {
    nAGQinit <- 0L
  } else {
    nAGQinit <- 1L
  }

  devfun <- do.call(lme4::mkGlmerDevfun, c(glmod, list(verbose = verbose,
    control = control, nAGQ = nAGQinit)))
  if (nAGQ == 0 && devFunOnly) return(devfun)

  if (is.list(start)) {
    start.bad <- setdiff(names(start), c("theta", "fixef"))
    if (length(start.bad) > 0) {
      stop(sprintf("bad name(s) for start vector (%s); should be %s and/or %s",
        paste(start.bad, collapse = ", "), shQuote("theta"),
        shQuote("fixef")), call. = FALSE)
    }
    if (!is.null(start$fixef) && nAGQ == 0) {
      stop("should not specify both start$fixef and nAGQ==0")
    }
  }

  if (control$nAGQ0initStep) {
    opt <- lme4::optimizeGlmer(devfun, optimizer = control$optimizer[[1]],
      restart_edge = if (nAGQ == 0) control$restart_edge else FALSE,
      boundary.tol = if (nAGQ == 0) control$boundary.tol else 0,
      control = control$optCtrl, start = start, nAGQ = 0, verbose = verbose,
      calc.derivs = FALSE)
  }

  if (nAGQ > 0L) {
    start <- lme4_updateStart(start, theta = opt$par)
    devfun <- lme4::updateGlmerDevfun(devfun, glmod$reTrms, nAGQ = nAGQ)
    if (devFunOnly) return(devfun)
    opt <- lme4::optimizeGlmer(devfun, optimizer = control$optimizer[[2]],
      restart_edge = control$restart_edge, boundary.tol = control$boundary.tol,
      control = control$optCtrl, start = start, nAGQ = nAGQ,
      verbose = verbose, stage = 2, calc.derivs = control$calc.derivs,
      use.last.params = control$use.last.params)
  }

  if (!control$calc.derivs) {
    cc <- NULL
  } else {
    if (verbose > 10) cat("checking convergence\n")
    cc <- lme4_checkConv(attr(opt, "derivs"), opt$par,
      ctrl = control$checkConv, lbound = environment(devfun)$lower)
  }

  return(nauf.glmerMod(lme4::mkMerMod(environment(devfun), opt, glmod$reTrms,
    fr = glmod$fr, mc = mcout, lme4conv = cc)))
}


#' @rdname nauf_glmer
#' @export
nauf_lmer <- function(formula, data = NULL, REML = TRUE,
                      control = lme4::lmerControl(), start = NULL, verbose = 0L,
                      subset, weights, na.action = na.pass, offset,
                      contrasts = NULL, devFunOnly = FALSE,
                      ncs_scale = attr(formula, "standardized.scale"), ...) {
  # based on lme4::lmer
  mc <- mcout <- match.call()
  missCtrl <- missing(control)

  if (!is.null(contrasts)) warning("Ignoring 'contrasts'; must be NULL")
  if (!isTRUE(all.equal(na.action, na.pass))) {
    warning("Ignoring 'na.action'; must be na.pass")
  }

  mc$contrasts <- NULL
  mc$na.action <- na.pass

  if (!missCtrl && !inherits(control, "lmerControl")) {
    if (!is.list(control)) stop("'control' is not a list; use lmerControl()")
    warning("passing control as list is deprecated: please use lmerControl() instead",
      immediate. = TRUE)
    control <- do.call(lme4::lmerControl, control)
  }

  if (!is.null(list(...)[["family"]])) {
    warning("calling nauf_lmer with 'family' is deprecated; please use ",
      "nauf_glmer instead")
    mc[[1]] <- quote(nauf::nauf_glmer)
    if (missCtrl) mc$control <- lme4::glmerControl()
    return(eval(mc, parent.frame(1L)))
  }

  mc$control <- control
  mc[[1]] <- quote(nauf::nauf_lFormula)
  lmod <- eval(mc, parent.frame(1L))
  mcout$formula <- lmod$formula
  lmod$formula <- NULL
  devfun <- do.call(lme4::mkLmerDevfun, c(lmod, list(start = start,
    verbose = verbose, control = control)))
  if (devFunOnly) return(devfun)

  if (control$optimizer == "none") {
    opt <- list(par = NA, fval = NA, conv = 1000, message = "no optimization")
  } else {
    opt <- lme4::optimizeLmer(devfun, optimizer = control$optimizer,
      restart_edge = control$restart_edge, boundary.tol = control$boundary.tol,
      control = control$optCtrl, verbose = verbose, start = start,
      calc.derivs = control$calc.derivs,
      use.last.params = control$use.last.params)
  }

  cc <- lme4_checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,
    lbound = environment(devfun)$lower)

  return(nauf.lmerMod(lme4::mkMerMod(environment(devfun), opt, lmod$reTrms,
    fr = lmod$fr, mc = mcout, lme4conv = cc)))
}


#' @rdname nauf_glmer
#' @export
nauf_glmer.nb <- function (..., interval = log(th) + c(-3, 3), tol = 5e-05,
                           verbose = FALSE, nb.control = NULL,
                           initCtrl = list(limit = 20, eps = 2 * tol,
                           trace = verbose)) {
  # based on lme4::glmer.nb
  dotE <- as.list(substitute(E(...))[-1])
  mc <- match.call()
  mc[[1]] <- quote(nauf::nauf_glmer)
  mc$family <- quote(stats::poisson)
  mc$verbose <- (verbose >= 2)
  g0 <- eval(mc, parent.frame(1L))

  th <- lme4_est_theta(g0, limit = initCtrl$limit, eps = initCtrl$eps,
    trace = initCtrl$trace)
  if (verbose) cat("th := est_theta(glmer(..)) =", format(th))

  mc$family <- bquote(MASS::negative.binomial(theta = .(th)))
  g1 <- eval(mc, parent.frame(1L))
  if (verbose) cat(" --> dev.= -2*logLik(.) =", format(-2 * logLik(g1)), "\n")
  if ("data" %in% names(g1@call)) {
    if (!is.null(dotE[["data"]])) {
      g1@call[["data"]] <- dotE[["data"]]
    }
  } else {
    warning("no 'data = *' in nauf_glmer.nb() call ... Not much is guaranteed")
  }

  other.args <- c("verbose", "control")
  for (a in other.args) {
    if (a %in% names(g1@call)) {
      g1@call[[a]] <- dotE[[a]]
    }
  }

  return(nauf.glmerMod(lme4_optTheta(g1, interval = interval, tol = tol,
    verbose = verbose, control = c(eval.parent(g1@call$control), nb.control))))
}


#' Fit a fixed effects regression using \code{nauf} contrasts.
#'
#' The fixed effects regression functions \code{nauf_lm}, \code{nauf_glm.nb},
#' and \code{nauf_glm} fit linear, negative binomial, and other generalized
#' linear models, respectively, impelementing \code{\link{nauf_contrasts}}.
#'
#' \code{nauf_lm} is based on \code{\link[stats]{lm}} in the
#' \code{stats} package, \code{nauf_glm} on \code{\link[stats]{glm}} in the
#' \code{stats} package, and \code{nauf_glm.nb} on \code{\link[MASS]{glm.nb}} in
#' the \code{MASS} package; but implementing \code{\link{nauf_contrasts}}.  The
#' \code{nauf} functions have all the same arguments as the functions they are
#' based on, but additionally \code{ncs_scale}, which is passed to
#' \code{\link{nauf_model.frame}}.  Other than \code{ncs_scale}, the arguments
#' have the same functions as they do in the functions they are based on.  The
#' default values for \code{na.action}, \code{contrasts}, \code{model},
#' \code{x}, and \code{y} cannot be changed.  For \code{na.action} and
#' \code{contrasts}, see \code{\link{nauf_model.frame}}.  Forcing \code{model},
#' \code{x}, and \code{y} to be \code{TRUE} ensures that the fitted model
#' retains the model frame, model matrix, and response, respectively.  This is
#' necessary for some generic functions applied to the fitted model to work
#' properly.
#'
#' @param formula,data,subset,offset,weights,method,control,start,etastart,mustart,qr,singular.ok,family,init.theta,link,... See
#'   \code{\link[stats]{lm}}, \code{\link[stats]{glm}}, and
#'   \code{\link[MASS]{glm.nb}}.
#' @param na.action,contrasts Changes to the default values for these arguments
#'   are ignored with a warning.  See \code{\link{nauf_model.frame}}.
#' @param model,x,y Chages to the default values for these arguments are ignored
#'   with a warning.  See 'Details'.
#' @param ncs_scale A positive number to be passed as the \code{scale} argument
#'   to \code{\link[standardize]{named_contr_sum}} for all unordered factors.
#'   See \code{\link{nauf_model.frame}}.
#'
#' @return A fitted model with class \code{nauf.glm} (inheriting from \code{lm},
#'   and also possibly \code{glm} and/or \code{negbin} depending on the
#'   regression).  For the elements contained in the object, and additional
#'   class attributes the model may contain, see \code{\link[stats]{lm}},
#'   \code{\link[stats]{glm}}, and \code{\link[MASS]{glm.nb}}.
#'
#' @examples
#' dat <- plosives
#' dat$spont[dat$dialect == "Valladolid"] <- NA
#' sobj <- standardize(intdiff ~ voicing * dialect * spont, dat)
#' mod <- nauf_lm(sobj$formula, sobj$data)
#'
#' @seealso \code{\link{nauf_contrasts}} for a description of the contrasts
#'   applied to unordered factors; and \code{\link[stats]{lm}},
#'   \code{\link[stats]{glm}}, and \code{\link[MASS]{glm.nb}} for argument
#'   definitions.
#'
#' @section Note on Generics: Methods for S3 generic functions from the
#'   \code{stats} and \code{MASS} packages should work for \code{nauf.glm}
#'   models as they normally would for the related regression models not fit
#'   with \code{\link{nauf_contrasts}}.  If you encounter a generic function in
#'   these packages which does not function properly, please report the issue at
#'   \url{https://github.com/CDEager/nauf/issues}.
#'
#' @export
nauf_glm <- function(formula, family = gaussian, data = NULL, weights, subset,
                     na.action = na.pass, start = NULL, etastart, mustart,
                     offset, control = list(...), model = TRUE,
                     method = "glm.fit", x = TRUE, y = TRUE, contrasts = NULL,
                     ncs_scale = attr(formula, "standardized.scale"), ...) {
  # based on stats::glm

  call <- match.call()
  mf <- match.call(expand.dots = FALSE)

  if (!model) warning("Ignoring 'model'; must be TRUE")
  if (!x) warning("Ignoring 'x'; must be TRUE")
  if (!y) warning("Ignoring 'y'; must be TRUE")
  if (!is.null(contrasts)) warning("Ignoring 'contrasts'; must be NULL")
  if (!isTRUE(all.equal(na.action, na.pass))) {
    warning("Ignoring 'na.action'; must be na.pass")
  }

  family <- get_family(family)
  if (is.character(family) && family == "negbin") {
    stop("To fit a negative binomial model, use nauf_glm.nb")
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  m <- match(c("formula", "data", "subset", "weights", "ncs_scale", "etastart",
    "mustart", "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(nauf::nauf_model.frame)
  mf <- eval(mf, parent.frame())

  if (identical(method, "model.frame")) return(mf)
  if (!is.character(method) && !is.function(method)) {
    stop("invalid 'method' argument")
  }
  if (identical(method, "glm.fit")) {
    control <- do.call(stats::glm.control, control)
  }

  mt <- attr(mf, "terms")
  Y <- stats::model.response(mf, "any")
  if (length(dim(Y)) == 1L) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if (!is.null(nm)) names(Y) <- nm
  }

  if (!stats::is.empty.model(mt)) {
    X <- nauf_model.matrix(mf)
  } else {
    X <- matrix(, NROW(Y), 0L)
  }

  weights <- as.vector(stats::model.weights(mf))
  if (!is.null(weights) && !is.numeric(weights)) {
    stop("'weights' must be a numeric vector")
  }
  if (!is.null(weights) && any(weights < 0)) {
    stop("negative weights not allowed")
  }

  offset <- as.vector(stats::model.offset(mf))
  if (!is.null(offset) && length(offset) != NROW(Y)) {
    stop(gettextf("number of offsets is %d should equal %d (number of observations)",
      length(offset), NROW(Y)), domain = NA)
  }

  mustart <- stats::model.extract(mf, "mustart")
  etastart <- stats::model.extract(mf, "etastart")
  fit <- eval(call(if (is.function(method)) "method" else method,
    x = X, y = Y, weights = weights, start = start, etastart = etastart,
    mustart = mustart, offset = offset, family = family, control = control,
    intercept = attr(mt, "intercept") > 0L))

  if (length(offset) && attr(mt, "intercept") > 0L) {
    fit2 <- eval(call(if (is.function(method)) "method" else method,
      x = X[, "(Intercept)", drop = FALSE], y = Y, weights = weights,
      offset = offset, family = family, control = control, intercept = TRUE))
    if (!fit2$converged) {
      warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
    }
    fit$null.deviance <- fit2$deviance
  }

  fit$model <- mf
  fit$na.action <- attr(mf, "na.action")
  fit$x <- X
  fit <- c(fit, list(call = call, formula = formula, terms = mt, data = data,
    offset = offset, control = control, method = method, contrasts = NULL,
    xlevels = .getXlevels(mt, mf)))
  
  add_class(fit) <- list("nauf.glm", c("glm", "lm"))

  return(fit)
}


#' @rdname nauf_glm
#' @export
nauf_lm <- function(formula, data = NULL, subset, weights, na.action = na.pass,
                    method = "qr", model = TRUE, x = TRUE, y = TRUE, qr = TRUE,
                    singular.ok = TRUE, contrasts = NULL, offset,
                    ncs_scale = attr(formula, "standardized.scale"), ...) {
  # based on stats::lm

  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)

  if (!model) warning("Ignoring 'model'; must be TRUE")
  if (!x) warning("Ignoring 'x'; must be TRUE")
  if (!y) warning("Ignoring 'y'; must be TRUE")
  if (!is.null(contrasts)) warning("Ignoring 'contrasts'; must be NULL")
  if (!isTRUE(all.equal(na.action, na.pass))) {
    warning("Ignoring 'na.action'; must be na.pass")
  }

  m <- match(c("formula", "data", "subset", "weights", "offset", "ncs_scale"),
    names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(nauf::nauf_model.frame)
  mf <- eval(mf, parent.frame())

  if (method == "model.frame") return(mf)

  if (method != "qr") {
    warning(gettextf("method = '%s' is not supported. Using 'qr'",
      method), domain = NA)
  }

  mt <- attr(mf, "terms")
  y <- stats::model.response(mf, "numeric")

  w <- as.vector(stats::model.weights(mf))
  if (!is.null(w) && !is.numeric(w)) stop("'weights' must be a numeric vector")

  offset <- as.vector(stats::model.offset(mf))
  if (!is.null(offset) && length(offset) != NROW(y)) {
    stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
      length(offset), NROW(y)), domain = NA)
  }

  if (stats::is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 3) else numeric(),
      residuals = y, fitted.values = 0 * y, weights = w, rank = 0L,
      df.residual = if (!is.null(w)) sum(w != 0) else if (is.matrix(y)) nrow(y)
      else length(y))
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }

  } else {
    x <- nauf_model.matrix(mf)
    if (is.null(w)) {
      z <- stats::lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...)
    } else {
      z <- stats::lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,
        ...)
    }
  }

  class(z) <- c("nauf.glm", if (is.matrix(y)) "mlm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- NULL
  z$xlevels <- stats::.getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  z$model <- mf
  z$x <- x
  z$y <- y
  if (!qr) z$qr <- NULL

  return(z)
}


#' @rdname nauf_glm
#' @export
nauf_glm.nb <- function(formula, data = NULL, weights, subset,
                        na.action = na.pass, start = NULL, etastart, mustart,
                        control = stats::glm.control(...), method = "glm.fit",
                        model = TRUE, x = TRUE, y = TRUE, contrasts = NULL,
                        ncs_scale = attr(formula, "standardized.scale"), ...,
                        init.theta, link = log) {
  # based on MASS::glm.nb

  loglik <- function(n, th, mu, y, w) {
    sum(w * (lgamma(th + y) - lgamma(th) - lgamma(y + 1) + th * log(th) + y *
      log(mu + (y == 0)) - (th + y) * log(th + mu)))
  }

  link <- substitute(link)
  if (missing(init.theta)) {
    fam0 <- do.call(stats::poisson, list(link = link))
  } else {
    fam0 <- do.call(MASS::negative.binomial, list(theta = init.theta,
      link = link))
  }

  if (!model) warning("Ignoring 'model'; must be TRUE")
  if (!x) warning("Ignoring 'x'; must be TRUE")
  if (!y) warning("Ignoring 'y'; must be TRUE")
  if (!is.null(contrasts)) warning("Ignoring 'contrasts'; must be NULL")
  if (!isTRUE(all.equal(na.action, na.pass))) {
    warning("Ignoring 'na.action'; must be na.pass")
  }

  mf <- Call <- match.call()
  m <- match(c("formula", "data", "subset", "weights", "ncs_scale", "etastart",
    "mustart", "offset"), names(mf), 0)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(nauf::nauf_model.frame)
  mf <- eval.parent(mf)

  Terms <- attr(mf, "terms")
  if (method == "model.frame") return(mf)

  Y <- stats::model.response(mf, "numeric")
  if (!stats::is.empty.model(Terms)) {
    X <- nauf_model.matrix(mf)
  } else {
    X <- matrix(, NROW(Y), 0)
  }

  w <- stats::model.weights(mf)
  if (!length(w)) {
    w <- rep(1, nrow(mf))
  } else if (any(w < 0)) {
    stop("negative weights not allowed")
  }

  offset <- stats::model.offset(mf)
  mustart <- stats::model.extract(mf, "mustart")
  etastart <- stats::model.extract(mf, "etastart")
  n <- length(Y)

  if (!missing(method)) {
    if (!exists(method, mode = "function")) {
      stop(gettextf("unimplemented method: %s", sQuote(method)), domain = NA)
    }
    glm.fitter <- get(method)
  } else {
      method <- "glm.fit"
      glm.fitter <- stats::glm.fit
  }

  if (control$trace > 1) message("Initial fit:")
  fit <- glm.fitter(x = X, y = Y, w = w, start = start, etastart = etastart,
    mustart = mustart, offset = offset, family = fam0,
    control = list(maxit = control$maxit, epsilon = control$epsilon,
    trace = control$trace > 1), intercept = attr(Terms, "intercept") > 0)
  class(fit) <- c("glm", "lm")

  mu <- fit$fitted.values
  th <- as.vector(MASS::theta.ml(Y, mu, sum(w), w, limit = control$maxit,
    trace = control$trace > 2))
  if (control$trace > 1) {
    message(gettextf("Initial value for 'theta': %f", signif(th)), domain = NA)
  }
  fam <- do.call(MASS::negative.binomial, list(theta = th, link = link))

  iter <- 0
  d1 <- sqrt(2 * max(1, fit$df.residual))
  d2 <- del <- 1
  g <- fam$linkfun
  Lm <- loglik(n, th, mu, Y, w)
  Lm0 <- Lm + 2 * d1

  while ((iter <- iter + 1) <= control$maxit &&
  (abs(Lm0 - Lm)/d1 + abs(del)/d2) > control$epsilon) {
    eta <- g(mu)
    fit <- glm.fitter(x = X, y = Y, w = w, etastart = eta, offset = offset,
      family = fam, control = list(maxit = control$maxit,
      epsilon = control$epsilon, trace = control$trace > 1),
      intercept = attr(Terms, "intercept") > 0)
    t0 <- th
    th <- MASS::theta.ml(Y, mu, sum(w), w, limit = control$maxit,
      trace = control$trace > 2)
    fam <- do.call(MASS::negative.binomial, list(theta = th, link = link))
    mu <- fit$fitted.values
    del <- t0 - th
    Lm0 <- Lm
    Lm <- loglik(n, th, mu, Y, w)
    if (control$trace) {
      Ls <- loglik(n, th, Y, Y, w)
      Dev <- 2 * (Ls - Lm)
      message(sprintf("Theta(%d) = %f, 2(Ls - Lm) = %f", iter, signif(th),
        signif(Dev)), domain = NA)
    }
  }

  if (!is.null(attr(th, "warn"))) fit$th.warn <- attr(th, "warn")
  if (iter > control$maxit) {
    warning("alternation limit reached")
    fit$th.warn <- gettext("alternation limit reached")
  }

  if (length(offset) && attr(Terms, "intercept")) {
    if (length(Terms)) {
      null.deviance <- glm.fitter(X[, "(Intercept)", drop = FALSE], Y, w,
        offset = offset, family = fam, control = list(maxit = control$maxit,
        epsilon = control$epsilon, trace = control$trace > 1),
        intercept = TRUE)$deviance
    } else {
      null.deviance <- fit$deviance
    }
    fit$null.deviance <- null.deviance
  }

  class(fit) <- c("nauf.glm", "negbin", "glm", "lm")
  fit$terms <- Terms
  fit$formula <- as.vector(attr(Terms, "formula"))
  Call$init.theta <- signif(as.vector(th), 10)
  Call$link <- link
  fit$call <- Call
  fit$model <- mf
  fit$na.action <- attr(mf, "na.action")
  fit$x <- X
  fit$theta <- as.vector(th)
  fit$SE.theta <- attr(th, "SE")
  fit$twologlik <- as.vector(2 * Lm)
  fit$aic <- -fit$twologlik + 2 * fit$rank + 2
  fit$contrasts <- NULL
  fit$xlevels <- .getXlevels(Terms, mf)
  fit$method <- method
  fit$control <- control
  fit$offset <- offset

  return(fit)
}


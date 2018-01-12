function (method, nlme.model = NULL, randomFormula, fixedFormula = NULL, 
          data, start, group, rPartFormula, weight = NULL, R = NULL, 
          min.dev = NULL, control = rpart.control()) 
{
  if (method == "lme") {
    lmeFormula <- fixedFormula
  }
  if (is.null(min.dev) == FALSE) {
    if (method == "lme") {
      mod <- lme(lmeFormula, data = data, random = randomFormula, 
                 correlation = R, na.action = na.omit)
    }
    else {
      mod <- nlme(model = nlme.model, fixed = fixedFormula, 
                  data = data, random = randomFormula, correlation = R, 
                  na.action = na.omit, start = start, groups = group)
    }
    control$cp <- 1 - (-2 * mod$logLik - min.dev)/(-2 * mod$logLik)
  }
  if (method == "lme") {
    groupingName = attr(terms(splitFormula(randomFormula, 
                                           "|")[[2]]), "term.labels")
    responseName = attr(terms(getResponseFormula(lmeFormula)), 
                        "term.labels")
    groupingFactor = data[, names(data) == groupingName]
    terms = attr(terms(lmeFormula), "term.labels")
    continuous = !is.factor(data[, names(data) == terms[1]])
    evaluation <- function(y, wt, parms) {
      model = lme(lmeFormula, data = parms[groupingFactor %in% 
                                             y, ], random = randomFormula, correlation = R, 
                  na.action = na.omit)
      if (continuous) {
        slope = model$coefficients$fixed[2]
      }
      else {
        levels = length(levels(data[, names(data) == 
                                      terms[1]]))
        y = model$coefficients$fixed[1:levels]
        x = 1:levels
        slope = lm(y ~ x)$coefficients[2]
      }
      list(label = slope, deviance = -2 * (model$logLik))
    }
  }
  else if (method == "nlme") {
    groupingName = attr(terms(splitFormula(group, "~")[[1]]), 
                        "term.labels")
    responseName = attr(terms(getResponseFormula(nlme.model)), 
                        "term.labels")
    groupingFactor = data[, names(data) == groupingName]
    evaluation <- function(y, wt, parms) {
      model = nlme(model = nlme.model, fixed = fixedFormula, 
                   data = parms[groupingFactor %in% y, ], random = randomFormula, 
                   correlation = R, na.action = na.omit, start = start, 
                   groups = group)
      slope = 1
      list(label = slope, deviance = -2 * (model$logLik))
    }
  }
  split <- function(y, wt, x, parms, continuous) {
    print(paste("splitting:", length(unique(x)), "values"))
    dev = vector()
    xUnique = unique(x)
    if (method == "lme") {
      rootDev = lme(lmeFormula, data = parms[groupingFactor %in% 
                                               y, ], random = randomFormula, correlation = R, 
                    na.action = na.omit)$logLik
    }
    else if (method == "nlme") {
      rootDev = nlme(model = nlme.model, fixed = fixedFormula, 
                     data = parms[groupingFactor %in% y, ], random = randomFormula, 
                     correlation = R, na.action = na.omit, start = start, 
                     groups = group)$logLik
    }
    if (continuous) {
      for (i in xUnique) {
        yLeft = y[x <= i]
        yRight = y[x > i]
        if (length(yLeft) < control$minbucket || length(yRight) < 
            control$minbucket) {
          dev = c(dev, 0)
        }
        else {
          if (method == "lme") {
            modelLeft = try(lme(lmeFormula, data = parms[groupingFactor %in% 
                                                           yLeft, ], random = randomFormula, correlation = R, 
                                na.action = na.omit), silent = TRUE)
            modelRight = try(lme(lmeFormula, data = parms[groupingFactor %in% 
                                                            yRight, ], random = randomFormula, correlation = R, 
                                 na.action = na.omit), silent = TRUE)
          }
          else if (method == "nlme") {
            modelLeft = try(nlme(model = nlme.model, 
                                 fixed = fixedFormula, data = parms[groupingFactor %in% 
                                                                      yLeft, ], random = randomFormula, correlation = R, 
                                 na.action = na.omit, start = start, groups = group), 
                            silent = TRUE)
            modelRight = try(nlme(model = nlme.model, 
                                  fixed = fixedFormula, data = parms[groupingFactor %in% 
                                                                       yRight, ], random = randomFormula, correlation = R, 
                                  na.action = na.omit, start = start, groups = group), 
                             silent = TRUE)
          }
          if (any(class(modelLeft) == "lme") && any(class(modelRight) == 
                                                    "lme")) {
            dev = c(dev, modelLeft$logLik + modelRight$logLik)
          }
          else {
            dev = c(dev, 0)
          }
        }
      }
      good = rep(0, length(x))
      for (i in 1:length(xUnique)) {
        good[x == xUnique[i]] = dev[i]
      }
      good = good[1:(length(good) - 1)]
      list(goodness = good + abs(rootDev) * (good != 0) * 
             2, direction = rep(-1, length(good)))
    }
    else {
      order = rep(0, length(xUnique))
      response = parms[, names(parms) == responseName]
      for (i in 1:length(xUnique)) {
        order[i] = mean(response[x == xUnique[i]], na.rm = TRUE)
      }
      dir = sort(order, index.return = TRUE)$ix
      for (i in 1:(length(dir) - 1)) {
        yLeft = y[x %in% dir[1:i]]
        yRight = y[x %in% dir[(i + 1):length(dir)]]
        if (length(yLeft) < control$minbucket || length(yRight) < 
            control$minbucket) {
          dev = c(dev, 0)
        }
        else {
          if (method == "lme") {
            modelLeft = try(lme(lmeFormula, data = parms[groupingFactor %in% 
                                                           yLeft, ], random = randomFormula, correlation = R, 
                                na.action = na.omit), silent = TRUE)
            modelRight = try(lme(lmeFormula, data = parms[groupingFactor %in% 
                                                            yRight, ], random = randomFormula, correlation = R, 
                                 na.action = na.omit), silent = TRUE)
          }
          else if (method == "nlme") {
            modelLeft = try(nlme(model = nlme.model, 
                                 fixed = fixedFormula, data = parms[groupingFactor %in% 
                                                                      yLeft, ], random = randomFormula, correlation = R, 
                                 na.action = na.omit, start = start, groups = group), 
                            silent = TRUE)
            modelRight = try(nlme(model = nlme.model, 
                                  fixed = fixedFormula, data = parms[groupingFactor %in% 
                                                                       yRight, ], random = randomFormula, correlation = R, 
                                  na.action = na.omit, start = start, groups = group), 
                             silent = TRUE)
          }
          
          
          ####I think the following Logic statement is wrong. The program collapses when one of the two nodes did not converge.
          ####Maybe copy-paste the same logic statement used for the "continuous" covariates.
          
          if (any(class(modelLeft) == "lme") | any(class(modelLeft) == 
                                                   "nlme") && any(class(modelRight) == "lme") | 
              any(class(modelRight) == "nlme")) {
            dev = c(dev, modelLeft$logLik + modelRight$logLik)
          }
          else {
            dev = c(dev, 0)
          }
        }
      }
      list(goodness = dev + abs(rootDev) * (dev != 0) * 
             2, direction = dir)
    }
  }
  initialize <- function(y, offset, parms = 0, wt) {
    list(y = y, parms = parms, numresp = 1, numy = 1, summary = function(yval, 
                                                                         dev, wt, ylevel, digits) {
      paste("deviance (-2logLik)", format(signif(dev), 
                                          3), "slope", signif(yval, 2))
    }, text = function(yval, dev, wt, ylevel, digits, n, 
                       use.n) {
      if (!use.n) {
        paste("m:", format(signif(yval, 1)))
      } else {
        paste("n:", n)
      }
    })
  }
  model <- list()
  model.rpart = rpart(paste(groupingName, c(rPartFormula)), 
                      method = list(eval = evaluation, split = split, init = initialize), 
                      control = control, data = data, parms = data)
  model$rpart_out <- model.rpart
  if (method == "lme") {
    model$lmeModel = lme(lmeFormula, data = data, random = randomFormula, 
                         correlation = R, na.action = na.omit)
    model$fixedFormula = lmeFormula
    model$lmeFormula = lmeFormula
  }
  else if (method == "nlme") {
    model$nlmeModel <- nlme(model = nlme.model, fixed = fixedFormula, 
                            data = data, random = randomFormula, correlation = R, 
                            na.action = na.omit, start = start, groups = group)
    model$fixedFormula <- fixedFormula
    model$nlme.model <- nlme.model
  }
  model$leaf_node <- model.rpart$where
  summary = fixed_effects = var.corr = resid.var = list()
  for (j in 1:length(table(model.rpart$where))) {
    id <- names(table(model.rpart$where))[j] == model.rpart$where
    if (method == "lme") {
      model.out = lme(lmeFormula, data = data[id, ], random = randomFormula, 
                      correlation = R, na.action = na.omit)
      summary[[as.numeric(names(table(model.rpart$where)))[j]]] <- summary(model.out)
      fixed_effects[[as.numeric(names(table(model.rpart$where)))[j]]] <- fixed.effects(model.out)
      var.corr[[as.numeric(names(table(model.rpart$where)))[j]]] <- model.out$modelStruct[[1]]
      resid.var[[as.numeric(names(table(model.rpart$where)))[j]]] <- model.out$sigma
    }
    else if (method == "nlme") {
      model.out <- nlme(model = nlme.model, fixed = fixedFormula, 
                        data = data[id, ], random = randomFormula, correlation = R, 
                        na.action = na.omit, start = start, groups = group)
      summary[[as.numeric(names(table(model.rpart$where)))[j]]] <- summary(model.out)
      fixed_effects[[as.numeric(names(table(model.rpart$where)))[j]]] <- fixed.effects(model.out)
      var.corr[[as.numeric(names(table(model.rpart$where)))[j]]] <- model.out$modelStruct[[1]]
      resid.var[[as.numeric(names(table(model.rpart$where)))[j]]] <- model.out$sigma
    }
  }
  model$summary <- summary
  model$fixed_effects <- fixed_effects
  model$var.corr <- var.corr
  model$resid.var <- resid.var
  model$rpart_out <- model.rpart
  model$randomFormula = randomFormula
  model$R = R
  if (method == "nlme") {
    model$group = group
    model$start = start
  }
  model$method = method
  model$data = data
  model$groupingName = groupingName
  model$rPartFormula = rPartFormula
  return(model)
}
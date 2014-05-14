#FIXME: handle seed
# FIXME: use defaults of params

#' Runs algorithm on an instance with given parameters and returns performance.
#'
#' @param scen [\code{AClibScenario}]\cr
#'   Scenario.
#' @param instance [\code{instance}]\cr
#'   Name of instance to run on, must be from training or test set of \code{scen}.
#' @param vals [named \code{list}]\cr
#'   Setting for parameters. Names are parameters names, elements their values.
#'   If a parameter ist not listed here, its default value is taken from \code{scen$par.set}.
#' @export
runAlgoOnInstance = function(scen, instance, vals, seed = 123L, cutoff.time) {
  checkArg(scen, "AClibScenario")
  checkArg(instance, choices = c(scen$train.instances, scen$test.instance.file))
  seed = convertInteger(seed)
  checkArg(seed, "integer", len = 1L, na.ok = FALSE)
  if (missing(cutoff.time)) {
    cutoff.time = scen$cutoff.time
  } else {
    cutoff.time = convertInteger(cutoff.time)
    checkArg(cutoff.time, "integer", len = 1L, na.ok = FALSE)
  }
  cmd = file.path(scen$aclib.dir, scen$algo)

  args = character(0)
  args[1L] = file.path(scen$aclib.dir, instance)
  args[2L] = 0
  args[3L] = scen$cutoff.time
  #FIXME: what it this?
  args[4L] = 2^31 - 1L
  args[5L] = seed

  vals = collapse(sprintf("-%s '%s'", names(vals), vals), " ")
  args = c(args, vals)

  system3(cmd, args)
}


#FIXME: handle seed
# FIXME: use defaults of params

#' Runs algorithm on an instance with given parameters and returns performance.
#'
#' @param scen [\code{AClibScenario}]\cr
#'   Scenario.
#' @param instance [\code{character(1)}]\cr
#'   Name of instance to run on, must be from training or test set of \code{scen}.
#' @param vals [named \code{list}]\cr
#'   Setting for parameters. Names are parameters names, elements their values.
#'   If a parameter ist not listed here, its default value is taken from \code{scen$par.set}.
#'   Missing values are removed last, before passing them to the algorithm script.
#' @param cutoff.time [\code{numeric(1)}]\cr
#'   Cutoff time for algorithm run in seconds.
#'   An upper limit is \code{scen$cutoff.time}.
#'   Default is \code{scen$cutoff.time}.
#' @export
runAlgoOnInstance = function(scen, instance, vals, seed = 123L, cutoff.time, show.info = TRUE) {
  checkAClibScenario(scen)
  ps = scen$par.set
  checkArg(instance, choices = c(scen$train.instances, scen$test.instance.file))
  checkArg(vals, "list")
  if (!isFeasible(ps, vals))
    stopf("Parameter setting is not feasible:\n%s", paramValueToString(ps, vals))
  seed = convertInteger(seed)
  seed = checkArg(seed, "integer", len = 1L, na.ok = FALSE)
  if (missing(cutoff.time)) {
    cutoff.time = scen$cutoff.time
  } else {
    checkArg(cutoff.time, "numeric", lower = 0, upper = scen$cutoff.time)
  }
  checkArg(show.info, "logical", len = 1L, na.ok = FALSE)
  # cmd = file.path(scen$aclib.dir, scen$algo)

  # defs = getDefaultValues(ps)
  # insert(vals, defs, which(names(defs) %nin% names(vals)))
  vals = removeMissingValues(vals)

  # build args for cmd
  args = character(0)
  args[1L] = instance # instance to run on, maybe with extra info in line
  args[2L] = cutoff.time # cutoff.time for algo
  args[3L] = 2^31 - 1L # cutlength. this is like iterations for a solver. usually not used.
  args[4L] = seed # seed for run

  # change wd to aclib
  owd = getwd()
  on.exit(setwd(owd))
  setwd(scen$aclib.dir)

  # cmd is algo script (can be already cmd with whitespaces and args) + args + vals
  cmd = scen$algo
  args = collapse(args, " ")
  cmd = paste(cmd, args, sep = " ")
  vals = sprintf("-%s '%s'", names(vals), vals)
  vals = collapse(vals, " ")
  cmd = paste(cmd, vals, sep = " ")

  if (show.info)
    messagef(cmd)
  res = system(cmd, wait = TRUE, intern = TRUE)
  if (show.info)
    messagef(res)

  result.regexp = "\\s*(Final)?\\s*[Rr]esult\\s+(([Ff]or)|([oO]f))\\s+((HAL)|(ParamILS)|(SMAC)|([Tt]his [wW]rapper))"
  res = as.character(res)
  j = str_detect(res, result.regexp)
  if (sum(j) != 1L)
    stopf("There must be extactly one result line in output:\n%s", collapse(res, "\n"))
  res.line = res[j]
  # Result for ParamILS: <solved>, <runtime>, <runlength>, <quality>, <seed>,<additional rundata>
  res.line = str_replace(res.line, result.regexp, "")
  s = splitAndTrim(res, ",")

  solved = s[1L]
  runtime = s[2L]
  quality = as.numeric(s[4L])
  seed2 = as.integer(s[5L])

  # check what we got in solved pattern
  if (solved %in% c("CRASHED", "TIMEOUT")) {
    success = FALSE
  } else if (solved  %in% c("SAT", "UNSAT")) {
    success = TRUE
  } else {
    stopf("Unknown pattern for 'solved' in run result: %s", solved)
  }

  if (seed != seed2)
    stopf("Output seed %i does not match input seed %i!", seed2, seed)

  return(list(success = success, solved = solved, y = y, runtime = runtime, quality = quality))
}




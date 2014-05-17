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
#' @param cutoff.time [\code{integer(1)}]\cr
#'   Cutoff time for algorithm run in seconds.
#'   An upper limit is \code{scen$cutoff.time}.
#'   Default is \code{scen$cutoff.time}.
#' @param
#' @export
runAlgoOnInstance = function(scen, instance, vals, seed = 123L, cutoff.time, show.info = TRUE) {
  checkArg(scen, "AClibScenario")
  checkArg(instance, choices = c(scen$train.instances, scen$test.instance.file))
  seed = convertInteger(seed)
  checkArg(seed, "integer", len = 1L, na.ok = FALSE)
  if (missing(cutoff.time)) {
    cutoff.time = scen$cutoff.time
  } else {
    cutoff.time = convertInteger(cutoff.time)
    checkArg(cutoff.time, "integer", len = 1L, lower = 0L, upper = scen$cutoff.time, na.ok = FALSE)
  }
  # cmd = file.path(scen$aclib.dir, scen$algo)

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
  res = trimAndRemoveEmptyLines(res)
  res = splitAndTrim(res, ",")
  as.numeric(res[2L])
}

# ruby ./target_algorithms/sat/scripts/generic_solver_wrapper.rb ./target_algorithms/sat/Solver43 instances/sat/data/K3-inst/k3-v325-c1385/unif-v325-c1385-477-S1889366673.cnf UNSATISFIABLE 300.0 2147483647 7959944 -blocksort '1' -innerlimit '10' -elimbound '0' -definesort '1' -rnd-freq '0' -localtimelimit '5' -R '1.4' -maxcommon '-1' -elimsort '1' -scorefactor '1.5' -numlimit '-1' -K '0.8'



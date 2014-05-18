#' Parses AClib scenario into an S3 object.
#'
#' @param aclib.dir [\code{character(1)}]\cr
#'   File path to AClib installation dir.
#' @param scen.dir [\code{character(1)}]\cr
#'   File path to scenario dir.
#'   Relative to \code{aclib.dir}.
#' @return [\code{AClibScenario}].
#' @export
parseScenario = function(aclib.dir, scen.dir) {
  adir(aclib.dir)
  # FIXME: reldir check in checkmate? so that the string is simply a path? or what?
  astring(scen.dir)

  # read txt file, get s3 object
  s = parseScenarioFile(aclib.dir, scen.dir)
  # construct remaining slots
  s$train.instances = parseInstancesTrain(s)
  s$test.instances = parseInstancesTrain(s)
  s$par.set = parseParamFile(s)
  return(s)
}

#' @export
print.AClibScenario = function(x, ...) {
  for (i in 1:8) {
    catf("%-20s : %s", names(x)[i], x[[i]])
  }
  catf("%-20s : %06i", "train instances", length(x$train.instances))
  catf("%-20s : %06i", "test instances", length(x$train.instances))
}




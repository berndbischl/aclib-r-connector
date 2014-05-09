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
  checkArg(aclib.dir, "character", len = 1L, na.ok = FALSE)
  checkArg(scen.dir, "character", len = 1L, na.ok = FALSE)

  # read txt file, get s3 object
  s = parseScenarioFile(aclib.dir, scen.dir)
  # construct remaining slots
  s$train.instances = parseInstancesTrain(s)
  s$test.instances = parseInstancesTrain(s)
  s$par.set = parseParamFile(s)
  return(s)
}

#' @S3method print AClibScenario
print.AClibScenario = function(x, ...) {
  # print 
  for (i in 1:8) {
    catf("%-20s : %s", names(x)[i], x[[i]])
  }
  catf("%-20s : %06i", "train instances", length(x$train.instances))
  catf("%-20s : %06i", "test instances", length(x$train.instances))
}



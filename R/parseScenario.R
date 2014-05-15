#' Parses AClib scenario into an S3 object.
#'
#' @param aclib.dir [\code{character(1)}]\cr
#'   File path to AClib installation dir.
#' @param scen.dir [\code{character(1)}]\cr
#'   File path to scenario dir.
#'   Relative to \code{aclib.dir}.
#' @return [\code{AClibScenario}].
#' @export
parseScenario = function(aclib.dir, scen.dir, inst.dir) {
  checkArg(aclib.dir, "character", len = 1L, na.ok = FALSE)
  checkArg(scen.dir, "character", len = 1L, na.ok = FALSE)
  checkArg(inst.dir, "character", len = 1L, na.ok = FALSE)
  # read txt file, get s3 object
  s = parseScenarioFile(aclib.dir, scen.dir)
  # construct remaining slots
  pathTest=file.path(aclib.dir, inst.dir, "test.txt")
  pathTrain=file.path(aclib.dir, inst.dir, "training.txt")
  pathFeatures=file.path(aclib.dir,inst.dir, "features.txt")

  s$train.instances = readLines(con=pathTest)
  s$test.instances = readLines(con=pathTrain)
  
  # the first element of "features" contains the parameter names
  features=readLines(con=pathFeatures)
  tempNames=str_split(features[1],",")
  # par.set in the form of a data frame
  s$par.set=data.frame(matrix(0,ncol=length(simplify2array(tempNames))))
  names(s$par.set)=simplify2array(tempNames)
  # Note: this loop takes a long time to finish
  for(i in 1:(length(features)-1))  s$par.set[i,]= simplify2array(str_split(features[i+1],","))
  
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

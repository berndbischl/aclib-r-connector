parseScenario = function(aclib.dir, scen.dir) {
  checkArg(aclib.dir, "character", len = 1L, na.ok = FALSE)
  checkArg(scen.dir, "character", len = 1L, na.ok = FALSE)

  s = parseScenarioFile(aclib.dir, scen.dir)
  s$train.instances = parseInstancesTrain(s)
  s$test.instances = parseInstancesTrain(s)
  s$par.set = parseParamFile(s)
  return(s)
}

print.AClibScenario = function(x, ...) {

  for (i in 1:8) {
    catf("%-20s : %s", names(x)[i], x[[i]])
  }
  catf("%-20s : %04i", "train instances", length(x$train.instances))
  catf("%-20s : %04i", "test instances", length(x$train.instances))
}



# parses instance file paths form txt into a char vector
parseInstances = function(scen, instance.file) {
  checkAClibScenario(scen)
  checkArg(instance.file, choices = c("train.instance.file", "test.instance.file"))

  path = file.path(scen$aclib.dir, scen[[instance.file]])
  readTxtTrimAndRemove(path)
}

parseInstancesTrain = function(scen) parseInstances(scen, "train.instance.file")

parseInstancesTest = function(scen) parseInstances(scen, "test.instance.file")


# parses the main scenario txt file into scenario s3 object
# note that the object is only partially constructed
# the rest is done via parseScenario
parseScenarioFile = function(aclib.dir, scen.dir) {
  checkArg(aclib.dir, "character", len = 1L, na.ok = FALSE)
  checkArg(scen.dir, "character", len = 1L, na.ok = FALSE)

  path = file.path(aclib.dir, scen.dir)
  lines = readLines(path)
  lines = trimAndRemoveEmptyLines(lines)

  # get line which starts with '<prefix> =' and extract 2nd part after '='
  getPart = function(prefix, convert = as.character) {
    prefix = sprintf("%s =", prefix)
    j = str_detect(lines, prefix)
    line = lines[j]
    res = str_split(line, "=")[[1]][2]
    convert(str_trim(res))
  }

  s = makeS3Obj("AClibScenario",
    aclib.dir = aclib.dir,
    algo = getPart("algo"),
    deterministic = getPart("deterministic", as.integer),
    run.obj = getPart("run_obj", ),
    cutoff.time = getPart("cutoff_time", as.integer),
    paramfile = getPart("paramfile"),
    train.instance.file = getPart("instance_file"),
    test.instance.file = getPart("test_instance_file")
  )
  return(s)
}




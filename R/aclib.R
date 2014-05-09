library(stringr)

removeEmptyLines = function(lines) {
  Filter(function(x) str_trim(x) != "", lines)
}

trimLines = function(lines) {
  str_trim(lines)
}

trimAndRemoveEmptyLines = function(lines) {
  removeEmptyLines(trimLines(lines))
}


splitAndTrim = function(x, s, n = Inf, convert = as.character) {
  x = str_split(x, s, n = n)[[1]]
  x = str_trim(x)
  convert(x)
}

removeChars = function(x, chars) {
  for (ch in chars)
    x = str_replace_all(x, ch, "")
  return(x)
}

consume = function(s, regexp) {
  e = str_extract(s, regexp)
  r = str_split(s, e)[[1L]][1L]
  list(match = e, rest = r)
}


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


parseInstancesTrain = function(scen) {
  checkArg(scen, "AClibScenario")
  path = file.path(scen$aclib.dir, scen$train.instance.file)
  lines = readLines(path)
  lines = trimAndRemoveEmptyLines(lines)
  return(lines)
}

parseInstancesTest = function(scen) {
  checkArg(scen, "AClibScenario")
  path = file.path(scen$aclib.dir, scen$test.instance.file)
  lines = readLines(path)
  lines = trimAndRemoveEmptyLines(lines)
  return(lines)
}

parseParamFile = function(scen) {
  checkArg(scen, "AClibScenario")
  path = file.path(scen$aclib.dir, scen$paramfile)
  lines = readLines(path)
  lines = trimAndRemoveEmptyLines(lines)
  index.cond = which(str_detect(lines, "Conditionals:"))
  lines1 = lines[1:(index.cond-1)]
  pars = list()

  for (i in seq_along(lines1)) {
    line = lines[i]
    line = splitAndTrim(line, " ", n = 2L)
    id = line[1L]
    rest = line[-1L]
    if (str_detect(rest, "^\\[.*\\]")) {
      # num or int param
      z = consume(rest, "^\\[.*?\\]")
      bounds = removeChars(z$match, c("\\[", "\\]"))
      bounds = splitAndTrim(bounds, ",", convert = as.numeric)
      if (str_detect(rest, "i"))
        par = makeIntegerParam(id = id, lower = bounds[1L], upper = bounds[2L])
      else
        par = makeNumericParam(id = id, lower = bounds[1L], upper = bounds[2L])
    } else if (str_detect(rest, "^\\{.*\\}")) {
      # discrete
      constraints = str_extract(rest, "^\\{.*\\}")
      values = removeChars(constraints, c("\\{", "\\}"))
      values = splitAndTrim(values, ",")
      par = makeDiscreteParam(id = id, values = values)
    }
    pars[[id]] = par
  }
  pars = makeParamSet(params = pars)
  return(pars)
}



#FIXME: handle seed
runAlgoOnInstance = function(scen, instance, vals, seed = 123L, cutoff.time = 1L) {
  
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

aclib = "/home/bischl/cos/aclib/aclib"
scen = "scenarios/tsp/ACOTSP-RUE-1k-smac/scenario.txt"
s = parseScenarioFile(aclib, scen)
v = sampleValue(s$par.set)
runAlgoOnInstance(s, s$train.instances[1], v)

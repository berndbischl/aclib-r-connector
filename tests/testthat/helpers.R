aclib.dir = "~/cos/aclib"

test.scenario.paths1 = list.files(file.path(aclib.dir, "scenarios"))
test.scenario.paths = sapply(test.scenario.paths1, function(x) {
  ns = list.files(file.path(aclib.dir, "scenarios", x))
  file.path(x, ns)
}, USE.NAMES = FALSE)

test.scenario.paths = unlist(test.scenario.paths)

# FIXME: exlude rissg for now
test.scenario.paths = test.scenario.paths[!str_detect(test.scenario.paths, "riss3g")]


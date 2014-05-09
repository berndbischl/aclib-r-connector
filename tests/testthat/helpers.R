aclib.dir = "~/cos/aclib"

test.scenario.paths1 = list.files(file.path(aclib.dir, "scenarios"))
test.scenario.paths = sapply(test.scenario.paths1, function(x) {
  ns = list.files(file.path(aclib.dir, "scenarios", x)) 
  file.path(x, ns)
}, USE.NAMES = FALSE)




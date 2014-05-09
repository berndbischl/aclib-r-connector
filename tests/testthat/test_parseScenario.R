context("parseScenario")

test_that("parseScenario", {
  
  for (tp in test.scenario.paths) {
    context(sprintf("parseScenario: %s", tp))
    s = parseScenario(aclib.dir, tp)
    expect_is(s, "AClibScenario")
  }
  
})

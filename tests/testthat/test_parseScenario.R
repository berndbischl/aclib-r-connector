context("parseScenario")

test_that("parseScenario", {
  for (tp in test.scenario.paths) {
    
    context(sprintf("parseScenario: %s", tp))
    
    # test that we can parse the scenario
    s = parseScenario(aclib.dir, tp)
    expect_is(s, "AClibScenario")
    ps = s$par.set

    # test that we can generate a design and all config are feasible
    n = 100L
    des = generateDesign(n, par.set = ps)
    xs = dfRowsToList(des, par.set = ps)
    ok = sapply(xs, isFeasible, par = ps)
    expect_true(all(ok))
  }
})

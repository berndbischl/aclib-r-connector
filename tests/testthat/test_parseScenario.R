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

    # run on a few vals to see it that works
    xs = dfRowsToList(des[1:3, ], par.set = ps)
    ys = sapply(xs, runAlgoOnInstance, scen = s, instance = s$train.instances[1L], show.info = FALSE,
      seed = 1, cutoff.time = 1L)

  }
})

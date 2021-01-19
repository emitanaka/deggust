test_that("autoplot", {

  test_that("autoplot graph of edibble variables", {
    des <- edibble::start_design("An awesome design") %>%
      edibble::set_units(mainplot = 4,
                         subplot = edibble::nested_in(mainplot, 2))

    expect_doppelganger("plot design: low-view", autoplot(des, view = "low"))
    expect_doppelganger("plot design: high-view", autoplot(des, view = "high"))
    expect_doppelganger("plot graph: low-view", autoplot(des$graph, view = "low"))
    expect_doppelganger("plot graph: high-view", autoplot(des$graph, view = "high"))
    expect_doppelganger("plot table: low-view", autoplot(serve_table(des)), view = "low")
    expect_doppelganger("plot table: high-view", autoplot(serve_table(des)), view = "high")

  })

})

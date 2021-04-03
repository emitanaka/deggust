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


test_that("proof of concept", {
  unit1 <- start_design(name = "One unit") %>%
    set_trts(trt = 4) %>%
    set_units(unit = 16) %>%
    allocate_trts(trt ~ unit) %>%
    randomise_trts() %>%
    serve_table()

  autoplot(unit1)

  unit2 <- start_design(name = "Two units") %>%
    set_trts(trt = 3) %>%
    set_units(block = 4,
              unit = nested_in(block, 5)) %>%
    allocate_trts(trt ~ unit) %>%
    randomise_trts() %>%
    serve_table()



  # unit3 <- start_design(name = "Three units") %>%
  #   set_trts(trt = 3) %>%
  #   set_units(block = 2,
  #             row = nested_in(block, 4),
  #             plot = nested_in(row, 6)) %>%
  #   allocate_trts(trt ~ plot) %>%
  #   randomise_trts() %>%
  #   serve_table()
})

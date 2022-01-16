test_that("autoplot", {

  test_that("autoplot graph of edibble variables", {
    crd <- make_classical("crd", .output = FALSE)
    autoplot(crd)

    rcbd <- make_classical("rcbd", .output = FALSE)
    autoplot(rcbd)

    lsd <- make_classical("lsd", .output = FALSE)
    autoplot(lsd)



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

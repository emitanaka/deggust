test_that("CRD", {
  crd <- start_design("Completely Randomised Design") %>%
    set_units(unit = 28) %>%
    set_trts(trt = 6) %>%
    allot_trts(trt ~ unit) %>%
    assign_trts("random", seed = 1) %>%
    serve_table()
  expect_doppelganger("crd", autoplot(crd))
})


test_that("2-Factorial", {
  fac2 <- start_design("Factorial Design") %>%
    set_units(unit = 28) %>%
    set_trts(trt1 = 3,
             trt2 = 2) %>%
    allot_trts(trt1:trt2 ~ unit) %>%
    assign_trts("random", seed = 1) %>%
    serve_table()
  expect_doppelganger("fac2", autoplot(fac2))
})

fac3 <- start_design("Factorial Design") %>%
  set_units(unit = 28) %>%
  set_trts(trt1 = 3,
           trt2 = 2,
           trt3 = 2) %>%
  allot_trts(~ unit) %>%
  assign_trts("random", seed = 1) %>%
  serve_table()
g <- autoplot(fac3)

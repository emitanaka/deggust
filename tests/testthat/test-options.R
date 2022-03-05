test_that("plot options", {
  expect_doppelganger("many_trts1", autoplot(takeout(menu_crd(t = 10, r = 2, seed = 1))))
  expect_doppelganger("many_trts2", autoplot(takeout(menu_crd(t = 8, r = 2, seed = 1)),
                                             control = deggust_control(nfill_max = 4)))
  expect_doppelganger("many_units", autoplot(takeout(menu_crd(t = 8, r = 2, seed = 1)),
                                             control = deggust_control(nnode_max = 10)))

})

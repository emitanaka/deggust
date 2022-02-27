test_that("autoplot", {
  expect_doppelganger("bibd", autoplot(takeout(menu_bibd(t = 5, r = 4, k = 3, seed = 1))))
  expect_doppelganger("crd", autoplot(takeout(menu_crd(t = 8, n = 20, seed = 1))))
  expect_doppelganger("fac", autoplot(takeout(menu_factorial(trt = c(2, 4, 3), r = 2, seed = 1))))
  expect_doppelganger("graeco", autoplot(takeout(menu_graeco(t = 7, seed = 1))))
  expect_doppelganger("hyper-graeco", autoplot(takeout(menu_hyper_graeco(t = 4, seed = 1))))
  expect_doppelganger("lsd", autoplot(takeout(menu_lsd(t = 7, seed = 1))))
  expect_doppelganger("rcbd", autoplot(takeout(menu_rcbd(t = 8, r = 2, seed = 1))))
  expect_doppelganger("split", autoplot(takeout(menu_split(t1 = 3, t2 = 4, r = 3, seed = 1))))
  expect_doppelganger("strip", autoplot(takeout(menu_strip(t1 = 3, t2 = 4, r = 3, seed = 1))))
  expect_doppelganger("youden", autoplot(takeout(menu_youden(nc = 3, t = 4, seed = 1))))


})


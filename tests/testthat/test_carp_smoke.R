context("Smoke tests for CARP")

test_that("CARP-VIZ [L2] works", {
  ## Also smoke test status printing here
  expect_no_error(carp_fit <- CARP(presidential_speech, back_track = TRUE, status = TRUE))
  expect_no_error(print(carp_fit))
})

test_that("CARP-VIZ [L1] works", {
  expect_no_error(carp_fit <- CARP(presidential_speech, back_track = TRUE, norm = 1))
  expect_no_error(print(carp_fit))
})

test_that("CARP [L2] works", {
  expect_no_error(carp_fit <- CARP(presidential_speech, back_track = FALSE, t = 1.2))
  expect_no_error(CARP(presidential_speech, back_track = FALSE, t = 1.1))
  expect_no_error(CARP(presidential_speech, back_track = FALSE, t = 1.05))
  expect_no_error(print(carp_fit))
})

test_that("CARP [L1] works", {
  expect_no_error(carp_fit <- CARP(presidential_speech, back_track = FALSE, t = 1.2, norm = 1))
  expect_no_error(CARP(presidential_speech, back_track = FALSE, t =1.1, norm = 1))
  expect_no_error(CARP(presidential_speech, back_track = FALSE, t = 1.05, norm = 1))
  expect_no_error(print(carp_fit))
})

test_that("CARP interactive dendrogram plot works", {
  carp_fit <- CARP(presidential_speech[1:5,1:5])

  ## static
  expect_no_error(print(plot(carp_fit, type = "dendrogram", interactive = T, dynamic = F)))
  expect_no_error(print(plot(carp_fit, type = "dendrogram", interactive = T, dynamic = F, k = 3)))
  expect_no_error(print(plot(carp_fit, type = "dendrogram", interactive = T, dynamic = F, percent = 0.25)))

  ## dynamic
  expect_no_error(print(plot(carp_fit, type = "dendrogram", interactive = T, dynamic = T)))
})

test_that("CARP interactive path plot works", {
  carp_fit <- CARP(presidential_speech[1:5,1:5])

  ## static
  expect_no_error(print(plot(carp_fit, type = "path", interactive = T, dynamic = F)))
  expect_no_error(print(plot(carp_fit, type = "path", interactive = T, dynamic = F, k = 3)))
  expect_no_error(print(plot(carp_fit, type = "path", interactive = T, dynamic = F, percent = 0.25)))

  ## dynamic
  expect_no_error(print(plot(carp_fit, type = "path", interactive = T, dynamic = T)))
})

test_that("CARP interactive heatmap works", {
  carp_fit <- CARP(presidential_speech[1:5,1:5])

  ## dynamic
  expect_no_error(print(plot(carp_fit, type = "heatmap", interactive = T, dynamic = T)))
})

test_that("CARP non-interactive dynamic path plot works", {
  carp_fit <- CARP(presidential_speech)

  expect_no_error(print(plot(carp_fit, type = "path", interactive = F, dynamic = T)))
})

test_that("CARP non-interactive dynamic dendrogram plot works", {
  carp_fit <- CARP(presidential_speech)

  expect_no_error(print(plot(carp_fit, type = "dendrogram", interactive = F, dynamic = T)))
})

test_that("CARP non-interactive heatmap plot works", {
  carp_fit <- CARP(presidential_speech)

  ## static
  expect_no_error(print(plot(carp_fit, type = "heatmap", interactive = F, dynamic = F)))
  expect_no_error(print(plot(carp_fit, type = "heatmap", interactive = F, dynamic = F, k = 3)))
  expect_no_error(print(plot(carp_fit, type = "heatmap", interactive = F, dynamic = F, percent = 0.25)))

  ## dynamic
  expect_no_error(print(plot(carp_fit, type = "heatmap", interactive = F, dynamic = T)))
})

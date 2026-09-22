# assert_no_rows ----

test_that("assert_no_rows() generates an error when `df` is NON-empty", {
  x <- data.frame(x = c(rep(1, 5), 6:10), y = c(rep(11, 3), 14:20))
  expect_error(assert_no_rows(x |> dplyr::filter(y == 14)))
})

test_that("assert_no_rows() does NOT generate an error when `df` is empty", {
  x <- data.frame(x = c(rep(1, 5), 6:10), y = c(rep(11, 3), 14:20))
  expect_no_error(assert_no_rows(x |> dplyr::filter(y == 999)))
})

# assert_unique ----

test_that("assert_unique() generates an error when `df` is NOT unique by a single var", {
  x <- data.frame(x = c(rep(1, 5), 6:10), y = c(rep(11, 3), 14:20))
  expect_error(assert_unique(x, by = "x"))
})

test_that("assert_unique() generates an error when `df` is NOT unique by some combination of vars", {
  x <- data.frame(x = c(rep(1, 5), 6:10), y = c(rep(11, 3), 14:20))
  expect_error(assert_unique(x, by = c("x", "y")))
})

test_that("assert_unique() does NOT generate an error when `df` is unique by a single var", {
  x <- data.frame(x = c(1:10), y = 11:20)
  expect_no_error(assert_unique(x, by = "x"))
})

test_that("assert_unique() does NOT generate an error when `df` is unique by some combination of vars", {
  x <- data.frame(x = c(1:10), y = 11:20)
  expect_no_error(assert_unique(x, by = c("x", "y")))
})

test_that("assert_vars_in_df() generates an error when `df` does not contain a var specified in `vars`", {
  expect_error(
    data.frame(x = letters[1:3], y = 1:3) |> assert_vars_in_df(c("y", "u", "v"))
  )
})

test_that("assert_vars_in_df() generates an error when `df` does not contain a var specified in `vars`", {
  expect_no_error(
    data.frame(x = letters[1:3], y = 1:3, u = 4:6, v = 7:9) |>
      assert_vars_in_df(c("y", "u", "v"))
  )
})

test_that("assert_vars_in_df() generates errors with bad inputs", {
  expect_error(
    letters[1:3] |>
      assert_vars_in_df(c("x"))
  )

  expect_error(
    data.frame(x = letters[1:3]) |>
      assert_vars_in_df(1)
  )

  expect_error(
    data.frame(x = letters[1:3]) |>
      assert_vars_in_df(c())
  )
})

test_that("subsets work", {
  it <- iterstring("My name is Jacob.")
  expect_equal(it[0], iterstring(''))
  expect_equal(it[2], iterstring('y'))
  expect_equal(it[1:10], iterstring('My name is'))
  expect_equal(it[-3], iterstring('o'))
  expect_equal(it[-3:-1], iterstring('ob.'))
  expect_equal(it[100], iterstring(''))
  expect_is(it[3], 'iterstring')
})

test_that("extraction works", {
  it <- iterstring("My name is Jacob.")
  expect_equal(it[[0]], '')
  expect_equal(it[[4]], 'n')
  expect_error(it[[4:6]])
  expect_equal(it[[-3]], 'o')

})

test_that("assignment works", {
  it <- iterstring("My name is Jacob.")
  it[1:2] <- 'no'
  it[-3:-1] <- 'mmm'
  it[[3]] <- '_'
  expect_equal(it, iterstring('no_name is Jacmmm'))
})

test_that("islists work with character vecs", {
  vec <- c("my", "name", "is", "Jacob")
  isvec <- char_to_islist(vec)
  revec <- as.character(isvec)
  expect_is(isvec, 'islist')
  expect_is(revec, 'character')
  expect_equal(revec, vec)
  expect_is(isvec[[1]], 'iterstring')
  expect_equal(isvec[[1]], iterstring(revec[1]))
})

test_that("along and print work",{
  it <- iterstring("my name is Jacob")
  list <- char_to_islist(c("my", "name", "is"))
  expect_equal(char_along(it), 1:16)
  expect_output(print(it), 'my name is Jacob')
})

test_that("insertion works", {
  it <- iterstring("My name is Jacob.")
  it1 <- insert(it, "yes", 0)
  it2 <- insert(it, "", 1)
  it4 <- insert(it, "yes", 2)
  it5 <- insert(it, "yes", 17)
  it6 <- insert(it, "yes", -17)
  it7 <- insert(it, "yes", -1)
  it8 <- insert(it, "yes", -0L)
  expect_equal(it1, iterstring("yesMy name is Jacob."))
  expect_equal(it2, iterstring("My name is Jacob."))
  expect_error(insert(it, "yes", 190))
  expect_equal(it4, iterstring("Myyes name is Jacob."))
  expect_equal(it5, iterstring("My name is Jacob.yes"))
  expect_equal(it6, iterstring("Myesy name is Jacob."))
  expect_equal(it7, iterstring("My name is Jacob.yes"))
  expect_equal(it8, it1)
})

test_that("c() works fine", {
  it <- iterstring("My name is Jacob.")
  it2 <- iterstring(" Yes it is.")
  it3 <- c(it, it2)
  it4 <- c(it, "")
  it5 <- c(iterstring(""), it)
  expect_equal(it3, iterstring("My name is Jacob. Yes it is."))
  expect_equal(it4, it)
  expect_equal(it5, it)
})


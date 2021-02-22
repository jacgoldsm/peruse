test_that("prime_numbers", {
  expect_equal((2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0))[7],
               17L)
})

test_that("prime numbers with logical coercion", {
  expect_equal((2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y))[7],
               17L)
})

test_that("prime_within_two_of_square_number", {
  expect_equal(((2:100 %>% that_for_all(range(2,.x)) %>% we_have(~.x %% .y != 0)) %>%
                               that_for_any(range(2, .x)) %>% we_have(~sqrt(.x + 2) == .y | sqrt(.x - 2) == .y))[2],
          11L)
})

test_that("collatz", {
   collatz <- Iterator({
                if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1
              },
              initial = list(n = 50),
              yield = n)

   expect_equal(yield_next(collatz), 25L)
})

test_that("prime_iter", {
  primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0, "Iterator")
  expect_equal(yield_more(primes, 2), c(2L, 3L))
 })

test_that("grepl_set", {
  expect_equal({
    c("I", "Don't", "wan't", "chicken") %>%
                               that_for_all("\'") %>%
                               we_have(~grepl(.y, .x))},
    c("Don't", "wan't"))
})

test_that("is_Iterator", {
  collatz <- Iterator({
              if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1
             },
             initial = list(n = 50),
             yield = n);
  expect_true(is_Iterator(collatz))
})

test_that("range",{
  expect_equal(range(0, 100, by = 2L), seq(0,99, by = 2L))
  })

test_that("range_empty",{
  expect_equal(range(100, 100, by = 2L), numeric())
})

test_that("yield_more",  {
  expr <- quote( if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1 )
  collatz <- Iterator(result = !!expr,
                      initial = list(n = 50),
                      yield = n)
  expect_equal(yield_more(collatz, 10), c(25,76,38,19,58,29,88,44,22,11))
})

test_that("current",  {
  expr <- quote(if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1)
  collatz <- Iterator(result = !!expr,
                      initial = list(n = 50),
                      yield = n)
  expect_equal(current(collatz), 50L)
})



test_that("move_more", {
   expr <- quote(if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1)
   collatz <- Iterator(result = !!expr,
                       initial = list(n = 50),
                       yield = n);
   move_more(collatz, 10)
   expect_equal(current(collatz), 11L)
})

test_that("yield_while", {

  collatz <- Iterator({
              if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1
             },
             initial = list(n = 50),
             yield = n)
  expect_equal(yield_while(collatz, n != 1L),
               c(25, 76, 38, 19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10,  5, 16,  8,  4,  2,  1))
})

test_that("Iterators work with environment variables", {
  m <- 100
  n <- 10
  expr <- quote(out <- n + m)
  it <- Iterator(result = !!expr,
                 initial = list(n = -10),
                 yield = out)
  one <- yield_more(it, 5)
  expect_equal(one, rep(90L, 5))
})

test_that("clone creates a copy", {
  it <- Iterator({m <- m + n}, list(m = 0, n = 1), m)
  other <- clone(it, m = 100, n = 100)
  yield_next(it)
  expect_equal(current(other) == current(it), FALSE)
  a <- yield_next(other)
  expect_equal(a, 200)
})

test_that("stochastic functions work properly", {
  p_success <- 0.8
  threshold <- 100

  iter <- Iterator({
            set.seed(seeds[.iter])
            n <- n + sample(c(1,-1), 1, prob = c(p_success, 1 - p_success))
          },
          list(n = 0, seeds = 1000:1e6),
          n)

  sequence <- yield_while(iter, n <= threshold)
  expect_equal(sequence[1:4], c(1,0,1,2))
})

test_that("yield_while() can see `.iter`", {
  expr <- quote({m <- m + 1})
  it <- Iterator(!!expr, list(m = 0), m)
  sequence <- yield_while(it, .iter < 5)
  expect_equal(sequence, c(1,2,3,4))
})

test_that("yield_while() can see environment variables", {
  expr <- quote({m <- m + 1})
  r <- 5
  it <- Iterator(!!expr, list(m = 0), m)
  sequence <- yield_while(it, .iter < r)
  expect_equal(sequence, c(1,2,3,4))
})

test_that("Sequence ends work right with `yield_while()", {
  primes_100 <- 2:100 %>%
    that_for_all(range(2, .x)) %>%
    we_have(~.x %% .y, "Iterator")
  primes_100_2 <- clone(primes_100)
  primes_100_3 <- clone(primes_100)
  expect_equal(yield_while(primes_100, .x_vector[.i] <= 100),
               yield_while(primes_100_2, T))
  expect_equal(yield_while(primes_100_3, T),
               2:100 %>%
                 that_for_all(range(2, .x)) %>%
                 we_have(~.x %% .y))
  expect_error(yield_next(primes_100))
})

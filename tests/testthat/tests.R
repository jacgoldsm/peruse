test_that("prime_numbers", {
  expect_equal((2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0))[7],
               17L)
})

test_that("prime_within_two", {
          expect_equal(((2:100 %>% that_for_all(range(2,.x)) %>% we_have(~.x %% .y != 0)) %>%
                               that_for_any(range(2, .x)) %>% we_have(~sqrt(.x + 2) == .y | sqrt(.x - 2) == .y))[2],
          11L)
})

test_that("collatz", { .yieldenv <- new.env(parent = emptyenv())
          expect_equal({expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1";
           collatz <- Iterator(result = expr,
                             initial = c(n = 50),
                              yield = n);
            yield_next(collatz)},
          25L)
})

test_that("prime_iter", {
 expect_equal({
   primes <- 2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0, "Iterator")
   yield_next(primes)},
   2L)
 })

test_that("grepl_set", {
  expect_equal({
    c("I", "Don't", "wan't", "chicken") %>%
                               that_for_all("\'") %>%
                               we_have(~grepl(.y, .x))},
    c("Don't", "wan't"))
})

test_that("is_Iterator", { .yieldenv <- new.env(parent = emptyenv())
expect_true({expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1";
collatz <- Iterator(result = expr,
                    initial = c(n = 50),
                    yield = n);
is_Iterator(collatz)})
})

test_that("range",{
          expect_equal(
            range(0, 100, by = 2L),
            seq(0,99, by = 2L)
          )
  })

test_that("range_empty",{
  expect_equal(
    range(100, 100, by = 2L),
    numeric()
  )
})

test_that("yield_more",  {
               expect_equal({expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1";
               .yieldenv <- new.env(parent = emptyenv())
               collatz <- Iterator(result = expr,
                                   initial = c(n = 50),
                                   yield = n)
               yield_more(collatz, 10)},
               c(25,76,38,19,58,29,88,44,22,11))
})

test_that("current",  {
  expect_equal({
  .yieldenv <- new.env(parent = emptyenv());
  expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1";
  collatz <- Iterator(result = expr,
                      initial = c(n = 50),
                      yield = n);
  current(collatz)},
  50L)
})



test_that("move_more", { .yieldenv <- new.env(parent = emptyenv())
expect_equal({expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1";
collatz <- Iterator(result = expr,
                    initial = c(n = 50),
                    yield = n);
move_more(collatz, 10);
current(collatz)},
11L)
})

test_that("yield_while", { .yieldenv <- new.env(parent = emptyenv())
expect_equal({expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1";
collatz <- Iterator(result = expr,
                    initial = c(n = 50),
                    yield = n);
yield_while(collatz, "n != 1L")},
c(25, 76, 38, 19, 58, 29, 88, 44, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10,  5, 16,  8,  4,  2,  1))
})




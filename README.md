
<!-- README.md is generated from README.Rmd. Please edit that file -->

# itertools

<!-- badges: start -->

<!-- badges: end -->

The {itertools} package is aimed at making it easier to generate
irregular sequences that are difficult to generate with existing tools.
The heart of {itertools} is the `S3` class `Iterator`. An `Iterator`
allows the user to write an arbitrary R expression that returns the next
element of a sequence of R objects. It then saves the state of the
`Iterator`, meaning the next time `yield_next` is called, the subsequent
element of the sequence will be returned.

The package also provides a simple, tidy API for set building, allowing
the user to generate a set consisting of the elements of a vector that
meet specific criteria. This can either return a vector consisting of
all the chosen elements or it can return an `Iterator` that lazily
generates the chosen elements.

Finally, {itertools} also provides a new data structure stores a
`data.frame` as an object with reference semantics and `O(1)` access to
columns. This is useful when iterating over `data.frame`s with many
columns, because the object is modified in place, rather than making a
shallow copy on every iteration.

## Installation

You can install the released version of itertools from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("itertools")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jacgoldsm/itertools")
```

## Example

### Collatz Sequence

A Collatz sequence is a particular sequence of natural numbers that
mathematicians think always reaches \(1\) at some point, no matter the
starting point. We can’t prove that one way or the other, but we can
create an `Iterator` that lazily generates a Collatz sequence until it
reaches \(1\):

``` r
library(itertools)
#> 
#> Attaching package: 'itertools'
#> The following object is masked from 'package:base':
#> 
#>     range
library(magrittr)
expr <- "if (n %% 2 == 0) n <- n / 2 else n <- n*3 + 1"
  
# Collatz generator starting at 50
collatz <- Iterator(result = expr,
                    initial = c(n = 50),
                    yield = n)

i <- 0
while (i != 1L) {
  i <- yield_next(collatz)
  cat(paste0(i, "\n"))
}
#> 25
#> 76
#> 38
#> 19
#> 58
#> 29
#> 88
#> 44
#> 22
#> 11
#> 34
#> 17
#> 52
#> 26
#> 13
#> 40
#> 20
#> 10
#> 5
#> 16
#> 8
#> 4
#> 2
#> 1
```

### Primes

How about generating all the prime numbers between \(1\) and \(100\)? We
can easily do that with the set-builder API:

``` r
cat(2:100 %>% that_for_all(range(2, .x)) %>% we_have(~.x %% .y != 0))
#> 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97
```

But how about if we want to generate the first \(100\) prime numbers? We
don’t know the range of values this should fall in (well, mathematicians
do), so we can use laziness to our advantage:

``` r
primes <- 2:10000 %>%
            that_for_all(range(2, .x)) %>% 
            we_have(~.x %% .y != 0, "Iterator")

sequence <- c()
while (length(sequence) <= 100) {
  sequence <- c(sequence, yield_next(primes))
}

cat(sequence)
#> 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547
```

### hash\_df

When dealing with text data, it is common to run models on a very wide
data set, with thousands of variables representing each token. Doing
transformations on a wide data set can be very expensive, as every time
a column is transformed, a shallow copy is made of the `data.frame`.

Here is an example of a wide data set representing the reviews of a
great variety of wines. Each column represents a token, and the values
represent the raw count of the number of times the word appears in a
given review.

    #>   word_2015 word_a word_accents word_and word_blackberry
    #> 1         1      2            1        4               1
    #> 2         0      1            0        2               0
    #> 3         0      0            0        1               0
    #> 4         0      0            0        2               0
    #> 5         0      1            0        3               0

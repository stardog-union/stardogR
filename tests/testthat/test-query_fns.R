
## Following examples based on https://r-pkgs.org/testing-basics.html

## Examples:

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

# test_that("basic duplication works", {
#   expect_equal(str_dup("a", 3), "aaa")
#   expect_equal(str_dup("abc", 2), "abcabc")
#   expect_equal(str_dup(c("a", "b"), 2), c("aa", "bb"))
#   expect_equal(str_dup(c("a", "b"), c(2, 3)), c("aa", "bbb"))
# })
# #> Test passed 🎉
# 
# test_that("0 duplicates equals empty string", {
#   expect_equal(str_dup("a", 0), "")
#   expect_equal(str_dup(c("a", "b"), 0), rep("", 2))
# })
# #> Test passed 🎉
# 
# test_that("uses tidyverse recycling rules", {
#   expect_error(str_dup(1:2, 1:3), class = "vctrs_error_incompatible_size")
# })
# #> Test passed 🌈


## without brace lets you get the rest result immediately, whereas using braces is recommended.
## eg. without brace
# test_that("this error message appears",
#   expect_error(
#     stringr::str_dup(1:2, 1:3) , "Can't recycle"
#   )
# )
# 
# ## eg. with braces
# test_that("this error message appears", {
#           expect_error(
#             stringr::str_dup(1:2, 1:3) , "Can't recycle"
#           )
# })

## -----------------------------

## default connection for reads:
# stardog = list(
#   endpoint = "https://express.stardog.cloud:5820",
#   username = "anonymous",
#   password = "anonymous", 
#   database = "marketplace"
# )

########################### select()
test_that("select count(*) query returns a 1 obs 1 var df with integer value", {
  stardog = list(
    endpoint = "https://express.stardog.cloud:5820",
    username = "anonymous",
    password = "anonymous",
    database = "marketplace"
  )
  x = select(stardog, q = 'select (count(*) as ?n) {?s ?p ?o .}')
  expect_equal(x %>% class , "data.frame")
  expect_equal(x %>% nrow() , 1)
  expect_equal(x %>% length() , 1)
  expect_equal(x %>% names() , "n")
  expect_equal(x %>% .[["n"]] %>% .[[1]] %>% class() , "integer")
})
## Test passed 🎉


test_that("select spo query returns a 3 var df", {
  stardog = list(
    endpoint = "https://express.stardog.cloud:5820",
    username = "anonymous",
    password = "anonymous",
    database = "marketplace"
  )
  x = select(stardog, q = 'select * {?s ?p ?o .} limit 10')
  expect_equal(x %>% class , "data.frame")
  expect_equal(x %>% length() , 3)
  expect_equal(x %>% names() , c("s", "p", "o"))
})
## Test passed 🎉

## To do: 

## test the namedGraph arg functionality eg. namedGraph = "urn:demo:healthcare:model"

## test error returned for non-supported query options

## test remote error messages are returned when Stardog raises an error

## test http response codes are ... available? returned upon failure / non 200 values? 


###################################### ask()



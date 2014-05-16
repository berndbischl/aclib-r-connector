library(methods)
library(devtools)
library(testthat)
library(BBmisc)
library(stringr)

if (interactive()) {
  load_all(".", reset=TRUE)
} else {
  library(aclibr)
}
test_dir("tests/testthat")



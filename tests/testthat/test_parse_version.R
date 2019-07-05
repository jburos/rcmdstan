
library(testthat)
library(rcmdstan)
testthat::context('installation')

testthat::test_that('Default version returns sensible response', {
  ver <- rcmdstan:::parse_cmdstanpy_version('default')
  testthat::expect_equal(ver$version, 'master')
})

testthat::test_that('Can pass git+git:// string as version', {
  ver <- rcmdstan:::parse_cmdstanpy_version('git+git://github.com/stan-dev/cmdstanpy@dev')
  testthat::expect_true(is.na(ver$version))
  testthat::expect_equal(ver$package, 'git+git://github.com/stan-dev/cmdstanpy@dev')
})

testthat::test_that('`git+` prefix added to git:// version string', {
  ver <- rcmdstan:::parse_cmdstanpy_version('git://github.com/stan-dev/cmdstanpy@dev')
  testthat::expect_true(is.na(ver$version))
  testthat::expect_equal(ver$package, 'git+git://github.com/stan-dev/cmdstanpy@dev')
})

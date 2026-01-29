# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(raquarius)
set_state_inspector(
  function() {
    vars <- c(
      "AQUARIUS_URL", "AQUARIUS_USER", "AQUARIUS_PW", "",
      "AQUARIUS_WEBPORTAL_PW", "AQUARIUS_WEBPORTAL_URL", "AQUARIUS_WEBPORTAL_USER"
    )
    list(
      envvar = Sys.getenv(vars)
    )
  }
)
test_check("raquarius")

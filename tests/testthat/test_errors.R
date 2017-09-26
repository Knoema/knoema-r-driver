context("test dataset id is null error")
test_that("test dataset id is null error",{
  expect_error(Knoema(NULL), "dataset.id should be a string. Can't be NULL")
})

context("test dataset id is double error")
test_that("test dataset id is double error",{
  expect_error(Knoema(123), "dataset.id should be a string. Can't be double")
})

context("test selection is not specified error")
test_that("test selection is not specified error",{
  expect_error(Knoema("IMFWEO2017Apr", client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU"), 'argument "selection" is missing, with no default')
})

context("test wrong dimension error")
test_that("test wrong dimension error error",{
  expect_error(Knoema("IMFWEO2017Apr", list(indicator = "LP;NGDP")), "Dimension with id or name indicator is not found")
})

context("test empty dimension selection error")
test_that("test empty dimension selection error",{
  expect_error(Knoema("IMFWEO2017Apr", list(country = "", subject = "LP;NGDP"), client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  , "Selection for dimension Country is empty")
})

context("test wrong dimension selection error")
test_that("test wrong dimension selection error",{
  expect_error(Knoema("IMFWEO2017Apr", list(country = "914;512;111", subject = "L1P;N1GDP"), client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
               ,'Selection for dimension Subject is empty')
})

context("test incorrect dataset id error")
test_that("test incorrect dataset id error",{
  expect_error(Knoema("incorrect id", list(domedim = "val1;val2"), client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU"),"Requested dataset doesn't exist or you don't have access to it.")
  })

context("test incorrect client.id and client.secret error")
test_that("test incorrect client.id and client.secret error",{
  expect_error(Knoema("IMFWEO2017Apr", list(country = "914;512;111", subject = "LP;NGDP"), client.id = "b", client.secret = "s"),"Client error: (403) Forbidden", fixed = TRUE)
})

context("test not all dims in filter error")
test_that("test not all dims in filter error", {
  expect_error(Knoema("bmlaaaf", list("Country"= "Albania",
                                          "Borrower" = 'Ministry of Finance',
                                          "Guarantor"= "Albania",
                                          "Loan type"= "B loan",
                                          "Loan status"= "EFFECTIVE"),
                          client.id = "bHcV5UkOVyKcBw",
                          client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU"),
"The following dimension(s) are not set: Currency of Commitment,Measure", fixed=TRUE)
})

context("test incorect frequencies error")
test_that("test incorect frequencies error",{
  expect_error(Knoema("IMFWEO2017Apr", list(country = "914;512;111", subject = "LP;NGDP", frequency = "A;G;R"), client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
               , "The following frequencies are not correct: G,R", fixed = TRUE)
})

context("test incorect host error")
test_that("test incorect host error",{
  expect_error(Knoema("IMFWEO2017Apr", list(country = "914;512;111", subject = "LP;NGDP", frequency = "A"), host = 'knoema_incorect.com', client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
               , "The specified host knoema_incorect.com does not exist", fixed = TRUE)
})

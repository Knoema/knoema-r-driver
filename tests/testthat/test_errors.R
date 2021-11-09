context("test dataset id is null error")
test_that("test dataset id is null error",{
  expect_error(Knoema(NULL), "dataset.id should be a string. Can't be NULL")
})

context("test dataset id is double error")
test_that("test dataset id is double error",{
  expect_error(Knoema(123), "dataset.id should be a string. Can't be double")
})

context("test selection and mnemonics in one call error")
test_that("test selection and mnemonics in one call error",{
  expect_error(Knoema("IMFWEO2017Oct", list(country = "912", subject = "LP;NGDP"), mnemonics = "test", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"), 'The function does not support specifying mnemonics and selection in a single call')
})

context("test wrong dimension error")
test_that("test wrong dimension error error",{
  expect_error(Knoema("IMFWEO2017Oct", list(indicator = "LP;NGDP"), client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"), "Dimension with id or name indicator is not found")
})

context("test empty dimension selection error")
test_that("test empty dimension selection error",{
  expect_error(Knoema("IMFWEO2017Oct", list(country = "", subject = "LP;NGDP"), client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  , "Selection for dimension Country is empty")
})

context("test wrong dimension selection error")
test_that("test wrong dimension selection error",{
  expect_error(Knoema("IMFWEO2017Oct", list(country = "914;512;111", subject = "L1P;N1GDP"), client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
               ,'Selection for dimension Subject is empty')
})

context("test incorrect dataset id error")
test_that("test incorrect dataset id error",{
  expect_error(Knoema("incorrect_id", list(domedim = "val1;val2"), client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"),"Requested dataset doesn't exist or you don't have access to it.")
  })

context("test incorrect client.id and client.secret error")
test_that("test incorrect client.id and client.secret error",{
  expect_error(Knoema("IMFWEO2017Oct", list(country = "914;512;111", subject = "LP;NGDP"), client.id = "b", client.secret = "s"),"Client error: (403) Forbidden", fixed = TRUE)
})

context("test incorrect frequencies error")
test_that("test incorrect frequencies error",{
  expect_error(Knoema("IMFWEO2017Oct", list(country = "914;512;111", subject = "LP;NGDP", frequency = "A;G;R"), client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
               , "The following frequencies are not correct: G,R", fixed = TRUE)
})

context("test incorrect host error")
test_that("test incorrect host error",{
  expect_error(Knoema("IMFWEO2017Oct", list(country = "914;512;111", subject = "LP;NGDP", frequency = "A"), host = 'knoema_incorrect.com', client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
               , "The specified host knoema_incorrect.com does not exist", fixed = TRUE)
})

context("test unknown type error")
test_that("test unknown type error",{
  expect_error(Knoema("IMFWEO2017Oct", list(country = "914;512;111", subject = "LP;NGDP", frequency = "A"), type = "incorrect_type", host = 'knoema.com', client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
               , "Unknown type incorrect_type", fixed = TRUE)
})

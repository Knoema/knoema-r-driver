context("get data from flat dataset ts error")
test_that("get data from flat dataset ts error",{
  expect_error(Knoema("zvmljnd", list(Country="Australia"), client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"),
               "Types ts, xts, zoo are not supported for flat datasets", fixed = TRUE)
})

context("get data from flat dataset xts error")
test_that("get data from flat dataset xts error",{
  expect_error(Knoema("zvmljnd", list(Country="Australia"), type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"),
               "Types ts, xts, zoo are not supported for flat datasets", fixed = TRUE)
})

context("get data from flat dataset zoo error")
test_that("get data from flat dataset zoo error",{
  expect_error(Knoema("zvmljnd", list(Country="Australia"), type = "zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"),
               "Types ts, xts, zoo are not supported for flat datasets", fixed = TRUE)
})

context("get data from flat dataset DataFrame")
test_that("get data from flat dataset DataFrame",{
  data_frame = Knoema("zvmljnd", list(Country="Australia"), type = "DataFrame", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame), 1)
  sname = "Australia - Asia-Pacific - Sydney -  - D"
  expect_equal(data_frame[["All time",sname]], 1)
  sname ="Australia - Asia-Pacific - Sydney -  - D"
  expect_equal(data_frame[["All time",sname]], 1)
})

context("get data from flat dataset MetaDataFrame")
test_that("get data from flat dataset MetaDataFrame",{
  data_frame = Knoema("zvmljnd", list(Country="Australia"), type = "MetaDataFrame", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame), 1)
  sname = "Australia - Asia-Pacific - Sydney -  - D"
  expect_equal(data_frame[["Unit",sname]], "# of records")
  sname ="Australia - Asia-Pacific - Sydney -  - D"
  expect_equal(data_frame[["Scale",sname]], "1")
})

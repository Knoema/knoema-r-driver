context("get data from flat dataset ts error")
test_that("get data from flat dataset ts error",{
  expect_error(Knoema("cblymmf", list(Country="Albania;Australia", Keyword="FGP;TWP;TRP"), client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"),
  "Types ts, xts, zoo are not supported for flat datasets", fixed = TRUE)
})

context("get data from flat dataset xts error")
test_that("get data from flat dataset xts error",{
  expect_error(Knoema("cblymmf", list(Country="Albania;Australia", Keyword="FGP;TWP;TRP"), type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"),
    "Types ts, xts, zoo are not supported for flat datasets", fixed = TRUE)
})

context("get data from flat dataset zoo error")
test_that("get data from flat dataset zoo error",{
  expect_error(Knoema("cblymmf", list(Country="Albania;Australia", Keyword="FGP;TWP;TRP"), type = "zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA"),
    "Types ts, xts, zoo are not supported for flat datasets", fixed = TRUE)
})

context("get data from flat dataset DataFrame")
test_that("get data from flat dataset DataFrame",{
    data_frame = Knoema("cblymmf", list(Country="Albania;Australia", Keyword="FGP;TWP;TRP"), type = "DataFrame", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
    expect_equal(length(data_frame), 4)
    sname = "Albania - FGP - D"
    expect_equal(data_frame[["All time",sname]], 8)
    sname ="Australia - TWP - D"
    expect_equal(data_frame[["All time",sname]], 8)
})

context("get data from flat dataset MetaDataFrame")
test_that("get data from flat dataset MetaDataFrame",{
  data_frame = Knoema("cblymmf", list(Country="Albania;Australia", Keyword="FGP;TWP;TRP"), type = "MetaDataFrame", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame), 4)
  sname = "Albania - FGP - D"
  expect_equal(data_frame[["Unit",sname]], "# of records")
  sname ="Australia - TWP - D"
  expect_equal(data_frame[["Scale",sname]], "1")
})

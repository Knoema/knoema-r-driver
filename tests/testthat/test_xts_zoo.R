
context("get data single series by member id xts")
test_that("get data single series by member id xts",{
  data_frame = Knoema("IMFWEO2017Oct", list(country="914", subject="lp"), type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(ncol(data_frame),1)
  time_ser = data_frame$"Albania - Population (Persons) - A"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 2.762)
})

context("get data multi series by member id zoo")
test_that("get data multi series by member id zoo",{
  data_frame = Knoema("IMFWEO2017Oct", list(country="914;512;111", subject="lp;ngdp"), type= "zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 2862.475)
  value = coredata(time_ser[as.yearqtr(as.Date("2017-01-01"))])[1]
  expect_equal(value, 19362.129)
})

context("get data multi series by member name xts")
test_that("get data multi series by member name xts",{
  subj_names = "Gross domestic product, current prices (National currency);population (persons)"
  data_frame = Knoema("IMFWEO2017Oct", list(country = "albania;afghanistan;united states", subject=subj_names), type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(ncol(data_frame),6)
  time_ser = data_frame$"United States - Gross domestic product, current prices (National currency) - A"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 2862.475)
  value = coredata(time_ser[as.yearqtr(as.Date("2022-01-01"))])[1]
  expect_equal(value, 23505.309)
})

context("get data multi series by member id range zoo")
test_that("get data multi series by member id range zoo",{
  data_frame = Knoema("IMFWEO2017Oct", list(country = "914;512;111", subject = "lp;ngdp", timerange = "2015-2020"), type = "zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 18120.70)
  value = coredata(time_ser[as.yearqtr(as.Date("2020-01-01"))])[1]
  expect_equal(value, 21846.33, tolerance = 0.01)
})

context("get data single series different frequencies by member id xts")
test_that("get data single series different frequencies by member id xts",{
  data_frame = Knoema('eqohmpb', list(country='512', Indicator='NGDP'), type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(ncol(data_frame),6)
  time_ser = data_frame$"Afghanistan - Gross domestic product, current prices - M"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 26.02858, tolerance = 0.0001)
  value = coredata(time_ser["2003-02-01"])[1]
  expect_equal(value, 80.71440, tolerance = 0.0001)

  time_ser = data_frame$"Afghanistan - Gross domestic product, current prices - Q"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser["2002-07-01"])[1]
  expect_equal(value, 4)
  value = coredata(time_ser["2003-04-01"])[1]
  expect_equal(value, 5)
})

context("get data multi series single frequency by member id zoo")
test_that("get data multi series single frequency by member id zoo",{
  data_frame = Knoema('eqohmpb', list(country='512', Indicator='NGDP;NGDP_D', frequency='M'), type="zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame),2)

  sname = "M - Afghanistan - Gross domestic product, current prices"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 26.02858, tolerance = 0.0001)
  value = coredata(time_ser[as.yearmon(as.Date("2003-02-01"))])[1]
  expect_equal(value, 80.71440, tolerance = 0.0001)

  sname = "M - Afghanistan - Gross domestic product, deflator"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2003-01-01"))])[1]
  expect_equal(value, 16)
})

context("get data multi series multi frequency by member id xts")
test_that("get data multi series multi frequency by member id xts",{
  data_frame = Knoema('eqohmpb', list(country='512', Indicator='NGDP;NGDP_D', frequency='A;M'),  type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(ncol(data_frame),4)
  time_ser = data_frame$"Afghanistan - Gross domestic product, current prices - M"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 26.02858, tolerance = 0.0001)
  value = coredata(time_ser[as.yearmon(as.Date("2003-02-01"))])[1]
  expect_equal(value, 80.71440, tolerance = 0.0001)

  time_ser = data_frame$"Afghanistan - Gross domestic product, deflator - A"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2003-01-01"))])[1]
  expect_equal(value, 110.706)

})

context("get data multi series multi frequency by member id range zoo")
test_that("get data multi series multi frequency by member id range zoo",{
  data_frame = Knoema('eqohmpb', list(country='512', Indicator='NGDP;NGDP_D', frequency='A;M', timerange = '2004M1-2006M12'), type = "zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame),4)

  sname = "M - Afghanistan - Gross domestic product, current prices"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2004-01-01"))])[1]
  expect_equal(value, 90.82493, tolerance = 0.0001)

  sname = "A - Afghanistan - Gross domestic product, deflator"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2004-01-01"))])[1]
  expect_equal(value, 123.063)
})

context("get data from dataset with multiword dimnames xts")
test_that("get data from dataset with multiword dimnames xts",{
  data_frame = Knoema('eqohmpb', list("Country"='512', "Indicator"='NGDP;NGDP_D', frequency='A;M'),  type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(ncol(data_frame),4)

  time_ser = data_frame$"Afghanistan - Gross domestic product, current prices - M"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 26.02858, tolerance = 0.0001)
  value = coredata(time_ser[as.yearmon(as.Date("2003-02-01"))])[1]
  expect_equal(value, 80.71440, tolerance = 0.0001)

  time_ser = data_frame$"Afghanistan - Gross domestic product, deflator - A"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2003-01-01"))])[1]
  expect_equal(value, 110.706)
})

context("get data multi series by member key zoo")
test_that("get data multi series by member key zoo",{

  data_frame = Knoema("IMFWEO2017Oct", list(country = "1000010;1000000;1001830", subject = "1000370;1000040"), type = "zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame),6)

  sname = "A - Afghanistan - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 178.756)
  value = coredata(time_ser[as.yearmon(as.Date("2012-01-01"))])[1]
  expect_equal(value, 1033.591)
})

context("get data from dataset by dim ids xts")
test_that("get data from dataset by dim ids xts",{
  data_frame = Knoema('eqohmpb', list("country"='512', "indicator"='NGDP;NGDP_D', frequency='A;M'),  type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(ncol(data_frame),4)

  time_ser = data_frame$"Afghanistan - Gross domestic product, current prices - M"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 26.02858, tolerance = 0.0001)
  value = coredata(time_ser[as.yearmon(as.Date("2003-02-01"))])[1]
  expect_equal(value, 80.71440, tolerance = 0.0001)

  time_ser = data_frame$"Afghanistan - Gross domestic product, deflator - A"
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2003-01-01"))])[1]
  expect_equal(value, 110.706)
})

context ("get series from dataset by partial selection xts")
test_that("get series from dataset by partial selection xts",{
  data_frame = Knoema('IMFWEO2017Oct', list(subject = 'flibor6'), type = "xts", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(ncol(data_frame),2)

  time_ser = data_frame$"Japan - Six-month London interbank offered rate (LIBOR) (Percent) - A"
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(coredata(time_ser)[1], 10.861)
  expect_equal(coredata(time_ser)[39], 0.178)
})


context ("get series from dataset by partial selection zoo")
test_that("get series from dataset by partial selection zoo",{
  data_frame = Knoema('IMFWEO2017Oct', list(subject = 'flibor6'),type = "zoo", client.id = "FzOYqDg", client.secret="SPrvmY8eGRcGA")
  expect_equal(length(data_frame),2)

  sname = "A - Japan - Six-month London interbank offered rate (LIBOR) (Percent)"

  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 10.861)
  expect_equal(coredata(time_ser)[39], 0.178)
})

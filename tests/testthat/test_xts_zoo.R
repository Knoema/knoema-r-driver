
context("get data single series by member id xts")
test_that("get data single series by member id xts",{
  data_frame = Knoema("IMFWEO2017Apr", list(country="914", subject="lp"), type = "xts", client.id="bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),1)

  sname = "A - Albania - Population (Persons)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 2.762)
})

context("get data multi series by member id zoo")
test_that("get data multi series by member id zoo",{
  data_frame = Knoema("IMFWEO2017Apr", list(country="914;512;111", subject="lp;ngdp"), type= "zoo", client.id="bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 2862.475)
  expect_equal(coredata(time_ser)[length(time_ser)], 23760.331)
})

context("get data multi series by member name xts")
test_that("get data multi series by member name xts",{
  subj_names = "Gross domestic product, current prices (National currency);population (persons)"
  data_frame = Knoema("IMFWEO2017Apr", list(country = "albania;afghanistan;united states", subject=subj_names), type = "xts", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 2862.475)
  expect_equal(coredata(time_ser)[length(time_ser)], 23760.331)
})

context("get data multi series by member id range zoo")
test_that("get data multi series by member id range zoo",{
  data_frame = Knoema("IMFWEO2017Apr", list(country = "914;512;111", subject = "lp;ngdp", timerange = "2015-2020"), type = "zoo", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 18036.650)
  expect_equal(coredata(time_ser)[length(time_ser)], 22063.044)
})

context("get data single series different frequencies by member id xts")
test_that("get data single series different frequencies by member id xts",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location = "AT", subject = "BSCI", measure = "blsa"), type="xts", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),2)

  sname = "M - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], -5.0)
  value = coredata(time_ser["2017-06-01"])[1]
  expect_equal(value, 5.1)

  sname = "Q - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], -5.233333)

  value = coredata(time_ser["2017-01-01"])[1]
  expect_equal(value, 1.566667)
})

context("get data multi series single frequency by member id zoo")
test_that("get data multi series single frequency by member id zoo",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location = "AT;AU", subject = "BSCI", measure = "blsa", frequency = "Q"), type = "zoo", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),2)

  sname = "Q - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearqtr(as.Date("2017-01-01"))])[1]
  expect_equal(value, 1.566667)
})

context("get data multi series multi frequency by member id xts")
test_that("get data multi series multi frequency by member id xts",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location = "AT;AU", subject = "BSCI", measure = "blsa", frequency = "Q;M"), type = "xts", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),3)

  sname = "M - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2017-03-01"))])[1]
  expect_equal(value, 2.4)
})

context("get data multi series multi frequency by member id range zoo")
test_that("get data multi series multi frequency by member id range zoo",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location = "AT;BE", subject = "BSCI", measure = "blsa", frequency = "Q;M", timerange = "2010M1-2015M11"), type = "zoo", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),3)

  sname = "M - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  value = coredata(time_ser[as.yearmon(as.Date("2012-12-01"))])[1]
  expect_equal(value, -12.4)
})

context("get data from dataset with multiword dimnames xts")
test_that("get data from dataset with multiword dimnames xts",{
  data_frame = Knoema("FDI_FLOW_CTRY", list("Reporting country"= "AUS",
                                                "Partner country/territory"= "w0",
                                                "Measurement principle"= "DI",
                                                "Type of FDI"= "T_FA_F",
                                                "Type of entity"= "ALL",
                                                "Accounting entry"= "NET",
                                                "Level of counterpart"= "IMC",
                                                "Currency"= "USD"),"xts",client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")

  expect_equal(length(data_frame),1)

  sname = "A - Australia - WORLD - Directional principle: Inward - FDI financial flows - Total - All resident units - Net - Immediate counterpart (Immediate investor or immediate host) - US Dollar"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 31666.667, tolerance=0.001)
  expect_equal(coredata(time_ser)[length((time_ser))], 22267.638, tolerance=0.001)
})

context("get data multi series by member key zoo")
test_that("get data multi series by member key zoo",{

  data_frame = Knoema("IMFWEO2017Apr", list(country = "1000010;1000000;1001830", subject = "1000370;1000040"), "zoo", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 2862.475)

  expect_equal(coredata(time_ser)[length(time_ser)], 23760.331)
})

context("get data from dataset by dim ids xts")
test_that("get data from dataset by dim ids xts",{
  data_frame = Knoema("FDI_FLOW_CTRY", list("Reporting-country"= "AUS",
                                                "Partner-country"= "w0",
                                                "Measurement-principle"= "DI",
                                                "Type-of-FDI"= "T_FA_F",
                                                "Type-of-entity"= "ALL",
                                                "Accounting-entry"= "NET",
                                                "Level-of-counterpart"= "IMC",
                                                "Currency"= "USD"), "xts", client.id = "bHcV5UkOVyKcBw", client.secret = "/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),1)

  sname = "A - Australia - WORLD - Directional principle: Inward - FDI financial flows - Total - All resident units - Net - Immediate counterpart (Immediate investor or immediate host) - US Dollar"

  time_ser = data_frame[[sname]]
  expect_equal(is.xts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),TRUE)
  expect_equal(is.ts(time_ser),FALSE)
  expect_equal(coredata(time_ser)[1], 31666.667, tolerance=0.001)
  expect_equal(coredata(time_ser)[length(time_ser)], 22267.638, tolerance=0.001)
})

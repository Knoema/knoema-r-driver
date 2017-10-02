context("get data single series by member id ts")
test_that("get data single series by member id ts",{
  data_frame = Knoema("IMFWEO2017Apr", list(country = "914", subject = "lp"), client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),1)

  sname = "A - Albania - Population (Persons)"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], 2.762)
})

context("get data multi series by member id ts")
test_that("get data multi series by member id ts",{
  data_frame = Knoema("IMFWEO2017Apr", list(country="914;512;111", subject="lp;ngdp"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], 2862.475)
  expect_equal(time_ser[length(time_ser)], 23760.331)
})

context("get data multi series by member name ts")
test_that("get data multi series by member name",{
  subj_names = "Gross domestic product, current prices (National currency);population (persons)"
  data_frame = Knoema("IMFWEO2017Apr", list(country="albania;afghanistan;united states", subject=subj_names), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], 2862.475)
  expect_equal(time_ser[length(time_ser)], 23760.331)
})

context("get data multi series by member id range ts")
test_that("get data multi series by member id range",{
  data_frame = Knoema("IMFWEO2017Apr", list(country="914;512;111", subject="lp;ngdp", timerange="2015-2020"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], 18036.650)
  expect_equal(time_ser[length(time_ser)], 22063.044)
})

context("get data single series different frequencies by member id ts")
test_that("get data single series different frequencies by member id ts",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location="AT", subject="BSCI", measure="blsa"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),2)

  sname = "M - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], -5.0)
  value = window(time_ser, start=c(2017,6),frequency=12)[[1]]
  expect_equal(value, 5.1)

  sname = "Q - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], -5.233333)

  value = window(time_ser, start=c(2017,1),frequency=4)[[1]]
  expect_equal(value, 1.566667)
})

context("get data multi series single frequency by member id ts")
test_that("get data multi series single frequency by member id ts",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location="AT;AU", subject="BSCI", measure="blsa", frequency="Q"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),2)

  sname = "Q - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  value = window(time_ser, start=c(2017,1),frequency=4)[[1]]
  expect_equal(value, 1.566667)
})

context("get data multi series multi frequency by member id ts")
test_that("get data multi series multi frequency by member id ts",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location="AT;AU", subject="BSCI", measure="blsa", frequency="Q;M"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),3)

  sname = "M - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  value = window(time_ser, start=c(2017,3),frequency=12)[[1]]
  expect_equal(value, 2.4)
})

context("get data multi series multi frequency by member id range ts")
test_that("get data multi series multi frequency by member id range ts",{
  data_frame = Knoema("MEI_BTS_COS_2015", list(location="AT;BE", subject="BSCI", measure="blsa", frequency="Q;M", timerange="2010M1-2015M11"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),3)

  sname = "M - Austria - Confidence indicators - Balance; Seasonally adjusted"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  value = window(time_ser, start=c(2012,12),frequency=12)[[1]]
  expect_equal(value, -12.4)
})

context("get data from dataset with multiword dimnames ts")
test_that("get data from dataset with multiword dimnames ts",{
  data_frame = Knoema("FDI_FLOW_CTRY", list("Reporting country"= "AUS",
                                                "Partner country/territory"= "w0",
                                                "Measurement principle"= "DI",
                                                "Type of FDI"= "T_FA_F",
                                                "Type of entity"= "ALL",
                                                "Accounting entry"= "NET",
                                                "Level of counterpart"= "IMC",
                                                "Currency"= "USD"),"ts",client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")

  expect_equal(length(data_frame),1)

  sname = "A - Australia - WORLD - Directional principle: Inward - FDI financial flows - Total - All resident units - Net - Immediate counterpart (Immediate investor or immediate host) - US Dollar"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], 31666.667, tolerance=0.001)
  expect_equal(time_ser[length((time_ser))], 22267.638, tolerance=0.001)
})

context("get data multi series by member key ts")
test_that("get data multi series by member key ts",{

  data_frame = Knoema("IMFWEO2017Apr", list(country="1000010;1000000;1001830", subject="1000370;1000040"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),6)

  sname = "A - United States - Gross domestic product, current prices (National currency)"
  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], 2862.475)

  expect_equal(time_ser[length(time_ser)], 23760.331)
})

context("get data from dataset by dim ids ts")
test_that("get data from dataset by dim ids ts",{
  data_frame = Knoema("FDI_FLOW_CTRY", list("Reporting-country"= "AUS",
                                                "Partner-country"= "w0",
                                                "Measurement-principle"= "DI",
                                                "Type-of-FDI"= "T_FA_F",
                                                "Type-of-entity"= "ALL",
                                                "Accounting-entry"= "NET",
                                                "Level-of-counterpart"= "IMC",
                                                "Currency"= "USD"), client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),1)

  sname = "A - Australia - WORLD - Directional principle: Inward - FDI financial flows - Total - All resident units - Net - Immediate counterpart (Immediate investor or immediate host) - US Dollar"

  time_ser = data_frame[[sname]]
  expect_equal(is.ts(time_ser),TRUE)
  expect_equal(is.zoo(time_ser),FALSE)
  expect_equal(is.xts(time_ser),FALSE)
  expect_equal(time_ser[1], 31666.667, tolerance=0.001)
  expect_equal(time_ser[length(time_ser)], 22267.638, tolerance=0.001)
})


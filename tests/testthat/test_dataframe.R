context("get data frame")
test_that("get data frame",{
  data_frame = Knoema("IMFWEO2017Oct", list(country = "914", subject = "lp"), type = "DataFrame", client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Albania - Population (Persons) - A"
  expect_equal(data_frame[['2020-01-01',sname]], 2.865)
})

context ("get data frame with multi selection")
test_that("get data frame with multi selection",{
  data_frame = Knoema('IMFWEO2017Oct', list('frequency' = 'A', 'Country' = '914;612;512', 'Subject' = 'LP;NID_NGDP'),type = 'DataFrame',client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Albania - Investment (Percent of GDP) - A"
  expect_equal(data_frame[['2012-01-01',sname]], 29.824)
  sname ="Afghanistan - Investment (Percent of GDP) - A"
  expect_equal(data_frame[['2007-01-01',sname]], 55.856)
})

context("get metadata frame")
test_that("get metadata frame",{
  data_frame = Knoema("IMFWEO2017Oct", list(country = "914", subject = "lp"), type = "MetaDataFrame", client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Albania - Population (Persons) - A"
  expect_equal(data_frame[['Country Id',sname]], "914")
  expect_equal(data_frame[['Unit',sname]], "Persons (Millions)")
})

context ("get metadata frame with multi selection")
test_that("get metadata frame with multi selection",{
  data_frame = Knoema('IMFWEO2017Oct', list('frequency' = 'A', 'Country' = '914;612;512', 'Subject' = 'LP;NID_NGDP'),type = 'MetaDataFrame',client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Algeria - Population (Persons) - A"
  expect_equal(data_frame[['Unit',sname]], "Persons (Millions)")
  sname ="Afghanistan - Investment (Percent of GDP) - A"
  expect_equal(data_frame[['Subject SubjectDescription',sname]], "Investment")
  expect_equal(data_frame[['Unit',sname]], "Percent of GDP")
})

context ("get series from dataset by partial selection dataframe")
test_that("get series from dataset by partial selection",{
  data_frame = Knoema('IMFWEO2017Oct', list(subject = 'flibor6'),type = "DataFrame", client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),2)
  sname = "Japan - Six-month London interbank offered rate (LIBOR) (Percent) - A"
  expect_equal(data_frame[['2012-01-01',sname]], 0.325)
  sname ="United States - Six-month London interbank offered rate (LIBOR) (Percent) - A"
  expect_equal(data_frame[['2007-01-01',sname]], 5.252)
})

context ("get series from dataset by partial selection Metadataframe")
test_that("get series from dataset by partial selection",{
  data_frame = Knoema('IMFWEO2017Oct', list(subject = 'flibor6'), type = "MetaDataFrame", client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),2)
  sname = "Japan - Six-month London interbank offered rate (LIBOR) (Percent) - A"
  expect_equal(data_frame[['Unit',sname]], "Percent")
  expect_equal(data_frame[['Subject Id',sname]], "FLIBOR6")
  sname ="United States - Six-month London interbank offered rate (LIBOR) (Percent) - A"
  expect_equal(data_frame[['Country Id',sname]], "111")
  expect_equal(data_frame[['Mnemonics',sname]], "NULL")
})

context ("get series from dataset by weekly frequency DataFame")
test_that("get series from dataset by weekly frequency DataFame",{
  data_frame = Knoema('eqohmpb', list(country='512', Indicator='NGDP', frequency='W'), type = 'DataFrame', client.id="bHcV5UkOVyKcBw",client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  expect_equal(length(data_frame),1)
  sname = "Afghanistan - Gross domestic product, current prices - W"
  expect_equal(data_frame[['2006-01-09',sname]], 10)
  expect_equal(data_frame[['2007-04-09',sname]], 14)
})


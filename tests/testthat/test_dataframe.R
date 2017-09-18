context("get data frame")
test_that("get data frame",{
  data_frame = Knoema("IMFWEO2017Apr", list(country = "914", subject = "lp"), type = "DataFrame", client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Albania - Population (Persons) - A"
  expect_equal(data_frame[['2020-01-01',sname]], 2.867)
})

context ("get data frame with multi selection")
test_that("get data frame with multi selection",{
  data_frame = Knoema('IMFWEO2017Apr', list('frequency' = 'A', 'Country' = '914;612;512', 'Subject' = 'LP;NID_NGDP'),'DataFrame',client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Albania - Investment (Percent of GDP) - A"
  expect_equal(data_frame[['2012-01-01',sname]], 29.824)
  sname ="Afghanistan - Investment (Percent of GDP) - A"
  expect_equal(data_frame[['2007-01-01',sname]], 55.856)
})

context("get metadata frame")
test_that("get metadata frame",{
  data_frame = Knoema("IMFWEO2017Apr", list(country = "914", subject = "lp"), type = "MetaDataFrame", client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Albania - Population (Persons) - A"
  expect_equal(data_frame[['Country Id',sname]], "914")
  expect_equal(data_frame[['Unit',sname]], "Persons (Millions)")
})

context ("get metadata frame with multi selection")
test_that("get metadata frame with multi selection",{
  data_frame = Knoema('IMFWEO2017Apr', list('frequency' = 'A', 'Country' = '914;612;512', 'Subject' = 'LP;NID_NGDP'),'MetaDataFrame',client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Algeria - Population (Persons) - A"
  expect_equal(data_frame[['Unit',sname]], "Persons (Millions)")
  sname ="Afghanistan - Investment (Percent of GDP) - A"
  expect_equal(data_frame[['Subject SubjectNotes',sname]], "Expressed as a ratio of total investment in current local currency and GDP in current local currency. Investment or gross capital formation is measured by the total value of the gross fixed capital formation and changes in inventories and acquisitions less disposals of valuables for a unit or sector. [SNA 1993]")
  expect_equal(data_frame[['Unit',sname]], "Percent of GDP")
})

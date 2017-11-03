context("search by mnememonics - annual - dataframe")
test_that("search by mnememonics - annual - dataframe",{
  data_frame = Knoema("eqohmpb", list(mnemonics="512NGDP_A_in_test_dataset"), type = "DataFrame", client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Afghanistan - Gross domestic product, current prices - A"
  expect_equal(nrow(data_frame),3)
  expect_equal(data_frame[['2002-01-01',sname]], 0)
})

context("search by mnememonics - annual - MetaDataFrame")
test_that("search by mnememonics - annual - MetaDataFrame",{
  data_frame = Knoema("eqohmpb", list(mnemonics="512NGDP_A_in_test_dataset"), type = "MetaDataFrame", client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "Afghanistan - Gross domestic product, current prices - A"
  expect_equal(nrow(data_frame),5)
  expect_equal(data_frame[['Mnemonics',sname]], '512NGDP_A_in_test_dataset')
})

context("search by mnememonics - annual - ts")
test_that("search by mnememonics - annual - ts",{
  data_frame = Knoema("eqohmpb", list(mnemonics="512NGDP_A_in_test_dataset"), client.id = "bHcV5UkOVyKcBw", client.secret="/0itYgLqnD0i49kmdBVSZ1qLjPU")
  sname = "A - Afghanistan - Gross domestic product, current prices"
  time_ser = data_frame[[sname]]
  value = window(time_ser, start=c(2004,1),frequency=1)[[1]]
  expect_equal(value, 0)
})

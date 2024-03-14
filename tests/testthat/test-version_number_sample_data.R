test_that("sample dataset has the same version number as package, i.e. is updated", {
  # package_version <- utils::packageDescription("treenetproc",
                                               # fields = "Version", drop = TRUE)
  data_version <- '0.1.4'
  dendro_data_L1_version <- dendro_data_L1$version[1]
  dendro_data_L2_version <- dendro_data_L2$version[1]
  temp_data_L1_version <- temp_data_L1$version[1]

  expect_equal(data_version, dendro_data_L1_version)
  expect_equal(data_version, dendro_data_L2_version)
  expect_equal(data_version, temp_data_L1_version)
})

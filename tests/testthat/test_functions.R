library("cbpManager")

test_that("convertDataFrame works correctly", {

  inputDf <- data.frame(
    ATTRIBUTE_1=c("shortName_1", "longName_1", "STRING", "value_1"),
    ATTRIBUTE_2=c("shortName_2", "longName_2", "STRING", "value_2"),
    ATTRIBUTE_3=c("shortName_3", "longName_3", "STRING", "")
  )

  outputDf <- data.frame(
    ATTRIBUTE_1=c("#shortName_1", "#longName_1", "#STRING", "#1", "ATTRIBUTE_1", "value_1"),
    ATTRIBUTE_2=c("shortName_2", "longName_2", "STRING", "1", "ATTRIBUTE_2", "value_2"),
    ATTRIBUTE_3=c("shortName_3", "longName_3", "STRING", "1", "ATTRIBUTE_3", NA)
  )

  expect_equal(cbpManager:::convertDataFrame(inputDf), outputDf)

})

test_that("IsDate works correctly", {
  expect_false(cbpManager:::IsDate("30.10.2021"))
  expect_true(cbpManager:::IsDate("30-10-2021"))
})

test_that("check_input_dates works correctly", {
  expect_equal(cbpManager:::check_input_dates("2021-01-01"), 0)
  expect_equal(cbpManager:::check_input_dates("2020-01-01", "2021-01-01"), 0)
  expect_equal(cbpManager:::check_input_dates("2021-01-01", startDate = "2020-01-01"), 2)
  expect_equal(cbpManager:::check_input_dates("2020-01-01", startDate = "2021-01-01",), 0)
  expect_equal(cbpManager:::check_input_dates("2021-01-01", endDate = "2020-01-01",), 2)
  expect_equal(cbpManager:::check_input_dates("2020-01-01", endDate = "2021-01-01",), 0)
  expect_equal(cbpManager:::check_input_dates("2020-01-01", "2021-01-01", "2021-02-01"), 0)
  expect_equal(cbpManager:::check_input_dates("2020-01-01", "2021-02-01", "2021-01-01"), 1)
})

test_that("create_name works correctly", {
  expect_equal(cbpManager:::create_name("test A"), "TEST_A")
  expect_equal(cbpManager:::create_name("test A", toupper = FALSE), "test_a")
})

test_that("generateUIwidgets works correctly", {
  expect_s3_class(cbpManager:::generateUIwidgets("TEST"), "shiny.tag")
  i1 <- cbpManager:::generateUIwidgets("TEST")

  expect_snapshot_output(i1)

  i2 <- cbpManager:::generateUIwidgets("OS_STATUS")

  expect_snapshot_output(i2)

  i3 <- cbpManager:::generateUIwidgets(
    "PATIENT_ID",
    mode = "edit",
    tab = "Sample",
    data = data.frame(PATIENT_ID=c("p1", "p2"), ATTRIBUTE=c("val1", "val2")),
    selected_row = 2,
    patientIDs = c("p1", "p2")
  )

  expect_snapshot_output(i3)
})

test_that("fncols works correctly", {
  inputDf <- data.frame(
    PATIENT_ID=c("p1", "p2"),
    ATTRIBUTE=c("val1", "val2")
  )
  outputDf <- data.frame(
    PATIENT_ID=c("p1", "p2"),
    ATTRIBUTE=c("val1", "val2"),
    test=c("", "")
  )
  expect_equal(cbpManager:::fncols(inputDf, "test"), outputDf)
})

test_that("cBioPortalToDataFrame works correctly", {
  inputDf <- data.frame(
    V1=c("#shortName_1", "#longName_1", "#STRING", "#1", "ATTRIBUTE_1", "value_1"),
    V2=c("shortName_2", "longName_2", "STRING", "1", "ATTRIBUTE_2", "value_2")
  )
  outputDf <- data.frame(
    ATTRIBUTE_1=c("shortName_1", "longName_1", "STRING", "value_1"),
    ATTRIBUTE_2=c("shortName_2", "longName_2", "STRING", "value_2")
  )

  expect_equal(cbpManager:::cBioPortalToDataFrame(inputDf), outputDf)
})

test_that("getSampleIDs works correctly", {
  expect_true(all.equal(
    cbpManager:::getSampleIDs("test_data/data_clinical_sample.txt",
                              "Testpatient"),
    c("Testpatient_01", "Testpatient_02")
  ))
})



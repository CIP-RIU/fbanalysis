context("Incosistency of data")


test_that("When you have many unbalanced replications in rcbd", {

  datos <- nodata <- readxl::read_excel("excel/incosistency_values.xlsx", "Fieldbook")
  datos <- as.data.frame(datos)
  rcbd_error <- try(mve.rcbd(trait = "TTWP", "INSTN", "REP", datos))
  rcbd_error <- attributes(rcbd_error)
  msg_error <- unlist(rcbd_error$condition)
  msg_error <- msg_error$message
  testthat::expect_equal(msg_error, "Some treatments have additional replications. Remove those replications to proceed.")
  })






# test_that("When some treatments have zero frequency in RCBD", {
#   
#   datos <- nodata <- readxl::read_excel("excel/incosistency_values.xlsx", "Fieldbook")
#   datos <- as.data.frame(datos)
#   rcbd_error <- try(repo.rcbd(trait = "AVDM", "INSTN", "REP", datos))
#   rcbd_error <- attributes(rcbd_error)
#   msg_error <- unlist(rcbd_error$condition)
#   msg_error <- msg_error$message
#   testthat::expect_equal(msg_error, "Some treatments have zero frequency. Remove treatments to proceed.")
# })


test_that("When some treatments have zero frequency in RCBD", {
  
  datos <- nodata <- readxl::read_excel("excel/incosistency_values.xlsx", "Fieldbook")
  datos <- as.data.frame(datos)
  #rcbd_error <- try(repo.rcbd(trait = "AVDM", "INSTN", "REP", datos))
  out <- single_error(mve.rcbd(trait = "AVDM",treat =  "INSTN", rep = "REP", data = datos), "AVDM")
  msg_error <- out$error
  testthat::expect_equal(msg_error, "Some treatments have zero frequency. Remove treatments to proceed.")
})



test_that("There are no missing values to estimate", {
  
  datos <- nodata <- readxl::read_excel("excel/SPAgronomic062016_TONO_exp1.xlsx", "Fieldbook")
  datos <- as.data.frame(datos)
  #rcbd_error <- try(repo.rcbd(trait = "AVDM", "INSTN", "REP", datos))
  out <- single_error(mve.rcbd(trait = "CYTHA",treat =  "INSTN", rep = "REP", data = datos), "CYTHA")
  msg_error <- out$error
  testthat::expect_equal(msg_error, "OK")
})




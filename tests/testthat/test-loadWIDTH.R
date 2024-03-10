library("loadFile")

path1 <- file.path(system.file("/tests/testthat/data", package = "loadFile"))
path <- paste0(path1, "/")

# test load prn file
testthat::test_that(
  "Load all prn files which with no column title that are in the folder and combine them.", {
  
  a <- read.fwf(paste0(path, "/", "a.prn"), header=FALSE, widths=c(6, 3, 2, 1))
  b <- read.fwf(paste0(path, "/", "b.prn"), header=FALSE, widths=c(6, 3, 2, 1))
  c <- read.fwf(paste0(path, "/", "c.prn"), header=FALSE, widths=c(6, 3, 2, 1))
  abc <- rbind(a,b,c)
  
  loadWIDTH(Filepath=path, Header=FALSE, Width=c(6, 3, 2, 1))
  
  expect_equal(WIDTHdataset, abc)
  }
)

testthat::test_that(
  "Load a.prn and c.prn which with no column title, then become two tibbles in Test_data.", {
  
  a <- read.fwf(paste0(path, "/", "a.prn"), header=FALSE, widths=c(6, 3, 2, 1))
  c <- read.fwf(paste0(path, "/", "c.prn"), header=FALSE, widths=c(6, 3, 2, 1))
  ac <- rbind(a,c)
  ac_list <- list(a,c)
  names(ac_list) <- c("a.prn", "c.prn")
  
  loadWIDTH(Dataname=c("Test_data"), Filepath=path, File=c("a.prn", "c.prn"), Combine=FALSE, Header=FALSE, Width=c(6, 3, 2, 1))
  
  expect_equal(Test_data, ac_list)
  expect_failure(expect_equal(Test_data, ac))
  }
)

testthat::test_that(
  "Load all prn files which with no column title and set the column title.", {
    
  a <- read.fwf(paste0(path, "/", "a.prn"), header=FALSE, widths=c(6, 3, 2, 1), col.names=c("Name_d", "H", "W", "score"))
  b <- read.fwf(paste0(path, "/", "b.prn"), header=FALSE, widths=c(6, 3, 2, 1), col.names=c("Name_d", "H", "W", "score"))
  c <- read.fwf(paste0(path, "/", "c.prn"), header=FALSE, widths=c(6, 3, 2, 1), col.names=c("Name_d", "H", "W", "score"))
  abc <- rbind(a,b,c)
    
  loadWIDTH(Filepath=path, Header=FALSE, Width=c(6, 3, 2, 1), Colname=c("Name_d", "H", "W", "score"))
    
  expect_equal(WIDTHdataset, abc)
  }
)

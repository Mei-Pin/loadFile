library("loadFile")

path1 <- file.path(system.file("/tests/testthat/data", package = "loadFile"))
path <- paste0(path1, "/")

testthat::test_that(
  "Load all excel files that are in the folder and combine them.", {
  
  a <- readxl::read_excel(paste0(path, "a.xlsx"))
  b <- readxl::read_excel(paste0(path, "b.xlsx"))
  c <- readxl::read_excel(paste0(path, "c.xlsx"))
  abc <- rbind(a,b,c)
  
  loadEXCEL(Filepath=path)
  
  expect_equal(EXCELdataset, abc)
  }
)

testthat::test_that(
  "Load a.xlsx and c.xlsx, then become two tibbles in Test_data.", {
  
  a <- readxl::read_excel(paste0(path, "a.xlsx"))
  c <- readxl::read_excel(paste0(path, "c.xlsx"))
  ac <- rbind(a,c)
  ac_list <- list(a,c)
  names(ac_list) <- c("a.xlsx", "c.xlsx")
  
  loadEXCEL(Dataname=c("Test_data"), Filepath=path, File=c("a.xlsx", "c.xlsx"), Combine=FALSE)
  
  expect_equal(Test_data, ac_list)
  expect_failure(expect_equal(Test_data, ac))
  }
)

testthat::test_that(
  "Load all excel files and skip column title, then set the new.", {
    
  a <- readxl::read_excel(paste0(path, "a.xlsx"), col_names=c("Name_d", "H", "W", "score"), skip=1)
  b <- readxl::read_excel(paste0(path, "b.xlsx"), col_names=c("Name_d", "H", "W", "score"), skip=1)
  c <- readxl::read_excel(paste0(path, "c.xlsx"), col_names=c("Name_d", "H", "W", "score"), skip=1)
  abc <- rbind(a,b,c)

  loadEXCEL(Filepath=path, Colname=c("Name_d", "H", "W", "score"), Skip=1)
    
  expect_equal(EXCELdataset, abc)
  }
)

library("loadFile")

path1 <- file.path(system.file("/tests/testthat/data", package = "loadFile"))
path <- paste0(path1, "/")

# test load csv file
testthat::test_that(
  "Load all csv files that are in the folder and combine them.", {
  
  a <- read.csv(paste0(path, "/", "a.csv"))
  b <- read.csv(paste0(path, "/", "b.csv"))
  c <- read.csv(paste0(path, "/", "c.csv"))
  abc <- rbind(a,b,c)
  
  loadDELIM(Filepath=path)
  
  expect_equal(DELIMdataset, abc)
  }
)

testthat::test_that(
  "Load a.csv and c.csv, then become two tibbles in Test_data.", {
  
  a <- read.csv(paste0(path, "/", "a.csv"))
  c <- read.csv(paste0(path, "/", "c.csv"))
  ac <- rbind(a,c)
  ac_list <- list(a,c)
  names(ac_list) <- c("a.csv", "c.csv")
  
  loadDELIM(Dataname=c("Test_data"), Filepath=path, File=c("a.csv", "c.csv"), Combine=FALSE)
  
  expect_equal(Test_data, ac_list)
  expect_failure(expect_equal(Test_data, ac))
  }
)

testthat::test_that(
  "Load all csv files and skip column title, then set the new.", {
    
  a <- read.csv(paste0(path, "/", "a.csv"), col.names=c("Name_d", "H", "W", "score"))
  b <- read.csv(paste0(path, "/", "b.csv"), col.names=c("Name_d", "H", "W", "score"))
  c <- read.csv(paste0(path, "/", "c.csv"), col.names=c("Name_d", "H", "W", "score"))
  abc <- rbind(a,b,c)
    
  loadDELIM(Filepath=path, Colname=c("Name_d", "H", "W", "score"))
    
  expect_equal(DELIMdataset, abc)
  }
)

# test load txt file
testthat::test_that(
  "Load all txt files that are in the folder and combine them.", {
    
    a <- read.csv(paste0(path, "/", "a.txt"), sep="")
    b <- read.csv(paste0(path, "/", "b.txt"), sep="")
    c <- read.csv(paste0(path, "/", "c.txt"), sep="")
    abc <- rbind(a,b,c)
    
    loadDELIM(Filepath=path)
    
    expect_equal(DELIMdataset, abc)
  }
)

testthat::test_that(
  "Load a.txt and c.txt, then become two tibbles in Test_data.", {
    
    a <- read.csv(paste0(path, "/", "a.txt"), sep="")
    c <- read.csv(paste0(path, "/", "c.txt"), sep="")
    ac <- rbind(a,c)
    ac_list <- list(a,c)
    names(ac_list) <- c("a.txt", "c.txt")
    
    loadDELIM(Dataname=c("Test_data"), Filepath=path, File=c("a.txt", "c.txt"), Combine=FALSE, FileExtension="txt", Sep="")
    
    expect_equal(Test_data, ac_list)
    expect_failure(expect_equal(Test_data, ac))
  }
)

testthat::test_that(
  "Load all csv files and skip column title, then set the new.", {
    
    a <- read.csv(paste0(path, "/", "a.txt"), sep="", col.names=c("Name_d", "H", "W", "score"))
    b <- read.csv(paste0(path, "/", "b.txt"), sep="", col.names=c("Name_d", "H", "W", "score"))
    c <- read.csv(paste0(path, "/", "c.txt"), sep="", col.names=c("Name_d", "H", "W", "score"))
    abc <- rbind(a,b,c)
    
    loadDELIM(Filepath=path, FileExtension="txt", Sep="", Colname=c("Name_d", "H", "W", "score"))
    
    expect_equal(DELIMdataset, abc)
  }
)

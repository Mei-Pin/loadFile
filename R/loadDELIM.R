#' @title load delimiter-separate files
#' @description 
#' Loads delimiter-separate files that are in the same folder and with same 
#' format, #' then combines them into a dataset.
#' 
#' @param Dataname Default "DELIMdataset".
#' The dataset's name you want.
#' @param Filepath
#' The folder path.
#' @param File Default \code{FALSE}.
#' Sets one or several files' name with their extension to a vector, or 
#' put all the excel files in the folder into the function in default.
#' @param Combine Default \code{TRUE}.
#' Combines the files to one dataset in default, or loads file each to 
#' be a large list's dataset.
#' @param FileExtension Default "csv".
#' Types in the file extension.
#' @param Header Default \code{TRUE}.
#' Default is setting the first row to column title and read the data from 
#' second row.
#' Sets FALSE to read the data from the first row and use V1, V2,... for 
#' column title instead.
#' @param Sep Default ",".
#' Sets the delimiter, and comma in default.
#' @param Colname Default \code{FALSE}.
#' Sets the column title to a vector, or gets the original title in default.
#' @param Encoding Default \code{FALSE}.
#' Fills in what the file's encoding are, and unknown in default.
#' @return A tibble
#' @examples
#' # There are several files in the test folder called "data".
#' path <- "data"
#' 
#' # The files include "xlsx", "csv", "txt", and "prn".
#' # The files with same extension have same format.
#' # "xlsx" files are excel files.
#' # "csv" files' data are comma-seperate.
#' # "txt" files' data are tab-seperate.
#' # "prn" files' data are fixed-width.
#' 
#' # csv file
#' # "a.csv" "b.csv" "c.csv"
#' # Column titles are "Name", "Height", "Weight", and "Score".
#' # Data starts from the second row.
#' 
#' # Load all csv file that is in the folder and combine them.
#' loadDELIM(Filepath=path)
#' 
#' # Load "a.csv" and "c.csv", then become two tibble in "Test_data".
#' loadDELIM(Dataname=c("Test_data_1"), Filepath=path, File=c("a.csv", "c.csv"), Combine=FALSE)
#' 
#' # Load all csv file and skip column title, then set the new.
#' loadDELIM(Filepath=path, Colname=c("Name_d", "H", "W", "score"))
#' 
#' # txt file
#' # "a.txt" "b.txt" "c.txt"
#' # Column titles are "Name", "Height", "Weight", and "Score".
#' # Data starts from the second row.
#' 
#' # Load all txt files that are in the folder and combine them.
#' loadDELIM(Filepath=path, FileExtension="txt", Sep="")
#' 
#' # Load "a.txt" and "c.txt", then become two tibble in "Test_data".
#' loadDELIM(Dataname=c("Test_data_2"), Filepath=path, File=c("a.txt", "c.txt"), Combine=FALSE, FileExtension="txt", Sep="")
#' 
#' # Load all txt files and skip column title, then set the new.
#' loadDELIM(Filepath=path, Colname=c("Name_d", "H", "W", "score"), FileExtension="txt", Sep="")
#' 
#' @export

# "loadDELIM" FUNCTION ---------------------------------
#set the "loadDELIM" function
loadDELIM <- function(Dataname="DELIMdataset", Filepath, File=FALSE, Combine=TRUE,
                      FileExtension="csv", Header=TRUE, Sep=",", Colname=FALSE, Encoding=FALSE) {
  Package<-installed.packages()
  Package<-Package[,1]
  loadDELIM(Filepath=Filepath,Header=F)
  if (is.element("tidyverse", Package)) {
    library(tidyverse)
  } else {
    install.packages("tidyverse")
    library(tidyverse)
  }
  
  rm(Package)
  
  setwd(Filepath)
  
  if (File[1]==FALSE) {
    Filename <- list.files()
  } else {
    Filename <- File
  }
  
  File_list <- data.frame(Filename)
  File_input <- File_list %>% 
    filter(file_extension(Filename) %in% FileExtension)
  
  if (Colname[1]!=FALSE) {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$File) {
        if (exists("Dataset")) {
          Temp_dataset <- read.csv(i, header=Header, sep=Sep, col.names=Colname)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.csv(i, header=Header, sep=Sep, col.names=Colname)
        }
      }
      
      
    } else if (Combine!=TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- read.csv(i, header=Header, sep=Sep, col.names=Colname)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.csv(i, header=Header, sep=Sep, col.names=Colname)
        }
      }
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$File) {
        Dataset[[i]] <- read.csv(i, header=Header, sep=Sep, col.names=Colname)
      }
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- read.csv(i, header=Header, sep=Sep, col.names=Colname)
      }
    }
    
  } else if (Encoding!=FALSE) {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$File) {
        if (exists("Dataset")) {
          Temp_dataset <- read.csv(i, header=TRUE, sep=Sep, fileEncoding=Encoding)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.csv(i, header=TRUE, sep=Sep, fileEncoding=Encoding)
        }
      }
      
      
    } else if (Combine==TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- read.csv(i, header=TRUE, sep=Sep, fileEncoding=Encoding)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.csv(i, header=TRUE, sep=Sep, fileEncoding=Encoding)
        }
      }
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$File) {
        Dataset[[i]] <- read.csv(i, header=TRUE, sep=Sep, fileEncoding=Encoding)
      }
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- read.csv(i, header=TRUE, sep=Sep, fileEncoding=Encoding)
      }
    }
    
  } else {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$File) {
        if (exists("Dataset")) {
          Temp_dataset <- read.csv(i, header=Header, sep=Sep)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.csv(i, header=Header, sep=Sep)
        }
      }
      
      
    } else if (Combine==TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- read.csv(i, header=Header, sep=Sep)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- read.csv(i, header=Header, sep=Sep)
        }
      }
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$File) {
        Dataset[[i]] <- read.csv(i, header=Header, sep=Sep)
      }
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- read.csv(i, header=Header, sep=Sep)
      }
    }
    
  }
  
  assign(Dataname, Dataset, .GlobalEnv)
  rm(File_list, File_input, i, Dataset)
}

# "file_extension" FUNCTION ---------------------------------
#get the file extension
#credit "asperamanka" answered Jan 11, 2022 at 17:22
#https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path
file_extension <- function(filenames) {
  sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", filenames, perl = TRUE)
}
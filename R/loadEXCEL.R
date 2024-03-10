#' @title load excel files
#' @description 
#' Loads excel files that are in the same folder and with same format, 
#' then combines them into a dataset.
#' 
#' @param Dataname Default "Exceldataset".
#' The dataset's name you want.
#' @param Filepath
#' The folder path.
#' @param File Default \code{FALSE}.
#' Sets one or several files' name with their extension to a vector, or 
#' put all the excel files in the folder into the function in default.
#' @param Combine Default \code{TRUE}.
#' Combines the files to one dataset in default, or loads file each to 
#' be a large list's dataset.
#' @param Colname Default \code{FALSE}.
#' Sets the column title to a vector, or gets the original title in default.
#' @param Skip
#' Number of rows skipped before reading file, and 0 in default.
#' @return A tibble or tibbles in one list
#' @import magrittr
#' @import tidyverse
#' @importFrom dplyr filter
#' @examples
#' 
#' # There are several files in the test folder called "extdata".
#' # The folder path should fill in the folder directory of "extdata".
#' \dontshow{
#'   path1 <- file.path(system.file("extdata", package = "loadFile"))
#'   path <- paste0(path1, "/")
#'   Dataset <- list()
#' }
#' # The files include "xlsx", "csv", "txt", and "prn".
#' # The files with same extension have same format.
#' # "xlsx" files are excel files.
#' # "csv" files' data are comma-seperate.
#' # "txt" files' data are tab-seperate.
#' # "prn" files' data are fixed-width.
#' 
#' # xlsx file
#' # "a.xlsx" "b.xlsx" "c.xlsx"
#' # Column titles are "Name", "Height", "Weight", and "Score".
#' # Data starts from the second row.
#' 
#' # Load all excel files that are in the folder and combine them.
#' loadEXCEL(Dataname=c("Test_All"), Filepath=path)
#'
#' # Load "a.xlsx" and "c.xlsx", then become two tibble in "Test_a&c".
#' loadEXCEL(Dataname=c("Test_a&c"), Filepath=path, File=c("a.xlsx", 
#' "c.xlsx"), Combine=FALSE)
#' 
#' # Load all excel files and skip column title, then set the new.
#' loadEXCEL(Dataname=c("Test_Colname"), Filepath=path, Colname=c("Name_d", 
#' "H", "W", "score"), Skip=1)
#' 
#' @export

# "loadEXCEL" FUNCTION ---------------------------------
#set the "loadEXCEL" function
loadEXCEL <- function(Dataname="EXCELdataset", Filepath, File=FALSE, Combine=TRUE, Colname=FALSE, Skip=0) {

  if (File[1]==FALSE) {
    Filename <- list.files(Filepath)
  } else {
    Filename <- File
  }
  
  File_list <- data.frame(Filename)
  File_input <- File_list %>% 
    dplyr::filter(file_extension(Filename) %in% "xlsx")
  
  if (Colname[1]!=FALSE) {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$Filename) {
        if (exists("Dataset")) {
          Temp_dataset <- readxl::read_excel(paste0(Filepath, i), col_names=FALSE, skip=Skip)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- readxl::read_excel(paste0(Filepath, i), col_names=FALSE, skip=Skip)
        }
      }
      names(Dataset) <- Colname
      
    } else if (Combine==TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- readxl::read_excel(paste0(Filepath, i), col_names=FALSE, skip=Skip)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- readxl::read_excel(paste0(Filepath, i), col_names=FALSE, skip=Skip)
        }
      }
      names(Dataset) <- Colname
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$Filename) {
        Dataset[[i]] <- readxl::read_excel(paste0(Filepath, i), col_names=FALSE, skip=Skip)
      }
      names(Dataset) <- Colname
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- readxl::read_excel(paste0(Filepath, i), col_names=FALSE, skip=Skip)
      }
      names(Dataset) <- Colname
      
    }
    
  } else {
    if (File[1]==FALSE & Combine==TRUE) {
      for (i in File_input$Filename) {
        if (exists("Dataset")) {
          Temp_dataset <- readxl::read_excel(paste0(Filepath, i), skip=Skip)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- readxl::read_excel(paste0(Filepath, i), skip=Skip)
        }
      }
      
    } else if (Combine==TRUE) {
      for (i in File){
        if (exists("Dataset")) {
          Temp_dataset <- readxl::read_excel(paste0(Filepath, i), skip=Skip)
          Dataset <- rbind(Dataset, Temp_dataset)
          rm(Temp_dataset)
        } else {
          Dataset <- readxl::read_excel(paste0(Filepath, i), skip=Skip)
        }
      }
      
    } else if (File[1]==FALSE & Combine==FALSE) {
      Dataset <- list()
      
      for (i in File_input$Filename) {
        Dataset[[i]] <- readxl::read_excel(paste0(Filepath, i), skip=Skip)
      }
      
    } else {
      Dataset <- list()
      
      for (i in File) {
        Dataset[[i]] <- readxl::read_excel(paste0(Filepath, i), skip=Skip)
      }
    }
    
  }
  
  Glo <- .GlobalEnv
  assign(Dataname, Dataset, Glo)
  rm(File_list, File_input, i, Glo)
}

# "file_extension" FUNCTION ---------------------------------
#get the file extension
#credit "asperamanka" answered Jan 11, 2022 at 17:22
#https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path
file_extension <- function(filenames) {
  sub(pattern = "^(.*\\.|[^.]+)(?=[^.]*)", replacement = "", filenames, perl = TRUE)
}

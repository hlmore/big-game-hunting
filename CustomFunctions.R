# Function to read xls or xlsx file
#
# Including sheet name and column types is optional.
# Input is a list with keys:
#   "path" = directory of file (string)
#   "fileName" = file name, optionally prepended by subdirectory (string)
#   "sheetName" = name of worksheet to read (string) [optional]
#   "columnTypes" = type of data in each column (vector) [optional]
# Output is data frame with cleaned column names and NA rows and cols removed
ReadExcel <- function(inputVar) {
  # Find names of tags
  inputTags <- names(inputVar)
  if ( ("sheetName" %in% inputTags) && ("columnTypes" %in% inputTags) ) {
    df <- read_excel(file.path(inputVar[["path"]],
                               inputVar[["fileName"]],
                               fsep = .Platform$file.sep),
                     sheet = inputVar[["sheetName"]],
                     col_types = inputVar[["columnTypes"]]
                     )
  } else if ("sheetName" %in% inputTags) {
    df <-read_excel(file.path(inputVar[["path"]],
                              inputVar[["fileName"]],
                              fsep = .Platform$file.sep),
                    sheet = inputVar[["sheetName"]]
    )
  } else if ("columnTypes" %in% inputTags) {
    df <- read_excel(file.path(inputVar[["path"]],
                               inputVar[["fileName"]],
                               fsep = .Platform$file.sep),
                     col_types = inputVar[["columnTypes"]]
    )
  } else {
    df <- read_excel(file.path(inputVar[["path"]],
                               inputVar[["fileName"]],
                               fsep = .Platform$file.sep)
    )
  }
  df <- df %>% 
    clean_names() %>%  # clean column names
    remove_empty(which = c("rows", "cols"))  # from janitor package -- remove rows and columns with only NAs
  # Return output
  return(df)
}
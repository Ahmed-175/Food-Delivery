
load_raw_data <- function(file) {
  req(file)
  read.csv(file$datapath)
}

clean_data_module <- function(df) {
  clean_data(df) 
}
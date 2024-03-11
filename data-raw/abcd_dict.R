library(readr)

abcd_dict <- read_csv("~/mnt/hpf/external/abcd/5.0/data-dictionary/data_dictionary.csv")

usethis::use_data(abcd_dict, overwrite = TRUE)


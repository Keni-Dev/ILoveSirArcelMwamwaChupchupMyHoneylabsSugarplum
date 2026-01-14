sirarcelmyloves <- read_csv(file = here("01 - Flat files/flat file 02.txt"),col_names = c("Name", "Airplane", "Kangkong"), skip=1, n_max = 10)

sirarcelmylovess <- read_csv(file = here("01 - Flat files/flat file 04.txt"), skip=1, comment = "comment: ", na = "NDA")


sirarcelmylovesssssssss <- read_fwf(file = here("01 - Flat files/flat file 06.txt"), col_positions = fwf_widths(
  widths = c(10, 3, 3, 5), 
  col_names = c("Kangkong", "Kingkong", "Rodelyn Bading", "akndf")
), na="")

read_fwf


sirarcelmylovessss <- read_table(file = here("01 - Flat files/flat file 06.txt"), col_names = c("Kangkong", "Kingkong", "Rodelyn Bading","akndf"), na="")

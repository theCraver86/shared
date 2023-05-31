#install.packages("readxl")
library(readxl)
library(writexl)

target <- read_xlsx(path = "C:/Users/rtadd/OneDrive/Desktop/R/2import/target.xlsx")
volato <- read_xlsx(path = "C:/Users/rtadd/OneDrive/Desktop/R/2import/SF_Volato.xlsx")

volato_elab = data.frame(volato) %>% 
  arrange(PTP_vol) %>% 
  a <- sub(" ", "_", volato$PTP_vol)
  mutate(
    BCR = replace("PTP_vol","","")
  )  
target_elab = data.frame(target) 
  
write_xlsx(volato_elab, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/volato_elab.xlsx")
write_xlsx(target_elab, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/target_elab.xlsx")


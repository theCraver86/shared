#install.packages("janitor")
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
#library(janitor) #clean_names() %>% 

target <- read_xlsx(path = "C:/Users/rtadd/OneDrive/Desktop/R/2import/target.xlsx")
volato <- read_xlsx(path = "C:/Users/rtadd/OneDrive/Desktop/R/2import/SF_Volato.xlsx")

volato_elab = data.frame(volato) %>% 
  mutate(
    PTP = str_replace(PTP, " ", "")
    ) 
  
target_elab = data.frame(target) 

master = target_elab %>% 
  left_join(volato_elab, by = c("country","PTP") , suffix = c("tgt","vol")) %>% 
  select(-ends_with(c("3","4"))) %>% 
  mutate_at(-c(1,2), ~coalesce(.,0)) %>% 
  mutate(
    
    May = X01.05.2023vol - X01.05.2023tgt,
    June = X01.06.2023vol - X01.06.2023tgt,
    July = X01.07.2023vol - X01.07.2023tgt,
    August = X01.08.2023vol - X01.08.2023tgt,
    Septembre = X01.09.2023vol - X01.09.2023tgt,
    October = X01.10.2023vol - X01.10.2023tgt,
    November = X01.11.2023vol - X01.11.2023tgt,
    December = X01.12.2023vol - X01.12.2023tgt,
    
    May_perc = ifelse(X01.05.2023tgt > 0, (X01.05.2023vol - X01.05.2023tgt)/X01.05.2023tgt,0),
    June_perc = ifelse(X01.06.2023tgt > 0, (X01.06.2023vol - X01.06.2023tgt)/X01.06.2023tgt,0),
    July_perc = ifelse(X01.07.2023tgt > 0, (X01.07.2023vol - X01.07.2023tgt)/X01.07.2023tgt,0),
    August_perc = ifelse(X01.08.2023tgt > 0, (X01.08.2023vol - X01.08.2023tgt)/X01.08.2023tgt,0),
    Septembre_perc = ifelse(X01.09.2023tgt > 0, (X01.09.2023vol - X01.09.2023tgt)/X01.09.2023tgt,0),
    October_perc = ifelse(X01.10.2023tgt > 0, (X01.10.2023vol - X01.10.2023tgt)/X01.10.2023tgt,0),
    November_perc = ifelse(X01.11.2023tgt > 0, (X01.11.2023vol - X01.11.2023tgt)/X01.11.2023tgt,0),
    December_perc = ifelse(X01.12.2023tgt > 0, (X01.12.2023vol - X01.12.2023tgt)/X01.12.2023tgt,0)
  )

write_xlsx(volato_elab, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/volato_elab.xlsx")
write_xlsx(target_elab, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/target_elab.xlsx")
write_xlsx(master, "C:/Users/rtadd/OneDrive/Desktop/R/1exported/master.xlsx")
options(OutDec=",")
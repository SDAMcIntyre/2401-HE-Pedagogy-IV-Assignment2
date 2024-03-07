library(readxl)
library(tidyverse)

raw_data <- read_excel("Survey on campus versus distance learning(1-43).xlsx")

recommend_data <- raw_data %>% 
  mutate(
    Recommend = case_when(
      `Would you recommend distance learning for future students?` == "Maybe" ~ "Yes/Maybe",
      `Would you recommend distance learning for future students?` == "Yes" ~ "Yes/Maybe",
      TRUE ~ `Would you recommend distance learning for future students?`,
    ),
    ManageTime = case_when(
      `How well could you manage time while learning remotely?` == "Moderately well" ~ "Moderately/Very well",
      `How well could you manage time while learning remotely?` == "Very well" ~ "Moderately/Very well",
      TRUE ~ `How well could you manage time while learning remotely?`
    ),
    Motivated = case_when(
      `How motivated and engaged did you feel in this course? ` == "Somewhat" ~ "Somewhat/Very much",
      `How motivated and engaged did you feel in this course? ` == "Very much" ~ "Somewhat/Very much",
      TRUE ~ `How motivated and engaged did you feel in this course? `
    )
    ) %>% 
  select(Recommend, ManageTime, Motivated) 


xtabs(~ Recommend + ManageTime, recommend_data)
chisq.test(recommend_data$Recommend, recommend_data$ManageTime)

xtabs(~ Recommend + Motivated, recommend_data)
chisq.test(recommend_data$Recommend, recommend_data$Motivated)

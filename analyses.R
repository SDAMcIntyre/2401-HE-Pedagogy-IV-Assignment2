library(readxl)
library(tidyverse)

raw_data <- read_excel("Survey on campus versus distance learning(1-43).xlsx")

# from summary_results_colors.pdf
tool_names <- c("Zoom video calls", "Email", "MS Teams video calls", "MS Teams text chats", "Other")
communication_data <- tibble(
  comm_tool = factor(tool_names, levels = rev(tool_names)),
  Frequency = c(37,19,13,6,3)
 ) %>% 
  mutate(Percent = 100*Frequency/43)

ggplot(
  data = communication_data,
  mapping = aes(x = Percent, y = comm_tool)
) +
  geom_col(fill = "#4DBBD5FF") +
  scale_x_continuous(limits = c(0,100)) +
  theme_classic(base_size = 16) +
  labs(y = NULL, subtitle = "What communication channels do you use for online learning?")

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


library(data.table)
library(dplyr)
library(tidyverse)
library(tibble)
library(ggplot2)
library(stringr)

# read in data set
shooter_info <- fread("mass_shooting_events_stanford_msa_release_06142016.csv")

# convert to tibble
shooter_info <- as_tibble(shooter_info)

names(shooter_info) <- str_replace_all(names(shooter_info), "[\\s\\-]", "_")

summary <- shooter_info %>% 
  group_by(History_of_Mental_Illness___General) %>% 
  summarise(min = summary(Total_Number_of_Victims)[[1]],
            first_quartile = summary(Total_Number_of_Victims)[[2]],
            median = summary(Total_Number_of_Victims)[[3]],
            mean = summary(Total_Number_of_Victims)[[4]],
            third_quartile = summary(Total_Number_of_Victims)[[5]],
            max = summary(Total_Number_of_Victims)[[6]]) %>% 
  filter(History_of_Mental_Illness___General %in% c("No", "Unknown", "Yes"))

summary # (table summary)

df <- shooter_info %>% 
  select(History_of_Mental_Illness___General, Total_Number_of_Victims) %>%
  filter(nchar(History_of_Mental_Illness___General) > 0) %>% as.data.frame()

yes <- df[df$History_of_Mental_Illness___General == "Yes", ]
no <- df[df$History_of_Mental_Illness___General == "No", ]
unknown <- df[df$History_of_Mental_Illness___General == "Unknown", ]

# (boxplot of summary) :
boxplot(list("Yes" = yes$Total_Number_of_Victims,
             "No" = no$Total_Number_of_Victims,
             "Unknown" = unknown$Total_Number_of_Victims),
        xlab = "Shooter's History of Mental Illness - General",
        ylab = "Total Number of Victims",
        main = "Relationship between Mental Illness and Severity of Mass Shooting",
        col = c("firebrick1", "royalblue1", "mediumpurple1"))


#ggplot(data = df,
#       mapping = aes(
#         x = History_of_Mental_Illness___General,
#         y = Total_Number_of_Victims,
#         color = History_of_Mental_Illness___General,
#       )) +
#  geom_boxplot()



mental_illness <- list(Unknown = sum(shooter_info$History_of_Mental_Illness___Detailed == "Unknown" | 
                                       shooter_info$History_of_Mental_Illness___Detailed == "unknown"), 
                       None = sum(shooter_info$History_of_Mental_Illness___Detailed == "No" | 
                                    shooter_info$History_of_Mental_Illness___Detailed == "None"),
                       'personality disorder' = 3, 'schizotypal personality disorder' = 1,
                       'narcissistic personality disorder' = 1, 'borderline personality disorder' = 3,
                       depression = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Dd]epress")),
                       schizophrenia = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Ss]chizophreni")),
                       psychopathy = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Pp]sychopath")),
                       paranoia = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Pp]aranoi")),
                       insanity = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Ii]nsan")),
                       compulsivity = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Cc]ompulsiv")),
                       anxiety = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Aa]nxiety")),
                       PTSD = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "(post-?[:space:]?traumatic stress disorder|PTSD)")),
                       'bipolar disorder' = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "bi-?polar")),
                       OCD = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "(obsessi[veon]+-compulsive disorder|OCD)")),
                       'dissociative disorder' = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "dissociativ")),
                       'delusional disorder' = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Dd]elusion")),
                       'panic disorder' = 1, ADHD = 1, 'sensory processing disorder' = 1, 'dysphoric mania' = 1,
                       grandiose = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "grandiose")),
                       'character behavior disorder' = 1,
                       pychosis = sum(str_detect(shooter_info$History_of_Mental_Illness___Detailed, "[Pp]sycho(sis|tic)"))
                       )

## $Unknown
## [1] 114

## $None
## [1] 58

## $`personality disorder`
## [1] 3

## $`schizotypal personality disorder`
## [1] 1

## $`narcissistic personality disorder`
## [1] 1

## $`borderline personality disorder`
## [1] 3

## $depression
## [1] 31

## $schizophrenia
## [1] 24

## $psychopathy
## [1] 1

## $paranoia
## [1] 27

## $insanity
## [1] 13

## $compulsivity
## [1] 4

## $anxiety
## [1] 6

## $PTSD
## [1] 8

## $`bipolar disorder`
## [1] 6

## $OCD
## [1] 3

## $`dissociative disorder`
## [1] 2

## $`delusional disorder`
## [1] 15

## $`panic disorder`
## [1] 1

## $ADHD
## [1] 1

## $`sensory processing disorder`
## [1] 1

## $`dysphoric mania`
## [1] 1

## $grandiose
## [1] 2

## $`character behavior disorder`
## [1] 1

## $pychosis
## [1] 13


result <- character(0)
for (i in seq_along(mental_illness)) {
  result <- c(result, rep(names(mental_illness)[i], mental_illness[[i]]))
}

cat(result, file = "mental_illness_list.txt")







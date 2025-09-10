rm(list = ls())
setwd(here::here("generate", "data", "cleaned_data"))

files <- grep("[0-9]+_data", dir(), value = T)
library(readr)
library(tidyverse)
library(magrittr)
data <- NULL
#files <- files[c(-1:-3)]
#files <- files[1:3]
for (ii in seq_along(files)) {
  tmp <- read_csv(files[ii])
  if (nchar(files[ii]) > 13) {
    tmp$year <- substr(files[ii], 6, 9)
  } else {
    tmp$year <- substr(files[ii], 1, 4)
  }
  data <- plyr::rbind.fill(data, tmp)
}

data <- data %>%
  filter(candidate != "Self") %>%
  drop_na(rating) %>%
  filter(!(rating >= 8 & year >= 2009)) %>%
  filter(rating >= 0 & rating <= 100)


# data$state <- tolower(data$state)

## Combining together based on state, district, candidate, and type.
data <- data %>%
  mutate(state = tolower(state)) %>%
  mutate(district = as.numeric(district)) %>%
  mutate(district = ifelse(is.na(district), 0, district))

id_data <- read_csv("../supplementary_data/matched_with_all.csv")
id_data %<>% select(state, candidate, type, district, Unique_Project_ID)
rep_data <- data %>% filter(type == "Rep") %>% left_join(id_data)
state_data <- data %>%
  filter(type %in% c("Sen", "Gov")) %>%
  left_join(select(id_data, -district))
common_data <- data %>%
  filter(type == "Common") %>%
  left_join(distinct(select(id_data, candidate, Unique_Project_ID)))

data <- rbind(rep_data, state_data, common_data)
#data %>% group_by(Unique_Project_ID) %>% summarize(n()) %>% arrange(`n()`)
mean(is.na(data$Unique_Project_ID)) == 0 ## Should be true

data <- data %>% filter(Unique_Project_ID > 0)
data <- data %>%
  mutate(
    UUID = paste0(
      "id",
      stringr::str_pad(Unique_Project_ID, side = "left", pad = 0, width = 5)
    )
  )
data <- data %>% select(-Unique_Project_ID)

write_csv(data, "full_data.csv")

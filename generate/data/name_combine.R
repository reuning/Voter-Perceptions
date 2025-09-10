## Do not need to run this.
## Used to generate `bio_matched.csv` which was initially used to then handcode the unique ids.
setwd(here::here("generate", "data"))

library(tidyverse)

files <- dir("cleaned_data")
files <- paste("cleaned_data", grep("stats", files, value = T), sep = "/")
all_data <- read_csv(files[1])
all_data$survey <- gsub("cleaned_data/|_stats.csv", "", files[1])

for (ii in 2:length(files)) {
    tmp <- read_csv(files[ii])
    tmp$survey <- gsub("cleaned_data/|_stats.csv", "", files[ii])
    if (!"party" %in% names(tmp)) {
        tmp$party <- "Not in Data"
    }

    all_data <- rbind(all_data, tmp)
}

all_data$state <- tolower(all_data$state)

## Combining together based on state, district, candidate, and type.
full <- all_data %>%
    mutate(district = as.numeric(district)) %>%
    mutate(district = ifelse(is.na(district), 0, district)) %>%
    pivot_wider(
        id_cols = c(state, district, candidate, type),
        names_from = survey,
        values_from = n,
        values_fn = function(x) sum(x, na.rm = T)
    ) %>%
    arrange(state, candidate, district)

# write_csv(full, "full_data.csv",    na="")

name_clean <- function(x) {
    x <- stringr::str_split(x, ",")[[1]]
    x <- stringr::str_trim(x)
    tolower(paste(x[2], x[1], sep = " "))
}
Name_clean <- Vectorize(name_clean, "x")
#Name_clean(nom_data$bioname)

nom_data <- read_csv("supplementary_data/HSall_members.csv")

fips <- tigris::fips_codes
fips <- unique(fips[, c("state_name", "state")])
names(fips) <- c("state", "state_abbrev")
nom_data <- left_join(nom_data, fips, by = "state_abbrev")


nom_data <- nom_data %>%
    mutate(state = tolower(state)) %>%
    filter(congress > 108 & chamber != "President") %>%
    select(chamber, bioguide_id, state, district_code, bioname) %>%
    unique() %>%
    mutate(
        match_name = Name_clean(bioname),
        type = ifelse(chamber == "House", "Rep", "Sen")
    )

full$match_name <- tolower(full$candidate)

names(nom_data)[4] <- "district"
full_bio <- left_join(
    full,
    nom_data,
    by = c("state", "match_name", "type", "district")
)
full_bio <- full_bio %>% select(state:type, chamber:bioguide_id, `2006`:`2024`)
# write_csv(full_bio, "supplementary_data/bio_matched.csv", na = "")

### checking with old data
### used when updating data
matched_all <- read_csv("supplementary_data/matched_with_all.csv")

#### WARNING THIS WILL DROP THE COMMON SPACE ONES.
new_data <- left_join(
    select(full_bio, -bioguide_id),
    select(matched_all, -starts_with("2"))
)
write_csv(new_data, "supplementary_data/matched_with_all_2024.csv", na = "")

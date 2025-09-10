# region setup
setwd(here::here("generate", "data"))

library(haven)
library(tidyverse)
# endregion

# region 2006
df <- read_dta("ces_data/2006/cces_2006_common.dta")

fips <- tigris::fips_codes
fips <- unique(fips[, c("state_name", "state")])
names(fips) <- c("state", "v1002")
df <- left_join(df, fips, by = "v1002")


## Self, Democrat Party, Republican Party, House Cand 1, House Cand 2, Senate Cand 1, Senate Cand 2, Senator Inc 1, Senator Inc 2, Gov Cand 1, Gov Cand 2, Gov Inc
## Self, Democratic Party, Republican Party, V5001, V5003, V5005, V5007, V5015, V5017, V5009, V5011, V5012, V5019
scores <- c(paste0("v", 3042:3053))
names <- c(
    "Self" = "v3042",
    "Democratic Party" = "v3043",
    "Republican Party" = "v3044",
    "v5001" = "v3045",
    "v5003" = "v3046",
    "v5005" = "v3047",
    "v5007" = "v3048",
    "v5015" = "v3049",
    "v5017" = "v3050",
    "v5009" = "v3051",
    "v5011" = "v3052",
    "v5019" = "v3053"
)

name_party <- c(
    "Self" = "v3042",
    "Democratic Party" = "v3043",
    "Republican Party" = "v3044",
    "v5002" = "v3045",
    "v5004" = "v3046",
    "v5006" = "v3047",
    "v5008" = "v3048",
    "v5016" = "v3049",
    "v5018" = "v3050",
    "v5010" = "v3051",
    "v5012" = "v3052",
    "v5020" = "v3053"
)

congressional <- c("v3045", "v3046")
senate <- c("v3047", "v3048", "v3049", "v3050")
gov <- c("v3051", "v3052", "v3053")
candidates <- c("v3045", "v3046", "v3047", "v3048", "v3051", "v3052")
incumbents <- c("v3049", "v3050", "v3053")
## State V1002
## District V1003
## V1000
demos <- c("v1000", "state", "v1003")
scores_df <- df %>%
    select(all_of(demos), v3042:v3053) %>%
    pivot_longer(v3042:v3053) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-3]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)

name_party_cols <- names(name_party)[-1:-3]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "__NA__") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2006_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, status, party, candidate, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2006_stats.csv")

rm(list = ls())
# endregion

# region 2007
df <- read_sav("ces_data/2007/CCES07_OUTPUT.sav", encoding = "latin1")

# CC06_V1000
scores <- c(paste0("CC", 27:33))
names <- c(
    "Self" = "CC27",
    "Democratic Party" = "CC28",
    "Republican Party" = "CC29",
    "George Bush" = "CC30",
    "repname" = "CC31", # Incumbent House of Rep
    "sen1name" = "CC32", # Incumbent Sen 1
    "sen2name" = "CC33" # Incumbent Sen 2
)
## No Party info

congressional <- c("CC31")
senate <- c("CC32", "CC33")
gov <- c()
candidates <- c()
incumbents <- c("CC31", "CC32", "CC33")
demos <- c("CC06_V1000", "inputstate", "cdid_num")

scores_df <- df %>%
    select(all_of(demos), CC27:CC33) %>%
    pivot_longer(CC27:CC33) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-4]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "George Bush" = "George Bush"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


data_out <- all_df %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2007_data.csv")

rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats)
write_csv(stats, "cleaned_data/2007_stats.csv")
# endregion

# region 2008

df <- read_dta("ces_data/2008/cces_2008_common.dta")

scores <- c(paste0(
    "CC317",
    c(
        "a",
        "b",
        "c",
        "d",
        "h",
        "g",
        "e",
        "f",
        "i",
        "j",
        "iS2",
        "jS2",
        "k",
        "l",
        "m"
    )
))


names <- c(
    "Self" = "CC317a",
    "Democratic Party" = "CC317b",
    "Republican Party" = "CC317c",
    "George Bush" = "CC317d",
    "Barack Obama" = "CC317h",
    "John McCain" = "CC317g",
    "V527" = "CC317m", # Incumbent House of Rep
    "house_cand_1" = "CC317k", # Candidate House of Rep 1
    "house_cand_2" = "CC317l", # Candidate House of Rep 2
    "V551" = "CC317e", # Incumbent Sen 1
    "V552" = "CC317f", # Incumbent Sen 2
    "V553" = "CC317i", # Candidate Sen 1
    "V555" = "CC317j", # Candidate Sen 2
    "V554" = "CC317iS2", # Candidate Sen 1 MS/WY
    "V556" = "CC317jS2" # Candidate Sen 2 MS/WY
)


# For all offices, Candidate 1 is the Democrat and Candidate 2 is the Republican,
# except when no Democrat is running.  When no Democrat is running, the Republican
# is listed as Candidate 1.  When only one candidate is running, Candidate 2 is listed as “NA'.

n_common <- 6


name_party <- c(
    "Self" = "CC317a",
    "Democratic Party" = "CC317b",
    "Republican Party" = "CC317c",
    "George Bush" = "CC317d",
    "Barack Obama" = "CC317h",
    "John McCain" = "CC317g",
    "House Inc 1" = "CC317m", # Incumbent House of Rep
    "house_cand_party_1" = "CC317k", # Candidate House of Rep 1
    "house_cand_party_2" = "CC317l", # Candidate House of Rep 2
    "Sen Inc 1" = "CC317e", # Incumbent Sen 1
    "Sen Inc 2" = "CC317f", # Incumbent Sen 2
    "sen_cand_party_1" = "CC317i", # Candidate Sen 1
    "sen_cand_party_2" = "CC317j", # Candidate Sen 2
    "sen_cand_party_1_sp" = "CC317iS2", # Candidate Sen 1 MS/WY
    "sen_cand_party_2_sp" = "CC317jS2" # Candidate Sen 2 MS/WY
)

df <- df %>%
    mutate(
        house_cand_1 = case_when(
            is.na(V518) ~ V519, # No Dem candidate, cand 1 is Rep cand
            TRUE ~ V518
        ),
        house_cand_party_1 = case_when(
            is.na(V518) ~ "Republican", # No Dem candidate, cand 1 is Rep cand
            TRUE ~ "Democrat"
        ), # If there is a Dem candidate, cand 1 is Dem cand
        house_cand_2 = case_when(
            is.na(V518) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V519) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ V519 # Else it is the Republican candidate
        ),
        house_cand_party_2 = case_when(
            is.na(V518) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V519) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ "Republican" # Else it is the Republican candidate
        ),
        sen_cand_party_1 = case_when(
            is.na(V553) ~ "Republican", # No Dem candidate, cand 1 is Rep cand
            TRUE ~ "Democrat"
        ),
        sen_cand_party_2 = case_when(
            is.na(V553) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V555) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ "Republican" # Else it is the Republican candidate
        ),
        sen_cand_party_1_sp = case_when(
            is.na(V554) ~ "Republican", # No Dem candidate, cand 1 is Rep cand
            TRUE ~ "Democrat"
        ),
        sen_cand_party_2_sp = case_when(
            is.na(V554) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V556) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ "Republican" # Else it is the Republican candidate
        )
    )


congressional <- c("CC317m", "CC317k", "CC317l")
senate <- c("CC317e", "CC317f", "CC317i", "CC317j", "CC317iS2", "CC317jS2")
gov <- c()
candidates <- c("CC317k", "CC317l", "CC317i", "CC317j", "CC317iS2", "CC317jS2")
incumbents <- c("CC317f", "CC317e", "CC317m")
demos <- c("V100", "V206", "V250")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "George Bush" = "George Bush",
        "Barack Obama" = "Barack Obama",
        "John McCain" = "John McCain",
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    mutate(
        "House Inc 1" = "Not in Data",
        "Sen Inc 1" = "Not in Data",
        "Sen Inc 2" = "Not in Data"
    ) %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "George Bush" = "Republican",
        "Barack Obama" = "Democrat",
        "John McCain" = "Republican"
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "__NA__") %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2008_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats)
write_csv(stats, "cleaned_data/2008_stats.csv")
#endregion

############################################################################
############################################################################
############################################################################

#region 2009

df <- read_dta("ces_data/2009/cces09_cmn_output_2.dta", encoding = "latin1")

scores <- paste0("cc09_42", letters[1:8])
names <- c(
    "Self" = "cc09_42a",
    "Democratic Party" = "cc09_42d",
    "Republican Party" = "cc09_42e",
    "Barack Obama" = "cc09_42c",
    "v627" = "cc09_42f", # Incumbent House of Rep
    "v651" = "cc09_42g", # Incumbent Sen 1
    "v652" = "cc09_42h", # Incumbent Sen 2
    "v608" = "cc09_42b" # governor
)
n_common <- 4
## No Party info

congressional <- c("cc09_42f")
senate <- c("cc09_42g", "cc09_42h")
gov <- c("cc09_42b")
candidates <- c()
incumbents <- c("cc09_42f", "cc09_42g", "cc09_42h", "cc09_42b")
demos <- c("v100", "v259", "v264")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


data_out <- all_df %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2009_data.csv")

rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2009_stats.csv")

#endregion

############################################################################
############################################################################
############################################################################

#region 2010

df <- read_dta("ces_data/2010/cces_2010_common_validated.dta")
names(df)

scores <- paste0("CC334", c(LETTERS[1:13], "Hb", "Ib"))


names <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334D",
    "Republican Party" = "CC334E",
    "Barack Obama" = "CC334C",
    "Tea Party Movement" = "CC334M", # Tea Party Movement
    "V501" = "CC334L", # Incumbent House of Rep
    "house_cand_1" = "CC334J", # Candidate House of Rep 1
    "house_cand_2" = "CC334K", # Candidate House of Rep 2
    "V513" = "CC334F", # Incumbent Sen 1
    "V521" = "CC334G", # Incumbent Sen 2
    "sen_cand_1" = "CC334H", # Candidate Sen 1
    "sen_cand_2" = "CC334I", # Candidate Sen 2
    "sen_cand_1_sp" = "CC334Hb", # Candidate Sen 1 extra
    "sen_cand_2_sp" = "CC334Ib", # Candidate Sen 2 extra
    "V529" = "CC334B" # Incumbent gov
)


# For all offices, Candidate 1 is the Democrat and Candidate 2 is the Republican,
# except when no Democrat is running.  When no Democrat is running, the Republican
# is listed as Candidate 1.  When only one candidate is running, Candidate 2 is listed as “NA'.

n_common <- 5


name_party <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334D",
    "Republican Party" = "CC334E",
    "Barack Obama" = "CC334C",
    "Tea Party Movement" = "CC334M", # Tea Party Movement
    "V502" = "CC334L", # Incumbent House of Rep
    "house_cand_party_1" = "CC334J", # Candidate House of Rep 1
    "house_cand_party_2" = "CC334K", # Candidate House of Rep 2
    "V514" = "CC334F", # Incumbent Sen 1
    "V522" = "CC334G", # Incumbent Sen 2
    "sen_cand_party_1" = "CC334H", # Candidate Sen 1
    "sen_cand_party_2" = "CC334I", # Candidate Sen 2
    "sen_cand_party_1_sp" = "CC334Hb", # Candidate Sen 1 extra
    "sen_cand_party_2_sp" = "CC334Ib", # Candidate Sen 2 extra
    "V530" = "CC334B" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

df <- df %>%
    mutate(
        house_cand_1 = case_when(
            is.na(V533) ~ V536, # No Dem candidate, cand 1 is Rep cand
            TRUE ~ V533
        ),
        house_cand_party_1 = case_when(
            is.na(V533) ~ "Republican", # No Dem candidate, cand 1 is Rep cand
            TRUE ~ "Democrat"
        ), # If there is a Dem candidate, cand 1 is Dem cand
        house_cand_2 = case_when(
            is.na(V533) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V536) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ V536 # Else it is the Republican candidate
        ),
        house_cand_party_2 = case_when(
            is.na(V533) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V536) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ "Republican" # Else it is the Republican candidate
        ),
        ## Names
        sen_cand_1 = case_when(
            is.na(V548) ~ V551, # No Dem candidate, cand 1 is Rep cand
            TRUE ~ V548
        ),
        sen_cand_2 = case_when(
            is.na(V548) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V551) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ V551 # Else it is the Republican candidate
        ),
        sen_cand_1_sp = case_when(
            is.na(V558) ~ V561, # No Dem candidate, cand 1 is Rep cand
            TRUE ~ V558
        ),
        sen_cand_2_sp = case_when(
            is.na(V558) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V561) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ V561 # Else it is the Republican candidate
        ),
        ## Parties
        sen_cand_party_1 = case_when(
            is.na(V548) ~ "Republican", # No Dem candidate, cand 1 is Rep cand
            TRUE ~ "Democrat"
        ),
        sen_cand_party_2 = case_when(
            is.na(V548) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V551) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ "Republican" # Else it is the Republican candidate
        ),
        sen_cand_party_1_sp = case_when(
            is.na(V558) ~ "Republican", # No Dem candidate, cand 1 is Rep cand
            TRUE ~ "Democrat"
        ),
        sen_cand_party_2_sp = case_when(
            is.na(V558) ~ NA_character_, # No Dem Candidate, cand 2 is missing
            is.na(V561) ~ NA_character_, # No Rep Candidate, cand 2 is missing
            TRUE ~ "Republican" # Else it is the Republican candidate
        )
    )

congressional <- c("CC334L", "CC334J", "CC334K")
senate <- c("CC334F", "CC334G", "CC334H", "CC334I", "CC334Hb", "CC334Ib")
gov <- c("CC334B")
candidates <- c("CC334J", "CC334K", "CC334H", "CC334I", "CC334Hb", "CC334Ib")
incumbents <- c("CC334L", "CC334F", "CC334G", "CC334B")
demos <- c("V100", "V206", "V276")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Tea Party Movement" = "Tea Party Movement",
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "Tea Party Movement" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "__NA__") %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2010_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2010_stats.csv")


#endregion

############################################################################
############################################################################
############################################################################

### NAMES ARE WRONG
# Codebook
#V501 Current Senator 1 Name
#V513 Current Senator 2 Name
#V521 Current House Name
# But V521 has senators
# Going to flip 501 and 521 for now.

# Schaffner
#V501 Current House Name
#V513 Current Senator 1 Name
#V521 Current Senator 2 Name

### NEED TO FIX CODE HAVE NOT CURRENTLY.

#region 2011
rm(list = ls())
df <- read_dta("ces_data/2011/CCES11_Common_OUTPUT.dta")

scores <- paste0("CC342", c(LETTERS[1:9], "M"))
scores[6] <- "CC331F"
names <- c(
    "Self" = "CC342A",
    "Democratic Party" = "CC342D",
    "Republican Party" = "CC342E",
    "Barack Obama" = "CC342C",
    "Tea Party Movement" = "CC342M",
    "Supreme Court" = "CC342I",
    "V501" = "CC342H", # Incumbent House of Rep
    "V513" = "CC331F", # Incumbent Sen 1
    "V521" = "CC342G", # Incumbent Sen 2
    "V529" = "CC342B" # governor
)
n_common <- 6
## No Party info

congressional <- c("CC342H")
senate <- c("CC331F", "CC342G")
gov <- c("CC342B")
candidates <- c()
incumbents <- c("CC342H", "CC331F", "CC342G", "CC342B")
demos <- c("V100", "V206", "V276")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Tea Party Movement" = "Tea Party Movement",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


data_out <- all_df %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2011_data.csv")

rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2011_stats.csv")

#endregion

############################################################################
############################################################################
############################################################################

#region 2012

rm(list = ls())
df <- read_dta("ces_data/2012/commoncontent2012.dta")

scores <- paste0("CC334", c(LETTERS[1:14], "P"))


names <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334E",
    "Republican Party" = "CC334F",
    "Barack Obama" = "CC334C",
    "Mitt Romney" = "CC334D",
    "Tea Party Movement" = "CC334G", # Tea Party Movement
    "Supreme Court" = "CC334P",
    "CurrentHouseName" = "CC334N", # Incumbent House of Rep
    "HouseCand1Name" = "CC334L", # Candidate House of Rep 1
    "HouseCand2Name" = "CC334M", # Candidate House of Rep 2
    "CurrentSen1Name" = "CC334H", # Incumbent Sen 1
    "CurrentSen2Name" = "CC334I", # Incumbent Sen 2
    "SenCand1Name" = "CC334J", # Candidate Sen 1
    "SenCand2Name" = "CC334K", # Candidate Sen 2
    "CurrentGovName" = "CC334B" # Incumbent gov
)


n_common <- 7


name_party <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334E",
    "Republican Party" = "CC334F",
    "Barack Obama" = "CC334C",
    "Mitt Romney" = "CC334D",
    "Tea Party Movement" = "CC334G", # Tea Party Movement
    "Supreme Court" = "CC334P",
    "CurrentHouseParty" = "CC334N", # Incumbent House of Rep
    "HouseCand1Party" = "CC334L", # Candidate House of Rep 1
    "HouseCand2Party" = "CC334M", # Candidate House of Rep 2
    "CurrentSen1Party" = "CC334H", # Incumbent Sen 1
    "CurrentSen2Party" = "CC334I", # Incumbent Sen 2
    "SenCand1Party" = "CC334J", # Candidate Sen 1
    "SenCand2Party" = "CC334K", # Candidate Sen 2
    "CurrentGovParty" = "CC334B" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC334N", "CC334L", "CC334M")
senate <- c("CC334H", "CC334I", "CC334J", "CC334K")
gov <- c("CC334B")
candidates <- c("CC334L", "CC334M", "CC334J", "CC334K")
incumbents <- c("CC334N", "CC334H", "CC334I", "CC334B")
demos <- c("V101", "inputstate", "cdid113")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Mitt Romney" = "Mitt Romney",
        "Supreme Court" = "Supreme Court",
        "Tea Party Movement" = "Tea Party Movement",
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "Mitt Romney" = "Republican",
        "Supreme Court" = NA,
        "Tea Party Movement" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2012_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2012_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

#region 2013
rm(list = ls())
df <- read_dta("ces_data/2013/Common Content Data.dta")

scores <- paste0("CC334", c(LETTERS[1:9], "N", "P"))

names <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334E",
    "Republican Party" = "CC334F",
    "Barack Obama" = "CC334C",
    "Mitt Romney" = "CC334D",
    "Tea Party Movement" = "CC334G",
    "Supreme Court" = "CC334P",
    "CurrentHouseName" = "CC334N", # Incumbent House of Rep
    "CurrentSen1Name" = "CC334H", # Incumbent Sen 1
    "CurrentSen2Name" = "CC334I", # Incumbent Sen 2
    "CurrentGovName" = "CC334B" # governor
)
n_common <- 7
## No Party info

name_party <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334E",
    "Republican Party" = "CC334F",
    "Barack Obama" = "CC334C",
    "Mitt Romney" = "CC334D",
    "Tea Party Movement" = "CC334G",
    "Supreme Court" = "CC334P",
    "CurrentHouseParty" = "CC334N", # Incumbent House of Rep
    "CurrentSen1Party" = "CC334H", # Incumbent Sen 1
    "CurrentSen2Party" = "CC334I", # Incumbent Sen 2
    "CurrentGovParty" = "CC334B" # governor
)


congressional <- c("CC334N")
senate <- c("CC334H", "CC334I")
gov <- c("CC334B")
candidates <- c()
incumbents <- c("CC334N", "CC334H", "CC334I", "CC334B")
demos <- c("caseid", "inputstate", "cdid113")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Mitt Romney" = "Mitt Romney",
        "Tea Party Movement" = "Tea Party Movement",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "Mitt Romney" = "Republican",
        "Supreme Court" = NA,
        "Tea Party Movement" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 101, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2013_data.csv")

rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2013_stats.csv")

#endregion

############################################################################
############################################################################
############################################################################

#region 2014

rm(list = ls())
df <- read_dta("ces_data/2014/CCES14_Common_Content_Validated.dta")

scores <- paste0("CC334", c(LETTERS[1:7], LETTERS[11:23]))


names <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334K",
    "Republican Party" = "CC334L",
    "Barack Obama" = "CC334C",
    "Hillary Clinton" = "CC334D",
    "Ted Cruz" = "CC334E",
    "Rand Paul" = "CC334F",
    "Jeb Bush" = "CC334G",
    "Tea Party Movement" = "CC334M",
    "Supreme Court" = "CC334W",

    "CurrentHouseName" = "CC334V", # Incumbent House of Rep
    "HouseCand1Name" = "CC334T", # Candidate House of Rep 1
    "HouseCand2Name" = "CC334U", # Candidate House of Rep 2
    "CurrentSen1Name" = "CC334N", # Incumbent Sen 1
    "CurrentSen2Name" = "CC334O", # Incumbent Sen 2
    "SenCand1Name" = "CC334P", # Candidate Sen 1
    "SenCand2Name" = "CC334Q", # Candidate Sen 2
    "SenCand1Name2" = "CC334R", # Candidate Sen 1 SP
    "SenCand2Name2" = "CC334S", # Candidate Sen 2 SP
    "CurrentGovName" = "CC334B" # Incumbent gov
)


n_common <- 10


name_party <- c(
    "Self" = "CC334A",
    "Democratic Party" = "CC334K",
    "Republican Party" = "CC334L",
    "Barack Obama" = "CC334C",
    "Hillary Clinton" = "CC334D",
    "Ted Cruz" = "CC334E",
    "Rand Paul" = "CC334F",
    "Jeb Bush" = "CC334G",
    "Tea Party Movement" = "CC334M",
    "Supreme Court" = "CC334W",

    "CurrentHouseParty" = "CC334V", # Incumbent House of Rep
    "HouseCand1Party" = "CC334T", # Candidate House of Rep 1
    "HouseCand2Party" = "CC334U", # Candidate House of Rep 2
    "CurrentSen1Party" = "CC334N", # Incumbent Sen 1
    "CurrentSen2Party" = "CC334O", # Incumbent Sen 2
    "SenCand1Party" = "CC334P", # Candidate Sen 1
    "SenCand2Party" = "CC334Q", # Candidate Sen 2
    "SenCand1Party2" = "CC334R", # Candidate Sen 1 SP
    "SenCand2Party2" = "CC334S", # Candidate Sen 2 SP
    "CurrentGovParty" = "CC334B" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC334V", "CC334T", "CC334U")
senate <- c("CC334N", "CC334O", "CC334P", "CC334Q", "CC334R", "CC334S")
gov <- c("CC334B")
candidates <- c("CC334T", "CC334U", "CC334P", "CC334Q", "CC334R", "CC334S")
incumbents <- c("CC334V", "CC334N", "CC334O", "CC334B")
demos <- c("V101", "inputstate", "cdid")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Hillary Clinton" = "Hillary Clinton",
        "Ted Cruz" = "Ted Cruz",
        "Rand Paul" = "Rand Paul",
        "Jeb Bush" = "Jeb Bush",
        "Tea Party Movement" = "Tea Party Movement",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "Hillary Clinton" = "Democrat",
        "Ted Cruz" = "Republican",
        "Rand Paul" = "Republican",
        "Jeb Bush" = "Republican",
        "Supreme Court" = NA,
        "Tea Party Movement" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2014_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2014_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

#region 2015
rm(list = ls())
df <- read_dta("ces_data/2015/CCES15_Common_OUTPUT_Jan2016.dta")

scores <- paste0("CC15_340", c(letters[1:9]))

names <- c(
    "Self" = "CC15_340a",
    "Democratic Party" = "CC15_340d",
    "Republican Party" = "CC15_340e",
    "Barack Obama" = "CC15_340c",
    "Supreme Court" = "CC15_340i",
    "CurrentHouseName" = "CC15_340h", # Incumbent House of Rep
    "CurrentSen1Name" = "CC15_340f", # Incumbent Sen 1
    "CurrentSen2Name" = "CC15_340g", # Incumbent Sen 2
    "CurrentGovName" = "CC15_340b" # governor
)
n_common <- 5

name_party <- c(
    "Self" = "CC15_340a",
    "Democratic Party" = "CC15_340d",
    "Republican Party" = "CC15_340e",
    "Barack Obama" = "CC15_340c",
    "Supreme Court" = "CC15_340i",
    "CurrentHouseParty" = "CC15_340h", # Incumbent House of Rep
    "CurrentSen1Party" = "CC15_340f", # Incumbent Sen 1
    "CurrentSen2Party" = "CC15_340g", # Incumbent Sen 2
    "CurrentGovParty" = "CC15_340b" # governor
)


congressional <- c("CC15_340h")
senate <- c("CC15_340f", "CC15_340g")
gov <- c("CC15_340b")
candidates <- c()
incumbents <- c("CC15_340h", "CC15_340f", "CC15_340g", "CC15_340b")
demos <- c("V101", "inputstate", "cdid")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "Supreme Court" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2015_data.csv")

rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2015_stats.csv")

#endregion

############################################################################
############################################################################
############################################################################

# region 2016

rm(list = ls())
df <- read_dta("ces_data/2016/CCES16_Common_OUTPUT_Feb2018_VV.dta")

scores <- paste0("CC16_340", c(letters[1:16]))

names <- c(
    "Self" = "CC16_340a",
    "Democratic Party" = "CC16_340g",
    "Republican Party" = "CC16_340h",
    "Barack Obama" = "CC16_340c",
    "Hillary Clinton" = "CC16_340d",
    "Donald Trump" = "CC16_340e",
    "Merrick Garland" = "CC16_340f",
    "Supreme Court" = "CC16_340i",

    "CurrentHouseName" = "CC16_340p", # Incumbent House of Rep
    "HouseCand1Name" = "CC16_340n", # Candidate House of Rep 1
    "HouseCand2Name" = "CC16_340o", # Candidate House of Rep 2
    "CurrentSen1Name" = "CC16_340j", # Incumbent Sen 1
    "CurrentSen2Name" = "CC16_340k", # Incumbent Sen 2
    "SenCand1Name" = "CC16_340l", # Candidate Sen 1
    "SenCand2Name" = "CC16_340m", # Candidate Sen 2
    "CurrentGovName" = "CC16_340b" # Incumbent gov
)


n_common <- 8


name_party <- c(
    "Self" = "CC16_340a",
    "Democratic Party" = "CC16_340g",
    "Republican Party" = "CC16_340h",
    "Barack Obama" = "CC16_340c",
    "Hillary Clinton" = "CC16_340d",
    "Donald Trump" = "CC16_340e",
    "Merrick Garland" = "CC16_340f",
    "Supreme Court" = "CC16_340i",

    "CurrentHouseParty" = "CC16_340p", # Incumbent House of Rep
    "HouseCand1Party" = "CC16_340n", # Candidate House of Rep 1
    "HouseCand2Party" = "CC16_340o", # Candidate House of Rep 2
    "CurrentSen1Party" = "CC16_340j", # Incumbent Sen 1
    "CurrentSen2Party" = "CC16_340k", # Incumbent Sen 2
    "SenCand1Party" = "CC16_340l", # Candidate Sen 1
    "SenCand2Party" = "CC16_340m", # Candidate Sen 2
    "CurrentGovParty" = "CC16_340b" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC16_340p", "CC16_340n", "CC16_340o")
senate <- c("CC16_340j", "CC16_340k", "CC16_340l", "CC16_340m")
gov <- c("CC16_340b")
candidates <- c("CC16_340n", "CC16_340o", "CC16_340l", "CC16_340m")
incumbents <- c("CC16_340p", "CC16_340j", "CC16_340k", "CC16_340b")
demos <- c("V101", "inputstate", "cdid115")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Hillary Clinton" = "Hillary Clinton",
        "Donald Trump" = "Donald Trump",
        "Merrick Garland" = "Merrick Garland",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "Hillary Clinton" = "Democrat",
        "Donald Trump" = "Republican",
        "Merrick Garland" = NA,
        "Supreme Court" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2016_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2016_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2017

rm(list = ls())
df <- read_dta("ces_data/2017/Common Content Data.dta")

scores <- paste0("CC17_350", c(letters[1:9]))

names <- c(
    "Self" = "CC17_350a",
    "Democratic Party" = "CC17_350d",
    "Republican Party" = "CC17_350e",
    "Donald Trump" = "CC17_350c",
    "Supreme Court" = "CC17_350i",
    "CurrentHouseName" = "CC17_350h", # Incumbent House of Rep
    "CurrentSen1Name" = "CC17_350f", # Incumbent Sen 1
    "CurrentSen2Name" = "CC17_350g", # Incumbent Sen 2
    "CurrentGovName" = "CC17_350b" # governor
)
n_common <- 5

name_party <- c(
    "Self" = "CC17_350a",
    "Democratic Party" = "CC17_350d",
    "Republican Party" = "CC17_350e",
    "Donald Trump" = "CC17_350c",
    "Supreme Court" = "CC17_350i",
    "CurrentHouseParty" = "CC17_350h", # Incumbent House of Rep
    "CurrentSen1Party" = "CC17_350f", # Incumbent Sen 1
    "CurrentSen2Party" = "CC17_350g", # Incumbent Sen 2
    "CurrentGovParty" = "CC17_350b" # governor
)


congressional <- c("CC17_350h")
senate <- c("CC17_350f", "CC17_350g")
gov <- c("CC17_350b")
candidates <- c()
incumbents <- c("CC17_350h", "CC17_350f", "CC17_350g", "CC17_350b")
demos <- c("V101", "inputstate", "cdid115")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Supreme Court" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2017_data.csv")

rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2017_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2018

rm(list = ls())
df <- read_csv("ces_data/2018/cces18_common_vv.csv")

fips <- tigris::fips_codes
fips <- unique(fips[, c("state_name", "state_code")])
names(fips) <- c("state", "inputstate")
fips$inputstate <- as.numeric(fips$inputstate)
df <- left_join(df, fips, by = "inputstate")

scores <- paste0("CC18_334", c(LETTERS[1:10], "I2", "J2", "M", "N", "O"))

names <- c(
    "Self" = "CC18_334A",
    "Democratic Party" = "CC18_334D",
    "Republican Party" = "CC18_334E",
    "Donald Trump" = "CC18_334C",
    "Supreme Court" = "CC18_334F",

    "CurrentHouseName" = "CC18_334O", # Incumbent House of Rep
    "HouseCand1Name" = "CC18_334M", # Candidate House of Rep 1
    "HouseCand2Name" = "CC18_334N", # Candidate House of Rep 2
    "CurrentSen1Name" = "CC18_334G", # Incumbent Sen 1
    "CurrentSen2Name" = "CC18_334H", # Incumbent Sen 2
    "SenCand1Name" = "CC18_334I", # Candidate Sen 1
    "SenCand2Name" = "CC18_334J", # Candidate Sen 2
    "SenCand1Name2" = "CC18_334I2", # Candidate Sen 1 SP
    "SenCand2Name2" = "CC18_334J2", # Candidate Sen 2 SP
    "CurrentGovName" = "CC18_334B" # Incumbent gov
)


n_common <- 5


name_party <- c(
    "Self" = "CC18_334A",
    "Democratic Party" = "CC18_334D",
    "Republican Party" = "CC18_334E",
    "Donald Trump" = "CC18_334C",
    "Supreme Court" = "CC18_334F",

    "CurrentHouseParty" = "CC18_334O", # Incumbent House of Rep
    "HouseCand1Party" = "CC18_334M", # Candidate House of Rep 1
    "HouseCand2Party" = "CC18_334N", # Candidate House of Rep 2
    "CurrentSen1Party" = "CC18_334G", # Incumbent Sen 1
    "CurrentSen2Party" = "CC18_334H", # Incumbent Sen 2
    "SenCand1Party" = "CC18_334I", # Candidate Sen 1
    "SenCand2Party" = "CC18_334J", # Candidate Sen 2
    "SenCand1Party2" = "CC18_334I2", # Candidate Sen 1 SP
    "SenCand2Party2" = "CC18_334J2", # Candidate Sen 2 SP
    "CurrentGovParty" = "CC18_334B" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC18_334O", "CC18_334M", "CC18_334N")
senate <- c(
    "CC18_334G",
    "CC18_334H",
    "CC18_334I",
    "CC18_334J",
    "CC18_334I2",
    "CC18_334J2"
)
gov <- c("CC18_334B")
candidates <- c(
    "CC18_334M",
    "CC18_334N",
    "CC18_334I",
    "CC18_334J",
    "CC18_334I2",
    "CC18_334J2"
)
incumbents <- c("CC18_334O", "CC18_334G", "CC18_334H", "CC18_334B")
demos <- c("caseid", "state", "cdid116")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Supreme Court" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2018_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2018_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2019

rm(list = ls())
df <- read_dta("ces_data/2019/CCES19_Common_OUTPUT.dta")

scores <- paste0("CC19_334", c(letters[1:9]))

names <- c(
    "Self" = "CC19_334a",
    "Democratic Party" = "CC19_334d",
    "Republican Party" = "CC19_334e",
    "Donald Trump" = "CC19_334c",
    "Supreme Court" = "CC19_334f",
    "CurrentHouseName" = "CC19_334i", # Incumbent House of Rep
    "CurrentSen1Name" = "CC19_334g", # Incumbent Sen 1
    "CurrentSen2Name" = "CC19_334h", # Incumbent Sen 2
    "CurrentGovName" = "CC19_334b" # governor
)
n_common <- 5

name_party <- c(
    "Self" = "CC19_334a",
    "Democratic Party" = "CC19_334d",
    "Republican Party" = "CC19_334e",
    "Donald Trump" = "CC19_334c",
    "Supreme Court" = "CC19_334f",
    "CurrentHouseParty" = "CC19_334i", # Incumbent House of Rep
    "CurrentSen1Party" = "CC19_334g", # Incumbent Sen 1
    "CurrentSen2Party" = "CC19_334h", # Incumbent Sen 2
    "CurrentGovParty" = "CC19_334b" # governor
)


congressional <- c("CC19_334i")
senate <- c("CC19_334g", "CC19_334h")
gov <- c("CC19_334b")
candidates <- c()
incumbents <- c("CC19_334i", "CC19_334g", "CC19_334h", "CC19_334b")
demos <- c("caseid", "inputstate", "cdid116")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer(4:(length(names) + 3)) %>%
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Supreme Court" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2019_data.csv")

rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, status, type) %>%
    summarize(n = n())

stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2019_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2020

rm(list = ls())
df <- read_dta("ces_data/2020/CES20_Common_OUTPUT_vv.dta")

# Dropping the ones that were given the wrong names
misidentified <- read_csv("ces_data/2020/misidentifiedNCrespondents.csv")

df <- df %>% filter(!caseid %in% misidentified$caseid)

scores <- paste0("CC20_340", c(letters[1:13]))

names <- c(
    "Self" = "CC20_340a",
    "Democratic Party" = "CC20_340e",
    "Republican Party" = "CC20_340f",
    "Joe Biden" = "CC20_340c",
    "Donald Trump" = "CC20_340d",

    "CurrentHouseName" = "CC20_340m", # Incumbent House of Rep
    "HouseCand1Name" = "CC20_340k", # Candidate House of Rep 1
    "HouseCand2Name" = "CC20_340l", # Candidate House of Rep 2
    "CurrentSen1Name" = "CC20_340g", # Incumbent Sen 1
    "CurrentSen2Name" = "CC20_340h", # Incumbent Sen 2
    "SenCand1Name" = "CC20_340i", # Candidate Sen 1
    "SenCand2Name" = "CC20_340j", # Candidate Sen 2
    "CurrentGovName" = "CC20_340b" # Incumbent gov
)


n_common <- 5


name_party <- c(
    "Self" = "CC20_340a",
    "Democratic Party" = "CC20_340e",
    "Republican Party" = "CC20_340f",
    "Joe Biden" = "CC20_340c",
    "Donald Trump" = "CC20_340d",

    "CurrentHouseParty" = "CC20_340m", # Incumbent House of Rep
    "HouseCand1Party" = "CC20_340k", # Candidate House of Rep 1
    "HouseCand2Party" = "CC20_340l", # Candidate House of Rep 2
    "CurrentSen1Party" = "CC20_340g", # Incumbent Sen 1
    "CurrentSen2Party" = "CC20_340h", # Incumbent Sen 2
    "SenCand1Party" = "CC20_340i", # Candidate Sen 1
    "SenCand2Party" = "CC20_340j", # Candidate Sen 2
    "CurrentGovParty" = "CC20_340b" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC20_340m", "CC20_340k", "CC20_340l")
senate <- c("CC20_340g", "CC20_340h", "CC20_340i", "CC20_340j")
gov <- c("CC20_340b")
candidates <- c("CC20_340k", "CC20_340l", "CC20_340i", "CC20_340j")
incumbents <- c("CC20_340m", "CC20_340g", "CC20_340h", "CC20_340b")
demos <- c("caseid", "inputstate", "cdid116")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Joe Biden" = "Joe Biden"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Joe Biden" = "Democrat"
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2020_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2020_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2021

rm(list = ls())
df <- read_dta("ces_data/2021/CCES21_Common_OUTPUT.dta")

scores <- paste0("CC21_330", c(letters[1:9]))

names <- c(
    "Self" = "CC21_330a",
    "Democratic Party" = "CC21_330e",
    "Republican Party" = "CC21_330f",
    "Joe Biden" = "CC21_330c",
    "Donald Trump" = "CC21_330d",

    "CurrentHouseName" = "CC21_330i", # Incumbent House of Rep
    "CurrentSen1Name" = "CC21_330g", # Incumbent Sen 1
    "CurrentSen2Name" = "CC21_330h", # Incumbent Sen 2
    "CurrentGovName" = "CC21_330b" # Current Gov
)


n_common <- 5


name_party <- c(
    "Self" = "CC21_330a",
    "Democratic Party" = "CC21_330e",
    "Republican Party" = "CC21_330f",
    "Joe Biden" = "CC21_330c",
    "Donald Trump" = "CC21_330d",
    "CurrentHouseParty" = "CC21_330i", # Incumbent House of Rep
    "CurrentSen1Party" = "CC21_330g", # Incumbent Sen 1
    "CurrentSen2Party" = "CC21_330h", # Incumbent Sen 2
    "CurrentGovParty" = "CC21_330b" # Current Gov
)


congressional <- c("CC21_330i")
senate <- c("CC21_330g", "CC21_330h")
gov <- c("CC21_330b")
candidates <- NULL
incumbents <- c("CC21_330g", "CC21_330h", "CC21_330b", "CC21_330i")
demos <- c("caseid", "inputstate", "cdid117")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Joe Biden" = "Joe Biden"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Joe Biden" = "Democrat"
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2021_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2021_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2022

rm(list = ls())
df <- read_dta("ces_data/2022/CES22_Common.dta", encoding = "latin1")


scores <- paste0("CC22_340", c(letters[1:14]))

names <- c(
    "Self" = "CC22_340a",
    "Democratic Party" = "CC22_340e",
    "Republican Party" = "CC22_340f",
    "Joe Biden" = "CC22_340c",
    "Donald Trump" = "CC22_340d",
    "Supreme Court" = "CC22_340g",
    "CurrentHouseName" = "CC22_340n", # Incumbent House of Rep
    "HouseCand1Name" = "CC22_340l", # Candidate House of Rep 1
    "HouseCand2Name" = "CC22_340m", # Candidate House of Rep 2
    "CurrentSen1Name" = "CC22_340h", # Incumbent Sen 1
    "CurrentSen2Name" = "CC22_340i", # Incumbent Sen 2
    "SenCand1Name" = "CC22_340j", # Candidate Sen 1
    "SenCand2Name" = "CC22_340k", # Candidate Sen 2
    "CurrentGovName" = "CC22_340b" # Incumbent gov
)


n_common <- 6


name_party <- c(
    "Self" = "CC22_340a",
    "Democratic Party" = "CC22_340e",
    "Republican Party" = "CC22_340f",
    "Joe Biden" = "CC22_340c",
    "Donald Trump" = "CC22_340d",
    "Supreme Court" = "CC22_340g",
    "CurrentHouseParty" = "CC22_340n", # Incumbent House of Rep
    "HouseCand1Party" = "CC22_340l", # Candidate House of Rep 1
    "HouseCand2Party" = "CC22_340m", # Candidate House of Rep 2
    "CurrentSen1Party" = "CC22_340h", # Incumbent Sen 1
    "CurrentSen2Party" = "CC22_340i", # Incumbent Sen 2
    "SenCand1Party" = "CC22_340j", # Candidate Sen 1
    "SenCand2Party" = "CC22_340k", # Candidate Sen 2
    "CurrentGovParty" = "CC22_340b" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC22_340n", "CC22_340l", "CC22_340m")
senate <- c("CC22_340h", "CC22_340i", "CC22_340j", "CC22_340k")
gov <- c("CC22_340b")
candidates <- c("CC22_340l", "CC22_340m", "CC22_340j", "CC22_340k")
incumbents <- c("CC22_340n", "CC22_340h", "CC22_340i", "CC22_340b")
demos <- c("caseid", "inputstate", "cdid118")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Joe Biden" = "Joe Biden",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Joe Biden" = "Democrat",
        "Supreme Court" = "Supreme Court"
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2022_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2022_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2023

rm(list = ls())
df <- read_dta("ces_data/2023/CCES23_Common_OUTPUT.dta")


scores <- paste0("CC23_330", c(letters[1:10]))

names <- c(
    "Self" = "CC23_330a",
    "Democratic Party" = "CC23_330e",
    "Republican Party" = "CC23_330f",
    "Joe Biden" = "CC23_330c",
    "Donald Trump" = "CC23_330d",
    "Supreme Court" = "CC23_330g",
    "CurrentHouseName" = "CC23_330j", # Incumbent House of Rep
    "CurrentSen1Name" = "CC23_330h", # Incumbent Sen 1
    "CurrentSen2Name" = "CC23_330i", # Incumbent Sen 2
    "CurrentGovName" = "CC23_330b" # Incumbent gov
)


n_common <- 6


name_party <- c(
    "Self" = "CC23_330a",
    "Democratic Party" = "CC23_330e",
    "Republican Party" = "CC23_330f",
    "Joe Biden" = "CC23_330c",
    "Donald Trump" = "CC23_330d",
    "Supreme Court" = "CC23_330g",
    "CurrentHouseParty" = "CC23_330j", # Incumbent House of Rep
    "CurrentSen1Party" = "CC23_330h", # Incumbent Sen 1
    "CurrentSen2Party" = "CC23_330i", # Incumbent Sen 2
    "CurrentGovParty" = "CC23_330b" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC23_330j")
senate <- c("CC23_330h", "CC23_330i")
gov <- c("CC23_330b")
incumbents <- c("CC23_330j", "CC23_330h", "CC23_330i", "CC23_330b")
demos <- c("caseid", "inputstate", "cdid118")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Joe Biden" = "Joe Biden",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Joe Biden" = "Democrat",
        "Supreme Court" = "Supreme Court"
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2023_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2023_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

# region 2024

rm(list = ls())
df <- read_dta("ces_data/2024/CES24_Common.dta", encoding = "latin1")


scores <- paste0("CC24_330", c(letters[1:12]))
scores <- scores[-11] ## no k
names <- c(
    "Self" = "CC24_330a",
    "Democratic Party" = "CC24_330f",
    "Republican Party" = "CC24_330g",
    "Joe Biden" = "CC24_330c",
    "Kamala Harris" = "CC24_330d",
    "Donald Trump" = "CC24_330e",
    "Supreme Court" = "CC24_330h",
    "CurrentHouseName" = "CC24_330l", # Incumbent House of Rep
    "CurrentSen1Name" = "CC24_330i", # Incumbent Sen 1
    "CurrentSen2Name" = "CC24_330j", # Incumbent Sen 2
    "CurrentGovName" = "CC24_330b" # Incumbent gov
)


n_common <- 7


name_party <- c(
    "Self" = "CC24_330a",
    "Democratic Party" = "CC24_330f",
    "Republican Party" = "CC24_330g",
    "Joe Biden" = "CC24_330c",
    "Kamala Harris" = "CC24_330d",
    "Donald Trump" = "CC24_330e",
    "Supreme Court" = "CC24_330h",
    "CurrentHouseParty" = "CC24_330l", # Incumbent House of Rep
    "CurrentSen1Party" = "CC24_330i", # Incumbent Sen 1
    "CurrentSen2Party" = "CC24_330j", # Incumbent Sen 2
    "CurrentGovParty" = "CC24_330b" # Incumbent gov
)
#df %>% filter(V206 == 1) %>% select(all_of(congressional))

congressional <- c("CC24_330l")
senate <- c("CC24_330i", "CC24_330j")
gov <- c("CC24_330b")
incumbents <- c("CC24_330l", "CC24_330i", "CC24_330j", "CC24_330b")
demos <- c("caseid", "inputstate", "cdid118")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Donald Trump" = "Donald Trump",
        "Kamala Harris" = "Kamala Harris",
        "Joe Biden" = "Joe Biden",
        "Supreme Court" = "Supreme Court"
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Donald Trump" = "Republican",
        "Kamala Harris" = "Democrat",
        "Joe Biden" = "Democrat",
        "Supreme Court" = "Supreme Court"
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% incumbents ~ "Incumbent",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2024_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2024_stats.csv")

# endregion

############################################################################
############################################################################
############################################################################

#region 2010_2012_panel

df <- read_dta(
    "ces_data/Panel_2010_2012/CCES12_Panel_OUTPUT_10Oct2013_with2010_vv_V2.dta"
)

scores <- paste0("CC12_341", c(LETTERS[1:18]))


names <- c(
    "Self" = "CC12_341A",
    "Democratic Party" = "CC12_341E",
    "Republican Party" = "CC12_341F",
    "Barack Obama" = "CC12_341C",
    "Mitt Romney" = "CC12_341D",
    "Supreme Court" = "CC12_341O",
    "House of Representatives" = "CC12_341P",
    "Senate" = "CC12_341Q",
    "Tea Party Movement" = "CC12_341R", # Tea Party Movement
    "CC12_CurrentHouseName" = "CC12_341M", # Incumbent House of Rep
    "CC12_HouseCand1Name" = "CC12_341K", # Candidate House of Rep 1
    "CC12_HouseCand2Name" = "CC12_341L", # Candidate House of Rep 2
    "CC12_CurrentSen1Name" = "CC12_341G", # Incumbent Sen 1
    "CC12_CurrentSen2Name" = "CC12_341H", # Incumbent Sen 2
    "CC12_SenCand1Name" = "CC12_341I", # Candidate Sen 1
    "CC12_SenCand2Name" = "CC12_341J", # Candidate Sen 2
    "CC12_FormerHouseName" = "CC12_341N", # Former House Member
    "CC12_CurrentGovName" = "CC12_341B" # Incumbent gov
)


n_common <- 9


name_party <- c(
    "Self" = "CC12_341A",
    "Democratic Party" = "CC12_341E",
    "Republican Party" = "CC12_341F",
    "Barack Obama" = "CC12_341C",
    "Mitt Romney" = "CC12_341D",
    "Supreme Court" = "CC12_341O",
    "House of Representatives" = "CC12_341P",
    "Senate" = "CC12_341Q",
    "Tea Party Movement" = "CC12_341R", # Tea Party Movement
    "CC12_CurrentHouseParty" = "CC12_341M", # Incumbent House of Rep
    "CC12_HouseCand1Party" = "CC12_341K", # Candidate House of Rep 1
    "CC12_HouseCand2Party" = "CC12_341L", # Candidate House of Rep 2
    "CC12_CurrentSen1Party" = "CC12_341G", # Incumbent Sen 1
    "CC12_CurrentSen2Party" = "CC12_341H", # Incumbent Sen 2
    "CC12_SenCand1Party" = "CC12_341I", # Candidate Sen 1
    "CC12_SenCand2Party" = "CC12_341J", # Candidate Sen 2
    "CC12_FormerHouseParty" = "CC12_341N", # Former House Member
    "CC12_CurrentGovParty" = "CC12_341B" # Incumbent gov
)


congressional <- c("CC12_341M", "CC12_341K", "CC12_341L", "CC12_341N")
senate <- c("CC12_341G", "CC12_341H", "CC12_341I", "CC12_341J")
gov <- c("CC12_341B")
candidates <- c("CC12_341K", "CC12_341L", "CC12_341I", "CC12_341J")
incumbents <- c("CC12_341M", "CC12_341G", "CC12_341H", "CC12_341B")
former <- c("CC12_341N")
demos <- c("caseid", "inputstate_12", "cdid113_12")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "Mitt Romney" = "Mitt Romney",
        "House of Representatives" = "House of Representatives",
        "Senate" = "Senate",
        "Supreme Court" = "Supreme Court",
        "Tea Party Movement" = "Tea Party Movement",
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    mutate("CC12_FormerHouseParty" = "Not in Data") %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "Mitt Romney" = "Republican",
        "House of Representatives" = NA,
        "Senate" = NA,
        "Supreme Court" = NA,
        "Tea Party Movement" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "__NA__") %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            match %in% incumbents ~ "Former",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2010_2012_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2010_2012_stats.csv")


#endregion

############################################################################
############################################################################
############################################################################

#region 2010_2014_panel

df <- read_dta("ces_data/Panel_2010_2014/CCES_Panel_Full3waves_VV_V4.dta")

scores <- paste0("CC14_341", c(LETTERS[1:18]))
scores <- scores[-4]
scores <- scores[-13]

names <- c(
    "Self" = "CC14_341A",
    "Democratic Party" = "CC14_341E",
    "Republican Party" = "CC14_341F",
    "Barack Obama" = "CC14_341C",
    "Supreme Court" = "CC14_341O",
    "House of Representatives" = "CC14_341P",
    "Senate" = "CC14_341Q",
    "Tea Party Movement" = "CC14_341R",
    "CC14_CurrentHouseName" = "CC14_341M", # Incumbent House of Rep
    "CC14_HouseCand1Name" = "CC14_341K", # Candidate House of Rep 1
    "CC14_HouseCand2Name" = "CC14_341L", # Candidate House of Rep 2
    "CC14_CurrentSen1Name" = "CC14_341G", # Incumbent Sen 1
    "CC14_CurrentSen2Name" = "CC14_341H", # Incumbent Sen 2
    "CC14_SenCand1Name" = "CC14_341I", # Candidate Sen 1
    "CC14_SenCand2Name" = "CC14_341J", # Candidate Sen 2
    #"CC14_FormerHouseName" = "CC14_341N", # Former House Member
    "CC14_CurrentGovName" = "CC14_341B" # Incumbent gov
)


n_common <- 8


name_party <- c(
    "Self" = "CC14_341A",
    "Democratic Party" = "CC14_341E",
    "Republican Party" = "CC14_341F",
    "Barack Obama" = "CC14_341C",
    "Supreme Court" = "CC14_341O",
    "House of Representatives" = "CC14_341P",
    "Senate" = "CC14_341Q",
    "Tea Party Movement" = "CC14_341R", # Tea Party Movement
    "CC14_CurrentHouseParty" = "CC14_341M", # Incumbent House of Rep
    "CC14_HouseCand1Party" = "CC14_341K", # Candidate House of Rep 1
    "CC14_HouseCand2Party" = "CC14_341L", # Candidate House of Rep 2
    "CC14_CurrentSen1Party" = "CC14_341G", # Incumbent Sen 1
    "CC14_CurrentSen2Party" = "CC14_341H", # Incumbent Sen 2
    "CC14_SenCand1Party" = "CC14_341I", # Candidate Sen 1
    "CC14_SenCand2Party" = "CC14_341J", # Candidate Sen 2
    #"CC14_FormerHouseParty" = "CC14_341N", # Former House Member
    "CC14_CurrentGovParty" = "CC14_341B" # Incumbent gov
)


congressional <- c("CC14_341M", "CC14_341K", "CC14_341L", "CC14_341N")
senate <- c("CC14_341G", "CC14_341H", "CC14_341I", "CC14_341J")
gov <- c("CC14_341B")
candidates <- c("CC14_341K", "CC14_341L", "CC14_341I", "CC14_341J")
incumbents <- c("CC14_341M", "CC14_341G", "CC14_341H", "CC14_341B")
former <- c("CC14_341N")
demos <- c("caseid", "inputstate_14", "cdid114_14")

scores_df <- df %>%
    select(all_of(demos), all_of(scores)) %>%
    pivot_longer(all_of(scores)) %>%
    rename(match = name, rating = value)

name_cols <- names(names)[-1:-n_common]
all_df <- df %>%
    select(all_of(demos), all_of(name_cols)) %>%
    mutate(
        "Democratic Party" = "Democratic Party",
        "Republican Party" = "Republican Party",
        "Self" = "Self",
        "Barack Obama" = "Barack Obama",
        "House of Representatives" = "House of Representatives",
        "Senate" = "Senate",
        "Supreme Court" = "Supreme Court",
        "Tea Party Movement" = "Tea Party Movement",
    ) %>%
    pivot_longer((4):(length(names) + 3)) %>% # need to skip demogaphics
    mutate(match = names[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(candidate = value) %>%
    full_join(scores_df)


name_party_cols <- names(name_party)[-1:-n_common]
all_df <- df %>%
    mutate("CC14_FormerHouseParty" = "Not in Data") %>%
    select(all_of(demos), all_of(name_party_cols)) %>%
    mutate(
        "Democratic Party" = "Democrat",
        "Republican Party" = "Republican",
        "Self" = NA,
        "Barack Obama" = "Democrat",
        "House of Representatives" = NA,
        "Senate" = NA,
        "Supreme Court" = NA,
        "Tea Party Movement" = NA
    ) %>%
    mutate(across(all_of(name_party_cols), ~ as.character(as_factor(.x)))) %>%
    pivot_longer(4:(length(name_party) + 3)) %>%
    mutate(match = name_party[name]) %>%
    select(all_of(demos), value, match) %>%
    rename(party = value) %>%
    full_join(all_df)

data_out <- all_df %>%
    filter(candidate != "__NA__") %>%
    filter(candidate != "") %>%
    drop_na(rating) %>%
    mutate(
        rating = zap_labels(rating),
        rating = ifelse(rating == 8, NA, rating)
    ) %>%
    rename(voter_id = demos[1], state = demos[2], district = demos[3]) %>%
    mutate(state = as.character(as_factor(state))) %>%
    mutate(
        "type" = case_when(
            match %in% congressional ~ "Rep",
            match %in% senate ~ "Sen",
            match %in% gov ~ "Gov",
            TRUE ~ "Common"
        ),
        "status" = case_when(
            match %in% candidates ~ "Candidate",
            match %in% incumbents ~ "Incumbent",
            match %in% incumbents ~ "Former",
            TRUE ~ NA_character_
        )
    )

write_csv(data_out, "cleaned_data/2010_2014_data.csv")
rep_stats <- data_out %>%
    filter(type == "Rep") %>%
    group_by(state, district, candidate, party, status, type) %>%
    summarize(n = n())

sen_stats <- data_out %>%
    filter(type == "Sen") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())

gov_stats <- data_out %>%
    filter(type == "Gov") %>%
    group_by(state, candidate, party, status, type) %>%
    summarize(n = n())


stats <- rbind(rep_stats, sen_stats, gov_stats)
write_csv(stats, "cleaned_data/2010_2014_stats.csv")

#endregion

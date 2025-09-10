setwd(here::here("generate"))
library(tidyverse)


matched <- read_csv("data/supplementary_data/matched_with_all_pre_2023.csv")
matched <- matched |>
  pivot_longer(cols = `2006`:`2022`, names_to = "year", values_to = "obs") |>
  drop_na(obs) |>
  mutate(
    year = case_when(
      grepl("_", year) ~ str_split_i(year, "_", i = 2),
      .default = year
    )
  ) |>
  mutate(year = as.numeric(year)) |>
  drop_na(year)


load("data/supplementary_data/dime_recipients_1979_2024.rdata")
cands <- cands |>
  filter(
    cycle > 2002 &
      seat %in% c("federal:house", "federal:senate", "state:governor")
  )

cands <- cands |> mutate(name = str_trim(paste(ffname, lname, suffix)))
state <- tigris::fips_codes |> distinct(state, state_name)
cands <- left_join(cands, state, by = join_by(state == state)) |>
  mutate(state = tolower(state_name)) |>
  select(bonica.rid, cycle, name, state) |>
  distinct() |>
  rename(candidate = name, state = state, year = cycle)

write_csv(cands, "posterior/updating_crosswalk/bonica_data.csv", na = "")

write_csv(matched, "posterior/updating_crosswalk/ces_data.csv", na = "")

######### Go run python script

linked <- read_csv("posterior/updating_crosswalk/linked.csv")
linked <- linked |>
  drop_na(Unique_Project_ID, score) |>
  group_by(Unique_Project_ID) |>
  slice_max(score, n = 1)
linked$...1 <- NULL
load("data/supplementary_data/dime_recipients_1979_2024.rdata")

linked <- cands |>
  select(bonica.rid, cycle, seat, distcyc, district) |>
  right_join(
    linked,
    by = join_by(cycle == year_bonica, bonica.rid == bonica.rid),
    suffix = c("_bonica", "_ces")
  ) |>
  arrange(Unique_Project_ID) |>
  mutate(
    check = district_ces == as.numeric(str_split_i(distcyc, "_", i = 3))
  ) |>
  relocate(
    candidate_bonica,
    candidate_ces,
    bonica.rid,
    Unique_Project_ID,
    district_bonica,
    district_ces,
    check,
    .before = 1
  )


write_csv(linked, "posterior/updating_crosswalk/linked_check.csv", na = "")

#### Go and manually double check

## now check for duplicates. There should only be one Unique_Project_ID per Bonica id and vice versa
fixed <- read_csv("posterior/updating_crosswalk/linked_check.csv")

fixed |>
  select(bonica.rid, Unique_Project_ID) |>
  drop_na() |> ## missing from manual removal
  distinct() |> ## multiple years
  group_by(bonica.rid) |>
  tally() |>
  filter(n > 1) |>
  left_join(fixed) |>
  View()


fixed |>
  select(bonica.rid, Unique_Project_ID) |>
  drop_na() |> ## missing from manual removal
  distinct() |> ## multiple years
  group_by(Unique_Project_ID) |>
  tally() |>
  filter(n > 1) |>
  left_join(fixed) |>
  View()

## Issues from July 2025 all checked

# Mike Collins (cand1279) might be two people or one person (4251 and 1164) From 2014 on this is 4251, previous to that this is 1164
# Dale Kildee (cand342) might be two people or one person (1941 and 1938) In the 2012 election 1938 retired and 1941 took over but doesn't seem to show up in Bonica data until 2016
# Jennie Lee Lake (1454) might be two people or one person (cand146694 and cand144651) I am correct, this is the same person
# Liz Joy (cand148374) might be two people or one person (2591 and 4395) FIXED
# Jay Chen (cand40926) might be two people or one person (4168 and 424) FIXED
# 4586 - is now Menendez JR need to add to crosswalk FIXED

### Create the actual crosswalk

fixed <- read_csv("posterior/updating_crosswalk/linked_check.csv")

cross <- fixed |>
  select(bonica.rid, Unique_Project_ID) |>
  drop_na() |> ## missing from manual removal
  distinct()

#cross |> filter(bonica.rid == "cand148374" | bonica.rid == "cand40926")

cross <- cross |>
  mutate(
    Unique_Project_ID = case_match(
      ### Can remove when I re-run the model. This just writes over IDs with he "incorrect" ones
      bonica.rid,
      "cand148374" ~ 2591,
      "cand40926" ~ 424,
      .default = Unique_Project_ID
    )
  ) |>
  distinct() |>
  drop_na()

matched <- read_csv("data/supplementary_data/matched_with_all_pre_2023.csv")

matched <- matched |>
  pivot_longer(cols = `2006`:`2022`, names_to = "year", values_to = "obs") |>
  drop_na(obs) |>
  mutate(
    year = case_when(
      grepl("_", year) ~ str_split_i(year, "_", i = 2),
      .default = year
    )
  ) |>
  mutate(year = as.numeric(year)) |>
  select(-chamber, -bioguide_id, -`Ran prior to 2019`, -`Next ID`, -Notes) |>
  filter(Unique_Project_ID > 0) |>
  group_by(Unique_Project_ID, year) |>
  slice_max(obs) |> ## find the one most likely to be the correct district
  select(-obs)

cross_out <- matched |>
  #  filter(Unique_Project_ID != 1454) |>
  left_join(cross) |>
  filter(!(bonica.rid == "cand146694" & year < 2022) | is.na(bonica.rid), ) |>
  filter(!(bonica.rid == "cand144651" & year == 2022) | is.na(bonica.rid)) |>
  rename(ces_name = candidate)

cross_out |> group_by(Unique_Project_ID, year) |> tally() |> filter(n > 1) ## annoying duplicates


cross_out <- cross_out |>
  filter(!(Unique_Project_ID %in% c(389, 1011) & year > 2010)) |>
  filter(!(Unique_Project_ID == 1100 & district == 10)) |>
  mutate(
    district = case_when(
      Unique_Project_ID == 1100 & year == 2022 ~ 7,
      .default = district
    )
  ) |>
  filter(ces_name != "David A. Trott") |>
  filter(
    !(Unique_Project_ID == 3833 & year == 2018 & ces_name == "Robert Scott")
  ) |>
  filter(!(Unique_Project_ID == 2886 & year == 2010 & type == "Gov"))

write_csv(cross_out, "posterior/crosswalk.csv", na = "")

### Add 538 IDS

house_df <- read_csv("data/supplementary_data/election_results_house.csv")
senate_df <- read_csv("data/supplementary_data/election_results_senate.csv")
gov_df <- read_csv("data/supplementary_data/election_results_gubernatorial.csv")

election_df <- rbind(house_df, senate_df, gov_df) |>
  filter(cycle > 2005) |>
  mutate(state = str_to_lower(state)) |>
  select(candidate_name, politician_id, state, cycle) |>
  distinct()


write_csv(election_df, "posterior/updating_crosswalk/538_data.csv", na = "")


######### Go run python script

linked <- read_csv("posterior/updating_crosswalk/linked_538.csv")
linked <- linked |>
  drop_na(Unique_Project_ID, score) |>
  group_by(Unique_Project_ID) |>
  slice_max(score, n = 1)
linked$...1 <- NULL


house_df <- read_csv("data/supplementary_data/election_results_house.csv")
senate_df <- read_csv("data/supplementary_data/election_results_senate.csv")
gov_df <- read_csv("data/supplementary_data/election_results_gubernatorial.csv")

election_df <- rbind(house_df, senate_df, gov_df) |>
  filter(cycle > 2005)

linked <- election_df |>
  select(office_seat_name, politician_id, cycle) |>
  right_join(
    linked,
    by = join_by(cycle == year_bonica, politician_id == politician_id)
  ) |>
  arrange(Unique_Project_ID) |>
  relocate(
    candidate_bonica,
    candidate_ces,
    politician_id,
    Unique_Project_ID,
    office_seat_name,
    district,
    .before = 1
  )


write_csv(linked, "posterior/updating_crosswalk/linked_check_538.csv", na = "")

####### Double check

fixed <- read_csv("posterior/updating_crosswalk/linked_check_538.csv")

fixed |>
  select(politician_id, Unique_Project_ID) |>
  drop_na() |> ## missing from manual removal
  distinct() |> ## multiple years
  group_by(politician_id) |>
  tally() |>
  filter(n > 1) |>
  left_join(fixed) |>
  View()


fixed |>
  select(politician_id, Unique_Project_ID) |>
  drop_na() |> ## missing from manual removal
  distinct() |> ## multiple years
  group_by(Unique_Project_ID) |>
  tally() |>
  filter(n > 1) |>
  left_join(fixed) |>
  View()

####
fixed <- read_csv("posterior/updating_crosswalk/linked_check_538.csv")

cross <- read_csv("posterior/crosswalk.csv")

fixed <- fixed |>
  select(politician_id, Unique_Project_ID) |>
  mutate(
    Unique_Project_ID = case_match(
      ### Can remove when I re-run the model. This just writes over IDs with he "incorrect" ones
      politician_id,
      10594 ~ 2591,
      8509 ~ 424,
      .default = Unique_Project_ID
    )
  ) |>
  distinct() |>
  drop_na()


full <- fixed |> ### will give duplicate, fixed below
  right_join(cross, na_matches = "never")


# full |> group_by(Unique_Project_ID, year) |> tally() |> filter(n > 1)
# full |> filter(Unique_Project_ID == 1013) |> select(politician_id) |> unique()

# house_df <- read_csv("data/supplementary_data/election_results_house.csv")
# senate_df <- read_csv("data/supplementary_data/election_results_senate.csv")
# gov_df <- read_csv("data/supplementary_data/election_results_gubernatorial.csv")
# election_df <- rbind(house_df, senate_df, gov_df) |>
#   filter(cycle > 2005)

# election_df |>
#   filter(politician_id %in% c(18253, 2791)) |>
#   select(cycle, politician_id, candidate_id)

full <- full |>
  filter(!(politician_id == 2791 & year == 2022) | is.na(politician_id)) |>
  filter(!(politician_id == 18253 & year < 2022) | is.na(politician_id))

######## adding in icpsr

load("data/supplementary_data/dime_recipients_1979_2024.rdata")
cands <- cands |>
  filter(
    cycle > 2002 &
      seat %in% c("federal:house", "federal:senate", "state:governor")
  )

icpsr2 <- cands |>
  filter(
    !(seat == "federal:senate" & cycle == 2010 & bonica.rid == "cand35704")
  ) |>
  filter(
    !(seat == "federal:senate" & cycle == 2010 & bonica.rid == "cand104712")
  ) |>
  filter(
    !(seat == "federal:senate" & cycle == 2016 & bonica.rid == "cand122006")
  ) |>
  filter(
    !(seat == "state:governor" & cycle == 2018 & bonica.rid == "cand10001129")
  ) |>
  filter(
    !(seat == "federal:senate" &
      cycle %in% c(2012, 2016, 2018) &
      bonica.rid == "cand103960")
  ) |>
  select(ICPSR2, bonica.rid, cycle) |>
  distinct()

tmp <- full |>
  left_join(icpsr2, by = join_by(bonica.rid == bonica.rid, year == cycle)) |>
  mutate(
    ICPSR2 = case_when(
      bonica.rid == "cand103960" ~ "H2NV04045",
      .default = ICPSR2
    )
  )

full_with_id <- drop_na(full, bonica.rid)
full_without_id <- filter(full, is.na(bonica.rid))

cross <- tmp |>
  select(bonica.rid, ICPSR2) |>
  drop_na() |>
  distinct() |>
  right_join(full_with_id) |>
  filter(!(ICPSR2 == "H4OK02154" & year %in% c(2016))) |>
  filter(!(ICPSR2 == "H4OK02170" & year %in% c(2022, 2024))) |>
  filter(!(ICPSR2 == "H8CA07049" & year %in% c(2016))) |>
  filter(!(ICPSR2 == "H6CA11169" & year %in% c(2008, 2010))) |>
  filter(!(ICPSR2 == "PA5023799" & year > 2019)) |>
  filter(!(ICPSR2 == "PA_c_2014C1307" & year < 2020)) |>
  filter(!(ICPSR2 == "NY6466806" & year != 2014)) |>
  filter(!(ICPSR2 == "NY13066" & year == 2014))

full_without_id$ICPSR2 <- NA
cross <- rbind(cross, full_without_id)

write_csv(cross, "posterior/crosswalk.csv", na = "")

##### Updating with 2024

matched <- read_csv("data/supplementary_data/matched_with_all.csv")
crosswalk <- read_csv("posterior/crosswalk.csv")

crosswalk[which(!crosswalk$Unique_Project_ID %in% matched$Unique_Project_ID), ]

crosswalk <- crosswalk |>
  mutate(
    Unique_Project_ID = case_match(
      Unique_Project_ID,
      4219 ~ 3384,
      4229 ~ 977,
      .default = Unique_Project_ID
    )
  )

matched <- matched |>
  pivot_longer(cols = `2006`:`2024`, names_to = "year", values_to = "obs") |>
  drop_na(obs) |>
  mutate(
    year = case_when(
      grepl("_", year) ~ str_split_i(year, "_", i = 2),
      .default = year
    )
  ) |>
  mutate(year = as.numeric(year)) |>
  drop_na(year) |>
  filter(year %in% 2023:2024) |>
  select(-chamber, -bioguide_id, -`Ran prior to 2019`, -Notes) |>
  filter(Unique_Project_ID > 0) |>
  group_by(Unique_Project_ID, year) |>
  slice_max(obs) |> ## find the one most likely to be the correct district
  select(-obs) |>
  rename(ces_name = candidate)

distinct_cross <- crosswalk |>
  select(-year, -state, -district, -ces_name, -type) |>
  distinct()

new <- matched |> left_join(distinct_cross)

full <- rbind(crosswalk, new)

write_csv(full, "posterior/crosswalk.csv", na = "")

rm(list = ls())

library(tidyverse)

setwd(here::here("generate"))

data <- read_csv("data/cleaned_data/full_data.csv") |>
  filter(year > 2009)

candidates <- unique(data$UUID)
# Democratic Party is id04566
# Republican Party is id04569
candidates <- candidates[!candidates %in% c("id04566", "id04569")]
candidates <- c(c("id04566", "id04569"), candidates)

rm(data)


post_df <- read_csv("posterior/summary.csv")

post_df <- mutate(
  post_df,
  param = gsub("\\[[0-9]+\\]", "", variable),
  param_id = parse_number(variable)
)

cand_df <- read_csv("model/candidate_data.csv")

cand_df <- post_df %>%
  filter(param == "theta") %>%
  right_join(cand_df, by = c("param_id" = "theta_id"))


names <- cand_df %>%
  select(candidate_id, candidate) %>%
  drop_na() %>%
  distinct(candidate_id, .keep_all = T)

names(names)[2] <- "Candidate"
cand_df$candidate <- NULL

cand_df <- cand_df |>
  left_join(names) |>
  mutate(project_id = candidates[candidate_id])

cross_df <- read_csv("posterior/crosswalk.csv")

cand_df <- cand_df |>
  mutate(project_id = as.numeric(gsub("id", "", project_id))) |>
  left_join(
    cross_df,
    by = join_by(project_id == Unique_Project_ID, year == year)
  )


write_csv(cand_df, "out/all_scores.csv")

### election data set

house_df <- read_csv("data/supplementary_data/election_results_house.csv")
senate_df <- read_csv("data/supplementary_data/election_results_senate.csv")
gov_df <- read_csv("data/supplementary_data/election_results_gubernatorial.csv")

election_df <- rbind(house_df, senate_df, gov_df) |>
  filter(cycle > 2009) |>
  filter(is.na(ranked_choice_round) | ranked_choice_round == 1) |>
  filter(special == FALSE) |>
  mutate(state = str_to_lower(state))

election_df <- election_df |>
  mutate(
    stage = as.numeric(factor(
      stage,
      levels = c("general", "jungle primary", "runoff")
    ))
  ) |>
  group_by(
    candidate_name,
    politician_id,
    cycle,
    race_id,
    office_seat_name,
    office_name,
    state,
    winner,
    stage
  ) |>
  mutate(total_votes = sum(votes), percent = sum(percent)) |>
  slice_max(votes, n = 1) |>
  mutate() |>
  select(-votes) |>
  group_by(
    candidate_name,
    politician_id,
    cycle,
    office_id,
    office_seat_name,
    office_name,
    state,
  ) |>
  slice_max(stage, n = 1) |>
  ungroup() |>
  select(-state, -candidate_id)


all_df <- left_join(
  cand_df,
  election_df,
  by = join_by(politician_id == politician_id, year == cycle),
  na_matches = "never"
)

out <- all_df |>
  select(
    Candidate,
    mean,
    q5,
    q95,
    office_name,
    office_seat_name,
    state,
    ballot_party,
    year
  ) |>
  filter(!is.na(office_name)) |>
  mutate(
    Ideology = round(mean, 3),
    Lower = round(q5, 3),
    Upper = round(q95, 3)
  ) |>
  mutate(
    Party = case_match(
      ballot_party,
      "DEM" ~ "Democratic",
      "REP" ~ "Republican",
      .default = "Other"
    )
  ) |>
  mutate(State = str_to_title(state)) |>
  rename(Office = office_name, Seat = office_seat_name, Year = year) |>
  mutate(
    Seat = case_match(
      Office,
      "U.S. House" ~ gsub("District ", "CD-", Seat),
      "U.S. Senate" ~ gsub("Class ", "Sen-", Seat),
      "Governor" ~ "Gov",
      .default = NA
    )
  ) |>
  mutate(
    Order = case_when(
      Office == "U.S. House" ~ as.numeric(gsub("CD-", "", Seat)),
      Seat == "Sen-I" ~ 100,
      Seat == "Sen-II" ~ 101,
      Seat == "Sen-III" ~ 102,
      Seat == "Gov" ~ 103,
    )
  ) |>
  mutate(Seat = case_match(Seat, "CD-0" ~ "CD-All", .default = Seat)) |>
  select(-state, -ballot_party, -mean, -q95, -q5)

write_csv(out, "out/campaign_data.csv")

#### legislative

nominate_df <- read_csv("data/supplementary_data/HSall_members.csv") |>
  mutate(
    congress_after_election = (congress) * 2 + 1786,
    congress_before_election = (congress) * 2 + 1788
  ) |>
  group_by(congress_after_election, icpsr) |>
  slice_max(nominate_number_of_votes, n = 1) |>
  ungroup()
## 1788 -> 1st congress
leg_df <- cand_df |>
  mutate(ICPSR2 = as.numeric(ICPSR2)) |>
  left_join(
    select(
      nominate_df,
      party_code,
      nominate_dim1,
      nominate_dim2,
      nokken_poole_dim1,
      nokken_poole_dim2,
      bioname,
      chamber,
      icpsr,
      congress_after_election
    ),
    by = join_by(ICPSR2 == icpsr, year == congress_after_election)
  )

leg_df <- leg_df |>
  left_join(
    select(
      nominate_df,
      party_code,
      nominate_dim1,
      nominate_dim2,
      nokken_poole_dim1,
      nokken_poole_dim2,
      icpsr,
      chamber,
      congress_before_election
    ),
    by = join_by(ICPSR2 == icpsr, year == congress_before_election),
    suffix = c("_before_election", "_after_election")
  )

leg_df <- leg_df |>
  select(-party_code_before_election) |>
  rename(Party = party_code_after_election, Year = year) |>
  select(
    ends_with("before_election"),
    ends_with("after_election"),
    Party,
    mean,
    Candidate,
    Year
  ) |>
  drop_na(Party) |>
  mutate(
    Party = case_match(
      Party,
      100 ~ "Democratic",
      200 ~ "Republican",
      .default = "Other"
    )
  ) |>
  mutate(
    Ideology = round(mean, 3),
    Nominate_D1_before = round(nominate_dim1_before_election, 3),
    Nominate_D2_before = round(nominate_dim2_before_election, 3),
    NP_D1_before = round(nokken_poole_dim1_before_election, 3),
    NP_D2_before = round(nokken_poole_dim2_before_election, 3),
    Nominate_D1_after = round(nominate_dim1_after_election, 3),
    Nominate_D2_after = round(nominate_dim2_after_election, 3),
    NP_D1_after = round(nokken_poole_dim1_after_election, 3),
    NP_D2_after = round(nokken_poole_dim2_after_election, 3)
  ) |>
  select(
    -mean,
    -nominate_dim1_before_election,
    -nominate_dim2_before_election,
    -nokken_poole_dim1_before_election,
    -nokken_poole_dim2_before_election,
    -nominate_dim1_after_election,
    -nominate_dim2_after_election,
    -nokken_poole_dim1_after_election,
    -nokken_poole_dim2_after_election
  )

write_csv(leg_df, "out/legislative_data.csv")


##### Timeline

nominate_df <- read_csv("data/supplementary_data/HSall_members.csv")


common_groups <- cand_df |>
  group_by(project_id) |>
  tally() |>
  filter(n > 2) |>
  pull(project_id)
overtime_df <- cand_df |>
  filter(project_id %in% common_groups) |>
  select(Candidate, mean, q5, q95, year) |>
  mutate(
    Ideology = round(mean, 3),
    Lower = round(q5, 3),
    Upper = round(q95, 3)
  ) |>
  select(-mean, -q95, -q5)

write_csv(overtime_df, "out/overtime.csv")

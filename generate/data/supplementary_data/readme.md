The folder contains data that is either appended at the end or used to help create the measure or descriptives about the measure. 

## Missing Data

You will need to download several files if you want to append stuff at the end. These are all files that are freely available but they are not my files and I don't want to be responsible for them. 

- [HSall_members.csv](https://voteview.com/data) (The dataset with everything)
- [election_results*](https://github.com/fivethirtyeight/election-results) You will need to download the governor, senate, and house csv. 
- [dime_recipients_1979_2024.rdata](https://data.stanford.edu/dime) (I used V4 the last time I did this)

### Available data
- `matched_with_all.csv` This was processed, by hand, to identify each unique candidate and give them a unique ID. This is necessary as there are sometimes name changes or district changes. If there are any issues the results probably start here. 
- `matched_with_all_pre_2023.csv` Same as above but does not have 2023 and 2024.
- `bio_matched.csv` Has the data with the bioguide IDs added from `HSall_members.csv` which was coded automatically and then hand coded to create `matched_with_all.csv`
- `bridges.csv` Contains data on who is used as bridges. You don't need it. 
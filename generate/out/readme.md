Excluding `all_scores.csv` data is used in a variety of places on the website and is really just different subsets of the same data with different variables attached. I would not recommend using it directly. 

`all_scores.csv` contains all the estimates with IDs. You can download it from here but it is also available on the Harvard Dataverse. 

The codebook for all_scores.csv:

- mean, median, sd, mad, q5, q95: These are all estimates of the distribution of the theta. You should probably use mean unless you have reason not to.
- rhat, ess_bulk, ess_tail: These are the standard convergence/diagnostic measures from Stan. 
- year: The year of the survey used in this estimate. 
- candidate_ratings: The number of ratings (from survey participants) used in the estimate for this year-candidate (if this is missing then this person was NOT included in the survey and their scores are an extrapolation between the two years on either side). 
- candidate_mean: The raw mean of the ratings from the survey. 
- candidate_sd: The raw standard deviation of the rating from the survey. 
- first_instance: An indicator for if this is the _first_ year a candidate appeared in the data. 
- Candidate: This is a "standardized" name for the candidate (it was arbitrarily selected from the names used in the CES data).
- project_id: This is a unique identifier for each candidate used only within this project. 
- bonica.rid: This is the ID associated with the candidate in Bonica's DIME database. 
- ICPSR2: This is the ICSPR code for the candidate (taken from DIME). 
- politican_id: This is the ID for the candidate matched to the 538 Election Database.
- state: The most common state that survey participants were in who rated this candidate. 
- district: The most common district that survey participants were in who rated this candidate. 
- ces_name: The name of the candidate used on that year's CES survey. 
- param, variable, param_id, past_id, rating_type, type: You can ignore these. 
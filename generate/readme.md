The process to recreate the measure entirely:

1) Download all the C/CES data and place the data files into the folders in the `data/ces_data` folder. 
2) Run `ces_data/data_clean.R` script which will extract all the requisite survey questions about ideology and place them into the `cleaned` folder. 
3) You can then run `data/providing_ids.R` which will create a single giant csv with all the data.
4) Run `data/dynamic_prep.R` which will generate two files into the `model` folder. The `model_data.json` file will be used by stan, and the `candidate_data.csv` is used to connect data at the end. 
5) The actual estimation can be accomplished with `model/estimation.R`. I run this on the Ohio Super Computer and so the `model/run.sh` is used to do that. This will produce several very large files that contain the entire posterior. 
6) To summarize the posterior you can use `model/outputs.R` which is a very simple script (this takes a lot of RAM). This saves summary stats of the posterior as `summary.csv`. 
7) Finally to produce useful outputs run `posterior/post_prep.R` which will connect everything together. You will have to make sure that the `summary.csv` file is in the right spot, and that you've downloaded all the supplementary files to `data/supplementary_data`. 
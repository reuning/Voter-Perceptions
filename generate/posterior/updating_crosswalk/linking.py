import pandas as pd
import linktransformer as lt
import os

os.chdir("generate/posterior/updating_crosswalk/")
df_bonica = pd.read_csv("bonica_data.csv")
df_538 = pd.read_csv("538_data.csv")
df_538.rename(columns={"candidate_name": "candidate", "cycle": "year"}, inplace=True)

df_ces = pd.read_csv("ces_data.csv")

df_bonica = df_bonica.drop_duplicates(["candidate", "year", "state", "bonica.rid"])
df_538 = df_538.drop_duplicates(["candidate", "year", "state", "politician_id"])
df_ces = df_ces.drop_duplicates(["candidate", "year", "state"])

df_bonica = df_bonica.dropna(subset=["candidate", "state", "year"])
df_538 = df_538.dropna(subset=["candidate", "year", "state"])

df_ces = df_ces.dropna(subset=["year", "state"])
df_538.candidate = df_538.candidate.str.lower()
df_ces.candidate = df_ces.candidate.str.lower()

df_lm_matched = lt.merge_blocking(
    df_ces,
    df_bonica,
    on=["candidate"],
    left_on=None,
    right_on=None,
    blocking_vars=["year", "state"],
    suffixes=("_ces", "_bonica"),
    model="text-embedding-3-large",
    openai_key=key,
)

df_lm_matched.to_csv("linked.csv")

df_lm_matched = lt.merge_blocking(
    df_ces,
    df_538,
    on=["candidate"],
    left_on=None,
    right_on=None,
    blocking_vars=["year", "state"],
    suffixes=("_ces", "_bonica"),
    model="text-embedding-3-large",
    openai_key=key,
)
df_lm_matched.to_csv("linked_538.csv")

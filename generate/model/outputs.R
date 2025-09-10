library(cmdstanr)


files <- grep("model-.*.csv", dir(), value = T)

mod <- as_cmdstan_fit(files)
mod$diagnostic_summary()

sum <- mod$summary(.cores = 8)
write.csv(sum, file = "summary.csv", row.names = F)

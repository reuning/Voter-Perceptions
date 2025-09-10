library(cmdstanr)

cpp_opts <- list(
  stan_opencl = TRUE,
  STAN_NO_RANGE_CHECKS = TRUE,
  "CXXFLAGS+= -fweb -fivopts -ftree-vectorize -ftree-loop-distribution -funroll-loops -floop-unroll-and-jam -fsplit-loops"
)

# Fail on OSC: -ftree-loop-linear  -floop-strip-mine -floop-block -floop-nest-optimize

cmdstan_make_local(cpp_options = cpp_opts, append = FALSE)
rebuild_cmdstan()

model <- cmdstan_model(
  stan_file = "model.stan",
  cpp_options = cpp_opts,
  stanc_options = list("O1"),
  quiet = F,
  force_recompile = T
)

fit <- model$sample(
  data = "model_data.json",
  parallel_chains = 6,
  chains = 6,
  iter_warmup = 1000,
  iter_sampling = 500,
  thin = 1,
  refresh = 25,
  opencl_ids = c(0, 0)
)

cat("Model Completed\n")
cat("Saving Output Files\n")

fit$save_output_files()

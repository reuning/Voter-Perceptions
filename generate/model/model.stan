data {
  int voter_N;
  int scores_N;
  int candidates_N;
  
  array[scores_N] int rating;
  array[scores_N] int candidate_id;
  array[scores_N] int voter_id;
  
  array[candidates_N] int first_instance;
}
transformed data {
  array[sum(first_instance)] int first_id;
  array[candidates_N - sum(first_instance)] int not_first_id;
  
  int first_id_ii = 1;
  int not_first_id_ii = 1;
  for (ii in 1 : candidates_N) {
    if (first_instance[ii]) {
      first_id[first_id_ii] = ii;
      first_id_ii += 1;
    } else {
      not_first_id[not_first_id_ii] = ii;
      not_first_id_ii += 1;
    }
  }
}
parameters {
  vector[candidates_N] theta_raw;
  real<lower=0> theta_sd;
  
  ordered[6] tau;
  
  vector[voter_N] alpha;
  vector[voter_N] beta;
}
transformed parameters {
  vector[candidates_N] theta;
  
  for (ii in 1 : candidates_N) {
    theta[ii] = (first_instance[ii] ? theta_raw[ii]
                 : theta[ii - 1] + theta_raw[ii] * theta_sd);
  }
}
model {
  {
    vector[1] ones;
    ones[1] = 1;
    target += ordered_logistic_glm_lpmf(rating | to_matrix(alpha[voter_id]
                                                           + beta[voter_id]
                                                             .* theta[candidate_id]), ones, tau);
  }
  
  target += normal_lpdf(beta | 1.5, .5);
  target += std_normal_lpdf(alpha);
  
  target += normal_lpdf(tau | 0, 3);
  
  target += std_normal_lpdf(theta_sd);
  target += student_t_lpdf(theta_raw[not_first_id] | 4, 0, 1);
  target += std_normal_lpdf(theta_raw[first_id]);
}

model {
  // Matriz de transição P
  matrix[2, 2] P = [[1 - gama, gama],
                    [eps, 1 - eps]];
  for (i in 1:M) {
    row_vector[2] alfa = [1 - psi1, psi1] .*
                         verossim(y[i, 1], J, p);
    for (t in 2:T)
      alfa = (alfa * P) .* verossim(y[i, t], J, p);
    target += log(sum(alfa));
  }
  a_psi1 ~ normal(0, 1.5);
  a_gama ~ normal(0, 1.5);
  a_eps ~ normal(0, 1.5);
  a_p ~ normal(0, 1.5);
}
generated quantities {
  // Ocupação esperada por temporada
  vector[T] psi;
  psi[1] = psi1;
  for (t in 2:T)
    psi[t] = psi[t - 1] * (1 - eps) +
             (1 - psi[t - 1]) * gama;
  real psi_eq = gama / (gama + eps);
}

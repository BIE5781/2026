functions {
  // Verossimilhança das detecções em uma
  // temporada: (não ocupado, ocupado)
  row_vector verossim(int y, int J, real p) {
    return [y == 0, exp(binomial_lpmf(y | J, p))];
  }
}
data {
  int<lower=1> M;  // n de sítios
  int<lower=2> T;  // n de temporadas
  int<lower=1> J;  // n de visitas por temporada
  array[M, T] int<lower=0, upper=J> y;
}
parameters {
  real a_psi1;  // logitos
  real a_gama;
  real a_eps;
  real a_p;
}
transformed parameters {
  real psi1 = inv_logit(a_psi1);
  real gama = inv_logit(a_gama);
  real eps = inv_logit(a_eps);
  real p = inv_logit(a_p);
}
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

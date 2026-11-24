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

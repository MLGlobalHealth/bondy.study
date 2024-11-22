functions {
  
  real lambda(real L, int m) {
		real lam;
		lam = ((m*pi())/(2*L))^2;
				
		return lam;
	}
	
    // Squared Exponential Kernel
  real spd_SE(real alpha, real rho, real w) {
  		real S;
  		S = (alpha^2) * sqrt(2*pi()) * rho * exp(-0.5*(rho^2)*(w^2));
  				
  		return S;
  }
  
  vector phi_SE(real L, int m, vector x) {
		vector[rows(x)] fi;
		fi = 1/sqrt(L) * sin(m*pi()/(2*L) * (x+L));
				
		return fi;
	}
	
}

data {
  int<lower=0> N;         // Number of observations
  int<lower=0> N_TAF;         // dimension of input space
  int<lower=0> gp_input_dim; // number of months to compute GP approx for
  vector[N_TAF] x_TAF;            // input space (time on TAF)
  array[N] int TAF;        // Indicator for TAF
  array[N] int coords_TAF; // mapping coords
  vector[N] y;            // change in zscore
  real L_f;					// boundary value for function 1
  int<lower=1> M_f;			// num basis functions for function 1
}

transformed data {
  // Basis functions for f
	matrix[gp_input_dim,M_f] PHI_f;
	
	for (m in 1:M_f){ PHI_f[,m] = phi_SE(L_f, m, x_TAF); }

}

parameters {
  real beta0;             // Intercept
  real beta_TAF;          // Coefficient for x_TAF
  real<lower=0> sigma;    // Observation noise (standard deviation)

  //variables for the basis function models
	vector[M_f] beta_f;
	
	//GP hyperparameters
	real<lower=0> lscale;
	real<lower=0> alpha;
}

transformed parameters{

  vector[gp_input_dim] f_TAF;
	vector[M_f] diagSPD_f;
	vector[M_f] SPD_beta_f;
	
	// Spectral densities for f
	for(m in 1:M_f){ 
		diagSPD_f[m] =  sqrt(spd_SE(alpha, lscale, sqrt(lambda(L_f, m)))); 
	}
	
	SPD_beta_f = diagSPD_f .* beta_f;

	f_TAF = PHI_f[,] * SPD_beta_f;

}

model {
  // Priors
  beta0 ~ normal(0, 5); //intercept
  beta_TAF ~ normal(0, 2); //TAF coeff
  sigma ~ normal(0, 1); //noise
  beta_f ~ normal(0, 1);
  lscale ~ normal(0, 0.2);
  alpha ~ normal(0,0.5); //marginal variance/magnitude GP
  
  
  // Likelihood
  for (n in 1:N) {
    if(TAF[n]==0){
        y[n] ~ normal(beta0, sigma);
    }else{
        y[n] ~ normal(beta0 + beta_TAF + f_TAF[coords_TAF[n]], sigma);
    }
  }
}

generated quantities {
  vector[N] y_pred;
  for (n in 1:N) {
    if(TAF[n]==0){
      y_pred[n] = normal_rng(beta0, sigma);
    }else{
      y_pred[n] = normal_rng(beta0 + beta_TAF + f_TAF[coords_TAF[n]], sigma);
    }
  }
}

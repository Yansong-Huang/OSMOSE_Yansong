# visualisation des courbes de sélectivité
# date de création: 2024-03-18

# Sigmoid Selectivity
L50_sig_bottom_trawlers <- 18
L75_sig_bottom_trawlers <- 21.5
L50_sig_midwater_trawlers <- 18
L75_sig_midwater_trawlers <- 22
L50_sig_others <- 18.5
L75_sig_others <- 22

plot_sigmoid_selectivity <- function(L50_sig,L75_sig){
  s1 = L50_sig * log(3) / (L75_sig - L50_sig)
  s2 = s1/L50_sig;
  sigmoid <- function(L) {1/(1 + exp(s1-s2*L))} 
  plot.function(sigmoid,0,2*L50_sig)
}  

plot_sigmoid_selectivity(L50_sig_bottom_trawlers,L75_sig_bottom_trawlers)
plot_sigmoid_selectivity(L50_sig_midwater_trawlers,L75_sig_midwater_trawlers)
plot_sigmoid_selectivity(L50_sig_others,L75_sig_others)

# Gaussian Selectivity
L50_gau_netters <- 29
L75_gau_netters <- 33

q75 = 0.674489750196082
sd = (L75_gau_netters - L50_gau_netters) / q75

plot_sigmoid_selectivity <- function(L50_gau,L75_gau){
  sd = (L75_gau - L50_gau) / q75
  gaussian <- function(L){dnorm(L,L50_gau_netters,sd)}
  plot.function(gaussian,0,2*L50_gau)
}

plot_sigmoid_selectivity(L50_gau_netters,L75_gau_netters)



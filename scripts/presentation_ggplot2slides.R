
# Set theme plot
theme_set(ggpubr::theme_pubclean(base_size = 15)) # Load theme

# Create synthetic data
set.seed(1)

Sigma <- matrix(c(10,4,3,2),2,2)
Sigma

d <- data.table(cell_type = sample(c("Gram +", "Gram -"), 400, replace = TRUE)) 
d[, treatment := rep(c("control", "cofactor"), each = 200)]

d[treatment == "control", 
  c("cell_size", "superdrug_killing") :=  
    data.table(MASS::mvrnorm(n = 200, rep(1, 2), Sigma))]             
d[treatment == "cofactor", 
  c("cell_size", "superdrug_killing") :=  
    data.table(MASS::mvrnorm(n = 200, rep(1, 2), Sigma))]
d[treatment == "cofactor", superdrug_killing := superdrug_killing * 1.2 ]
d[, cell_size := ((cell_size + 10)/2 )]
d[cell_size < 0.5, cell_size := cell_size + 5]
d[cell_type == "Gram -", cell_size := cell_size + runif(.N, 2, 3)]
d[cell_type == "Gram -" & treatment == "cofactor", 
  superdrug_killing := superdrug_killing * runif(.N, min = 1, max = 1.5)]

d[, superdrug_killing := 10 * exp(ifelse(cell_type == "Gram -", 0.01, 0.03) * 
                              superdrug_killing) + ifelse(treatment == "cofactor", cell_size*0.02, -cell_size*0.1)]
d[treatment == "control" & cell_size < 5, superdrug_killing := superdrug_killing + 2/cell_size]
d[cell_type == "Gram -", superdrug_killing := superdrug_killing - runif(.N, 0, 0.2) * cell_size]

d 

# Sec 1: Univariate plots ------------------------------------------------

# Hist
ggplot(d, aes(x = superdrug_killing)) +
  geom_histogram(fill = "cornflowerblue", col = "gray80")

ggplot(d, aes(x = superdrug_killing)) +
  geom_histogram(fill = "cornflowerblue", col = "gray80", 
                 bins = 64)                              

# Quantile-to-quantile (QQ) plot
ggplot(d, aes(sample = superdrug_killing)) +
  geom_qq(col = "cornflowerblue") +
  geom_qq_line(distribution = qnorm)

# Sec 2: Bivariate plots --------------------------------------

# Bar plot
ggplot(d[, .(superdrug_killing = mean(superdrug_killing)), by = treatment], aes(x = treatment, y = superdrug_killing)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") 

# Boxplot
ggplot(d, aes(x = treatment, y = superdrug_killing)) +
  geom_boxplot(fill = "cornflowerblue") 

# Sec 4: Bivariate plot, continuous data -----------------------------------------

ggplot(d, aes(x = cell_size, y = superdrug_killing, shape = treatment)) +
  geom_point(col = "black", # fix colour
             size = 2) 

ggplot(d, aes(x = cell_size, y = superdrug_killing, col = treatment)) + # colour as variable
  geom_point(size = 2) 

# Sec 5: Simple modelling ---------------------------------------

ggplot(d, aes(x = cell_size, y = superdrug_killing, col = treatment)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(d, aes(x = cell_size, y = superdrug_killing)) +
  geom_point() 

ggplot(d, aes(x = cell_size, y = superdrug_killing)) +
  geom_point(aes(col = treatment)) +
  stat_smooth(method = "lm", col = "black")

ggplot(d, aes(x = cell_size, y = superdrug_killing, col = treatment)) +
  geom_point() +
  stat_smooth(method = "lm")

# Sec 6: Model representation with subsets -------------------

ggplot(d, 
       aes(x = cell_size, y = superdrug_killing, col = treatment)) +
  geom_point() +                                  
  stat_smooth(method = "lm", fullrange = T) +             
  facet_grid(~ cell_type,                            
             scales = "free_x") +
  scale_x_continuous() +
  labs(x = "Look at that slope", y = "This goes to science!") +
  scale_color_brewer(palette = "Set1")

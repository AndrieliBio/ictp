#21.07.22

source("C:/Users/Adminstrador/Desktop/Quantitative Ecology/Computation methods/2022_scientific_computing_intro-main/R/09_pop_model.R")
# named vector with parameters
p <- c(r = 1, a = 0.001)
# initial condition
y0 <- c(N = 10)
# time steps
t <- seq(1,200, lenght.out=400)

# give the function and the parameters to the ode function
out_log <- ode(y = y0, times = t, func = logGrowth, parms = p)

dim(out_log)
head(out_log)
class(out_log)

#Plotting the result using ggplot2.
#We will need to convert the out object into a data.frame


df_log <- as.data.frame(out_log)
class(df_log)

ggplot(df_log) +
  geom_line(aes(x = time, y = N)) +
  theme_classic()

#Lotka-Volterra competition model
#Creating a function to represent the ODEs

LVComp <- function(t, y, p) {
  N <- y
  with(as.list(p), {
    dN1.dt <- r[1] * N[1] * (1 - a[1, 1] * N[1] - a[1, 2] * N[2])
    dN2.dt <- r[2] * N[2] * (1 - a[2, 1] * N[1] - a[2, 2] * N[2])
    return(list(c(dN1.dt, dN2.dt)))
  })
}

#we solve the system using defined parameters and the function ode().
#Here, we have to define the α matrix with the competition coefficients

# LV parameters
a <- matrix(c(0.02, 0.01, 0.01, 0.03), nrow = 2)
r <- c(1, 1)
p2 <- list(r, a)
N0 <- c(10, 10)
t2 <- c(1:100)

out_lv <- ode(y = N0, times = t2, func = LVComp, parms = p2)
head(out_lv)

#pivot_longer do pacote tidyr cria dataframes
df_lv <- pivot_longer(as.data.frame(out_lv), cols = 2:3)
head(df_lv)

ggplot(df_lv) +
  geom_line(aes(x = time, y = value, color = name)) +
  labs(x = "Time", y = "N", color = "Species") +
  theme_classic()


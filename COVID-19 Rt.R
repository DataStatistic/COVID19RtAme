require(EpiEstim)
require(R0)
require(ggplot2)
require(plotly)
require(dplyr)
require(plyr)
require(tidyverse)
require(latex2exp)
loadfonts(device = "win")
windowsFonts("Arial")

# Read data frame
cases <- read.csv2("Cases AME.csv")

# Serial interval (SI) with a mean of 7.5 and a standard deviation of 3.4
# Incubation period: 5 days

# Generation Time

mGT <- generation.time("gamma", c(7.5, 3.4)) ; mGT

# Rt Colombia

colombia <-  na.omit(cases$Colombia)
pob = 50372424 # population size of the country

estR0Col <- estimate.R(colombia, mGT, begin=1, end=99, methods=c("EG","ML","TD","AR","SB"), pop.size=pob, nsim=1000)
estR0Col$estimates$EG$R
estR0Col$estimates$EG$conf.int

t_start <- seq(2, length(colombia) - 4) # 5 days is -4 
t_end <- t_start + 4 # 5 days is +4   

res = estimate_R(data.frame(colombia), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:92)
        
G1 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 19, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") + labs(y = TeX("$R_t$"), x = "Number of days", title = "Colombia") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text( size = 9))

G1

# Rt Argentina

argentina <-  na.omit(cases$Argentina)
pob = 45200000 # population size of the country

estR0Arg<-estimate.R(argentina, mGT, begin=1, end=102, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Arg$estimates$EG$R
estR0Arg$estimates$EG$conf.int

t_start <- seq(2, length(argentina)-4) # 5 days is -4  
t_end <- t_start + 4 # 5 days is +4

res = estimate_R(data.frame(argentina), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G2 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 17, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Argentina") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G2	  

# Rt Brazil

brazil <- na.omit(cases$Brazil)
pob = 211380000 # population size of the country

estR0Bra<-estimate.R(brazil, mGT, begin=1, end=108, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Bra$estimates$EG$R
estR0Bra$estimates$EG$conf.int

t_start <- seq(2, length(brazil)-4) # 5 days is -4   
t_end <- t_start + 4 # 5 days is +4 

res = estimate_R(data.frame(brazil), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G3 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 20, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Brazil") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G3

# Rt Chile

chile <- na.omit(cases$Chile)
pob = 18729160 # population size of the country

estR0Chil<-estimate.R(chile, mGT, begin=1, end=102, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Chil$estimates$EG$R
estR0Chil$estimates$EG$conf.int

t_start <- seq(2, length(chile)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(chile), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G4 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 11, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Chile") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G4	  

# Rt US

us <- na.omit(cases$US)
pob = 330222422 # population size of the country

estR0USA<-estimate.R(us, mGT, begin=1, end=79, methods = c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)

estR0USA$estimates$EG$R
estR0USA$estimates$EG$conf.int

t_start <- seq(2, length(us)-4) # 5 dias es -4 y +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(us), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G5 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 55, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "United States") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G5

# Rt Bolivia

bolivia <- na.omit(cases$Bolivia)
pob = 11633371 # population size of the country

estR0Bol<-estimate.R(bolivia, mGT, begin=1, end=94, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Bol$estimates$EG$R
estR0Bol$estimates$EG$conf.int

t_start <- seq(2, length(bolivia)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(bolivia), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G6 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 11, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Bolivia") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G6

# Rt Mexico

mexico <- na.omit(cases$Mexico)
pob = 135917138 # population size of the country

estR0mex<-estimate.R(mexico, mGT, begin=1, end=106, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0mex$estimates$EG$R
estR0mex$estimates$EG$conf.int

t_start <- seq(2, length(mexico)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(mexico), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G7 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 17, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Mexico") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G7

#  Dominican Republic

dominican <- na.omit(cases$Dominican.Republic)
pob = 11191857 # population size of the country

estR0rep <- estimate.R(dominican, mGT, begin=1, end=104, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0rep$estimates$EG$R
estR0rep$estimates$EG$conf.int

t_start <- seq(2, length(dominican)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(dominican), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G8 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 18, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Dominican Republic") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G8

# Rt Canada

canada <- na.omit(cases$Canada)
pob = 37831360 # population size of the country

estR0Cana<-estimate.R(canada, mGT, begin=1, end=82, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Cana$estimates$EG$R
estR0Cana$estimates$EG$conf.int

t_start <- seq(2, length(canada)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(incid = data.frame(canada), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G9 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 50, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Canada") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G9

# Rt El Salvador

salvador <- na.omit(cases$El.Salvador)
pob = 6765753 # population size of the country

estR0Salva <- estimate.R(salvador, mGT, begin=1, end=87, methods = c("EG","ML","TD","AR","SB"), pop.size=pob, nsim=1000)
estR0Salva$estimates$EG$R
estR0Salva$estimates$EG$conf.int

t_start <- seq(2, length(salvador)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(salvador), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G11 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 3, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "El Salvador") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G11

# Rt Guatemala

guatemala <- na.omit(cases$Guatemala)
pob = 18096932 # population size of the country

estR0Guate <- estimate.R(guatemala, mGT, begin=1, end=92, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Guate$estimates$EG$R
estR0Guate$estimates$EG$conf.int

t_start <- seq(2, length(guatemala)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(guatemala), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G12 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 66, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Guatemala") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G12

# Rt Panama

panama<- na.omit(cases$Panama)
pob = 4159000 # population size of the country

estR0Pana<-estimate.R(panama, mGT, begin=1, end=96, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Pana$estimates$EG$R
estR0Pana$estimates$EG$conf.int

t_start <- seq(2, length(panama)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(panama, method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G13 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 16, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Panama") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "Arial", size = 9))

G13

# Rt Peru

peru <- na.omit(cases$Peru)
pob = 32162184 # population size of the country

estR0Peru<-estimate.R(peru, mGT, begin=1, end=99, methods=c("EG", "ML", "TD", "AR", "SB"), pop.size=pob, nsim=1000)
estR0Peru$estimates$EG$R
estR0Peru$estimates$EG$conf.int

t_start <- seq(2, length(peru)-4) # 5 days is -4 and +4   
t_end <- t_start + 4 

res = estimate_R(data.frame(peru), method = "parametric_si", config = make_config(list(mean_si = mGT$mean, std_si=mGT$sd, t_start = t_start, t_end = t_end)))
res$R[1:5,]
dim(res$R) # the first 2 data are lost

datX <- data.frame(res$R[-c(1,2),c(3,5,11)], ID = 1:(dim(res$R)[1]-2))

G14 <- ggplot(datX, aes(ID)) + 
  geom_line(aes(y = Mean.R.), colour="black") + 
  geom_ribbon(aes(ymin = Quantile.0.025.R. , ymax = Quantile.0.975.R.), alpha=0.2) +
  theme_bw(20) +
  geom_vline(xintercept = 10, linetype = "dashed") + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(y = TeX("$R_t$"), x = "Number of days", title = "Peru") + 
  theme(plot.title = element_text(hjust = 0.5),text = element_text(family = "Arial", size = 9))

G14	  

# Graphing R0

R0est<- list(Argentina = estR0Arg, Bolivia = estR0Bol, Brazil = estR0Bra, Canada = estR0Cana, 
             Chile = estR0Chil, Colombia = estR0Col, "El Salvador" = estR0Salva, Guatemala = estR0Guate, Mexico = estR0mex, 
             Panama = estR0Pana, Peru = estR0Peru, "Dominican Rep." = estR0rep, US = estR0USA)

fx<- function(x) data.frame(R0 = x$estimates$ML$R, Li = x$estimates$ML$conf.int[1], Ls = x$estimates$ML$conf.int[2]) 

datos_R0 <- lapply(R0est, fx) %>% ldply(.id = "País")

G0 <- ggplot(datos_R0, aes(País)) +
  geom_point(aes(y = R0), colour = "black") +
  geom_errorbar(aes(ymin = Li , ymax = Ls)) +
  theme_bw(20) +
  labs(y = TeX("$R_0$"), x = "", title = "") + 
  theme(plot.title = element_text(hjust = 0), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2), text = element_text(family = "Arial", size = 9))

G0

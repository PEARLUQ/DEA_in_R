################################################################# 
#####      Illustration of DEA on Queensland hospitals      #####
################################################################# 

rm(list=ls())
graphics.off()

################### Data Process ###################

# Read data
data <- read.csv("QLD.csv")
names(data)[names(data)=="HOSID"] <- "id"
names(data)[names(data)=="Yeardummy"] <- "Year"

# Convert to panel data
library(plm)
paneldata<- pdata.frame(data, c("id","Year"))

attach(data)

# Input/Output for "Benchmarking"
X = as.matrix(cbind(BEDS, Agglabours, SUPP))
Y = as.matrix(Aggout)

# Input/Output for "FEAR"
Xt = t(X)
Yt = t(Y)

################### Technical Efficiency ###################

attach(data)

# Use the functions by Bogetoft and Otto (2022)
library(Benchmarking)

# Output-oriented
dea.crs.out.bench = 1/dea(X, Y, RTS="crs", ORIENTATION="out")$eff # CRS
dea.vrs.out.bench = 1/dea(X, Y, RTS="vrs", ORIENTATION="out")$eff # VRS
dea.nirs.out.bench = 1/dea(X, Y, RTS="drs", ORIENTATION="out")$eff # NIRS
dea.fdh.out.bench = 1/dea(X, Y, RTS="fdh", ORIENTATION="out")$eff # FDH

# Input-oriented
dea.crs.in.bench = dea(X, Y, RTS="crs", ORIENTATION="in")$eff
dea.vrs.in.bench = dea(X, Y, RTS="vrs", ORIENTATION="in")$eff
dea.nirs.in.bench = dea(X, Y, RTS="drs", ORIENTATION="in")$eff
dea.fdh.in.bench = dea(X, Y, RTS="fdh", ORIENTATION="in")$eff

# Use the functions by Wilson (2020)
# As the two functions have the same name, detach one package before attach another
detach("package:Benchmarking", unload = TRUE)
library(FEAR)

# Output-oriented
dea.crs.out.fear = dea(Xt, Yt, RTS=3, ORIENTATION=2)
dea.vrs.out.fear = dea(Xt, Yt, RTS=1, ORIENTATION=2)
dea.nirs.out.fear = dea(Xt, Yt, RTS=2, ORIENTATION=2)
dea.fdh.out.fear = fdh.eff(Xt, Yt, ORIENTATION=2)

# Input-oriented
dea.crs.in.fear = 1/dea(Xt, Yt, RTS=3, ORIENTATION=1)
dea.vrs.in.fear = 1/dea(Xt, Yt, RTS=1, ORIENTATION=1)
dea.nirs.in.fear = 1/dea(Xt, Yt, RTS=2, ORIENTATION=1)
dea.fdh.in.fear = 1/fdh.eff(Xt, Yt, ORIENTATION=1)

# Summarize the estimations
Effi <- as.data.frame(cbind(dea.crs.out.bench, dea.crs.out.fear,
                            dea.vrs.out.bench, dea.vrs.out.fear,
                            dea.nirs.out.bench, dea.nirs.out.fear,
                            dea.fdh.out.bench, dea.fdh.out.fear,
                            dea.crs.in.bench, dea.crs.in.fear,
                            dea.vrs.in.bench, dea.vrs.in.fear,
                            dea.nirs.in.bench, dea.nirs.in.fear,
                            dea.fdh.in.bench, dea.fdh.in.fear))
summary(Effi)

detach("package:FEAR", unload = TRUE)


# Boxplot of efficiencies

library(ggplot2)
library(dplyr)
library(tidyr)
library(Benchmarking)
# Output-oriented
CRS.Out = 1/dea(X, Y, RTS="crs", ORIENTATION="out")$eff # CRS
VRS.Out = 1/dea(X, Y, RTS="vrs", ORIENTATION="out")$eff # VRS
NIRS.Out = 1/dea(X, Y, RTS="drs", ORIENTATION="out")$eff # NIRS
FDH.Out = 1/dea(X, Y, RTS="fdh", ORIENTATION="out")$eff # FDH
# Input-oriented
CRS.In = dea(X, Y, RTS="crs", ORIENTATION="in")$eff
VRS.In = dea(X, Y, RTS="vrs", ORIENTATION="in")$eff
NIRS.In = dea(X, Y, RTS="drs", ORIENTATION="in")$eff
FDH.In = dea(X, Y, RTS="fdh", ORIENTATION="in")$eff

Effi <- as.data.frame(cbind(CRS.Out,CRS.In,VRS.Out,VRS.In,NIRS.Out,NIRS.In,FDH.Out,FDH.In))

DF <- gather(Effi) # Use gather to generate a multiple factor in one line
rts <- t(rbind(c(rep("CRS",2*nrow(Effi)),
             rep("VRS",2*nrow(Effi)),
             rep("NIRS",2*nrow(Effi)),
             rep("FDH",2*nrow(Effi))))) # Generate clusters
DF = cbind(DF,rts)
# Control the sequence in legend and x-axis
DF$key <- factor(DF$key , levels=c("CRS.In","CRS.Out","VRS.In","VRS.Out","NIRS.In","NIRS.Out","FDH.In","FDH.Out"))
DF$rts <- factor(DF$rts , levels=c("CRS","VRS","NIRS","FDH"))

# Box plot
ggplot(DF, aes(x=rts, y=value, fill=key)) + 
geom_boxplot(alpha=0.3) +
  scale_fill_brewer(palette="Set3") +
  xlab("") +
  ylab("Estimated efficiency")+
  scale_fill_discrete(name = "Model")

# Ridge plot
library(ggridges)
ggplot(DF, aes(x=value, y=key, fill=key)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

################### Cost/Revenue/Profit Efficiency ###################

attach(data)

# Generate artificial matrix of prices
w <- as.matrix(t(c(1,2,3)))
p <- as.matrix(4)

# Use the functions by Wilson (2020)
library(FEAR)

# Cost efficiency
dea.crs.cost1 = 1/dea.cost.min(Xt, Yt, t(w), RTS = 3, errchk = TRUE)$eff #CRS (以benchmarking为主，那w,p在FEAR中要transit)
dea.vrs.cost1 = 1/dea.cost.min(Xt, Yt, t(w), RTS = 1, errchk = TRUE)$eff #VRS
dea.nirs.cost1 = 1/dea.cost.min(Xt, Yt, t(w), RTS = 2, errchk = TRUE)$eff #NIRS

# Revenue efficiency
dea.crs.revenue1 = dea.revenue.max(Xt, Yt, p, RTS = 3, errchk = TRUE)$eff
dea.vrs.revenue1 = dea.revenue.max(Xt, Yt, p, RTS = 1, errchk = TRUE)$eff
dea.nirs.revenue1 = dea.revenue.max(Xt, Yt, p, RTS = 2, errchk = TRUE)$eff

# Profit efficiency
dea.crs.profit1 = dea.profit.max(Xt, Yt, t(w), p, RTS = 3, errchk = TRUE)$eff
dea.vrs.profit1 = dea.profit.max(Xt, Yt, t(w), p, RTS = 1, errchk = TRUE)$eff
dea.nirs.profit1 = dea.profit.max(Xt, Yt, t(w), p, RTS = 2, errchk = TRUE)$eff

# Use the functions by Bogetoft and Otto (2022)
library(Benchmarking)

# Cost efficiency
xopt = cost.opt(X, Y, w, RTS='crs') #CRS
cobs <- X %*% t(w) # Observed Cost
copt <- xopt$x %*% t(w) # Optimal Cost
dea.crs.cost = copt/cobs # cost efficiency

xopt = cost.opt(X, Y, w, RTS='vrs') #VRS
cobs <- X %*% t(w)
copt <- xopt$x %*% t(w)
dea.vrs.cost = copt/cobs

xopt = cost.opt(X, Y, w, RTS='drs') #NIRS
cobs <- X %*% t(w)
copt <- xopt$x %*% t(w)
dea.nirs.cost = copt/cobs

xopt = cost.opt(X, Y, w, RTS='fdh') #FDH
cobs <- X %*% t(w) # Observed Cost
copt <- xopt$x %*% t(w) # Optimal Cost
dea.fdh.cost = copt/cobs

# Revenue efficiency
yopt = revenue.opt(X, Y, p, RTS='crs') #CRS
yobs <- Y %*% p # Observed Revenue
yopt <- yopt$y %*% p # Optimal Revenue
dea.crs.revenue = yobs/yopt # Revenue efficiency

yopt = revenue.opt(X, Y, p, RTS='vrs') #VRS
yobs <- Y %*% p
yopt <- yopt$y %*% p
dea.vrs.revenue = yobs/yopt

yopt = revenue.opt(X, Y, p, RTS='drs') #NIRS
yobs <- Y %*% p
yopt <- yopt$y %*% p
dea.nirs.revenue = yobs/yopt

yopt = revenue.opt(X, Y, p, RTS='fdh') #FDH
yobs <- Y %*% p
yopt <- yopt$y %*% p
dea.fdh.revenue = yobs/yopt

# Profit efficiency
popt = profit.opt(X, Y, w, p, RTS='crs') #CRS
pobs <- Y %*% p -X %*% t(w) # Observed Profit
popt <- popt$y %*% p - popt$x %*% t(w) # Optimal Profit
dea.crs.revenue = pobs/popt # Profit efficiency

popt = profit.opt(X, Y, w, p, RTS='vrs') #VRS
pobs <- Y %*% p -X %*% t(w)
popt <- popt$y %*% p - popt$x %*% t(w)
dea.vrs.revenue = pobs/popt

popt = profit.opt(X, Y, w, p, RTS='drs') #NIRS
pobs <- Y %*% p -X %*% t(w)
popt <- popt$y %*% p - popt$x %*% t(w)
dea.nirs.revenue = pobs/popt

popt = profit.opt(X, Y, w, p, RTS='fdh') #FDH
pobs <- Y %*% p -X %*% t(w)
popt <- popt$y %*% p - popt$x %*% t(w)
dea.fdh.revenue = pobs/popt

################### MPI ###################

attach(data)

# Use the functions by Bogetoft and Otto (2022)
library(Benchmarking)

# Use period 1 and 2 as an example
X0 <- as.matrix(X[Year==1,])
X1 <- as.matrix(X[Year==2,])

Y0 <- as.matrix(Y[Year==1,])
Y1 <- as.matrix(Y[Year==2,])

ID0 <- as.matrix(id[Year==1])
ID1 <- as.matrix(id[Year==2])

mpi.crs.in = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.vrs.in = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "vrs", ORIENTATION = "in")$m
mpi.nirs.in = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "drs", ORIENTATION = "in")$m
mpi.crs.out = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "out")$m
mpi.vrs.out = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "vrs", ORIENTATION = "out")$m
mpi.nirs.out = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "drs", ORIENTATION = "out")$m

# Summarize the estimations
MPI <- as.data.frame(cbind(mpi.crs.in, mpi.vrs.in,
                            mpi.nirs.in, mpi.crs.out,
                            mpi.vrs.out, mpi.nirs.out))
summary(MPI)

# Estimate with DEA estimators (Fare et al. 1992)
# Same results by manual MPI (when implosion, using: (X2,Y2))
dea00<-dea(X0, Y0, RTS="crs", ORIENTATION="out", XREF=X0, YREF=Y0)$eff
dea11<-dea(X1, Y1, RTS="crs", ORIENTATION="out", XREF=X1, YREF=Y1)$eff
dea10<-dea(X0, Y0, RTS="crs", ORIENTATION="out", XREF=X1, YREF=Y1)$eff
dea01<-dea(X1, Y1, RTS="crs", ORIENTATION="out", XREF=X0, YREF=Y0)$eff

mpi.dea<-sqrt(dea01/dea00*dea11/dea10)
summary(mpi.dea)
summary(mpi.crs.out)

# Heatmap for MPI
IDs = cbind(data$id, data$NetworkID, rep(1,nrow(data)))[1:95,]
X0 <- as.matrix(X[Year==1,])
X1 <- as.matrix(X[Year==2,])
Y0 <- as.matrix(Y[Year==1,])
Y1 <- as.matrix(Y[Year==2,])
ID0 <- as.matrix(id[Year==1])
ID1 <- as.matrix(id[Year==2])
mpi.crs.in.1 = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.crs.in.1 = cbind(mpi.crs.in.1, IDs)

IDs = cbind(data$id, data$NetworkID, rep(2,nrow(data)))[1:95,]
X0 <- as.matrix(X[Year==2,])
X1 <- as.matrix(X[Year==3,])
Y0 <- as.matrix(Y[Year==2,])
Y1 <- as.matrix(Y[Year==3,])
ID0 <- as.matrix(id[Year==2])
ID1 <- as.matrix(id[Year==3])
mpi.crs.in.2 = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.crs.in.2 = cbind(mpi.crs.in.2, IDs)

IDs = cbind(data$id, data$NetworkID, rep(3,nrow(data)))[1:95,]
X0 <- as.matrix(X[Year==3,])
X1 <- as.matrix(X[Year==4,])
Y0 <- as.matrix(Y[Year==3,])
Y1 <- as.matrix(Y[Year==4,])
ID0 <- as.matrix(id[Year==3])
ID1 <- as.matrix(id[Year==4])
mpi.crs.in.3 = malmq(X0, Y0, ID0, X1, Y1, ID1, RTS = "crs", ORIENTATION = "in")$m
mpi.crs.in.3 = cbind(mpi.crs.in.3, IDs)

mpi.heat = as.data.frame(rbind(mpi.crs.in.1, mpi.crs.in.2, mpi.crs.in.3))
colnames(mpi.heat) <- c("mpi.crs.in", "id", "HHS", "Period")

require("ggplot2")
require("hrbrthemes")
ggplot(mpi.heat, aes(as.character(HHS), Period, fill= mpi.crs.in)) + 
  geom_tile() +
  xlab("Local Hospital Networks in Queensland")+
  ylab("Period")+
  scale_fill_distiller(palette = "GnBu")+
  scale_y_discrete(limit = c("12/13-13/14","13/14-14/5","14/15-15/16"))+
  guides(fill=guide_legend(title="MPI (CRS, input-oriented)"))+
  theme_bw(base_size = 16)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white")) 

# # Use lattice: 
# library(RColorBrewer)
# col <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
# 
# require("lattice")
# levelplot(mpi.crs.in ~ HHS*Period, data=mpi.heat,
# 
#           scales=list(
#             x=list(at=c(seq(312,320,1),322,seq(324,327,1))),
#             y=list(rot=90,at=1:3,labels=c("12/13-13/14","13/14-14/5","14/15-15/16"))
#           ),
#           xlab="Local Hospital Networks in Queensland", ylab="Period",
#           col.regions=col)

# MPI in FEAR is Deprecated

################### Bias-correction ###################

attach(data)

# Use the functions by Wilson (2020)
library(FEAR)
Bootstrap.fear = boot.sw98(Xt, Yt, NREP = 2000, RTS = 3, ORIENTATION = 2, alpha = 0.05, CI.TYPE=2)
dea.crs.out.fear = dea(Xt, Yt, RTS=3, ORIENTATION=2)

# Compare the original and bias-corrected estimates
cdea = 1/(dea.crs.out.fear-Bootstrap.fear$bias) # 注意第一结果是[0,1]所以是已经求了倒数了；为了看OTE,应该对其再次倒数
                                                # 因此，bias应该是减去，因为OTE估计值偏小，那倒数后的原始估计值偏大了
c = 1/Bootstrap.fear$dhat.bc                    # 当然cdea可以直接从结果中提取，dhat.bc即为修正后的dea
cdea = cbind(cdea,rep(1,380))
dea = 1/dea.crs.out.fear
dea = cbind(dea,rep(0,380))
correction = as.data.frame(rbind(cdea,dea))
colnames(correction) = c("DEA","method")

correction$Estimates[correction$method==1]='Bias-corrected'
correction$Estimates[correction$method==0]='Original'

attach(correction)
require("ggplot2")
.df <- na.omit(data.frame(x = correction$DEA))
.nbins <- pretty(range(.df$x), n = nclass.FD(.df$x), min.n = 1)
.dea <- ggplot(data = .df, aes(x = x, y = ..density..)) +
  # Epanechnikov kernel and CV bandwidth
  geom_density(
    kernel = "gaussian",
    bw = "ucv",
    alpha = 0.5,
    aes(color = Estimates, fill = Estimates)
  ) +
  scale_y_continuous(expand = c(0.01, 0)) +
  xlab("Estimated inefficiency") +
  ylab("Estimated Density") +
  labs(colour = "Estimates",
       shape = "Estimates",
       fill = "Estimates") +
  RcmdrPlugin.KMggplot2::theme_simple(base_size = 14, base_family = "sans")
print(.dea)
rm(.df, .nbins)

# Use the functions by Simm and Besstremyannaya (2020)
library(rDEA)
Bootstrap.rDEA = dea.robust(X, Y, W=NULL, model="output", RTS="constant", B=2000, alpha=0.05, bw="bw.ucv")
# dea.boot in Benchmarking has been noted as slower than boot.sw98, while users can also 
# use boot.sw98 from benchmarking by quoting the wrapper function boot.fear in Benchmarking

################### Aggregate ###################

# Generate artificial matrix of prices
p <- as.matrix(4)

# Calculate weight
weight = p%*%Yt/sum(p%*%rowSums(Yt))

# Aggregate
aggregate = sum(dea.crs.out.fear%*%t(weight))

################### SW07 ###################

attach(data)

# Use the functions by Simm and Besstremyannaya (2020)
library(rDEA)

# Define environmental variables
Z = as.matrix(cbind(TEACH, Small, Remote))

# Output-oriented & CRS
sw07 = dea.env.robust(X, Y, W=NULL, Z, "output", RTS="constant", L1=100, L2=2000, alpha=0.05)
sw07$beta_hat_hat
sw07$beta_ci

library(panelvar)
library(plm)
library(readxl)
library(collapse)
library(xtable)

library(readxl)
D <- read_excel("C:/Users/Joao Caetano/Desktop/D.xlsx")
View(D)

D<-pdata.frame(D, index=c("Country","Date"))


agdp1<-purtest(approval~trend, data=D, lags="AIC", pmax=12, test="levinlin")
summary(agdp1)
print(agdp1)

agdp2<-purtest(Expectations~trend, data=D, lags="AIC", pmax=12, test="levinlin")
summary(agdp2)
print(agdp2)

agdp3<-purtest(inflation~trend, data=D, lags="AIC", pmax=12, test="levinlin")
summary(agdp3)
print(agdp3)

agdp4<-purtest(epu~trend, data=D, lags="AIC", pmax=12, test="levinlin")
summary(agdp4)
print(agdp4)

agdp5<-purtest(unemployment~trend, data=D, lags="AIC", pmax=12, test="levinlin")
summary(agdp5)
print(agdp5)


varone<-pvargmm(
  dependent_vars = c("Expectations", "approval","inflation"),
  exog_vars=c("unemployment","epu","Effective_exchange_rate"),
  lags=4,
  transformation = "fd",
  data=D,
  panel_identifier = c("Country","Date"),
  steps = c("twostep"), 
  collapse=TRUE
)

summary(varone)
Andrews_Lu_MMSC(varone)
stab<-stability(varone)
stab

readRDS("C:/Users/Joao Caetano/Desktop/data_fundamentals.RData")


E <- read_excel("C:/Users/Joao Caetano/Desktop/E.xlsx")
View(E)

E<-pdata.frame(E, index=c("Country","Date"))


vartwo<-pvargmm(
  dependent_vars = c("d_expecs","d_inflation","d_approval","d_unemployment"),
  exog_vars=c("X2008_shock","Pparty_seat_share","d_eer","d_EPU"),
  lags=6,
  transformation = "fd",
  data=E,
  panel_identifier = c("Country","Date"),
  steps = c("twostep"), 
  collapse=TRUE
)

summary(vartwo)


vartwo_irf<-oirf(vartwo,n.ahead=6)
vartwo_girf<-girf(vartwo,n.ahead=6,ma_approx_steps = 6)
vartwo_bs<-bootstrap_irf(vartwo,typeof_irf = c("GIRF"),n.ahead = 4,
                         nof_Nstar_draws = 4, confidence.band = 0.90)


stab<-stability(vartwo)
plot(stab)
plot(vartwo_irf)
plot(vartwo_girf)

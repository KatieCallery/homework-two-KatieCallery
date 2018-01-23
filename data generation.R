curve(10*x*exp(-2*x),from=0,to=5)

x=seq(from=0,to=5,length=1000)

sets=rpois(1000,lambda=4)+1

y=rbinom(1000,prob=plogis(10*x*exp(-2*x)),size=sets)

y_m<-y/sets

plot(y_m~x)

sum(dbinom(y,prob=plogis(10*x*exp(-2*x)),size=sets),log=T)

sum(dbinom(y,prob=plogis(-50*x*exp(50*x)),size=sets),log=T)

recs<-cbind(y,sets-y)
# 
# (Intercept)           x      I(x^2)      I(x^3) 
# 1.38406170 -0.01199389 -0.28138336  0.04728792

curve(plogis(10*x*exp(-2*x)),col="green",add=T)
curve(plogis(1.44+0.19*x+-0.21*x^2+0.04*x^3),add=T,col="blue")

mosquito_data<-data.frame("Emergent_adults"=y,Egg_Count=sets,Detritus=round(x,2))

save(mosquito_data,file="mosquito_data.csv")

m1<-glm(response~predictor,family=binomial)

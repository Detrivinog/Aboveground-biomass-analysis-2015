datos<- read.csv2("Regresión.csv")
lin_ab<- with(datos, lm(b_anh~g))
log_ab<- with(datos, lm(log(b_anh)~log(g)))
lin_n<- with(datos, lm(b_anh~n))
log_n<- with(datos, lm(log(b_anh)~log(n)))
with(datos, plot(n, b_anh))
anova(lin_ab)
furn1<- sqrt(anova(lin_ab)[2,3])
variable<- 1/datos$b_anh
medgeo<- (prod(variable)^(1/length(variable)))
furn2<- (sqrt(anova(log_ab)[2,3]))*(1/medgeo)
datos$e1<- (predict(lin_ab)-datos$b_anh)/datos$b_anh
res_log1<- anova(log_ab)[, 3]
FC<- exp(res_log1[2]/2) 
datos$b_log<- FC*exp(predict(log_ab))
datos$e2<- (datos$b_log-datos$b_anh)/datos$b_anh
  
with(datos, plot(g, b_anh, pch=16, 
         ylab=expression("Biomasa aérea"~(Mg~ha^{-1})), 
         xlab=expression("Área basal"~(m^{2}~ha^{???1}))))
abline(lin_ab,col=4)


with(datos, plot(n, b_anh, pch=16, 
                 ylab=expression("Biomasa aérea"~(Mg~ha^{-1})),
                 xlab=expression("Número de árboles"~(ha^{???1}))))
abline(lin_n, col=4)
plot(lin_n)


mean(datos$n)
sd(datos$n)
summary(lin_n)
anova(lin_n)
range(datos$n)
with(datos, 100*(sum(hf))/(sum(bt)))



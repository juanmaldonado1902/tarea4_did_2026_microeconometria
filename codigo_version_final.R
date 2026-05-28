# Script tarea 4
# Creada por: Juan Pablo Maldonado 

rm(list=ls())
library(data.table); library(stringr); library(ggplot2); library(fixest); library(did)

# lectura, definición de los años del panel e identificación columnas, tratamiento anual y mortalidad anual.
wide <- fread("all_jails_ready_wide.csv"); years <- 2008:2019
idc <- intersect(c("id","jail","state","county","fips","statecode"), names(wide))
tc <- intersect(paste0("treated_", years), names(wide)); mc <- intersect(paste0("mortality_rate_", years), names(wide))

#Conversión a long generando private_provider a partir de treated_año
# Extrae el año del nombre de la columna y convierte el tratamiento a numérico hace lo mismo para moratlity_rate 
#Panel unifica el tratamiento y mortalidad 
t <- melt(wide,id.vars=idc,measure.vars=tc,variable.name="v",value.name="private_provider")
t[,`:=`(year=as.numeric(str_extract(v,"\\d{4}")),v=NULL,private_provider=as.numeric(private_provider))]
r <- melt(wide,id.vars="id",measure.vars=mc,variable.name="v",value.name="mortality_rate")
r[,`:=`(year=as.numeric(str_extract(v,"\\d{4}")),v=NULL,mortality_rate=as.numeric(mortality_rate))]
panel <- merge(t,r,by=c("id","year"),all.x=TRUE); setorder(panel,id,year)

# Guardado e impresión de la base panel long 
fwrite(panel,"jails_panel_long.csv")
cat("Base panel long guardada como: jails_panel_long.csv\n")
print(head(panel,20))

# Resume cada cárcel: años privados, años públicos, primer año tratado, último año tratado y secuencia de tratamiento.
cj <- panel[,.(jail=first(jail),state=first(state),county=first(county),
               nobs=sum(!is.na(private_provider)),npriv=sum(private_provider==1,na.rm=TRUE),npub=sum(private_provider==0,na.rm=TRUE),
               first_treat=ifelse(any(private_provider==1,na.rm=TRUE),min(year[private_provider==1],na.rm=TRUE),0),
               last_treat=ifelse(any(private_provider==1,na.rm=TRUE),max(year[private_provider==1],na.rm=TRUE),0),
               first_public=ifelse(any(private_provider==0,na.rm=TRUE),min(year[private_provider==0],na.rm=TRUE),0),
               last_public=ifelse(any(private_provider==0,na.rm=TRUE),max(year[private_provider==0],na.rm=TRUE),0),
               seq=paste(ifelse(is.na(private_provider),"NA",private_provider),collapse="")),by=id]

#Clasificacion de las carceles
cj[,TreatmentStatus:=fifelse(nobs==0,"Missing",fifelse(npriv>0 & npub==0,"Always",fifelse(npriv==0 & npub>0,"Never","Switch")))]
cj[,UnitExitsTreatment:=as.numeric(TreatmentStatus=="Switch" & (first_treat<first_public | last_treat<last_public))]
cj[,switcher_clean:=as.numeric(TreatmentStatus=="Switch" & UnitExitsTreatment==0)]
cj[,analytic_switcher_clean:=as.numeric(switcher_clean==1 & !(id %in% c(356,462)))]

# Pa es la muesta base de comparables (never y switcherclean)
panel <- merge(panel,cj[,.(id,TreatmentStatus,UnitExitsTreatment,switcher_clean,analytic_switcher_clean,first_treat)],by="id",all.x=TRUE)
pa <- panel[TreatmentStatus=="Never" | analytic_switcher_clean==1]

#Impresión de los datos pedidos y anotados en la pregunta 1
print(table(cj$TreatmentStatus)); print(table(cj[TreatmentStatus=="Switch"]$UnitExitsTreatment))
print(cj[switcher_clean==1,uniqueN(id)]); print(cj[analytic_switcher_clean==1,uniqueN(id)])
print(cj[id %in% c(181,356,462),.(id,jail,state,county,TreatmentStatus,UnitExitsTreatment,switcher_clean,analytic_switcher_clean,seq)])

# Generación tabla por año de privatización
tabla1 <- cj[analytic_switcher_clean==1,.N,by=first_treat][order(first_treat)]
setnames(tabla1,c("Primer año de tratamiento","Número de cárceles")); print(tabla1)

# Gráfica la distribución de este resultado
p1 <- ggplot(tabla1,aes(`Primer año de tratamiento`,`Número de cárceles`))+
  geom_col(fill="gray35")+theme_classic(base_size=13)+scale_x_continuous(breaks=years)+
  labs(title="Número de cárceles switcher-clean por año de privatización",x="Primer año de tratamiento",y="Número de cárceles")
print(p1)

# Esta es la parte de la replicación de la figura, la cual es establecida con Callaway/SA
cs_fig3 <- pa[!is.na(mortality_rate)]
cs_fig3[, g := fifelse(analytic_switcher_clean==1, first_treat, 0)]
attgt_fig3 <- att_gt(yname="mortality_rate",tname="year",idname="id",gname="g",data=cs_fig3,      # Calcula (att(g,t)) toma never treated como grupo de comparacion
                     panel=TRUE,allow_unbalanced_panel=TRUE,control_group="nevertreated",
                     xformla=~1,clustervars="id",bstrap=TRUE,biters=500)
dyn_fig3 <- aggte(attgt_fig3,type="dynamic",na.rm=TRUE)  # Agrega los efectos por tiempo relativo (aggte = callaway/stanna)
print(summary(attgt_fig3)); print(summary(dyn_fig3))

#Generación del event study
fig3_cs <- ggdid(dyn_fig3,ylim=c(-4,4))+theme_classic(base_size=18)+
  geom_vline(xintercept=-0.5,linetype="dotted")+geom_hline(yintercept=0,linetype="dashed")+
  theme(legend.position="none",plot.subtitle=element_text(size=13,color="grey50",face="italic"))+
  xlab("Years Pre/Post Health Care Privatization")+ylab("ATT (Deaths per 1,000 daily inmates)")+
  ggtitle("Overall Jail Mortality",subtitle="Average Effect of Privatization by Length of Exposure")
print(fig3_cs)

dir.create("figures",showWarnings=FALSE)
ggsave("figures/figura3_panel_superior_callaway_santanna.png",fig3_cs,width=10,height=6,dpi=300)

# DID SIMPLE 2008-2015  (me quedo solo con datos que abarcan del (2008-2015) ocupa como tratados los que ya se trataron en 2015)
did <- pa[year %in% c(2008,2015) & (TreatmentStatus=="Never" | (analytic_switcher_clean==1 & first_treat<=2015))]
did[,`:=`(treated_group=as.numeric(analytic_switcher_clean==1 & first_treat<=2015),post=as.numeric(year==2015))]
did[,did:=treated_group*post]

did_2x2 <- did[,.(media_mortalidad=mean(mortality_rate,na.rm=TRUE),
                  n_obs=sum(!is.na(mortality_rate)),
                  n_carceles=uniqueN(id[!is.na(mortality_rate)])),
               by=.(grupo=fifelse(treated_group==1,"Tratados: privatizan hasta 2015","Control: never-treated"),
                    periodo=fifelse(post==1,"Post: 2015","Pre: 2008"))][order(grupo,periodo)]

#Estimación manual
did_2x2_wide <- dcast(did_2x2,grupo~periodo,value.var="media_mortalidad")
did_2x2_wide[,cambio:=`Post: 2015`-`Pre: 2008`]
att_did_simple <- did_2x2_wide[grupo=="Tratados: privatizan hasta 2015",cambio]-
  did_2x2_wide[grupo=="Control: never-treated",cambio]
print(did_2x2); print(did_2x2_wide)
cat("\nATT DID simple 2008-2015 corregido:",round(att_did_simple,3),"\n")

#Regresión para generar el cambio
modelo_simple <- lm(mortality_rate~treated_group+post+did,data=did)
tabla2 <- data.table(Variable=c("Intercepto","treated_group","post","treated_group × post"),
                     Coeficiente=round(coef(summary(modelo_simple))[,1],3),
                     `Error estándar`=round(coef(summary(modelo_simple))[,2],3),
                     t=round(coef(summary(modelo_simple))[,3],3),
                     `p-valor`=round(coef(summary(modelo_simple))[,4],3))
print(tabla2)

#Modelo de efectos fijos por carcel y por estado, especificando la clusterización
did[,`:=`(post2015=post,treat_post2015=did)]
m1 <- feols(mortality_rate~post2015+treat_post2015|id,data=did,cluster=~id)
m2 <- feols(mortality_rate~post2015+treat_post2015|state,data=did,cluster=~state)
att_twfe <- feols(mortality_rate~private_provider|id+year,data=pa,cluster=~id)   #TWFE usando todos los años: efectos fijos de cárcel y año.

etable(m1,m2,dict=c(post2015="Dummy 2015",treat_post2015="Tratado × Post 2015"),fitstat=~n+wr2,se.below=TRUE)
etable(m1,att_twfe,dict=c(post2015="Dummy 2015",treat_post2015="Tratado × Post 2015",private_provider="Privatización médica"),
       headers=c("DID 2008-2015","TWFE ATT 2008-2019"),fitstat=~n+wr2,se.below=TRUE)

#creación del event sudy TWFE
pa[,`:=`(treated_group=as.numeric(analytic_switcher_clean==1),
         rel_year=fifelse(analytic_switcher_clean==1,year-first_treat,0))]
event_twfe <- feols(mortality_rate~i(rel_year,treated_group,ref=-1)|id+year,data=pa,cluster=~id)
print(summary(event_twfe))
iplot(event_twfe,ref.line=-1,xlab="Años relativos a la privatización",ylab="Efecto estimado sobre mortality_rate",
      main="Event study TWFE: privatización médica y mortalidad",ci_level=.95); abline(h=0,lty=2)

# Creación del modelo de Callaway and Santa Anna con 2013-2015
# Usa 2012 como pre y 2015 como post, att_cs genera el calculo "manual"
cs <- pa[(analytic_switcher_clean==1 & first_treat==2013)|TreatmentStatus=="Never"][year %in% c(2012,2015)]
cs[,`:=`(grupo=fifelse(analytic_switcher_clean==1 & first_treat==2013,"Tratados: privatizan en 2013","Control: never-treated"),
         periodo=fifelse(year==2012,"Pre: 2012","Post: 2015"))]
tab_cs <- cs[,.(media_mortalidad=mean(mortality_rate,na.rm=TRUE),
                n_obs=sum(!is.na(mortality_rate)),
                n_carceles=uniqueN(id[!is.na(mortality_rate)])),by=.(grupo,periodo)]
w <- dcast(tab_cs,grupo~periodo,value.var="media_mortalidad")
w[,cambio:=`Post: 2015`-`Pre: 2012`]
att_cs <- w[grupo=="Tratados: privatizan en 2013",cambio]-w[grupo=="Control: never-treated",cambio]
print(tab_cs); print(w); cat("\nATT C&S (2013,2015):",round(att_cs,3),"\n")

#Borusyak (bor fit genera las estimaciones para despues construir los imputados)
pa[,treated_now:=as.numeric(private_provider==1)]
bor_fit <- feols(mortality_rate~1|id+year,data=pa[treated_now==0 & !is.na(mortality_rate)],cluster=~id)
pa[,y0_hat:=predict(bor_fit,newdata=pa)]

# Calcula efectos individuales para las cárceles tratadas en 2013 observadas en 2015.
b13 <- pa[analytic_switcher_clean==1 & first_treat==2013 & year==2015 & !is.na(mortality_rate) & !is.na(y0_hat)]
b13[,att_i:=mortality_rate-y0_hat]

tabla_borusyak_10 <- b13[,.(id,Yi_2015=round(mortality_rate,3),
                            Yc_imputado_2015=round(y0_hat,3),
                            ATT_i=round(att_i,3))]
setorder(tabla_borusyak_10,id); print(tabla_borusyak_10)

cat("\nATT Borusyak (2013,2015):",round(mean(b13$att_i),3),"\n")
print(summary(b13$y0_hat))

#Generación del histograma
p_hist <- ggplot(b13,aes(y0_hat))+geom_histogram(bins=15,fill="gray35",color="white")+
  theme_classic(base_size=13)+
  labs(title="Histograma del contrafactual imputado",
       subtitle=expression(Y[i,2015]^C~"para cárceles que privatizan en 2013"),
       x=expression(hat(Y)[i,2015]^C),y="Número de cárceles")
print(p_hist)

pa[,`:=`(rel_year=fifelse(analytic_switcher_clean==1,year-first_treat,NA_real_),
         tau_hat=mortality_rate-y0_hat)]
eb <- pa[analytic_switcher_clean==1 & !is.na(rel_year) & !is.na(tau_hat),
         .(att=mean(tau_hat),se=sd(tau_hat)/sqrt(.N),
           n_obs=.N,n_carceles=uniqueN(id)),by=rel_year][order(rel_year)]
eb[,`:=`(ci_low=att-1.96*se,ci_high=att+1.96*se)]
print(eb)

p_eb <- ggplot(eb,aes(rel_year,att))+geom_hline(yintercept=0,linetype="dashed")+
  geom_vline(xintercept=-1,linetype="dotted")+geom_point(size=2)+
  geom_errorbar(aes(ymin=ci_low,ymax=ci_high),width=.15)+theme_classic(base_size=13)+
  labs(title="Event study Borusyak: privatización médica y mortalidad",
       x="Años relativos a la privatización",y="ATT imputado sobre mortality_rate")
print(p_eb)

# SA manual replica sunab()
# Defino g como el año de primer tratamiento para switchers y 0 para never-treated
# e como el tiempo relativo al tratamiento 
sa <- copy(pa[!is.na(mortality_rate)])
sa[,`:=`(g=fifelse(analytic_switcher_clean==1,first_treat,0),e=fifelse(analytic_switcher_clean==1,year-first_treat,NA_real_))]

#Para cada cohorte G calcula los períodos relativos factibles dentro del panel (ymin-G a ymax-G) 
#Excluye e = -1 como categoría de referencia, y crea una dummy binaria por cada combinación (G, e)

gs <- sort(unique(sa[g>0]$g)); ymin <- min(sa$year); ymax <- max(sa$year)
dummy_vars <- c()
for(G in gs){e_pos <- setdiff((ymin-G):(ymax-G),-1); for(E in e_pos){vn <- paste0("G",G,"_E",ifelse(E<0,paste0("m",abs(E)),E)); sa[,(vn):=as.integer(g==G & !is.na(e) & e==E)]; dummy_vars <- c(dummy_vars,vn)}}

# Cálculo de la regresión dummies cohorte por tiempo/ efectos fijos de carcel y año con cluster por carcel
# bsa - extraccion del coeficiente (2013,2015)
sa_manual <- feols(as.formula(paste("mortality_rate~",paste(dummy_vars,collapse="+"),"|id+year")),data=sa,cluster=~id)
bsa <- data.table(term=names(coef(sa_manual)),beta=coef(sa_manual),se=se(sa_manual))
bsa[,`:=`(G=as.integer(str_extract(term,"(?<=G)\\d+")),e=as.integer(gsub("m","-",str_extract(term,"(?<=_E)m?\\d+"))))]

# Cálculo del peso de cada cohorte como su proporción en el total de tratados
cs <- sa[analytic_switcher_clean==1,.(n_G=uniqueN(id)),by=.(G=first_treat)]; cs[,share:=n_G/sum(n_G)]
bsa <- merge(bsa,cs[,.(G,share)],by="G")
event_sa <- bsa[,.(att=weighted.mean(beta,share),se=sqrt(sum((share/sum(share))^2*se^2)),n_cohorts=.N),by=e][order(e)]
event_sa[,`:=`(ci_low=att-1.96*se,ci_high=att+1.96*se)]
# ATT cohorte 2013 observada en 2015 (e=2)
att_sa_2013_2015 <- bsa[G==2013 & e==2,beta]
cat("\nATT Sun & Abraham manual (2013, 2015):",round(att_sa_2013_2015,3),"\n"); print(event_sa)
p_sa <- ggplot(event_sa,aes(e,att))+geom_hline(yintercept=0,linetype="dashed")+geom_vline(xintercept=-1,linetype="dotted")+geom_point(size=2)+geom_errorbar(aes(ymin=ci_low,ymax=ci_high),width=.15)+theme_classic(base_size=13)+labs(title="Event study Sun & Abraham manual",x="Años relativos a la privatización",y="ATT sobre mortality_rate")
print(p_sa)

# Validación automática con sunab() de fixest
sa_auto <- feols(mortality_rate~sunab(g,year)|id+year,data=sa,cluster=~id)
auto_coefs <- data.table(term=names(coef(sa_auto)),beta_auto=as.numeric(coef(sa_auto)),se_auto=as.numeric(se(sa_auto)))
auto_coefs[,e:=as.integer(str_extract(term,"-?\\d+$"))]
# Comparación coeficiente a coeficiente: manual vs automático
comp <- merge(event_sa[,.(e,att_manual=att,se_manual=se)],auto_coefs[,.(e,beta_auto,se_auto)],by="e")[order(e)]
print(comp); print(summary(sa_auto))
iplot(sa_auto,main="Sun & Abraham automático",xlab="Años relativos a la privatización",ylab="ATT sobre mortality_rate"); abline(h=0,lty=2)
summary(aggregate(sa_auto,agg="att"))

ggsave("figures/event_study_sun_abraham_manual.png", p_sa, width = 8, height = 5, dpi = 300)
ggsave("figures/histograma_yc_borusyak_2013_2015.png",p_hist,width=8,height=5,dpi=300)
ggsave("figures/event_study_borusyak.png",p_eb,width=8,height=5,dpi=300)

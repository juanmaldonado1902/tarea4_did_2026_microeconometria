# Script tarea 4
# Creada por: Juan Pablo Maldonado 
# Revisada y editada por: Arturo Aguilar

rm(list=ls())
library(data.table) 
library(stringr) 
library(ggplot2) 
library(fixest) 
library(did)
library(dplyr)
library(tidyr)
library(didimputation)

# lectura, definición de los años del panel e identificación columnas, tratamiento anual y mortalidad anual.
wide <- read.csv("all_jails_ready_wide.csv") 
wide <- as.data.table(wide)
years <- 2008:2019

idc <- intersect(c("id","jail","state","county","fips","statecode"), names(wide))
tc <- intersect(paste0("treated_", years), names(wide)) 
mc <- intersect(paste0("mortality_rate_", years), names(wide))

#Conversión a long generando private_provider a partir de treated_año
# Extrae el año del nombre de la columna y convierte el tratamiento a numérico hace lo mismo para moratlity_rate 
#Panel unifica el tratamiento y mortalidad 
t <- wide %>%
  select(all_of(c(idc, tc))) %>%
  pivot_longer(cols = all_of(tc),names_to = "variable",
               values_to = "treated") %>%
  mutate(year = as.numeric(str_extract(variable, "\\d{4}"))) %>%
  select(-variable)

r <- wide %>%
  select(id, all_of(mc)) %>%
  pivot_longer(cols = all_of(mc),names_to = "variable",
               values_to = "mortality_rate") %>%
  mutate(year = as.numeric(str_extract(variable, "\\d{4}"))) %>%
  select(-variable)

panel <- t %>%
  left_join(r, by = c("id", "year")) %>%
  arrange(id, year)

# Guardado e impresión de la base panel long 
write.csv(panel,file="jails_panel_long.csv")
print(head(panel,20))

# Resume cada cárcel: años privados, años públicos, primer año tratado, último año tratado y secuencia de tratamiento.
cj <- panel %>% group_by(id) %>%
  summarise(jail = first(jail),
            state  = first(state),
            county = first(county),
            nobs  = sum(!is.na(treated)),
            npriv = sum(treated == 1, na.rm = TRUE), 
            npub  = sum(treated == 0, na.rm = TRUE),
            first_treat = if (any(treated == 1, na.rm = TRUE)) 
              min(year[treated == 1], na.rm = TRUE) else 0,
            last_treat = if (any(treated == 1, na.rm = TRUE))
            max(year[treated == 1], na.rm = TRUE) else 0,
            first_public = if (any(treated == 0, na.rm = TRUE))
            min(year[treated == 0], na.rm = TRUE) else 0,
            last_public = if (any(treated == 0, na.rm = TRUE))
            max(year[treated == 0], na.rm = TRUE) else 0,
            seq = paste(coalesce(as.character(treated), "NA"),
                collapse = ""),
            .groups = "drop")

#Clasificacion de las carceles
cj <- cj %>%
  mutate(TreatmentStatus = case_when(nobs == 0 ~ "Missing",
                                     npriv > 0 & npub == 0 ~ "Always",
                                     npriv == 0 & npub > 0 ~ "Never",      
                                     TRUE ~ "Switch"),  
         UnitExitsTreatment = as.numeric(TreatmentStatus == "Switch" &  
                                           (first_treat < first_public | last_treat < last_public)),  
         switcher_clean = as.numeric(TreatmentStatus == "Switch" &
                                       UnitExitsTreatment == 0),
         analytic_switcher_clean = as.numeric(switcher_clean == 1 &    
                                                !(id %in% c(356, 462))))

# Panel es la muesta base de comparables (never y switcherclean)
panel <- panel %>%
  left_join(cj %>% select(id,TreatmentStatus,UnitExitsTreatment,
                          switcher_clean,analytic_switcher_clean,
                          first_treat),by = "id") %>%
  filter(TreatmentStatus == "Never" | analytic_switcher_clean == 1)

#Impresión de los datos pedidos y anotados en la pregunta 1
print(table(cj$TreatmentStatus)) 
print(table(cj$UnitExitsTreatment[cj$TreatmentStatus=="Switch"]))

# ====/// 1b: Tabla y grafica de año de privatizacion \\\=====

# Tabla por año de privatización
tabla1 <- cj %>%
  filter(analytic_switcher_clean == 1) %>%
  count(first_treat, name = "N") %>%
  arrange(first_treat)
setnames(tabla1,c("Primer año de tratamiento","Número de cárceles")) 
print(tabla1)

# Gráfica la distribución
ggplot(tabla1,aes(`Primer año de tratamiento`,`Número de cárceles`)) +
  geom_col(fill="gray35") +
  theme_classic(base_size=13) +
  scale_x_continuous(breaks=years) +
  labs(title="Número de cárceles switcher-clean por año de privatización",
       x="Primer año de tratamiento",
       y="Número de cárceles")

ggsave("Grafica_1b.png",  width = 5.54, height = 4.95)


# ====/// 2: DiD 2x2 con 2008 y 2015 \\\=====
did <- panel %>% filter(year %in% c(2008, 2015)) %>%  
  group_by(id) %>%
  filter(n_distinct(year[!is.na(mortality_rate)]) == 2) %>%
  ungroup() %>%
  mutate(treated_group = as.numeric(analytic_switcher_clean == 1 & first_treat <= 2015),    
         post = as.numeric(year == 2015),   
         did = treated_group * post)

did_2x2 <- did %>%
  mutate(grupo = if_else(treated_group == 1,
                         "Tratados: privatizan antes de 2015",
                         "Control: never-treated"),    
         periodo = if_else(post == 1,"Post: 2015","Pre: 2008")) %>%
  group_by(grupo, periodo) %>%
  summarise(media_mortalidad = mean(mortality_rate, na.rm = TRUE),    
            n_obs = sum(!is.na(mortality_rate)),    
            .groups = "drop") %>%
  arrange(grupo, periodo)

(att_2x2 <- (did_2x2$media_mortalidad[did_2x2$grupo == "Tratados: privatizan antes de 2015" & did_2x2$periodo == "Post: 2015"]-
            did_2x2$media_mortalidad[did_2x2$grupo == "Tratados: privatizan antes de 2015" & did_2x2$periodo == "Pre: 2008"])-
            (did_2x2$media_mortalidad[did_2x2$grupo == "Control: never-treated" & did_2x2$periodo == "Post: 2015"]-
            did_2x2$media_mortalidad[did_2x2$grupo == "Control: never-treated" & did_2x2$periodo == "Pre: 2008"]))
(desv_est_Y = sd(did$mortality_rate[did$year==2008], na.rm=TRUE))
(att_2x2/desv_est_Y)


# ====/// 3: Estimacion con FE \\\=====

m1 <- feols(mortality_rate~post+did|id,data=did,cluster=~id)

# ====/// 5: TWFE y Event study \\\=====

# Balanced panel
all_years <- sort(unique(panel$year))
panel <- panel %>% group_by(id) %>%
  filter(n_distinct(year[!is.na(mortality_rate)]) == length(all_years)) %>%
  ungroup() %>%
  mutate(treated_group = as.numeric(analytic_switcher_clean == 1))

#TWFE
twfe <- feols(mortality_rate~treated|id+year,data=panel,cluster=~id)

# Event study design
panel <- panel %>%
  mutate(T_time = if_else(analytic_switcher_clean == 1,
                          year - first_treat,NA_real_))
event_twfe <- feols(mortality_rate ~ i(T_time,ref = -1) |
                    id + year,data = panel,cluster = ~id)

png("event_study.png",  width = 1200, height = 800, res = 150)
iplot(event_twfe,ref.line=-1,xlab="Años relativos a la privatización",ylab="ATT",
      main="Event study",ci_level=.95); abline(h=0,lty=2)
abline(h = 0, lty = 2)
dev.off()


# ====/// 6: DiD 2x2 Callaway y SantAnna \\\=====
did_cs <- panel %>% filter(year %in% c(2012, 2015)) %>%  
  group_by(id) %>%
  filter(n_distinct(year[!is.na(mortality_rate)]) == 2) %>%
  ungroup() %>%
  filter(TreatmentStatus == "Never" | first_treat==2013) %>%
  mutate(treated_group = as.numeric(first_treat == 2013),    
         post = as.numeric(year == 2015),   
         did = treated_group * post)

cs_2x2 <- did_cs %>%
  mutate(grupo = if_else(treated_group == 1,
                         "Tratados: privatizan en 2013",
                         "Control: never-treated"),    
         periodo = if_else(post == 1,"Post: 2015","Pre: 2012")) %>%
  group_by(grupo, periodo) %>%
  summarise(media_mortalidad = mean(mortality_rate, na.rm = TRUE),    
            n_obs = sum(!is.na(mortality_rate)),    
            .groups = "drop") %>%
  arrange(grupo, periodo)

(att_2x2_cs <- (cs_2x2$media_mortalidad[cs_2x2$grupo == "Tratados: privatizan en 2013" & cs_2x2$periodo == "Post: 2015"]-
                  cs_2x2$media_mortalidad[cs_2x2$grupo == "Tratados: privatizan en 2013" & cs_2x2$periodo == "Pre: 2012"])-
    (cs_2x2$media_mortalidad[cs_2x2$grupo == "Control: never-treated" & cs_2x2$periodo == "Post: 2015"]-
       cs_2x2$media_mortalidad[cs_2x2$grupo == "Control: never-treated" & cs_2x2$periodo == "Pre: 2012"]))
(desv_est_Y = sd(did_cs$mortality_rate[did_cs$year==2012], na.rm=TRUE))
(att_2x2_cs/desv_est_Y)

# ====/// 8: Replicacion Figura 3 \\\=====

cs_fig3 <- panel %>%
  filter(!is.na(mortality_rate)) %>%
  mutate(g = if_else(analytic_switcher_clean == 1, first_treat, 0))
attgt_fig3 <- att_gt(yname="mortality_rate",tname="year",idname="id",gname="g",data=cs_fig3,      # Calcula (att(g,t)) toma never treated como grupo de comparacion
                     panel=TRUE,allow_unbalanced_panel=FALSE,control_group="nevertreated",
                     xformla=~1,clustervars="id",bstrap=TRUE,biters=500)
dyn_fig3 <- aggte(attgt_fig3,type="dynamic",na.rm=TRUE)  # Agrega los efectos por tiempo relativo (aggte = callaway/stanna)
print(summary(attgt_fig3)); print(summary(dyn_fig3))

#Generación del event study
ggdid(dyn_fig3,ylim=c(-4,4))+theme_classic(base_size=18)+
  geom_vline(xintercept=-0.5,linetype="dotted")+geom_hline(yintercept=0,linetype="dashed")+
  theme(legend.position="none",plot.subtitle=element_text(size=13,color="grey50",face="italic"))+
  xlab("Years Pre/Post Health Care Privatization")+ylab("ATT (Deaths per 1,000 daily inmates)")+
  ggtitle("Overall Jail Mortality",subtitle="Average Effect of Privatization by Length of Exposure")

ggsave("figure3_callaway_santanna.png",  width = 5.54, height = 4.95)


# ====/// 9: Borusyak \\\=====

# ATT(2013,2015)
bjs_fe <- feols(mortality_rate~1|id+year,data=panel %>% filter(treated==0,!is.na(mortality_rate)),cluster=~id)


panel <- panel %>% mutate(mortality_bjs_hat = predict(bjs_fe, newdata = panel),
                          Treatment_eff=mortality_rate-mortality_bjs_hat)

ggplot(data = panel %>% filter(treated==1,year==2015, first_treat==2013), aes(x=mortality_bjs_hat)) +
  geom_histogram(bins=20) + theme(legend.position="none") + 
  theme_classic() + 
  labs(title="Histograma del Y_C contrafactual (year=2015,G=2013)",
                         x="mortalidad contrafactual")
ggsave("Graf9a.png",  width = 5.54, height = 4.95)

summary(panel$mortality_bjs_hat[panel$treated==1 & panel$first_treat==2013 & panel$year==2015])
(desv_est_Y <- sd(panel$mortality_bjs_hat[panel$treated==1 & panel$first_treat==2013 & panel$year==2015]))
(att_bjs_13_15 <- mean(panel$Treatment_eff[panel$treated==1 & panel$first_treat==2013 & panel$year==2015]))
(att_bjs_13_15/desv_est_Y)

# Event study
#Preparar datos
bjs_fig3 <- panel %>% filter(!is.na(mortality_rate)) %>%
  mutate(g = if_else(analytic_switcher_clean == 1,first_treat,0))

bjs_es <- did_imputation(data = bjs_fig3,
                         yname = "mortality_rate",gname = "g",  
                         tname = "year",idname = "id",  
                         horizon = TRUE,pretrends = TRUE)

bjs_es <- bjs_es %>%
  mutate(event_time = as.numeric(term))

ggplot(bjs_es,aes(x = event_time,y = estimate)) +
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_vline(xintercept = -0.5,linetype = "dotted") +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = conf.low,ymax = conf.high),width = .15) +
  theme_classic(base_size = 18) +
  labs(title="BJS Event Study Estimator", x="Years Pre/Post Privatization",
       y="ATT")

ggsave("Graf9b.png",  width = 5.54, height = 4.95)  


# ====/// 10: Otro metodo \\\=====
# Sun & Abraham, hecho por Juan P. Maldonado

# SA manual replica sunab()
# Defino g como el año de primer tratamiento para switchers y 0 para never-treated
# e como el tiempo relativo al tratamiento 
sa <- panel %>% filter(!is.na(mortality_rate))
sa <- sa %>%
  mutate(g = if_else(analytic_switcher_clean == 1, first_treat, 0),
         e = if_else(analytic_switcher_clean == 1, year - first_treat, NA_real_))
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

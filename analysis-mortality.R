library(data.table)
library(ggplot2)
library(fixest) 
library(did)
library(geofacet)
library(panelView)

#### File description
# This is the main analysis file
## it produces all mortality-related results
### generates Figures 1-5; all tables
#

# set seed so bootstrapped standard errors are consistent across runs
set.seed(02138)
# read Reuters jail death / facility data
jail = fread("all_jails.csv")
# gt healthcare
jail_long = melt(jail, id.vars = "id", measure.vars = patterns("med"))
jail_long[, variable := substr(variable,4,7),]
setnames(jail_long, "variable", "year")
setnames(jail_long, "value", "health_provider")

# get inmate population levels
inm = melt(jail, id.vars = "id", measure.vars = patterns("adp"))
inm[, variable := substr(variable,4,7),]
setnames(inm, "variable", "year")
setnames(inm, "value", "average_daily_population")
## merge
jail_long = merge(jail_long, inm, by = c("id", "year"))
## remove inm
rm(inm)

# get inmate overall death levels
inm = melt(jail, id.vars = "id", measure.vars = names(jail)[names(jail) %like% "d20" & !(names(jail) %like% "med")])
inm[, variable := substr(variable,2,5),]
setnames(inm, "variable", "year")
setnames(inm, "value", "total_deaths_annual")
## merge
jail_long = merge(jail_long, inm, by = c("id", "year"))
## remove inm
rm(inm)
# name of jail, state, county, etc
jail_long = merge(jail_long, jail[,c(1:8)], by = "id")
##
# create rate
jail_long$death_rate = jail_long$total_deaths_annual / jail_long$average_daily_population * 1000
jail_long$illness_suicide_death_rate = (jail_long$illness_deaths_annual + jail_long$suicide_deaths_annual + jail_long$overdose_deaths_annual) / jail_long$average_daily_population * 1000

# indicator public private
jail_long[, private_provider := as.numeric(health_provider != "public" & (health_provider != ""))]
jail_long[, public_provider := as.numeric(health_provider == "public" & (health_provider != ""))]
jail_long[, large_provider := as.numeric(stringr::str_to_lower(health_provider) %like%
                            "corizon|wellpath|advanced correctional healthcare|primecare medical")]

# replace 0 with missing when health_provider == ""
jail_long[health_provider == ""]$private_provider = NA
jail_long[health_provider == ""]$public_provider = NA
jail_long[health_provider == ""]$large_provider = NA
## data prep
# log dv
jail_long$log_death_rate = log(jail_long$death_rate+1)
# asinh dv
jail_long$asinh_death_rate = asinh(jail_long$death_rate)
# make first treated privatization var
jail_long[private_provider == 1, first.treat := min(year) , by = list(id)]
jail_long[is.na(first.treat)]$first.treat = 0
jail_long[,  first.treat := max(first.treat), by = list(id)]
jail_long$year = as.numeric(jail_long$year)
jail_long$first.treat = as.numeric(jail_long$first.treat)

# last private variable
jail_long[private_provider == 1, last.treat := max(year) , by = list(id)]
jail_long[is.na(last.treat)]$last.treat = 0
jail_long[,  last.treat := max(last.treat), by = list(id)]

# make first treated public var
jail_long[private_provider == 0, first.treat.public := min(year) , by = list(id)]
jail_long[is.na(first.treat.public)]$first.treat.public = 0
jail_long[,  first.treat.public := max(first.treat.public), by = list(id)]
jail_long$first.treat.public = as.numeric(jail_long$first.treat.public)

# last public variable
jail_long[private_provider == 0, last.treat.public := max(year) , by = list(id)]
jail_long[is.na(last.treat.public)]$last.treat.public = 0
jail_long[,  last.treat.public := max(last.treat.public), by = list(id)]

# make first big company var
jail_long[large_provider == 1, first.treat.large := min(year), by = list(id)]
jail_long[is.na(first.treat.large)]$first.treat.large = 0
jail_long[,  first.treat.large := max(first.treat.large), by = list(id)]
jail_long$year = as.numeric(jail_long$year)
jail_long$first.treat.large = as.numeric(jail_long$first.treat.large)

# last big company variable
jail_long[large_provider == 1, last.treat.large := max(year) , by = list(id)]
jail_long[is.na(last.treat.large)]$last.treat.large = 0
jail_long[,  last.treat.large := max(last.treat.large), by = list(id)]

# never treated, always treated, switcher
jail_long[, TreatmentStatus := ifelse(mean(na.omit(private_provider))==0,"Never", ifelse(mean(na.omit(private_provider))==1, "Always", "Switch")), by = list(id)]
jail_long$UnitExitsTreatment = as.numeric(jail_long$id %in% unique(jail_long[first.treat < first.treat.public & TreatmentStatus == "Switch"], by = "id"
)$id | (jail_long$id %in% unique(jail_long[last.treat < last.treat.public & TreatmentStatus == "Switch"], by = "id"
)$id)) 
jail_long$UnitExitsTreatmentPublic = as.numeric(jail_long$id %in% unique(jail_long[first.treat.public < first.treat & TreatmentStatus == "Switch"], by = "id"
)$id | (jail_long$id %in% unique(jail_long[last.treat.public < last.treat & TreatmentStatus == "Switch"], by = "id"
)$id))
jail_long$AlwaysMissingOrAlwaysTreated = as.numeric(jail_long$id %in% jail_long[!is.na(private_provider), list(onlyprivate=mean(private_provider)==1) , by = list(id)][onlyprivate==T]$id)
jail_long[, AlwaysTreatedWhenDVNotMissing := as.numeric(id %in% jail_long[!is.na(death_rate), list(AlwaysTreatedWhenDVNotMissing = mean(private_provider)==1) , by = list(id)][AlwaysTreatedWhenDVNotMissing==T]$id) , ]

# ============================================================
# IDS DE SWITCHER-CLEAN QUE EL REPLICATION NO INCLUYE
# EN EL ANÁLISIS PRINCIPAL
# ============================================================

library(data.table)

setDT(jail_long)

# 1) Switcher-clean según replication:
# Switchers que NO revierten
switcher_clean_ids <- unique(
  jail_long[
    TreatmentStatus == "Switch" &
      UnitExitsTreatment == 0,
    id
  ]
)

cat("\nTotal switcher-clean antes del filtro analítico:\n")
print(length(switcher_clean_ids))  # debería dar 98


# 2) Switcher-clean incluidos en análisis principal:
# filtro usado en att_gt:
# UnitExitsTreatment == 0 &
# TreatmentStatus != "Always" &
# AlwaysTreatedWhenDVNotMissing == 0

switcher_clean_included_ids <- unique(
  jail_long[
    TreatmentStatus == "Switch" &
      UnitExitsTreatment == 0 &
      AlwaysTreatedWhenDVNotMissing == 0,
    id
  ]
)

cat("\nTotal switcher-clean incluidos en análisis principal:\n")
print(length(switcher_clean_included_ids))  # debería dar 96


# 3) Switcher-clean excluidos
switcher_clean_excluded_ids <- setdiff(
  switcher_clean_ids,
  switcher_clean_included_ids
)

cat("\nIDs de switcher-clean NO incluidos en análisis principal:\n")
print(switcher_clean_excluded_ids)

cat("\nNúmero de switcher-clean excluidos:\n")
print(length(switcher_clean_excluded_ids))  # debería dar 2


# ============================================================
# TABLA CON NOMBRES Y TRAYECTORIAS DE LOS EXCLUIDOS
# ============================================================

excluded_switcher_clean_table <- jail_long[
  id %in% switcher_clean_excluded_ids,
  .(
    jail = first(jail),
    state = first(state),
    county = first(county),
    TreatmentStatus = first(TreatmentStatus),
    UnitExitsTreatment = first(UnitExitsTreatment),
    AlwaysTreatedWhenDVNotMissing = first(AlwaysTreatedWhenDVNotMissing),
    first_treat = first(first.treat),
    last_treat = first(last.treat),
    first_treat_public = first(first.treat.public),
    last_treat_public = first(last.treat.public),
    treatment_sequence = paste(
      ifelse(is.na(private_provider), "NA", private_provider),
      collapse = ""
    ),
    death_rate_sequence = paste(
      ifelse(is.na(death_rate), "NA", round(death_rate, 3)),
      collapse = " | "
    )
  ),
  by = id
]

cat("\nTabla de switcher-clean excluidos:\n")
print(excluded_switcher_clean_table)

# Guardar
fwrite(
  excluded_switcher_clean_table,
  "switcher_clean_excluidos_replication.csv"
)

cat("\nArchivo guardado: switcher_clean_excluidos_replication.csv\n")


### basic descriptive statistics enumerated in article text
# share privatized: ~47% in 2008; 66% in 2019; count = 523
jail_long[, list(percentage_treated = mean(na.omit(private_provider)),
      count = .N), by = list(year)][year %in% c(2008,2019)]
# 1 facility always missing on privatization variable
jail_long[,list(missingprivate=mean(is.na(private_provider))), by = list(id)][missingprivate==1]
# treatment status numbers always, never, switch
### 231 always private; 166 never private; 125 switch
table(jail_long[year == 2008]$TreatmentStatus)
### of the switch, 98 stay private and 27 revert
table(jail_long[year == 2008]$TreatmentStatus, jail_long[year == 2008]$UnitExitsTreatment)
### 2 additional facilities are always missing when not treated
length(unique(jail_long[UnitExitsTreatment==0 & TreatmentStatus != "Always" & AlwaysTreatedWhenDVNotMissing == 1,]$id))

# La facility excluida por completo: siempre missing en private_provider
excluded_facility <- jail_long[,
                               list(missingprivate = mean(is.na(private_provider))),
                               by = list(id, jail, county, state)
][missingprivate == 1]

print(excluded_facility)

## 27 de-privatize; of these, 10 re-privatize
table(jail_long[year == 2008]$UnitExitsTreatment,
      jail_long[year == 2008]$UnitExitsTreatmentPublic)
### 231 always private
length(unique(jail_long[TreatmentStatus == "Always"]$id))
### 2 of these always missing in dv [so 231 - 2 = 229]
nrow(jail_long[(TreatmentStatus == "Always")][, list(missing=mean(is.na(death_rate)), first.treat.public=mean(first.treat.public)), by = list(id)][missing==1])

### analysis 0 twfe
m = feols(death_rate ~ private_provider, data = jail_long)
m_fe = feols(death_rate ~ private_provider | id + year, data = jail_long, se = "cluster")
m_fe_pop = feols(death_rate ~ private_provider + log(average_daily_population) |
                   id + year, data = jail_long, se = "cluster")
m_fe_weight = feols(death_rate ~ private_provider | id + year, data = jail_long, se = "cluster",
                    weights = jail_long$average_daily_population)
m_fe_weight_log = feols(death_rate ~ private_provider | id + year, data = jail_long, se = "cluster",
                    weights = log(jail_long$average_daily_population+1))

# fixed effects poisson
m_fepois = fepois(total_deaths_annual ~ private_provider | id + year,
  data = jail_long, se = "twoway")
m_fepois_offset = fepois(total_deaths_annual ~ private_provider | id + year,
    offset = log(jail_long$average_daily_population), data = jail_long, se = "twoway")
esttable(m_fepois, m_fepois_offset, keep = "private_provider")
# logged + 1 dv
m_log = feols(log(death_rate+1) ~ private_provider, data = jail_long)
m_fe_log = feols(log(death_rate+1) ~ private_provider |
                   id + year, data = jail_long, se = "cluster")
m_fe_log_weight = feols(log(death_rate+1) ~ private_provider | id + year, data = jail_long, se = "cluster",
                    weights = jail_long$average_daily_population)
esttable(m_fe, m_fe_weight, m_fe_log, m_fe_log_weight, cluster=c("id", "year"))

# output plot
plotdat = data.table(cbind(
  rbind(summary(m_fe, se = "twoway")$coeftable[1:2],
        summary(m_fe_weight, se = "twoway")$coeftable[1:2],
        summary(m_fe_weight_log, se = "twoway")$coeftable[1:2],
  summary(m_fepois_offset, se = "twoway")$coeftable[1:2]),
Model = c("TWFE", "TWFE, WLS \n Population Weights","TWFE, WLS \n Log(Population) Weights", "Poisson TWFE")))

## analysis 1: overall death rate
attgt <- att_gt(yname = "death_rate", tname = "year", idname = "id",
      gname = "first.treat", panel = TRUE,
      allow_unbalanced_panel = TRUE, xformla = ~1, clustervars = "id",
      data = jail_long[UnitExitsTreatment==0 &
  TreatmentStatus != "Always" & AlwaysTreatedWhenDVNotMissing == 0,],
)
# number of jails, by treatment status, in main analysis
## 166 never; 96 switch
table(jail_long[id %in% 
                  unique(attgt$DIDparams$data$id) &
                  year == 2008]$TreatmentStatus)
gs = aggte(attgt, type = "group")
# get overall group-averaged ATT: 0.33 (0.22) [-0.76, 0.09]
summary(gs)
# save ATT estimates
plotdat = data.table(rbind(plotdat, data.table(gs$overall.att, gs$overall.se, "Doubly-Robust \n (Unbalanced Panel)"), use.names = F))

# dynamic effect and plot
mw.dyn <- aggte(attgt, type = "dynamic")
summary(mw.dyn)
#### FIGURE 3A
ggdid(mw.dyn, ylim = c(-4,4)) + theme_classic(base_size = 22) + labs(color = "Treated") +
  geom_vline(xintercept = -0.5, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size=2) + geom_errorbar(width = 0.9) + scale_color_grey() +
  theme(legend.position = "", plot.subtitle = element_text(size=15, color = "grey", face = "italic"))  +
  xlab("Years Pre/Post Health Care Privatization") +
  ylab("ATT (Deaths per 1,000 daily innmates)") +
  ggtitle("Overall Jail Mortality", subtitle = "Average Effect of Privatization by Length of Exposure")
ggsave("Figures/Figur3a_dynamic_eventstudy_overall_death_plot.png",
       width = 12, height = 8, units = "in", dpi = 300)

mw.dyn.balance <- aggte(attgt, type = "dynamic", balance_e=4)
summary(mw.dyn.balance)

#### FIGURE 3B
ggdid(mw.dyn.balance, ylim = c(-4,4)) + theme_classic(base_size = 22) + labs(color = "Treated") +
  geom_point(size=2) + geom_errorbar(width = 0.6) + scale_color_grey() +
  geom_vline(xintercept = -0.5, linetype = "dotted") +
  theme(legend.position = "", plot.subtitle = element_text(size=15, color = "grey", face = "italic"))  +
  xlab("Years Pre/Post Health Care Privatization") + geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("ATT (Deaths per 1,000 daily innmates)") +
  ggtitle("Overall Jail Mortality", subtitle = "Average Effect of Privatization by Treatment Duration (min 4 years)")
ggsave("Figures/Figure3b_dynamic_eventstudy_balance_overall_death_plot.png",
       width = 12, height = 8, units = "in", dpi = 300)

## analysis 1b: overall death rate --> balanced panel
attgt <- att_gt(yname = "death_rate", tname = "year", idname = "id",
         gname = "first.treat", allow_unbalanced_panel = FALSE,
         xformla = ~1, data = jail_long[UnitExitsTreatment==0 &
  TreatmentStatus != "Always" & AlwaysTreatedWhenDVNotMissing == 0])

# get overall group-averaged ATT
gs = aggte(attgt, type = "group")
# save ATT
plotdat = data.table(rbind(plotdat, data.table(gs$overall.att, gs$overall.se, "Doubly-Robust \n (Balanced Panel)"), use.names = F))
colnames(plotdat)[1:2] = c("Estimate", "Std.Error")
plotdat$Estimate = as.numeric(plotdat$Estimate)
plotdat$Std.Error = as.numeric(plotdat$Std.Error)

# Figure 2 plot att
ggplot(plotdat, aes(reorder(Model, -Estimate), Estimate,
                    color = ifelse(Model %like% "Unbalanced", "pink", "black"))) +
  geom_point(size = 2.5) + theme_bw(base_size = 21) + 
  coord_flip() + ylab("Estimated Effect of Privatization on Mortality") +
  xlab("") + geom_errorbar(data = plotdat, aes(ymin = Estimate - 1.96*`Std.Error`,
          ymax = Estimate + 1.96 * `Std.Error`), width = 0.2) + ylim(-1,1) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  ggtitle("Robustness Tests: ATT by Estimator ") + theme(legend.position = '')
ggsave("Figures/Figure2_att_plot.png", width = 12, height = 8, units = "in", dpi = 300)

# Figure 1
dplot = data.frame(jail_long[TreatmentStatus != "Always" &
                      UnitExitsTreatment == 0 & AlwaysMissingOrAlwaysTreated == 0 &
                      AlwaysTreatedWhenDVNotMissing == 0
                      ])
# drop high missings in dv & !(id %in% jail_long[, list(missingdeathrate = sum(is.na(death_rate))), by = list(id)][missingdeathrate>0]$id)
panelView(death_rate ~ private_provider,
          data = dplot, index = c("id", "year"), 
          main = "Rollout of Jail Health Care Privatization (Treated = Privatized)",
          pre.post = T, by.timing = TRUE, xlab="", ylab="") +
  theme_dark(base_size = 20) + labs(fill = " ", color = " ") +
  guides(fill = guide_legend(ncol=4, keywidth=0.4, keyheight=0.4, default.unit="inch")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        axis.ticks.y=element_blank(), panel.background = element_rect(fill = "gray", color = "gray"))
ggsave("Figures/Figure1_panelview_includesmissings_noreversion_noalwaystreated.png",
       width = 12, height = 8, units = "in", dpi = 300)

## Appendix Table A1
esttable(m_fe, m_fe_weight, m_fe_weight_log, m_fepois_offset,
         se = "twoway")

## Robustness: overall death rate [treated = public] (recall that variable names are for treated = private)
attgt <- att_gt(yname = "death_rate", tname = "year", idname = "id",
      gname = "first.treat.public", allow_unbalanced_panel = TRUE,
      panel = T, cluster = "id",xformla = ~1,
      data = jail_long[(TreatmentStatus == "Always") |
    (TreatmentStatus == "Switch" &
    (first.treat < first.treat.public) & (last.treat < last.treat.public))]
)

## group composition
### 229 always private
length(unique(attgt$DIDparams$data[attgt$DIDparams$data$TreatmentStatus=="Always",]$id))
### 17 switch to public, no reversion
length(unique(attgt$DIDparams$data[attgt$DIDparams$data$TreatmentStatus=="Switch",]$id))
# de-privatization GS: 0.72 (0.33) [0.08,1.36]
summary(aggte(attgt, type = "group", na.rm=T))

# de-privatization dynamic event
mw.dyn <- aggte(attgt, type = "dynamic", na.rm = T)

#### de-privatization event study plot
ggdid(mw.dyn, ylim = c(-4,4)) + theme_classic(base_size = 22) + labs(color = "Treated") +
  geom_vline(xintercept = -0.5, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size=2) + geom_errorbar(width = 0.9) + scale_color_grey() +
  theme(legend.position = "", plot.subtitle = element_text(size=15, color = "grey", face = "italic"))  +
  xlab("Years Pre/Post Health Care De-Privatization") +
  ylab("ATT (Deaths per 1,000 daily innmates)") +
  ggtitle("Overall Jail Mortality", subtitle = "Average Effect of De-Privatization by Length of Exposure")
ggsave("Figures/dynamic_eventstudy_overall_death_plot_deprivatization.png",
       width = 12, height = 8, units = "in", dpi = 300)

## analysis 1b: overall death rate --> balanced panel
attgt <- att_gt(yname = "death_rate", tname = "year",
      idname = "id", gname = "first.treat",
      allow_unbalanced_panel = FALSE, xformla = ~1,
    data = jail_long[UnitExitsTreatment==0 &
  TreatmentStatus != "Always" & AlwaysTreatedWhenDVNotMissing == 0],
)

# sunab estimates by state
holder_dat = data.table(state = character(), coef = numeric(), se = numeric())

for(ST in unique(jail_long$state)) {
  if(!BBmisc::is.error(try(feols(death_rate ~ sunab(first.treat,year) |
      id + year, data = jail_long[state == ST]), silent=T))) {
    att_state = summary(feols(death_rate ~ sunab(first.treat,year) |
      id + year, data = jail_long[state == ST], se = "cluster"),
      agg = "ATT")
    # save
    holder_dat = rbind(holder_dat,
    data.table(
      state=ST, coef=coef(att_state), se=se(att_state))
    ) 
  }
  else {
    print(ST)
  }

overall = summary(feols(death_rate ~ sunab(first.treat,year) |
id + year, data = jail_long, se = "cluster"), agg = "ATT")
  holder_dat = rbind(holder_dat,
    data.table(
    state="Overall", coef=coef(overall), se=se(overall)
  ))
}

#### FIGURE 4
ggplot(holder_dat, aes(reorder(state, -coef), coef)) + geom_point() +
  coord_flip() + geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = coef+1.96*se, ymax = coef-1.96*se)) +
  ggtitle("Estimated ATT, by state", subtitle = "via Sun & Abraham") +
  theme_bw(base_size = 17) +
  xlab("") + ylab("Estimate of Privatization on Mortality")
ggsave("Figures/Figure4_att_bystate_sunab.png", width=12,height=8,units="in")

##### heterogeneity analysis #3 --> chain providers
### large private: never treated and privatized to large ONLY
#### private health care companies that are non-large are omitted
attgt_largeprivate <- att_gt(yname = "death_rate",
                        tname = "year",
                        idname = "id",
                        gname = "first.treat.large",
                        allow_unbalanced_panel = T,
                        xformla = ~1,
                        data = jail_long[UnitExitsTreatment==0 &
                                           TreatmentStatus != "Always" & AlwaysTreatedWhenDVNotMissing == 0 &
                                           !(first.treat != 0 & first.treat.large==0)])
gs_largeprivate = aggte(attgt_largeprivate, type = "group")

### small private: never treated and privatized to non-large ONLY
#### private health care companies that are large are omitted
attgt_nochain <- att_gt(yname = "death_rate", tname = "year", 
      idname = "id", gname = "first.treat",
      allow_unbalanced_panel = T, xformla = ~1,
      data = jail_long[UnitExitsTreatment==0 &
  TreatmentStatus != "Always" & AlwaysTreatedWhenDVNotMissing == 0 &
  first.treat.large==0])

gs_nochain = aggte(attgt_nochain, type = "group")

# difference btw treatment effects --> p-val for difference
## (ATT1 - ATT2) / sqrt(se1**2 + se2**2)
pnorm((gs_nochain$overall.att - gs_largeprivate$overall.att) / sqrt(gs_nochain$overall.se**2 + gs_largeprivate$overall.se**2))

#### FIGURE 5
## plot the difference btw chain/no chain coefficients
plotdat_chain = data.table(coefs = 
                             c(gs_largeprivate$overall.att, gs_nochain$overall.att),
                           ses = c(gs_largeprivate$overall.se, gs_nochain$overall.se) ,
                           names = c("Larger providers", "Smaller providers"))
ggplot(plotdat_chain, aes(names, coefs)) + ylim(-1.5,1) +
  geom_hline(yintercept = 0, linetype = "dotted") + 
  ggtitle("Subgroup Analyses: Chain vs Smaller Private Providers") +
  geom_point(size=3) + xlab("") + ylab("Estimate of Privatization on Mortality") + theme_classic(base_size = 22) +
  geom_errorbar(aes(ymin=coefs-1.96*ses, ymax=coefs+1.96*ses), width = 0.1) 
ggsave("Figures/Figure5_chain-heterogeneity-analysis.png", width=12,height=8,units="in")

## number of treated facilities across category
length(unique(attgt_nochain$DIDparams$data[attgt_nochain$DIDparams$data$TreatmentStatus=="Switch",]$id)) # 46 if unbalanced # 37 if balance panel
length(unique(attgt_largeprivate$DIDparams$data[attgt_largeprivate$DIDparams$data$TreatmentStatus=="Switch",]$id)) # 50 if unbalanced panel # 44 if balance panel
length(unique(attgt$DIDparams$data[attgt$DIDparams$data$TreatmentStatus=="Switch",]$id)) # 96 if unbalanced # 81 if balanced (as expected arithmetically)

#### NOT IN PAPER
jail_long[state == "DC"]$state = "District of Columbia" 
ggplot(jail_long[, list(percentage_treated = mean(na.omit(private_provider))) , by = list(state,year)],
       aes(year, percentage_treated*100)) +
  facet_geo(~state) + scale_x_continuous(breaks = seq(2008,2019,4)) +
  geom_point() + geom_line() + ylim(0,100) + ggtitle("Adoption of Jail Privatization, by State (2008-2019)") +
  xlab("") + ylab("Share of Jails with Privatized Health Care (%)") + theme_classic()

ggsave("Figures/adoption_state.png", width = 12, height = 8, units = "in", dpi = 300)

# plot: trends in death rates by status
ggplot(jail_long[average_daily_population>750, list(weighted_average_deathrate = weighted.mean(death_rate, average_daily_population, na.rm = T)), by = list(year, TreatmentStatus)],
       aes(factor(year), weighted_average_deathrate, color = TreatmentStatus, group = TreatmentStatus)) +
  geom_point() + geom_line() + theme_classic() + xlab("")

ggplot(jail_long[, list(average_deathrate = mean(death_rate, na.rm = T)), by = list(year, TreatmentStatus)],
       aes(factor(year), average_deathrate, color = TreatmentStatus, group = TreatmentStatus)) +
  geom_point() + geom_line() + theme_classic() + xlab("")

ggplot(jail_long[, list(percentage_treated = mean(na.omit(private_provider))) , by = list(year)],
       aes(year, percentage_treated*100)) +
  scale_x_continuous(breaks = seq(2008,2019,1)) +
  #ylim(0,100) +
  geom_point() + geom_line() + ggtitle("Adoption of Jail Privatization") +
  xlab("") + ylab("Share of Jails with Privatized Health Care (%)") + theme_classic()
ggsave("Figures/adoption_year.png", width = 12, height = 8, units = "in", dpi = 300)

## panel plot of de-privatization
dplot =  data.frame(jail_long[(TreatmentStatus == "Always") |
                                (TreatmentStatus == "Switch" &
                                   (first.treat < first.treat.public) & (last.treat < last.treat.public))])
panelView(death_rate ~ public_provider,
          data = dplot, index = c("id", "year"),
          main = "Jail De-Privatization",
          pre.post = T, by.timing = TRUE, xlab="", ylab="") +
  theme_dark(base_size = 20) + labs(fill = " ", color = " ") +
  guides(fill = guide_legend(ncol=4, keywidth=0.4, keyheight=0.4, default.unit="inch")) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.spacing.x = unit(0.5, 'cm'),
        axis.ticks.y=element_blank(), panel.background = element_rect(fill = "gray", color = "gray"))
ggsave("Figures/panelview_privatetopublic.png",
       width = 12, height = 8, units = "in", dpi = 300)
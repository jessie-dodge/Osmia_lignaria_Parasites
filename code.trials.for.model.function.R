glm4<- glm(nTstanCells ~ Ann_ppt_mm, data = zo.data.or, family = poisson())

formulas <- list(nTstanCells ~ Ann_ppt_mm, nTstanCells ~ Ann_tmax_C)
models <- list(glm(family = poisson()), glm(family = poisson()) )

lapply(models, function(x) lapply(formulas, function(y) x(y, data = mtcars)))

d <- crossing(formula = c(nTstanCells ~ Ann_ppt_mm, nTstanCells ~ disp + wt),
              model = list("glm", "glm")) %>% 
  
  mutate(result = pmap(.l = list(model, formula),
                       .f = function(m, f) do.call(m, args = list(formula = f, data = substitute(mtcars)))))


vars <-c("Ann_ppt_mm", "Ann_tmax_C")
formula<- paste("nTstanCells ~", vars)
fitted.models <- lapply(formulas, glm, data = zo.data.or, family = poisson())
sapply(colnames(zo.data.or), fitted.models, nTstanCells, zo.data.or)
summary(fitted.models)


pvalues  <- vector(mode = "list")
singlevar  <- function(vars, y, df){
  model <- as.formula(paste0("nTstanCells ~ ", vars))
  pvalues[var] <- coef(summary(glm(model, family = "poisson", data = zo.data.or)))[var,4]
}
sapply(colnames(zo.data.or), singlevar, nTstanCells, zo.data.or)


# linear models
# predictor variables:
#"Habitat", "Year", "HLI", "Ann_ppt_mm", "Ann_tmin_C", "Ann_tmean_C", "Ann_tmax_C", "bi_ppt_mm", "bi_tmin_C", "bi_tmean_C", "bi_tmax_C", "qrt_ppt_mm", "qrt_tmin_C", "qrt_tmean_C", "qrt_tmax_C", "DD_accumulated", "stemdiv", "stemrich", "stemabun", "meanTreeCC", "TotMean_BA", "Dead_BA_ha", "Live_BA_ha", "EVT_abund", "EVT_H", "EVT_rich", "EVT_J", "Barren", "Coniferous_Forest", "Crops","Deciduous_Forest", "Rangeland", "WUI" 

label <- "oligRatio"
features <- setdiff(names(zo.data), label)
generate_formula <- function(feature) sprintf("%s ~ %s", label, feature) %>% as.formula
mods<-features %>% 
  map(~ lm(generate_formula(.x), data = zo.data)) 
# this works, but I would like it to look nicer
do.call(cbind, lapply(mods, function(z) summary(z)$coefficients["(Intercept)", ]))

nm1<- names(pvars)
oligReg <- vector('list', length(nm1))
for(i in seq_along(oligReg)) {
  oligReg[[i]] <- lm(as.formula(paste0("oligRatio ~ ", 
                                           nm1[i])), data = zo.data)
}
oligReg[[3]]


odata<- zo.data[,c(17, 2:4, 21:50)]
odata %>%
  gather(key = group, 
         value = measurement,
         -oligRatio) %>% 
  group_by(group) %>% 
  nest() %>%
  mutate(model = map(data, ~lm(oligRatio ~ measurement, data = .))) %>% 
  unnest(model %>% map(glance))

y= "oligRatio"
x= colnames(pvars)
expand.grid(y=y, x=x) %>%
  mutate(formula = paste(y,"~",x)) %>%
  group_by(formula) %>%
  #mutate(r_sq = summary(lm(formula, data=odata))$r.squared) %>% # works, but I want entire summary
  mutate(pval = summary(lm(formula, data=odata))$p.values) %>%
  ungroup()


df_long <- odata %>%
  gather(Variable, Value, Habitat:WUI) %>%
  group_by(Variable)

lms <- df_long %>% do(lm = lm(oligRatio ~ Value, data = .))
lms_list <- setNames(lms$lm, lms$Variable)

do.call(cbind, lapply(lms_list, function(z) summary(z)$coefficients["(Intercept)", ]))
do.call(cbind, lapply(lms_list, function(z) summary(z)$coefficients))

ondata %>% 
  gather(group, Value, Habitat:WUI) %>%
  group_by(group) %>% 
  #do(lm = lm(oligRatio ~ Value, data = .)) %>%
  #setNames(lms$lm, lms$Variable) %>% 
  mutate(group = as.factor(group)) %>% 
  group_by(group) %>% 
  group_split() %>% 
  map_dfr(.f = function(df) {
    lm(nOligGrowth ~ Value, data = df) %>%
      tidy() %>% # first output 
      #glance() %>% # second output
      add_column(group = unique(df$group), .before=1)
  })

onform<-paste("nOligGrowth ~", xvars)
ondata %>% 
  gather(Variable, Value, Habitat:WUI) %>%
  mutate(group = as.factor(Variable)) %>% 
  group_by(group) %>% 
  group_split() %>% 
  map_dfr(.f = function(df) {
    lm(onform, data = df) %>%
      tidy() })

# define a function to get coefficients from linear regression
do_lm <- function(var){ # var is the name of the column
  res <- lm(as.formula(paste0("nOligGrowth~", var)), data = ondata) # compute linear regression
  coefs <- c(intercept = res$coefficient[2], slope = res$coefficient[1]) # get coefficients
  return(coefs)
}

t(sapply(colnames(ondata), do_lm))

# t transposes the result 
# sapply : applies on "var2" ... "var10" the function do_lm

x = names(ondata[,-1])
out <- unlist(lapply(1, function(n) combn(x, 1, FUN=function(row) paste0("nOligGrowth ~ ", paste0(row, collapse = "+")))))
out

#To have the regression coefficients
tmp1 = bind_rows(lapply(out, function(frml) {
  a = tidy(lm(frml, data=ondata))
  a$frml = frml
  return(a)
}))

head(tmp1)

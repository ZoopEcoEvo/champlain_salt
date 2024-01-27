TITLE HERE
================
2024-01-27

- [Survival Analyses](#survival-analyses)
- [CTmax Data](#ctmax-data)

## Survival Analyses

``` r
ggplot(daily_prop_data, aes(x = treatment, y = prop_surv, colour = factor(exp_day))) + 
  facet_wrap(salt~.) + 
  geom_hline(yintercept = 0.5,
             colour = "grey", 
             linetype = "dashed") + 
  geom_point(size = 3) + 
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE,
              linewidth = 2) + 
  scale_colour_brewer(type = "seq", palette = 9) + 
  labs(x = "Salinity (mg/L)",
       y = "Proportion Surviving",
       colour = "Day") + 
  theme_matt()
```

<img src="../Figures/report/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

``` r
surv_obj = Surv(surv_data$hour, surv_data$ind_surv)
surv_fit = survfit2(Surv(hour, ind_surv) ~ treatment + salt, data = surv_data)

#summary(surv_fit_2)

ggsurvplot_facet(surv_fit, 
                 data = surv_data,
                 facet.by = "salt",
                 conf.int=T, pval=F, risk.table=F, 
                 conf.int.alpha = 0.1,
                 size = 2,
                 palette = "YlOrRd",
                 legend.title="Salt Treatment")
```

<img src="../Figures/report/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
cox.model = coxph(Surv(hour, ind_surv) ~ treatment + replicate + salt, data = surv_data)

cox.model
## Call:
## coxph(formula = Surv(hour, ind_surv) ~ treatment + replicate + 
##     salt, data = surv_data)
## 
##                   coef exp(coef)  se(coef)      z      p
## treatment    1.030e-03 1.001e+00 1.027e-04 10.029 <2e-16
## replicateB   1.986e-01 1.220e+00 2.485e-01  0.799  0.424
## saltRoadSalt 2.323e+01 1.228e+10 4.495e+03  0.005  0.996
## 
## Likelihood ratio test=297.8  on 3 df, p=< 2.2e-16
## n= 304, number of events= 68

#ggforest(cox.model, data = surv_data)
```

## CTmax Data

``` r
ctmax_data %>%  
  mutate("ID" = paste(experiment_date, "- Exp.", experiment)) %>% 
  ggplot(aes(x = treatment, y = ctmax, fill = ID)) +
  facet_wrap(salt~.) +
  geom_boxplot(width = 0.5,
               position = position_dodge(width = 0.7)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.7)) + 
  labs(x = "Treatment", 
       y = "CTmax (°C)") + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/report/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
ctmax_filtered = ctmax_data %>% 
  filter(!(experiment == 1 & experiment_date == "1/20/24"))

knitr::kable(ctmax_filtered %>% 
  group_by(salt, treatment) %>%  
  count())
```

| salt         | treatment |   n |
|:-------------|:----------|----:|
| InstantOcean | control   |  10 |
| InstantOcean | salt      |  10 |
| RoadSalt     | control   |  10 |
| RoadSalt     | salt      |  10 |

``` r

salt.model = lmer(data = ctmax_filtered, 
                  ctmax ~ treatment * salt + (1 + treatment|experiment_date))

# salt.model = lm(data = ctmax_filtered,
#                 ctmax ~ treatment * salt)

salt.model
## Linear mixed model fit by REML ['lmerMod']
## Formula: ctmax ~ treatment * salt + (1 + treatment | experiment_date)
##    Data: ctmax_filtered
## REML criterion at convergence: 159.9542
## Random effects:
##  Groups          Name          Std.Dev.  Corr 
##  experiment_date (Intercept)   0.0002178      
##                  treatmentsalt 0.0001321 -1.00
##  Residual                      1.9634941      
## Number of obs: 40, groups:  experiment_date, 3
## Fixed Effects:
##                (Intercept)               treatmentsalt                saltRoadSalt  
##                    26.8673                      0.9291                      0.8049  
## treatmentsalt:saltRoadSalt  
##                    -2.7594  
## optimizer (nloptwrap) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings

car::Anova(salt.model, type = "III")
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: ctmax
##                    Chisq Df Pr(>Chisq)    
## (Intercept)    1872.3601  1    < 2e-16 ***
## treatment         1.1194  1    0.29004    
## salt              0.8401  1    0.35936    
## treatment:salt    4.9375  1    0.02628 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

salt.means = emmeans::emmeans(salt.model,
                pairwise ~ treatment | salt)

salt.means
## $emmeans
## salt = InstantOcean:
##  treatment emmean    SE   df lower.CL upper.CL
##  control     26.9 0.621 0.25 -29441.5  29495.2
##  salt        27.8 0.621 0.25 -29440.6  29496.1
## 
## salt = RoadSalt:
##  treatment emmean    SE   df lower.CL upper.CL
##  control     27.7 0.621 1.00     19.8     35.6
##  salt        25.8 0.621 1.00     18.0     33.7
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## 
## $contrasts
## salt = InstantOcean:
##  contrast       estimate    SE   df t.ratio p.value
##  control - salt   -0.929 0.878 0.25  -1.058  0.7036
## 
## salt = RoadSalt:
##  contrast       estimate    SE   df t.ratio p.value
##  control - salt    1.830 0.878 1.00   2.084  0.2848
## 
## Degrees-of-freedom method: kenward-roger
```

``` r
ggplot(ctmax_filtered, aes(x = treatment, y = ctmax)) +
  facet_wrap(salt~.) + 
  geom_boxplot(width = 0.5) +
  geom_point(size = 4) + 
  labs(x = "Treatment", 
       y = "CTmax (°C)") + 
  theme_matt()
```

<img src="../Figures/report/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

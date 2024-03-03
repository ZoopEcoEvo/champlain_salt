TITLE HERE
================
2024-03-03

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
  guides(colour = guide_legend(nrow = 1)) + 
  labs(x = "Salinity (mg/L)",
       y = "Proportion Surviving",
       colour = "Day") + 
  theme_matt_facets() + 
  theme(legend.position = "bottom")
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

## CTmax Data

``` r
ctmax_filtered = ctmax_data %>% 
  filter(!(experiment == 1 & experiment_date == "1/20/24")) %>% 
  filter(!(experiment_date == "2/3/24")) %>% 
  mutate("ID" = paste(salt, treatment, sep = " - ")) %>% 
  mutate(group = case_when(
    treatment == "control" ~ "control",
    treatment == "salt" ~ salt,
    treatment == "salt" ~ salt
  ))

knitr::kable(ctmax_filtered %>% 
               group_by(salt, treatment) %>%  
               count())
```

| salt         | treatment |   n |
|:-------------|:----------|----:|
| InstantOcean | control   |  10 |
| InstantOcean | salt      |  10 |
| RoadSalt     | control   |  20 |
| RoadSalt     | salt      |  20 |

``` r

salt.model = lmer(data = ctmax_filtered,
                  ctmax ~ treatment * salt + (1|experiment_date))

# salt.model = lm(data = ctmax_filtered,
#                 ctmax ~ treatment * salt)

car::Anova(salt.model, type = "III")
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: ctmax
##                   Chisq Df Pr(>Chisq)    
## (Intercept)    297.1865  1  < 2.2e-16 ***
## treatment        0.9650  1   0.325929    
## salt             0.0461  1   0.830013    
## treatment:salt  10.1837  1   0.001417 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

salt.means = emmeans::emmeans(salt.model,
                              pairwise ~ treatment | salt)

salt.means
## $emmeans
## salt = InstantOcean:
##  treatment emmean    SE   df lower.CL upper.CL
##  control     26.9 1.559 2.13     20.6     33.2
##  salt        27.8 1.559 2.13     21.5     34.1
## 
## salt = RoadSalt:
##  treatment emmean    SE   df lower.CL upper.CL
##  control     27.3 0.948 2.60     24.0     30.6
##  salt        24.5 0.948 2.60     21.2     27.8
## 
## Degrees-of-freedom method: kenward-roger 
## Confidence level used: 0.95 
## 
## $contrasts
## salt = InstantOcean:
##  contrast       estimate    SE df t.ratio p.value
##  control - salt   -0.929 0.946 54  -0.982  0.3303
## 
## salt = RoadSalt:
##  contrast       estimate    SE df t.ratio p.value
##  control - salt    2.767 0.669 54   4.138  0.0001
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

``` r
ggplot(ctmax_filtered, aes(x = treatment, y = ctmax, fill = treatment)) +
  facet_wrap(salt~.) + 
  geom_violin(draw_quantiles = c(0.25,0.75)) + 
  geom_point(size = 4) + 
  labs(x = "Treatment", 
       y = "CTmax (°C)") + 
  theme_matt()
```

<img src="../Figures/report/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
mean_diff = load(ctmax_filtered, 
                 x = ID, y = ctmax,
  idx = list(
    c("InstantOcean - control", "InstantOcean - salt"),
    c("RoadSalt - control", "RoadSalt - salt"))) %>%
  mean_diff()

dabest_plot(mean_diff)
```

<img src="../Figures/report/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

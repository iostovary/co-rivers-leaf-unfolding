manual_models <- manual_models_input %>%
  mutate(
    final_con_int = map(data, safely(~ lme(fixed = doy ~ poly(GDD_conservative, degree = 2) + poly(STAND_HEIGHT, degree=2) + TREE_ELEVATION,
                                random = ~1 | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit))),
    
    ref_imp_con_int = map(data, safely(~ lme(fixed = doy ~ poly(GDD_conservative, degree = 2),
                                             random = ~1 | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit))),
    
    final_con_slo = map(data, safely(~ lme(fixed = doy ~ poly(GDD_conservative, degree = 2) + poly(STAND_HEIGHT, degree=2) + TREE_ELEVATION,
                                random = ~1 + GDD_conservative | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit, control = lmeControl(opt = "nlminb", maxIter = 100, msMaxIter = 100, niterEM = 30)))),
    
    ref_imp_con_slo = map(data, safely(~ lme(fixed = doy ~ poly(GDD_conservative, degree = 2),
                                         random = ~1 + GDD_conservative | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit)))
    )

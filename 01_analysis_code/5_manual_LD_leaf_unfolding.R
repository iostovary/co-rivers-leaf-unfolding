manual_models <- manual_models_input %>%
  mutate(
        final_con_int = map(data, safely(~ lme(fixed = doy ~ GDD_conservative * CDD_conservative,
                                    random = ~1 | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit))),
        
        ref_imp_con_int = map(data, safely(~ lme(fixed = doy ~ poly(GDD_conservative, degree = 2) + CDD_conservative,
                                                 random = ~1 | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit))),
        
        final_con_slo = map(data, safely(~ lme(fixed = doy ~ poly(GDD_conservative, degree=2) * CDD_conservative,
                                    random = ~1 + GDD_conservative | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit))),
        
        ref_imp_con_slo = map(data, safely(~ lme(fixed = doy ~ poly(GDD_conservative, degree = 2) * CDD_conservative,
                                             random = ~1 + GDD_conservative | METEO_ID, data = ., method="ML", correlation = autoregression_simple, na.action = na.omit)))
      )

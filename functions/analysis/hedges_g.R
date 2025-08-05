hedges_correction <- function (df) {
  exp(lgamma(df/2) - log(sqrt(df/2)) - lgamma((df - 1)/2))
}

trial_correction <- function(r, k) {
  # r - between-subject / within-subject variability ratio
  # k - number of epochs
  sqrt(1 + 1 / (r^2 * k))
}

compute_min_detectable_g <- function(test_type, 
                                     sample_size, 
                                     groups, 
                                     conditions,
                                     sig.level = 0.05,
                                     power.level = 0.8) {
  
  # 0) Early exit for tests we don’t handle
  if (test_type %in% c("unknown",
                       "none",
                       "Classification",
                       "Non-parametric comparison",
                       "F-test")) {
    return(NA_real_)
  }
  
  # 1) Input‐level checks
  if (! test_type %in% c("t-test","ANOVA","Correlation","Regression")) {
    stop(sprintf("`test_type` must be one of t-test/ANOVA/Correlation/Regression, got: %s",
                 test_type))
  }
  if (is.na(sample_size) || sample_size <= 0 || !is.numeric(sample_size) ) {
    stop(sprintf("`sample_size` must be > 0 and non-NA, got: %s", sample_size))
  }
  
  if (test_type == "t-test") {
    if (!groups %in% c(1, 2) && groups > 2) {
      if (!is.na(conditions) && conditions == 1) {
        groups <- 2
      } else {
        stop(sprintf(
          "Ambiguous 'groups' for t-test: got %s groups and %s conditions. Cannot infer two-sample setup.",
          groups, conditions
        ))
      }
    }
    # final sanity check
    if (! groups %in% c(1, 2)) {
      stop(sprintf("For t-test, 'groups' must be 1 or 2 (after adjustment), got: %s", groups))
    }
  }
  
  # 2. Determine df for J:
  df <- switch(test_type,
               # one-sample: groups=1  → df = N–1
               # two-sample: groups=2  → df = N–2
               "t-test"     = if (groups==1) sample_size - 1 else sample_size - 2,
               # ANOVA: within-cells df = N – (# cells)
               "ANOVA"      = {
                 # decide cells
                 if (groups>1 && conditions>1) {
                   cells <- groups * conditions
                 } else if (groups>1) {
                   cells <- groups
                 } else if (conditions>1) {
                   cells <- conditions
                 } else {
                   return(NA_real_)
                 }
                 sample_size - cells
               },
               # correlation and regression both use df = N–2
               "Correlation" = sample_size - 2,
               "Regression"  = sample_size - 2
  )
  if (df < 1) return(NA_real_)
  
  # 3. Exact Hedges' J correction
  J <- hedges_correction(df)
  
  # 4. Compute minimal-detectable raw d
  raw_d <- switch(test_type,
                  
                  # a) one- or two-sample t-test:
                  "t-test" = {
                    if (groups == 1) {
                      # one-sample
                      out <- pwr.t.test(n           = sample_size,
                                        d           = NULL,
                                        sig.level   = sig.level,
                                        power       = power.level,
                                        type        = "one.sample",
                                        alternative = "two.sided")
                    } else {
                      # two-sample, equal ns
                      out <- pwr.t.test(n           = sample_size/2,
                                        d           = NULL,
                                        sig.level   = sig.level,
                                        power       = power.level,
                                        type        = "two.sample",
                                        alternative = "two.sided")
                    }
                    out$d
                  },
                  
                  # b) ANOVA → f → d = 2f
                  "ANOVA" = {
                    # figure out number of cells
                    cells <- if (groups>1 && conditions>1) {
                      groups * conditions
                    } else if (groups>1) {
                      groups
                    } else {
                      conditions
                    }
                    out <- pwr.anova.test(k         = cells,
                                          n         = sample_size / groups,
                                          sig.level = sig.level,
                                          power     = power.level)
                    2 * out$f
                  },
                  
                  # c) correlation → r → d = 2r / sqrt(1 - r^2)
                  "Correlation" = {
                    out <- pwr.r.test(n         = sample_size,
                                      sig.level = sig.level,
                                      power     = power.level)
                    r <- out$r
                    2 * r / sqrt(1 - r^2)
                  },
                  
                  # d) regression (1 predictor): f2→f→d = 2f
                  "Regression" = {
                    out <- pwr.f2.test(u         = 1,
                                       v         = sample_size - 2,
                                       sig.level = sig.level,
                                       power     = power.level)
                    f <- sqrt(out$f2)
                    2 * f
                  }
  )
  
  # 5. Apply Hedges' g correction
  g <- raw_d * J
  return(g)
}


compute_effect_columns <- function(df, 
                                   test_type_col, 
                                   sample_size_col, 
                                   groups_col, 
                                   conditions_col,
                                   sig.level = 0.05,
                                   power.level = 0.8) {
  df %>%
    rowwise() %>%
    mutate(
      hedges_g = compute_min_detectable_g(
        test_type   = {{ test_type_col }},
        sample_size = as.numeric({{ sample_size_col }}),
        groups      = as.numeric({{ groups_col }}),
        conditions  = as.numeric({{ conditions_col }}),
        sig.level   = sig.level,
        power.level = power.level
      )
    ) %>%
    ungroup() %>%
    select(hedges_g)
}

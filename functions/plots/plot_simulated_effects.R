plot_simulated_effects <- function(
    d_type      = c('d', 'g'), # Cohens d or Hedges g
    d_test      = NULL, # provide effect size(s) to be adjusted. If null, we compute the minimum detectable d for a one-sample t test, and then adjust it
    mu_test     = NULL, # instead of Cohens d(s) to be adjusted, can provide mean expected effect(s) mu
    Ns, # number of subjects. integer or array
    ks, # number of epochs integer or array
    sigma_ratio = NULL, # either provide ratio or sigmas separately
    sigma_s     = NULL,
    sigma_t     = NULL,
    alpha       = 0.05,
    power       = 0.80,
    plot_type   = c('pure', 'marginal') # 'pure' outputs raw effect sizes, 'marginal' - difference to assymptotic effect size
) {
  # ----‑‑‑‑‑‑‑‑‑‑ setup & validity checks -------------------------------------------------
  
  d_type    <- match.arg(d_type)
  plot_type <- match.arg(plot_type)
  
  if (!is.null(mu_test) && !is.null(d_test)) {
    stop("Specify *either* mu_test *or* d_test — not both.")
  }
  
  # mu_test needs both SDs to convert to d
  if (!is.null(mu_test) && (is.null(sigma_s) || is.null(sigma_t))) {
    stop("When supplying mu_test you must also provide sigma_s and sigma_t.")
  }
  
  # derive r (σ_s / σ_t) ---------------------------------------------------------------
  if (!is.null(sigma_s) && !is.null(sigma_t)) {
    # explicit SDs supplied — r is fixed (vectorised if sigma_s|sigma_t are)
    r_vals <- sigma_s / sigma_t
  } else if (!is.null(sigma_ratio) && is.null(mu_test)) {
    # generic ratio supplied (only allowed when mu_test absent)
    r_vals <- sigma_ratio
  } else if (is.null(mu_test)) {
    stop("Provide either sigma_ratio or both sigma_s & sigma_t (or supply mu_test).")
  }
  
  # ----‑‑‑‑‑‑‑‑‑‑ build the design grid -------------------------------------------------
  # We create an expanded grid that includes every combination of:
  #   * N (sample size)
  #   * k (epochs per subject)
  #   * r (variance ratio)
  #   * effect‑size parameter (either μ or d)
  
  # Case 1: raw‑unit effects provided ----------------------------------------------------
  if (!is.null(mu_test)) {
    grid <- expand_grid(N = Ns, k = ks, r = r_vals, mu = mu_test)
    
    grid <- grid %>% mutate(
      sigma_test  = sqrt(sigma_s^2 + sigma_t^2 / k), # SD of the mean diff per cell
      d_test      = mu / sigma_test,                 # convert μ to d
      df          = N - 1L,
      facet_label = paste0("μ = ", mu)             # label for faceting
    )
    
    # Case 2: standardised d values provided ---------------------------------------------
  } else if (!is.null(d_test)) {
    grid <- expand_grid(N = Ns, k = ks, r = r_vals, d_fix = d_test)
    
    grid <- grid %>% mutate(
      d_test      = d_fix,  # just rename for consistency
      df          = N - 1L,
      facet_label = paste0("d = ", d_fix)
    )
    
    # Case 3: neither provided —back‑calculate d for desired power -----------------------
  } else {
    grid <- expand_grid(N = Ns, k = ks, r = r_vals)
    grid <- grid %>% rowwise() %>% mutate(
      df     = N - 1L,
      d_test = pwr.t.test(
        n         = N,
        sig.level = alpha,
        power     = power,
        type      = 'one.sample'
      )$d,
      facet_label = paste0(d_type, " (", power * 100, "% power)")
    ) %>% ungroup()
  }
  
  # ----‑‑‑‑‑‑‑‑‑‑ compute weighted effect size -----------------------------------------
  grid <- grid %>% mutate(
    d_weight = d_test * sqrt(1 + 1 / (r^2 * k))
  )
  
  # Hedges' g correction if needed ------------------------------------------------------
  if (d_type == 'g') {
    grid <- grid %>% mutate(
      J       = exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2)),
      eff_val = d_weight * J
    )
  } else {
    grid <- grid %>% mutate(eff_val = d_weight)
  }
  
  # ----‑‑‑‑‑‑‑‑‑‑ choose the colour scale value ----------------------------------------
  if (plot_type == 'marginal') {
    grid <- grid %>% mutate(
      asymptote = d_test * r,
      fill_val  = eff_val - asymptote
    )
    legend_title <- expression(Delta ~ effect)
  } else {
    grid$fill_val <- grid$eff_val
    legend_title  <- expression(Effect ~ size)
  }
  
  # ----‑‑‑‑‑‑‑‑‑‑ plot ------------------------------------------------------------------
  p <- ggplot(grid, aes(x = N, y = k, fill = fill_val)) +
    geom_tile() +
    facet_grid(r ~ facet_label, labeller = labeller(
      r = function(x) paste0("σs/σt = ", x)
    )) +
    scale_fill_viridis(name = legend_title, option = 'A') +
    labs(
      x     = 'Number of participants (N)',
      y     = 'Epochs per participant (k)',
      title = paste0('Minimal detectable ', d_type)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = '#EEEEEE'),
      strip.text       = element_text(face = 'bold')
    )
  
  return(p)
}

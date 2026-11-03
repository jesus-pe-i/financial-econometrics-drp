# Setup ----

library(quantmod)
library(ggplot2)
library(tsdistributions)
library(plotly)
library(mixtools)

# Prices and returns ----

tickers <- c(
  "S&P 500" = "SPY",
  "Bitcoin" = "BTC-USD",
  "NVIDIA" = "NVDA",
  "CarGurus" = "CARG",
  "Rigel Pharma" = "RIGL",
  "Alibaba" = "BABA"
)

prices <- lapply(tickers, function(s) {
  x <- getSymbols(s, src = "yahoo", from = "2018-01-02",
                  to = Sys.Date(), auto.assign = FALSE)
  na.omit(Ad(x))
})

price_df <- do.call(rbind, lapply(seq_along(prices), function(i) {
  x <- prices[[i]]
  data.frame(
    Date = as.Date(index(x)),
    Price = 100 * as.numeric(x) / as.numeric(first(x)),
    Asset = names(prices)[i]
  )
}))

return_df <- do.call(rbind, lapply(seq_along(prices), function(i) {
  r <- na.omit(100 * diff(log(prices[[i]])))
  data.frame(
    Date = as.Date(index(r)),
    Return = as.numeric(r),
    Asset = names(prices)[i]
  )
}))

price_df$Asset <- factor(price_df$Asset, levels = names(tickers))
return_df$Asset <- factor(return_df$Asset, levels = names(tickers))

p1 <- ggplot(price_df, aes(Date, Price)) +
  geom_line(color = "#0100CA", linewidth = 0.45) +
  facet_wrap(~ Asset, scales = "free_y", ncol = 2) +
  labs(
    title = "Financial prices",
    subtitle = "Adjusted prices indexed to 100 at the start of the sample",
    x = NULL, y = "Price index (start = 100)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p2 <- ggplot(return_df, aes(Date, Return)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_line(color = "#008C95", linewidth = 0.35) +
  facet_wrap(~ Asset, scales = "free_y", ncol = 2) +
  labs(
    title = "Financial returns",
    subtitle = "Daily log returns; independent vertical scales",
    x = NULL, y = "Log return (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p1)
print(p2)

dir.create("figures/01", recursive = TRUE, showWarnings = FALSE)
saveRDS(p1, "figures/01/prices.rds")
saveRDS(p2, "figures/01/returns.rds")


# Return distribution ----

returns <- split(return_df$Return, return_df$Asset)
distributions <- c(norm = "Normal", std = "Student-t", sstd = "Skew-t")

fits <- lapply(returns, function(x) {
  f <- lapply(names(distributions), function(d) {
    estimate(distribution_modelspec(x, d), use_hessian = FALSE)
  })
  setNames(f, names(distributions))
})

bic_df <- do.call(rbind, lapply(names(fits), function(a) {
  data.frame(
    Asset = a,
    Distribution = unname(distributions),
    BIC = sapply(fits[[a]], function(f) {
      -2 * as.numeric(logLik(f)) + length(coef(f)) * log(length(returns[[a]]))
    }),
    Offset = c(1.3, 2.7, 4.1)
  )
}))

bic_df$Distribution <- factor(bic_df$Distribution, levels = distributions)
bic_df$Asset <- factor(bic_df$Asset, levels = names(tickers))
bic_df$Label <- paste0(bic_df$Distribution, " BIC: ", round(bic_df$BIC))

print(round(xtabs(BIC ~ Asset + Distribution, bic_df), 1))
print(sapply(fits, function(f) sapply(f, function(z) z$solver_out$convergence)))

density_df <- do.call(rbind, lapply(names(fits), function(a) {
  x <- returns[[a]]
  z <- seq(min(x), max(x), length.out = 500)
  
  do.call(rbind, lapply(names(distributions), function(d) {
    data.frame(
      Asset = a,
      Return = z,
      Density = do.call(ddist, c(
        list(distribution = d, x = z),
        as.list(coef(fits[[a]][[d]]))
      )),
      Distribution = unname(distributions[d])
    )
  }))
}))


qq_df <- do.call(rbind, lapply(names(fits), function(a) {
  x <- sort(returns[[a]])
  q <- ppoints(length(x))
  
  do.call(rbind, lapply(names(distributions), function(d) {
    data.frame(
      Asset = a,
      Empirical = x,
      Theoretical = do.call(qdist, c(
        list(distribution = d, p = q),
        as.list(coef(fits[[a]][[d]]))
      )),
      Distribution = unname(distributions[d])
    )
  }))
}))

density_df$Asset <- factor(density_df$Asset, levels = names(tickers))
qq_df$Asset <- factor(qq_df$Asset, levels = names(tickers))
density_df$Distribution <- factor(density_df$Distribution, levels = distributions)
qq_df$Distribution <- factor(qq_df$Distribution, levels = distributions)

fit_colors <- c(
  "Normal" = "#0100CA",
  "Student-t" = "#008C95",
  "Skew-t" = "#C45B40"
)

p3 <- ggplot(return_df, aes(Return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 70,
                 fill = "grey85", color = "white", linewidth = 0.15) +
  geom_line(data = density_df,
            aes(y = Density, color = Distribution), linewidth = 0.8) +
  geom_text(data = bic_df,
            aes(x = -Inf, y = Inf, label = Label,
                color = Distribution, vjust = Offset),
            inherit.aes = FALSE, hjust = -0.05, size = 2.7,
            show.legend = FALSE) +
  facet_wrap(~ Asset, scales = "free", ncol = 2) +
  scale_color_manual(values = fit_colors) +
  labs(
    title = "The distribution of financial returns",
    subtitle = "Empirical densities and maximum-likelihood fits; lower BIC is better",
    x = "Daily log return (%)", y = "Density", color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

p4 <- ggplot(qq_df, aes(Theoretical, Empirical, color = Distribution)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2,
              color = "grey50", linewidth = 0.4) +
  geom_line(lwd = 0.8) +
  facet_wrap(~ Asset, scales = "free", ncol = 2) +
  scale_color_manual(values = fit_colors) +
  labs(
    title = "How well do the fitted distributions capture the tails?",
    subtitle = "Q-Q comparisons; dashed line indicates perfect quantile agreement",
    x = "Theoretical quantile (%)", y = "Empirical quantile (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(p3)
print(p4)

saveRDS(p3, "figures/01/distributions.rds")
saveRDS(p4, "figures/01/qqplots.rds")


# Predictability and autocorrelation ----

lags <- 1:20

shared <- lapply(returns, function(r) {
  list(
    values = unname(round(as.numeric(r), 4)),
    ranks = unname(rank(abs(r), ties.method = "average"))
  )
})

writeLines(
  paste0(
    "window.drpLagData = ",
    jsonlite::toJSON(shared, auto_unbox = TRUE, digits = NA),
    ";"
  ),
  "figures/01/lag-series.js",
  useBytes = TRUE
)

shared_dir <- normalizePath(
  "figures/01",
  winslash = "/",
  mustWork = TRUE
)

stopifnot(file.exists(file.path(shared_dir, "lag-series.js")))

shared_dep <- htmltools::htmlDependency(
  name = "drp-lag-series",
  version = "1.0.0",
  src = c(file = shared_dir),
  script = "lag-series.js",
  all_files = FALSE
)

lag_scatter <- function(absolute = FALSE, pobs = FALSE) {
  mode <- if (pobs) "pobs" else if (absolute) "absolute" else "signed"
  
  limits <- lapply(returns, function(r) {
    if (pobs) return(c(0, 1))
    cap <- unname(quantile(abs(r), 0.99))
    if (absolute) c(0, cap) else c(-cap, cap)
  })
  
  color <- if (absolute || pobs) "rgba(0,140,149,0.22)" else
    "rgba(1,0,202,0.22)"
  
  xlab <- if (pobs) "Rank of |r(t-h)|" else
    if (absolute) "|Return at t-h| (%)" else "Return at t-h (%)"
  
  ylab <- if (pobs) "Rank of |r(t)|" else
    if (absolute) "|Return at t| (%)" else "Return at t (%)"
  
  p <- plotly::plot_ly()
  
  for (j in seq_along(returns)) {
    id <- if (j == 1) "" else as.character(j)
    
    p <- plotly::add_trace(
      p, x = NA_real_, y = NA_real_,
      type = "scattergl", mode = "markers",
      xaxis = paste0("x", id),
      yaxis = paste0("y", id),
      marker = list(color = color, size = 4),
      showlegend = FALSE, inherit = FALSE,
      hovertemplate = paste0(
        "Lagged: %{x:.2f}<br>",
        "Current: %{y:.2f}<extra></extra>"
      )
    )
  }
  
  axes <- list()
  annotations <- list()
  
  for (j in seq_along(returns)) {
    id <- if (j == 1) "" else as.character(j)
    col <- (j - 1) %% 3
    row <- (j - 1) %/% 3
    
    xd <- list(
      c(0.04, 0.31),
      c(0.365, 0.635),
      c(0.69, 0.96)
    )[[col + 1]]
    
    yd <- list(
      c(0.59, 0.94),
      c(0.12, 0.47)
    )[[row + 1]]
    
    axes[[paste0("xaxis", id)]] <- list(
      domain = xd, anchor = paste0("y", id),
      range = limits[[j]], fixedrange = TRUE,
      title = if (row == 1) xlab else "",
      gridcolor = "#E4E6F2",
      zerolinecolor = "#A0A5B5"
    )
    
    axes[[paste0("yaxis", id)]] <- list(
      domain = yd, anchor = paste0("x", id),
      range = limits[[j]], fixedrange = TRUE,
      title = if (col == 0) ylab else "",
      gridcolor = "#E4E6F2",
      zerolinecolor = "#A0A5B5"
    )
    
    annotations[[j]] <- list(
      x = mean(xd), y = max(yd) + 0.03,
      xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "bottom",
      text = paste0("<b>", names(returns)[j], "</b>"),
      showarrow = FALSE,
      font = list(size = 13, color = "#000000")
    )
  }
  
  steps <- lapply(lags, function(h) {
    list(
      method = "skip",
      args = list(),
      label = as.character(h)
    )
  })
  
  p <- do.call(plotly::layout, c(
    list(p), axes,
    list(
      title = list(
        text = if (pobs) "Rank dependence in absolute returns" else
          if (absolute) "Dependence in absolute returns" else
            "Dependence in returns",
        x = 0.5,
        font = list(size = 21, color = "#000000")
      ),
      annotations = annotations,
      sliders = list(list(
        active = 0,
        steps = steps,
        currentvalue = list(prefix = "Lag h = "),
        x = 0.08, len = 0.84,
        pad = list(t = 35)
      )),
      autosize = TRUE,
      height = 650,
      margin = list(l = 65, r = 65, t = 80, b = 110),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF"
    )
  ))
  
  p <- plotly::config(
    p, responsive = TRUE, displaylogo = FALSE
  )
  
  p$dependencies <- c(p$dependencies, list(shared_dep))
  
  p <- htmlwidgets::onRender(
    p,
    "
    function(el, x, data) {
      var shared = window.drpLagData;

      if (!shared) {
        console.error('Shared lag data could not be loaded.');
        return;
      }

      var prepared = data.assets.map(function(a) {
        var s = shared[a];

        if (data.mode === 'pobs') {
          var n = s.ranks.length;
          return s.ranks.map(function(v) {
            return v / (n + 1);
          });
        }

        if (data.mode === 'absolute') {
          return s.values.map(function(v) {
            return Math.abs(v);
          });
        }

        return s.values;
      });

      function updateLag(h) {
        var xs = [];
        var ys = [];

        for (var j = 0; j < prepared.length; j++) {
          var r = prepared[j];
          var lim = data.limits[j];
          var xj = [];
          var yj = [];

          for (var t = h; t < r.length; t++) {
            var previous = r[t - h];
            var current = r[t];

            if (
              previous >= lim[0] &&
              previous <= lim[1] &&
              current >= lim[0] &&
              current <= lim[1]
            ) {
              xj.push(previous);
              yj.push(current);
            }
          }

          xs.push(xj);
          ys.push(yj);
        }

        Plotly.restyle(
          el,
          {x: xs, y: ys},
          [0, 1, 2, 3, 4, 5]
        );
      }

      el.on('plotly_sliderchange', function(e) {
        updateLag(Number(e.step.label));
      });

      updateLag(1);
    }
    ",
    data = list(
      assets = names(returns),
      mode = mode,
      limits = unname(limits)
    )
  )
  
  p
}

p5 <- lag_scatter()
p6 <- lag_scatter(absolute = TRUE)

p5
p6

saveRDS(p5, "figures/01/return-dependence.rds")
saveRDS(p6, "figures/01/absolute-return-dependence.rds")

## Pseudo-observations ----

p7 <- lag_scatter(pobs = TRUE)
p7

saveRDS(p7, "figures/01/absolute-return-ranks.rds")


# Volatility clustering ----

set.seed(2026)

mixture_fits <- lapply(returns, function(x) {
  mixtools::normalmixEM(
    x, k = 2, arbmean = FALSE, arbvar = TRUE,
    lambda = c(0.8, 0.2), mu = median(x),
    sigma = c(sd(x) * 0.6, sd(x) * 2),
    epsilon = 1e-7, maxit = 1000
  )
})

mixture_summary <- do.call(rbind, lapply(names(mixture_fits), function(a) {
  f <- mixture_fits[[a]]
  data.frame(
    Asset = a,
    Mean = f$mu[1],
    SD_Low = f$sigma * f$scale[1],
    SD_High = f$sigma * f$scale[2],
    Weight_High = f$lambda[2],
    BIC = -2 * f$loglik + 4 * log(length(returns[[a]]))
  )
}))

print(mixture_summary, row.names = FALSE, digits = 3)

caps <- sapply(returns, function(x) {
  unname(quantile(abs(x), 0.999))
})

vol_df <- do.call(rbind, lapply(names(returns), function(a) {
  i <- return_df$Asset == a
  
  data.frame(
    Date = return_df$Date[i],
    Asset = a,
    Return = as.numeric(returns[[a]]),
    PHigh = mixture_fits[[a]]$posterior[, 2],
    Cap = caps[a]
  )
}))

vol_df$Asset <- factor(vol_df$Asset, levels = names(tickers))
vol_df$Magnitude <- pmin(abs(vol_df$Return), vol_df$Cap)

p8 <- ggplot(vol_df, aes(Date, Magnitude)) +
  geom_segment(
    aes(xend = Date, yend = 0),
    color = "#008C95", linewidth = 0.25
  ) +
  facet_wrap(~ Asset, scales = "free_y", ncol = 3) +
  labs(
    title = "Volatility clustering in financial markets",
    subtitle = "Absolute daily returns; capped at the 99.9th percentile for display",
    x = NULL, y = "Absolute return (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(color = "black"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p8)


## Original versus shuffled returns ----

assets <- names(returns)
states <- c("Original", "Shuffled")

mixture_data <- setNames(lapply(assets, function(a) {
  i <- return_df$Asset == a
  r <- as.numeric(returns[[a]])
  
  list(
    dates = as.character(return_df$Date[i]),
    r = unname(round(r, 4)),
    p = unname(round(mixture_fits[[a]]$posterior[, 2], 4)),
    perm = unname(sample.int(length(r)) - 1L)
  )
}), assets)

p9 <- plotly::plot_ly()

for (j in seq_along(assets)) {
  id <- if (j == 1) "" else as.character(j)
  
  p9 <- plotly::add_trace(
    p9,
    x = NA_character_, y = NA_real_,
    type = "scattergl", mode = "lines+markers",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    line = list(
      color = "rgba(150,160,175,0.35)",
      width = 0.5
    ),
    marker = list(
      color = 0.5,
      size = 4,
      opacity = 0.85,
      cmin = 0, cmax = 1,
      colorscale = list(
        c(0, "#C5CAD5"),
        c(0.5, "#008C95"),
        c(1, "#0100CA")
      ),
      showscale = j == 1,
      colorbar = list(
        title = list(text = "P(high)"),
        len = 0.42,
        x = 1.01,
        y = 0.75
      )
    ),
    customdata = NA_real_,
    hovertemplate = paste0(
      "Date: %{x}<br>",
      "Return: %{y:.2f}%<br>",
      "P(high variance): %{customdata:.3f}",
      "<extra></extra>"
    ),
    showlegend = FALSE,
    inherit = FALSE
  )
}

axes <- list()
annotations <- list()

for (j in seq_along(assets)) {
  a <- assets[j]
  id <- if (j == 1) "" else as.character(j)
  col <- (j - 1) %% 3
  row <- (j - 1) %/% 3
  
  xd <- list(
    c(0.08, 0.34),
    c(0.40, 0.66),
    c(0.72, 0.92)
  )[[col + 1]]
  
  yd <- list(
    c(0.68, 0.93),
    c(0.29, 0.54)
  )[[row + 1]]
  
  axes[[paste0("xaxis", id)]] <- list(
    domain = xd,
    anchor = paste0("y", id),
    type = "date",
    range = range(mixture_data[[a]]$dates),
    fixedrange = TRUE,
    title = if (row == 1) "Date" else "",
    gridcolor = "#E4E6F2"
  )
  
  axes[[paste0("yaxis", id)]] <- list(
    domain = yd,
    anchor = paste0("x", id),
    range = c(-caps[a], caps[a]),
    fixedrange = TRUE,
    title = if (col == 0) "Return (%)" else "",
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  )
  
  annotations[[j]] <- list(
    x = mean(xd),
    y = max(yd) + 0.025,
    xref = "paper",
    yref = "paper",
    xanchor = "center",
    yanchor = "bottom",
    text = paste0("<b>", a, "</b>"),
    showarrow = FALSE,
    font = list(size = 13, color = "#000000")
  )
}

buttons <- lapply(states, function(s) {
  list(
    label = s,
    method = "skip",
    args = list()
  )
})

p9 <- do.call(plotly::layout, c(
  list(p9), axes,
  list(
    title = list(
      text = "Gaussian mixture: original versus shuffled returns",
      x = 0.5,
      font = list(size = 21, color = "#000000")
    ),
    annotations = annotations,
    updatemenus = list(list(
      type = "buttons",
      direction = "right",
      buttons = buttons,
      active = 0,
      showactive = TRUE,
      x = 0.5,
      xanchor = "center",
      y = 0.055,
      yanchor = "middle"
    )),
    autosize = TRUE,
    height = 700,
    margin = list(l = 65, r = 90, t = 85, b = 70),
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor = "#FFFFFF"
  )
))

p9 <- plotly::config(
  p9,
  responsive = TRUE,
  displaylogo = FALSE
)

p9 <- htmlwidgets::onRender(
  p9,
  "
  function(el, x, data) {
    var indices = [0, 1, 2, 3, 4, 5];

    function updateState(shuffled) {
      var xs = [];
      var ys = [];
      var colors = [];

      data.assets.forEach(function(a) {
        var s = data.series[a];

        if (shuffled) {
          ys.push(s.perm.map(function(i) {
            return s.r[i];
          }));

          colors.push(s.perm.map(function(i) {
            return s.p[i];
          }));
        } else {
          ys.push(s.r);
          colors.push(s.p);
        }

        xs.push(s.dates);
      });

      Plotly.restyle(
        el,
        {
          x: xs,
          y: ys,
          'marker.color': colors,
          customdata: colors
        },
        indices
      );
    }

    el.on('plotly_buttonclicked', function(e) {
      updateState(e.button.label === 'Shuffled');
    });

    updateState(false);
  }
  ",
  data = list(
    assets = assets,
    series = mixture_data
  )
)

p9

saveRDS(p8, "figures/01/volatility-clustering.rds")
saveRDS(p9, "figures/01/mixture-regimes.rds")

## Gaussian mixture fit ----

hist_df <- do.call(rbind, lapply(names(returns), function(a) {
  data.frame(
    Asset = a,
    Return = as.numeric(returns[[a]])
  )
}))

hist_df$Asset <- factor(hist_df$Asset, levels = names(tickers))

curve_df <- do.call(rbind, lapply(names(returns), function(a) {
  x <- as.numeric(returns[[a]])
  f <- mixture_fits[[a]]
  
  norm_par <- if (exists("fits")) {
    as.list(coef(fits[[a]][["norm"]]))
  } else {
    list(mu = mean(x), sigma = stats::sd(x))
  }
  
  sd_mix <- f$sigma * f$scale
  grid <- seq(min(x), max(x), length.out = 600)
  
  data.frame(
    Asset = a,
    x = grid,
    Gaussian = dnorm(grid, mean = norm_par$mu, sd = norm_par$sigma),
    Mixture = f$lambda[1] * dnorm(grid, mean = f$mu[1], sd = sd_mix[1]) +
      f$lambda[2] * dnorm(grid, mean = f$mu[1], sd = sd_mix[2])
  )
}))

curve_df$Asset <- factor(curve_df$Asset, levels = names(tickers))

curve_long <- rbind(
  data.frame(
    Asset = curve_df$Asset,
    x = curve_df$x,
    Density = curve_df$Gaussian,
    Model = "Gaussian"
  ),
  data.frame(
    Asset = curve_df$Asset,
    x = curve_df$x,
    Density = curve_df$Mixture,
    Model = "Gaussian mixture"
  )
)

curve_long$Model <- factor(
  curve_long$Model,
  levels = c("Gaussian", "Gaussian mixture")
)

p9_A <- ggplot() +
  geom_histogram(
    data = hist_df,
    aes(Return, after_stat(density)),
    bins = 60,
    fill = "#C5CAD5",
    color = "white",
    linewidth = 0.2
  ) +
  geom_line(
    data = curve_long,
    aes(x, Density, color = Model),
    linewidth = 0.8
  ) +
  facet_wrap(~ Asset, scales = "free", ncol = 2) +
  scale_color_manual(
    values = c(
      "Gaussian" = "#0100CA",
      "Gaussian mixture" = "#008C95"
    )
  ) +
  labs(
    title = "Gaussian versus Gaussian mixture fits",
    subtitle = "A two-component Gaussian mixture often captures the sharp centre and heavier tails more flexibly",
    x = "Return (%)",
    y = "Density",
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(color = "black"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(p9_A)

saveRDS(p9_A, "figures/01/gaussian-vs-mixture.rds")



# Simulating financial returns ----

set.seed(2026)

n_sim <- 100
methods <- c("Normal", "Student-t", "Skew-t", "Gaussian mixture", "Empirical bootstrap")
dist_codes <- c("Normal" = "norm", "Student-t" = "std", "Skew-t" = "sstd")
assets <- names(returns)

sim_dir <- "figures/01/simulation-data"
dir.create(sim_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(
  'window.drpSimBase = new URL(".", document.currentScript.src).href;',
  file.path(sim_dir, "loader.js")
)

bands <- vector("list", length(assets))
ranges <- vector("list", length(assets))
series <- vector("list", length(assets))
sim_files <- vector("list", length(assets))

for (j in seq_along(assets)) {
  a <- assets[j]
  r <- as.numeric(returns[[a]])
  dates <- as.character(return_df$Date[return_df$Asset == a])
  n <- length(r)
  
  stopifnot(length(dates) == n)
  
  series[[j]] <- list(dates = dates, observed = round(r, 4))
  ranges[[j]] <- range(r)
  bands[[j]] <- vector("list", length(methods))
  sim_files[[j]] <- character(length(methods))
  
  for (k in seq_along(methods)) {
    m <- methods[k]
    
    if (m %in% names(dist_codes)) {
      d <- unname(dist_codes[m])
      pars <- as.list(coef(fits[[a]][[d]]))
      
      z <- do.call(
        tsdistributions::rdist,
        c(list(distribution = d, n = n * n_sim), pars)
      )
      
    } else if (m == "Gaussian mixture") {
      f <- mixture_fits[[a]]
      component <- sample.int(2, n * n_sim, replace = TRUE, prob = f$lambda)
      
      z <- rnorm(
        n * n_sim,
        mean = f$mu[1],
        sd = (f$sigma * f$scale)[component]
      )
      
    } else {
      z <- sample(r, n * n_sim, replace = TRUE)
    }
    
    bands[[j]][[k]] <- unname(quantile(z, c(0.025, 0.975)))
    sim_files[[j]][k] <- sprintf("sim-%02d-%02d.bin", j, k)
    
    con <- file(file.path(sim_dir, sim_files[[j]][k]), "wb")
    writeBin(as.numeric(z), con, size = 4, endian = "little")
    close(con)
  }
}

sim_dep <- htmltools::htmlDependency(
  name = "drp-simulation-data",
  version = "1.0.0",
  src = c(file = normalizePath(sim_dir, winslash = "/")),
  script = "loader.js",
  all_files = TRUE
)

p10 <- plotly::plot_ly()

p10 <- plotly::add_trace(
  p10,
  x = NA_character_, y = NA_real_,
  type = "scattergl", mode = "markers",
  marker = list(
    color = "rgba(110,120,140,0.2)",
    size = 2.5
  ),
  showlegend = FALSE,
  hoverinfo = "skip",
  inherit = FALSE
)

p10 <- plotly::add_trace(
  p10,
  x = NA_character_, y = NA_real_,
  type = "scattergl", mode = "markers",
  marker = list(
    color = "#0100CA",
    size = 4,
    opacity = 0.95
  ),
  showlegend = FALSE,
  inherit = FALSE,
  hovertemplate = "Date: %{x}<br>Observed return: %{y:.2f}%<extra></extra>"
)

asset_buttons <- lapply(assets, function(a) {
  list(label = a, method = "skip", args = list())
})

method_buttons <- lapply(methods, function(m) {
  list(label = m, method = "skip", args = list())
})

p10 <- plotly::layout(
  p10,
  title = list(
    text = "Can fitted distributions reproduce financial returns?",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    title = "Date",
    type = "date",
    range = as.character(range(return_df$Date)),
    rangeslider = list(visible = TRUE, thickness = 0.12),
    gridcolor = "#E4E6F2"
  ),
  yaxis = list(
    title = "Daily log return (%)",
    range = ranges[[1]],
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  shapes = list(list(
    type = "rect",
    xref = "paper", yref = "y",
    x0 = 0, x1 = 1,
    y0 = bands[[1]][[1]][1],
    y1 = bands[[1]][[1]][2],
    fillcolor = "rgba(0,140,149,0.15)",
    line = list(
      color = "rgba(0,140,149,0.65)",
      width = 1
    ),
    layer = "below"
  )),
  updatemenus = list(
    list(
      type = "dropdown",
      direction = "down",
      buttons = asset_buttons,
      active = 0,
      x = 0.04, xanchor = "left",
      y = 1.00, yanchor = "bottom",
      showactive = TRUE
    ),
    list(
      type = "buttons",
      direction = "right",
      buttons = method_buttons,
      active = 0,
      x = 0.5, xanchor = "center",
      y = -0.45, yanchor = "top",
      showactive = TRUE
    )
  ),
  annotations = list(
    list(
      x = 0.04, y = 1.13,
      xref = "paper", yref = "paper",
      text = "<b>Asset</b>",
      showarrow = FALSE,
      xanchor = "left"
    ),
    list(
      x = 0.5, y = -0.42,
      xref = "paper", yref = "paper",
      text = "<b>Simulation method</b>",
      showarrow = FALSE,
      xanchor = "center"
    )
  ),
  autosize = TRUE,
  height = 940,
  margin = list(l = 65, r = 35, t = 170, b = 280),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF",
  uirevision = "simulation"
)

p10 <- plotly::config(
  p10, responsive = TRUE, displaylogo = FALSE
)

p10$dependencies <- c(p10$dependencies, list(sim_dep))

p10 <- htmlwidgets::onRender(
  p10,
  "
  function(el, x, data) {
    var asset = 0;
    var method = 0;
    var request = 0;
    var cache = {};
    var dateCache = {};

    function updatePlot() {
      var j = asset;
      var k = method;
      var token = ++request;
      var filename = data.files[j][k];

      if (!cache[filename]) {
        cache[filename] = fetch(window.drpSimBase + filename)
          .then(function(response) {
            if (!response.ok) {
              throw new Error('Cannot load ' + filename);
            }
            return response.arrayBuffer();
          })
          .catch(function(error) {
            delete cache[filename];
            throw error;
          });
      }

      cache[filename].then(function(buffer) {
        if (token !== request) return;

        var s = data.series[j];
        var draws = new Float32Array(buffer);
        var n = s.dates.length;

        if (!dateCache[j]) {
          var dates = new Array(draws.length);

          for (var i = 0; i < draws.length; i++) {
            dates[i] = s.dates[i % n];
          }

          dateCache[j] = dates;
        }

        return Plotly.restyle(
          el,
          {
            x: [dateCache[j], s.dates],
            y: [draws, s.observed]
          },
          [0, 1]
        ).then(function() {
          if (token !== request) return;

          var band = data.bands[j][k];

          return Plotly.relayout(el, {
            'yaxis.range': data.ranges[j],
            'shapes[0].y0': band[0],
            'shapes[0].y1': band[1]
          });
        });
      }).catch(function(error) {
        console.error('Simulation data loading failed:', error);
      });
    }

    el.on('plotly_buttonclicked', function(e) {
      var a = data.assets.indexOf(e.button.label);
      var m = data.methods.indexOf(e.button.label);

      if (a >= 0) asset = a;
      if (m >= 0) method = m;

      updatePlot();
    });

    updatePlot();
  }
  ",
  data = list(
    assets = assets,
    methods = methods,
    series = series,
    files = sim_files,
    bands = bands,
    ranges = ranges
  )
)

p10

saveRDS(p10, "figures/01/simulated-returns.rds")


# Exceedance dependence ----

set.seed(2026)

lags <- 1:20
n_perm <- 1000

lag_probability <- function(x) {
  events <- which(x)
  n <- length(x)
  
  vapply(lags, function(h) {
    previous <- events[events <= n - h]
    if (!length(previous)) return(NA_real_)
    mean(x[previous + h])
  }, numeric(1))
}

n_cases <- length(assets) * length(methods)

cases <- vector("list", n_cases)
statistics <- character(n_cases)
ymax <- numeric(n_cases)
diagnostics <- vector("list", n_cases)

for (j in seq_along(assets)) {
  a <- assets[j]
  r <- as.numeric(returns[[a]])
  dates <- return_df$Date[return_df$Asset == a]
  n <- length(r)
  
  for (k in seq_along(methods)) {
    m <- methods[k]
    id <- (j - 1) * length(methods) + k
    
    q <- bands[[j]][[k]]
    lower <- r < q[1]
    upper <- r > q[2]
    exceed <- lower | upper
    K <- sum(exceed)
    
    observed <- lag_probability(exceed)
    benchmark <- if (K > 0) (K - 1) / (n - 1) else NA_real_
    
    if (K >= 2) {
      permuted <- replicate(
        n_perm,
        lag_probability(sample(exceed))
      )
      
      lo <- apply(
        permuted, 1, quantile,
        probs = 0.025, na.rm = TRUE, names = FALSE
      )
      
      hi <- apply(
        permuted, 1, quantile,
        probs = 0.975, na.rm = TRUE, names = FALSE
      )
    } else {
      lo <- hi <- rep(NA_real_, length(lags))
    }
    
    cases[[id]] <- list(
      upper_dates = as.character(dates[upper]),
      upper_returns = unname(round(r[upper], 4)),
      lower_dates = as.character(dates[lower]),
      lower_returns = unname(round(r[lower], 4)),
      observed = unname(observed),
      lo = unname(lo),
      hi = unname(hi),
      benchmark = benchmark
    )
    
    statistics[id] <- sprintf(
      "Exceedances: %d/%d (%.1f%%; nominal 5%%) | P(next exceedance | current exceedance): %.1f%%",
      K, n, 100 * K / n, 100 * observed[1]
    )
    
    ymax[id] <- min(
      1, 1.15 * max(c(0.10, observed, hi, benchmark), na.rm = TRUE)
    )
    
    diagnostics[[id]] <- data.frame(
      Asset = a,
      Model = m,
      Exceedances = K,
      Rate = 100 * K / n,
      Lag1 = 100 * observed[1]
    )
  }
}

p11 <- plotly::plot_ly()

p11 <- plotly::add_trace(
  p11,
  x = NA_character_, y = NA_real_,
  type = "scatter", mode = "markers",
  xaxis = "x", yaxis = "y",
  marker = list(
    color = "#C45B40",
    size = 8,
    symbol = "circle"
  ),
  hovertemplate = "%{x}<br>Upper exceedance<br>%{text}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p11 <- plotly::add_trace(
  p11,
  x = NA_character_, y = NA_real_,
  type = "scatter", mode = "markers",
  xaxis = "x", yaxis = "y",
  marker = list(
    color = "#0100CA",
    size = 8,
    symbol = "circle"
  ),
  hovertemplate = "%{x}<br>Lower exceedance<br>%{text}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p11 <- plotly::add_trace(
  p11,
  x = NA_real_, y = NA_real_,
  type = "scatter", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  fill = "toself",
  fillcolor = "rgba(0,140,149,0.18)",
  line = list(width = 0),
  hoverinfo = "skip",
  showlegend = FALSE,
  inherit = FALSE
)

p11 <- plotly::add_trace(
  p11,
  x = NA_real_, y = NA_real_,
  type = "scatter", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  line = list(
    color = "#8E9BAE",
    width = 1.5,
    dash = "dash"
  ),
  hovertemplate = "Independence benchmark: %{y:.1%}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p11 <- plotly::add_trace(
  p11,
  x = NA_real_, y = NA_real_,
  type = "scatter", mode = "lines+markers",
  xaxis = "x2", yaxis = "y2",
  line = list(color = "#008C95", width = 2),
  marker = list(color = "#008C95", size = 6),
  hovertemplate = "Lag: %{x}<br>Conditional probability: %{y:.1%}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

asset_buttons <- lapply(assets, function(a) {
  list(label = a, method = "skip", args = list())
})

method_buttons <- lapply(methods, function(m) {
  list(label = m, method = "skip", args = list())
})

p11 <- plotly::layout(
  p11,
  title = list(
    text = "Are predictive interval exceedances independent?",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.08, 0.97),
    anchor = "y",
    type = "date",
    range = as.character(range(return_df$Date)),
    title = "",
    gridcolor = "#E4E6F2",
    tickfont = list(size = 10)
  ),
  yaxis = list(
    domain = c(0.76, 0.93),
    anchor = "x",
    range = c(-1.6, 1.6),
    tickvals = c(-1, 1),
    ticktext = c("Lower", "Upper"),
    fixedrange = TRUE,
    zeroline = FALSE,
    showgrid = FALSE
  ),
  xaxis2 = list(
    domain = c(0.08, 0.97),
    anchor = "y2",
    range = c(0.5, 20.5),
    tick0 = 1,
    dtick = 1,
    title = "Lag (observations)",
    gridcolor = "#E4E6F2"
  ),
  yaxis2 = list(
    domain = c(0.10, 0.42),
    anchor = "x2",
    range = c(0, ymax[1]),
    tickformat = ".0%",
    title = "Conditional probability",
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  updatemenus = list(
    list(
      type = "dropdown",
      direction = "down",
      buttons = asset_buttons,
      active = 0,
      x = 0.08, xanchor = "left",
      y = 0.62, yanchor = "top",
      showactive = TRUE
    ),
    list(
      type = "dropdown",
      direction = "down",
      buttons = method_buttons,
      active = 0,
      x = 0.54, xanchor = "left",
      y = 0.62, yanchor = "top",
      showactive = TRUE
    )
  ),
  annotations = list(
    list(
      x = 0.08, y = 0.975,
      xref = "paper", yref = "paper",
      text = "<b>Exceedance timeline</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.08, y = 0.685,
      xref = "paper", yref = "paper",
      text = statistics[1],
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 11, color = "#505568")
    ),
    list(
      x = 0.08, y = 0.48,
      xref = "paper", yref = "paper",
      text = "<b>Probability of another exceedance</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.08, y = 0.645,
      xref = "paper", yref = "paper",
      text = "<b>Asset</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 11, color = "#505568")
    ),
    list(
      x = 0.54, y = 0.645,
      xref = "paper", yref = "paper",
      text = "<b>Model</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 11, color = "#505568")
    )
  ),
  autosize = TRUE,
  height = 850,
  margin = list(l = 75, r = 30, t = 95, b = 100),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p11 <- plotly::config(
  p11,
  responsive = TRUE,
  displaylogo = FALSE
)

p11 <- htmlwidgets::onRender(
  p11,
  "
  function(el, x, data) {
    var asset = 0;
    var method = 0;
    var nMethods = data.methods.length;
    var lags = data.lags;
    var indices = [0, 1, 2, 3, 4];

    function updateCase() {
      var id = asset * nMethods + method;
      var s = data.cases[id];

      var xs = [
        s.upper_dates,
        s.lower_dates,
        lags.concat(lags.slice().reverse()),
        lags,
        lags
      ];

      var ys = [
        Array(s.upper_dates.length).fill(1),
        Array(s.lower_dates.length).fill(-1),
        s.lo.concat(s.hi.slice().reverse()),
        lags.map(function() { return s.benchmark; }),
        s.observed
      ];

      var texts = [
        s.upper_returns.map(function(v) {
          return 'Return: ' + v.toFixed(2) + '%';
        }),
        s.lower_returns.map(function(v) {
          return 'Return: ' + v.toFixed(2) + '%';
        }),
        [],
        [],
        []
      ];

      Plotly.update(
        el,
        {
          x: xs,
          y: ys,
          text: texts
        },
        {
          'yaxis2.range': [0, data.ymax[id]],
          'annotations[1].text': data.statistics[id]
        },
        indices
      );
    }

    el.on('plotly_buttonclicked', function(e) {
      var a = data.assets.indexOf(e.button.label);
      var m = data.methods.indexOf(e.button.label);

      if (a >= 0) asset = a;
      if (m >= 0) method = m;

      updateCase();
    });

    updateCase();
  }
  ",
  data = list(
    assets = assets,
    methods = methods,
    lags = lags,
    cases = cases,
    ymax = ymax,
    statistics = statistics
  )
)

p11

diagnostics <- do.call(rbind, diagnostics)
print(diagnostics, row.names = FALSE, digits = 3)

saveRDS(p11, "figures/01/exceedance-dependence.rds")

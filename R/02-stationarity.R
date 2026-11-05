# Setup ----

library(ggplot2)
library(plotly)
library(htmlwidgets)
library(readxl)
library(patchwork)

dir.create("figures/02", recursive = TRUE, showWarnings = FALSE)

axis_range <- function(x) {
  z <- range(x, finite = TRUE)
  pad <- max(0.08 * diff(z), 0.1)
  z + c(-pad, pad)
}

# Stochastic processes ----

set.seed(2026)

n <- 1000
n_paths <- 100
rho <- 0.9
n_initial <- 50
path_grid <- seq(50, 1000, by = 50)

wn <- matrix(rnorm(n * n_paths), nrow = n)

rw <- apply(
  matrix(rnorm(n * n_paths), nrow = n),
  2, cumsum
)

ar <- matrix(NA_real_, nrow = n, ncol = n_paths)
previous <- rnorm(n_paths, sd = 1 / sqrt(1 - rho^2))

for (t in seq_len(n)) {
  ar[t, ] <- rho * previous + rnorm(n_paths)
  previous <- ar[t, ]
}

processes <- c(
  "Gaussian white noise",
  "Random walk",
  "AR(1), rho = 0.9"
)

sim <- setNames(list(wn, rw, ar), processes)

colors <- c("#0100CA", "#C45B40", "#008C95")
faint <- c(
  "rgba(1,0,202,0.07)",
  "rgba(196,91,64,0.07)",
  "rgba(0,140,149,0.07)"
)

## Process assets ----

asset_dir <- "figures/02/stochastic-process-assets"
dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

series <- lapply(sim, function(z) {
  lapply(seq_len(ncol(z)), function(j) unname(round(z[, j], 4)))
})

writeLines(
  paste0(
    "window.drpProcess02 = ",
    jsonlite::toJSON(unname(series), auto_unbox = TRUE, digits = NA),
    ";"
  ),
  file.path(asset_dir, "process-data.js"),
  useBytes = TRUE
)

process_dep <- htmltools::htmlDependency(
  name = "stationarity-process-assets",
  version = "1.0.1",
  src = c(file = normalizePath(asset_dir, winslash = "/")),
  script = "process-data.js",
  all_files = FALSE
)

## Plot ----

p1 <- plotly::plot_ly()

for (i in seq_along(processes)) {
  id <- if (i == 1) "" else as.character(i)
  
  p1 <- plotly::add_trace(
    p1,
    x = seq_len(n_initial),
    y = sim[[i]][seq_len(n_initial), 2],
    type = "scattergl",
    mode = "lines",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    line = list(color = faint[i], width = 1),
    hoverinfo = "skip",
    visible = FALSE,
    showlegend = FALSE,
    inherit = FALSE
  )
  
  p1 <- plotly::add_trace(
    p1,
    x = seq_len(n_initial),
    y = sim[[i]][seq_len(n_initial), 1],
    type = "scattergl",
    mode = "lines",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    line = list(color = colors[i], width = 2.5),
    hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
    visible = TRUE,
    showlegend = FALSE,
    inherit = FALSE
  )
}

axes <- list()
annotations <- list()
domains <- list(
  c(0.75, 0.93),
  c(0.46, 0.64),
  c(0.17, 0.35)
)

for (i in seq_along(processes)) {
  id <- if (i == 1) "" else as.character(i)
  yd <- domains[[i]]
  
  axes[[paste0("xaxis", id)]] <- list(
    domain = c(0.09, 0.97),
    anchor = paste0("y", id),
    range = c(1, n_initial),
    fixedrange = TRUE,
    title = if (i == 3) "Observation" else "",
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  )
  
  axes[[paste0("yaxis", id)]] <- list(
    domain = yd,
    anchor = paste0("x", id),
    range = c(-1, 1),
    fixedrange = TRUE,
    title = "Value",
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  )
  
  annotations[[i]] <- list(
    x = 0.09,
    y = max(yd) + 0.025,
    xref = "paper",
    yref = "paper",
    text = paste0("<b>", processes[i], "</b>"),
    showarrow = FALSE,
    xanchor = "left",
    yanchor = "bottom",
    font = list(size = 13, color = "#000000")
  )
}

steps <- lapply(path_grid, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

buttons <- lapply(c("One realisation", "All realisations"), function(z) {
  list(label = z, method = "skip", args = list())
})

p1 <- do.call(plotly::layout, c(
  list(p1), axes,
  list(
    title = list(
      text = "Realisation vs Stochastic Process",
      x = 0.5,
      font = list(size = 21, color = "#000000")
    ),
    annotations = annotations,
    sliders = list(list(
      active = 0,
      steps = steps,
      currentvalue = list(prefix = "Sample size n = "),
      x = 0.12,
      len = 0.76,
      y = 0.02,
      pad = list(t = 30)
    )),
    updatemenus = list(list(
      type = "buttons",
      direction = "right",
      buttons = buttons,
      active = 0,
      showactive = TRUE,
      x = 0.5,
      xanchor = "center",
      y = 0,
      yanchor = "middle"
    )),
    autosize = TRUE,
    height = 1050,
    margin = list(l = 35, r = 35, t = 90, b = 190),
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor = "#FFFFFF"
  )
))

p1 <- plotly::config(
  p1,
  responsive = TRUE,
  displaylogo = FALSE
)

p1 <- htmlwidgets::onRender(
  p1,
  "
  function(el, x, data) {
    var sampleSize = data.nInitial;
    var showAll = false;
    var highlight = 0;
    var series = window.drpProcess02;
    var loaded = !!series;

    if (!loaded) {
      console.error('Process simulation data could not be loaded.');
      return;
    }

    function buildBackground(paths, sampleSize) {
      var x = [];
      var y = [];
      for (var j = 1; j < paths.length; j++) {
        for (var t = 0; t < sampleSize; t++) {
          x.push(t + 1);
          y.push(paths[j][t]);
        }
        x.push(null);
        y.push(null);
      }
      return {x: x, y: y};
    }

    function buildHighlight(paths, sampleSize) {
      return {
        x: Array.from({length: sampleSize}, function(_, i) { return i + 1; }),
        y: paths[highlight].slice(0, sampleSize)
      };
    }

    function processRange(paths, sampleSize, showAll) {
      var lo = Infinity;
      var hi = -Infinity;
      var start = showAll ? 0 : highlight;
      var end = showAll ? paths.length - 1 : highlight;

      for (var j = start; j <= end; j++) {
        for (var t = 0; t < sampleSize; t++) {
          var v = paths[j][t];
          if (v < lo) lo = v;
          if (v > hi) hi = v;
        }
      }

      var pad = Math.max(0.08 * (hi - lo), 0.1);
      return [lo - pad, hi + pad];
    }

    function updatePlot() {
      if (!loaded) return;

      var traceX = [];
      var traceY = [];
      var traceVisible = [];
      var ranges = [];

      for (var i = 0; i < 3; i++) {
        var bg = buildBackground(series[i], sampleSize);
        var hl = buildHighlight(series[i], sampleSize);

        traceX.push(bg.x);
        traceY.push(bg.y);
        traceVisible.push(showAll);

        traceX.push(hl.x);
        traceY.push(hl.y);
        traceVisible.push(true);

        ranges.push(processRange(series[i], sampleSize, showAll));
      }

      Plotly.restyle(
        el,
        {
          x: traceX,
          y: traceY,
          visible: traceVisible
        },
        [0, 1, 2, 3, 4, 5]
      );

      Plotly.relayout(el, {
        'xaxis.range': [1, sampleSize],
        'xaxis2.range': [1, sampleSize],
        'xaxis3.range': [1, sampleSize],
        'yaxis.range': ranges[0],
        'yaxis2.range': ranges[1],
        'yaxis3.range': ranges[2]
      });
    }

    updatePlot();

    el.on('plotly_sliderchange', function(e) {
      sampleSize = Number(e.step.label);
      updatePlot();
    });

    el.on('plotly_buttonclicked', function(e) {
      showAll = e.button.label === 'All realisations';
      updatePlot();
    });
  }
  ",
  data = list(
    n = n,
    nPaths = n_paths,
    nInitial = n_initial
  )
)

p1$dependencies <- c(p1$dependencies, list(process_dep))

p1
saveRDS(p1, "figures/02/stochastic-processes.rds")


# Stationarity ----

set.seed(2027)

n_max <- 3000
n_ensemble <- 500
n_display <- 11

rho_values <- c(0, 0.2, 0.5, 0.8, 0.9, 0.95, 0.98, 0.995, 1)
rho_reference <- 0.995
rw_sd <- sqrt(1 - rho_reference^2)
rho_start <- which(rho_values == 0.8)

n_values <- seq(50, n_max, by = 50)
path_order2 <- c(2:n_display, 1)
check_times <- c(50, 250, 500, 750, 1000, 1500, 2000, 3000)
kde_times <- lapply(n_values, function(n) as.integer(n * (1:5) / 5))

innovations <- matrix(rnorm(n_max * n_ensemble), nrow = n_max)
initial <- rnorm(n_ensemble)

density_grid <- seq(-3.5, 3.5, length.out = 121)
rw_grids <- lapply(n_values, function(n) {
  limit <- 3.5 * sqrt(1 + n * rw_sd^2)
  seq(-limit, limit, length.out = 121)
})

ar_paths <- vector("list", length(rho_values))
ar_kdes <- vector("list", length(rho_values))
stationarity_diag <- vector("list", length(rho_values))

for (i in seq_along(rho_values)) {
  rho_i <- rho_values[i]
  sd_i <- if (rho_i == 1) rw_sd else sqrt(1 - rho_i^2)
  bw_i <- if (rho_i == 1) "nrd0" else 0.25
  
  x <- matrix(NA_real_, n_max, n_ensemble)
  xt <- initial
  
  for (t in seq_len(n_max)) {
    xt <- rho_i * xt + sd_i * innovations[t, ]
    x[t, ] <- xt
  }
  
  ar_paths[[i]] <- lapply(path_order2, function(j) {
    unname(round(x[, j], 4))
  })
  
  ar_kdes[[i]] <- lapply(seq_along(n_values), function(h) {
    grid <- if (rho_i == 1) rw_grids[[h]] else density_grid
    lapply(kde_times[[h]], function(t) {
      unname(round(density(
        x[t, ], bw = bw_i,
        from = min(grid), to = max(grid), n = length(grid)
      )$y, 5))
    })
  })
  
  stationarity_diag[[i]] <- data.frame(
    Rho = rho_i,
    Time = check_times,
    Mean = rowMeans(x[check_times, , drop = FALSE]),
    Variance = apply(x[check_times, , drop = FALSE], 1, var)
  )
}

stationarity_diag <- do.call(rbind, stationarity_diag)

print(
  subset(stationarity_diag, Rho %in% c(0.8, 0.995, 1)),
  row.names = FALSE,
  digits = 3
)


## Stationary AR(1) explorer ----

kde_colors <- c("#0100CA", "#008C95", "#C45B40", "#8775AE", "#687589")
n_initial <- 50

asset_dir <- "figures/02/ar1-explorer-assets"
dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(
  paste0(
    "window.drpAR1Explorer02 = ",
    jsonlite::toJSON(list(
      paths = ar_paths,
      kdes = ar_kdes,
      stationaryGrid = density_grid,
      rwGrids = rw_grids,
      theoretical = unname(dnorm(density_grid))
    ), auto_unbox = TRUE, digits = NA),
    ";"
  ),
  file.path(asset_dir, "ar1-explorer-data.js"),
  useBytes = TRUE
)

ar_dep <- htmltools::htmlDependency(
  name = "drp-ar1-explorer",
  version = "1.0.0",
  src = c(file = normalizePath(asset_dir, winslash = "/")),
  script = "ar1-explorer-data.js",
  all_files = FALSE
)

p2 <- plotly::plot_ly()

p2 <- plotly::add_trace(
  p2,
  x = seq_len(n_initial),
  y = ar_paths[[rho_start]][[1]][seq_len(n_initial)],
  type = "scattergl", mode = "lines",
  xaxis = "x", yaxis = "y",
  visible = FALSE,
  line = list(color = "rgba(1,0,202,0.08)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p2 <- plotly::add_trace(
  p2,
  x = seq_len(n_initial),
  y = ar_paths[[rho_start]][[n_display]][seq_len(n_initial)],
  type = "scattergl", mode = "lines",
  xaxis = "x", yaxis = "y",
  line = list(color = "#0100CA", width = 2.5),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

for (k in 1:5) {
  p2 <- plotly::add_trace(
    p2,
    x = density_grid,
    y = ar_kdes[[rho_start]][[1]][[k]],
    type = "scatter", mode = "lines",
    xaxis = "x2", yaxis = "y2",
    name = paste0("t = ", 10 * k),
    line = list(color = kde_colors[k], width = 1.7),
    hovertemplate = "Value: %{x:.2f}<br>Density: %{y:.3f}<extra></extra>",
    showlegend = TRUE, inherit = FALSE
  )
}

p2 <- plotly::add_trace(
  p2,
  x = density_grid,
  y = dnorm(density_grid),
  type = "scatter", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  name = "N(0,1)",
  line = list(color = "#000000", width = 2.5, dash = "dash"),
  hovertemplate = "Theoretical density: %{y:.3f}<extra></extra>",
  showlegend = TRUE, inherit = FALSE
)

rho_steps <- lapply(rho_values, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})
n_steps <- lapply(n_values, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})
realisation_buttons <- lapply(c("One realisation", "All realisations"), function(z) {
  list(label = z, method = "skip", args = list())
})

p2 <- plotly::layout(
  p2,
  title = list(
    text = "Persistence, stationarity and marginal distributions",
    x = 0.5, font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.10, 0.97), anchor = "y",
    range = c(1, n_initial), title = "Observation",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  yaxis = list(
    domain = c(0.62, 0.93), anchor = "x",
    range = axis_range(ar_paths[[rho_start]][[n_display]][seq_len(n_initial)]),
    title = "Value", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.10, 0.77), anchor = "y2",
    range = c(-3.5, 3.5), title = "Value",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  yaxis2 = list(
    domain = c(0.12, 0.42), anchor = "x2",
    range = c(0, 0.60), title = "Density",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  annotations = list(
    list(
      x = 0.10, y = 0.975, xref = "paper", yref = "paper",
      text = "<b>Simulated trajectories</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.10, y = 0.47, xref = "paper", yref = "paper",
      text = "<b>Marginal distributions across independent realisations</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    )
  ),
  legend = list(
    x = 0.80, y = 0.42, xanchor = "left", yanchor = "top",
    font = list(size = 10),
    bgcolor = "rgba(255,255,255,0)"
  ),
  sliders = list(
    list(
      active = rho_start - 1,
      steps = rho_steps,
      currentvalue = list(prefix = "Persistence rho = "),
      x = 0.15, len = 0.70, y = 0.08,
      pad = list(t = 30)
    ),
    list(
      active = 0,
      steps = n_steps,
      currentvalue = list(prefix = "Sample size n = "),
      x = 0.15, len = 0.70, y = -0.035,
      pad = list(t = 30)
    )
  ),
  updatemenus = list(list(
    type = "buttons", direction = "right",
    buttons = realisation_buttons,
    active = 0, showactive = TRUE,
    x = 0.5, xanchor = "center",
    y = -0.23, yanchor = "middle"
  )),
  autosize = TRUE,
  height = 1000,
  margin = list(l = 80, r = 45, t = 95, b = 270),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p2 <- plotly::config(p2, responsive = TRUE, displaylogo = FALSE)
p2$dependencies <- c(p2$dependencies, list(ar_dep))

p2 <- htmlwidgets::onRender(
  p2,
  "
  function(el, x, data) {
    var shared = window.drpAR1Explorer02;
    if (!shared) {
      console.error('AR(1) simulation data could not be loaded.');
      return;
    }

    var rhoIndex = data.rhoStart;
    var nIndex = 0;
    var showAll = false;
    var indices = [0, 1, 2, 3, 4, 5, 6, 7];

    function updatePlot() {
      var n = data.nValues[nIndex];
      var isRW = data.rhoLabels[rhoIndex] === '1';
      var paths = shared.paths[rhoIndex];
      var kdes = shared.kdes[rhoIndex][nIndex];
      var grid = isRW ? shared.rwGrids[nIndex] : shared.stationaryGrid;

      var bgX = [], bgY = [];
      var lo = Infinity, hi = -Infinity;

      for (var j = 0; j < paths.length; j++) {
        if (j === paths.length - 1 && showAll) continue;
        if (!showAll && j !== paths.length - 1) continue;

        for (var t = 0; t < n; t++) {
          var v = paths[j][t];
          lo = Math.min(lo, v);
          hi = Math.max(hi, v);
        }
      }

      for (var j = 0; j < paths.length - 1; j++) {
        if (!showAll) break;
        for (var t = 0; t < n; t++) {
          bgX.push(t + 1);
          bgY.push(paths[j][t]);
        }
        bgX.push(null);
        bgY.push(null);
      }

      var times = Array.from({length: n}, function(_, i) { return i + 1; });
      var xs = [bgX, times];
      var ys = [bgY, paths[paths.length - 1].slice(0, n)];
      var visible = [showAll, true];
      var names = ['', ''];

      for (var k = 0; k < 5; k++) {
        xs.push(grid);
        ys.push(kdes[k]);
        visible.push(true);
        names.push('t = ' + Math.round(n * (k + 1) / 5));
      }

      xs.push(shared.stationaryGrid);
      ys.push(shared.theoretical);
      visible.push(true);
      names.push('N(0,1)');

      var pathPad = Math.max(0.08 * (hi - lo), 0.1);
      var kdeMax = 0.39895;

      for (var k = 0; k < 5; k++) {
        for (var j = 0; j < kdes[k].length; j++) {
          kdeMax = Math.max(kdeMax, kdes[k][j]);
        }
      }

      Plotly.update(
        el,
        {x: xs, y: ys, visible: visible, name: names},
        {
          'xaxis.range': [1, n],
          'yaxis.range': [lo - pathPad, hi + pathPad],
          'xaxis2.range': [grid[0], grid[grid.length - 1]],
          'yaxis2.range': [0, kdeMax * 1.18]
        },
        indices
      );
    }

    el.on('plotly_sliderchange', function(e) {
      var label = String(e.step.label);
      var r = data.rhoLabels.indexOf(label);
      var n = data.nLabels.indexOf(label);
      if (r >= 0) rhoIndex = r;
      if (n >= 0) nIndex = n;
      updatePlot();
    });

    el.on('plotly_buttonclicked', function(e) {
      showAll = e.button.label === 'All realisations';
      updatePlot();
    });

    updatePlot();
  }
  ",
  data = list(
    rhoLabels = as.character(rho_values),
    nLabels = as.character(n_values),
    rhoStart = rho_start - 1L,
    nValues = n_values
  )
)

p2
saveRDS(p2, "figures/02/stationary-ar1.rds")

## Stationary AR(1) versus random walk ----

set.seed(2028)

rho_near <- 0.995
n_pairs <- 100
n_initial <- 100
path_order3 <- c(2:n_pairs, 1)

shocks <- matrix(rnorm(n_max * n_pairs), nrow = n_max)

starts <- rnorm(
  n_pairs,
  sd = 1 / sqrt(1 - rho_near^2)
)

ar_near <- matrix(NA_real_, n_max, n_pairs)
rw_near <- matrix(NA_real_, n_max, n_pairs)

previous_ar <- starts
previous_rw <- starts

for (t in seq_len(n_max)) {
  ar_near[t, ] <- rho_near * previous_ar + shocks[t, ]
  rw_near[t, ] <- previous_rw + shocks[t, ]
  previous_ar <- ar_near[t, ]
  previous_rw <- rw_near[t, ]
}

paired_paths <- list(
  ar_near[, path_order3],
  rw_near[, path_order3]
)

paired_colors <- c("#0100CA", "#C45B40")
paired_faint <- c(
  "rgba(1,0,202,0.09)",
  "rgba(196,91,64,0.09)"
)

initial_range <- axis_range(c(
  ar_near[seq_len(n_initial), ],
  rw_near[seq_len(n_initial), ]
))

asset_dir <- "figures/02/ar1-rw-assets"
dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(
  paste0(
    "window.drpAR1RW02 = ",
    jsonlite::toJSON(
      lapply(paired_paths, function(z) {
        lapply(seq_len(ncol(z)), function(j) unname(round(z[, j], 4)))
      }),
      auto_unbox = TRUE,
      digits = NA
    ),
    ";"
  ),
  file.path(asset_dir, "ar1-rw-data.js"),
  useBytes = TRUE
)

ar_rw_dep <- htmltools::htmlDependency(
  name = "drp-ar1-rw",
  version = "1.0.0",
  src = c(file = normalizePath(asset_dir, winslash = "/")),
  script = "ar1-rw-data.js",
  all_files = FALSE
)

p3 <- plotly::plot_ly()

p3 <- plotly::add_trace(
  p3,
  x = seq_len(n_initial),
  y = paired_paths[[1]][seq_len(n_initial), 1],
  type = "scattergl",
  mode = "lines",
  xaxis = "x",
  yaxis = "y",
  line = list(color = paired_faint[1], width = 1),
  hoverinfo = "skip",
  showlegend = FALSE,
  inherit = FALSE
)

p3 <- plotly::add_trace(
  p3,
  x = seq_len(n_initial),
  y = paired_paths[[1]][seq_len(n_initial), n_pairs],
  type = "scattergl",
  mode = "lines",
  xaxis = "x",
  yaxis = "y",
  line = list(color = paired_colors[1], width = 2.5),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p3 <- plotly::add_trace(
  p3,
  x = seq_len(n_initial),
  y = paired_paths[[2]][seq_len(n_initial), 1],
  type = "scattergl",
  mode = "lines",
  xaxis = "x2",
  yaxis = "y2",
  line = list(color = paired_faint[2], width = 1),
  hoverinfo = "skip",
  showlegend = FALSE,
  inherit = FALSE
)

p3 <- plotly::add_trace(
  p3,
  x = seq_len(n_initial),
  y = paired_paths[[2]][seq_len(n_initial), n_pairs],
  type = "scattergl",
  mode = "lines",
  xaxis = "x2",
  yaxis = "y2",
  line = list(color = paired_colors[2], width = 2.5),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

sample_steps <- lapply(seq(100, 1000, by = 100), function(n_i) {
  list(label = as.character(n_i), method = "skip", args = list())
})

p3 <- plotly::layout(
  p3,
  title = list(
    text = "Can we distinguish stationarity from a random walk?",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.09, 0.97),
    anchor = "y",
    range = c(1, n_initial),
    fixedrange = TRUE,
    title = "",
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  yaxis = list(
    domain = c(0.60, 0.92),
    anchor = "x",
    range = initial_range,
    fixedrange = TRUE,
    title = "Value",
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.09, 0.97),
    anchor = "y2",
    range = c(1, n_initial),
    fixedrange = TRUE,
    title = "Observation",
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  yaxis2 = list(
    domain = c(0.12, 0.44),
    anchor = "x2",
    range = initial_range,
    fixedrange = TRUE,
    title = "Value",
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.09, y = 0.965,
      xref = "paper", yref = "paper",
      text = "<b>Stationary AR(1), rho = 0.995</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.09, y = 0.485,
      xref = "paper", yref = "paper",
      text = "<b>Random walk, rho = 1</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    )
  ),
  sliders = list(list(
    active = 0,
    steps = sample_steps,
    currentvalue = list(prefix = "Sample size n = "),
    x = 0.12,
    len = 0.76,
    y = 0.05,
    pad = list(t = 30)
  )),
  autosize = TRUE,
  height = 850,
  margin = list(l = 80, r = 40, t = 95, b = 150),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p3 <- plotly::config(
  p3,
  responsive = TRUE,
  displaylogo = FALSE
)

p3$dependencies <- c(p3$dependencies, list(ar_rw_dep))

p3 <- htmlwidgets::onRender(
  p3,
  "
  function(el, x, data) {
    var paths = window.drpAR1RW02;
    if (!paths) {
      console.error('AR vs RW simulation data could not be loaded.');
      return;
    }

    var sampleSize = 100;

    function buildBackground(series, sampleSize) {
      var x = [];
      var y = [];

      for (var j = 0; j < series.length - 1; j++) {
        for (var t = 0; t < sampleSize; t++) {
          x.push(t + 1);
          y.push(series[j][t]);
        }
        x.push(null);
        y.push(null);
      }

      return {x: x, y: y};
    }

    function updatePlot() {
      var times = Array.from({length: sampleSize}, function(_, i) { return i + 1; });

      var bgAR = buildBackground(paths[0], sampleSize);
      var bgRW = buildBackground(paths[1], sampleSize);

      var hiAR = paths[0][paths[0].length - 1].slice(0, sampleSize);
      var hiRW = paths[1][paths[1].length - 1].slice(0, sampleSize);

      var lo = Infinity;
      var hi = -Infinity;

      for (var j = 0; j < 2; j++) {
        for (var k = 0; k < paths[j].length; k++) {
          for (var t = 0; t < sampleSize; t++) {
            var v = paths[j][k][t];
            if (v < lo) lo = v;
            if (v > hi) hi = v;
          }
        }
      }

      var pad = Math.max(0.08 * (hi - lo), 0.1);
      var yrange = [lo - pad, hi + pad];

      Plotly.update(
        el,
        {
          x: [bgAR.x, times, bgRW.x, times],
          y: [bgAR.y, hiAR, bgRW.y, hiRW]
        },
        {
          'xaxis.range': [1, sampleSize],
          'xaxis2.range': [1, sampleSize],
          'yaxis.range': yrange,
          'yaxis2.range': yrange
        },
        [0, 1, 2, 3]
      );
    }

    el.on('plotly_sliderchange', function(e) {
      sampleSize = Number(e.step.label);
      updatePlot();
    });

    updatePlot();
  }
  "
)

theoretical_variances <- data.frame(
  Time = c(100, 500, 1000),
  AR1 = rep(1 / (1 - rho_near^2), 3),
  Random_Walk = 1 / (1 - rho_near^2) + c(100, 500, 1000)
)

print(theoretical_variances, row.names = FALSE, digits = 4)

p3
saveRDS(p3, "figures/02/ar1-vs-random-walk.rds")

## Conditional versus unconditional distributions ----

set.seed(2026)

n_cond <- 250
rho_cond <- 0.8
sd_cond <- sqrt(1 - rho_cond^2)

x_cond <- numeric(n_cond)
previous <- rnorm(1)

for (t in seq_len(n_cond)) {
  x_cond[t] <- rho_cond * previous + sd_cond * rnorm(1)
  previous <- x_cond[t]
}

conditioning_times <- seq(10, 240, by = 10)
s_initial <- conditioning_times[1]

density_grid4 <- seq(-5, 5, length.out = 401)

marginal_density <- dnorm(density_grid4)
conditional_density <- dnorm(
  density_grid4,
  mean = rho_cond * x_cond[s_initial],
  sd = sd_cond
)

path_range <- range(x_cond)
path_pad <- 0.10 * diff(path_range)

conditional_label <- function(s) {
  sprintf(
    "X<sub>%d</sub> = %.2f &nbsp;&nbsp; | &nbsp;&nbsp; E[X<sub>%d</sub> | X<sub>%d</sub>] = %.2f",
    s, x_cond[s], s + 1, s, rho_cond * x_cond[s]
  )
}

p4 <- plotly::plot_ly()

p4 <- plotly::add_trace(
  p4,
  x = seq_len(n_cond),
  y = x_cond,
  type = "scatter",
  mode = "lines",
  xaxis = "x",
  yaxis = "y",
  line = list(color = "#0100CA", width = 1.8),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p4 <- plotly::add_trace(
  p4,
  x = s_initial,
  y = x_cond[s_initial],
  type = "scatter",
  mode = "markers",
  xaxis = "x",
  yaxis = "y",
  marker = list(
    color = "#C45B40",
    size = 12,
    line = list(color = "#FFFFFF", width = 2)
  ),
  hovertemplate = "Conditioning observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p4 <- plotly::add_trace(
  p4,
  x = density_grid4,
  y = marginal_density,
  type = "scatter",
  mode = "lines",
  xaxis = "x2",
  yaxis = "y2",
  name = "Unconditional N(0,1)",
  line = list(color = "#0100CA", width = 2.5),
  hovertemplate = "Value: %{x:.2f}<br>Density: %{y:.3f}<extra></extra>",
  showlegend = TRUE,
  inherit = FALSE
)

p4 <- plotly::add_trace(
  p4,
  x = density_grid4,
  y = conditional_density,
  type = "scatter",
  mode = "lines",
  xaxis = "x2",
  yaxis = "y2",
  name = "Conditional distribution",
  line = list(color = "#C45B40", width = 2.5),
  hovertemplate = "Value: %{x:.2f}<br>Density: %{y:.3f}<extra></extra>",
  showlegend = TRUE,
  inherit = FALSE
)

conditioning_steps <- lapply(conditioning_times, function(s) {
  list(
    label = as.character(s),
    method = "skip",
    args = list()
  )
})

p4 <- plotly::layout(
  p4,
  title = list(
    text = "Stationarity does not imply independence",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.09, 0.97),
    anchor = "y",
    range = c(1, n_cond),
    title = "Observation",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  yaxis = list(
    domain = c(0.64, 0.92),
    anchor = "x",
    range = path_range + c(-path_pad, path_pad),
    title = "Value",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.09, 0.97),
    anchor = "y2",
    range = c(-5, 5),
    title = "Value",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  yaxis2 = list(
    domain = c(0.19, 0.44),
    anchor = "x2",
    range = c(0, 1.15 * dnorm(0, sd = sd_cond)),
    title = "Density",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  annotations = list(
    list(
      x = 0.09,
      y = 0.97,
      xref = "paper",
      yref = "paper",
      text = "<b>One stationary AR(1) realisation</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.09,
      y = 0.50,
      xref = "paper",
      yref = "paper",
      text = conditional_label(s_initial),
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    )
  ),
  legend = list(
    x = 0.97,
    y = 0.43,
    xanchor = "right",
    yanchor = "top",
    font = list(size = 11),
    bgcolor = "rgba(255,255,255,0.8)"
  ),
  sliders = list(list(
    active = 0,
    steps = conditioning_steps,
    currentvalue = list(prefix = "Conditioning time = "),
    x = 0.15,
    len = 0.70,
    y = 0.045,
    pad = list(t = 30)
  )),
  autosize = TRUE,
  height = 850,
  margin = list(l = 80, r = 45, t = 95, b = 145),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p4 <- plotly::config(
  p4,
  responsive = TRUE,
  displaylogo = FALSE
)

p4 <- htmlwidgets::onRender(
  p4,
  "
  function(el, x, data) {
    var variance = 1 - data.rho * data.rho;
    var sd = Math.sqrt(variance);

    function updatePlot(s) {
      var previous = data.path[s - 1];
      var mean = data.rho * previous;

      var conditional = data.grid.map(function(z) {
        var u = (z - mean) / sd;
        return Math.exp(-0.5 * u * u) /
          (sd * Math.sqrt(2 * Math.PI));
      });

      Plotly.restyle(
        el,
        {
          x: [[s], data.grid],
          y: [[previous], conditional]
        },
        [1, 3]
      );

      var label =
        'X<sub>' + s + '</sub> = ' + previous.toFixed(2) +
        ' &nbsp;&nbsp; | &nbsp;&nbsp; ' +
        'E[X<sub>' + (s + 1) + '</sub> | X<sub>' + s +
        '</sub>] = ' + mean.toFixed(2);

      Plotly.relayout(
        el,
        {'annotations[1].text': label}
      );
    }

    el.on('plotly_sliderchange', function(e) {
      updatePlot(Number(e.step.label));
    });

    updatePlot(data.initial);
  }
  ",
  data = list(
    path = unname(round(x_cond, 6)),
    grid = density_grid4,
    rho = rho_cond,
    initial = s_initial
  )
)

p4

saveRDS(p4, "figures/02/conditional-distributions.rds")



## Historical inflation ----

file <- "data/macroeconomic/inflation-worldbank.xlsx"
cpi_raw <- read_excel(file, sheet = "hcpi_m")

country_codes <- c("JPN", "GBR", "MEX", "TUR", "BRA")
country_names <- c("Japan", "United Kingdom", "Mexico", "Türkiye", "Brazil")
country_colors <- c("#0100CA", "#687589", "#008C95", "#C45B40", "#8775AE")

month_cols <- sort(grep("^[0-9]{6}$", names(cpi_raw), value = TRUE))
month_dates <- as.Date(paste0(substr(month_cols, 1, 4), "-",
                              substr(month_cols, 5, 6), "-01"))
stopifnot(identical(month_dates, seq(min(month_dates), by = "month",
                                     length.out = length(month_dates))))

country_rows <- match(country_codes, cpi_raw[["Country Code"]])
stopifnot(!anyNA(country_rows))

inflation_data <- lapply(country_rows, function(i) {
  index <- as.numeric(unlist(cpi_raw[i, month_cols], use.names = FALSE))
  current <- index[13:length(index)]
  previous <- index[1:(length(index) - 12)]
  inflation <- 100 * (current / previous - 1)
  dates <- month_dates[13:length(month_dates)]
  valid <- is.finite(inflation) & is.finite(current) &
    is.finite(previous) & current > 0 & previous > 0
  data.frame(
    date = as.character(dates[valid]),
    inflation = round(inflation[valid], 5)
  )
})
names(inflation_data) <- country_names

coverage <- data.frame(
  Country = country_names,
  First = sapply(inflation_data, function(z) z$date[1]),
  Last = sapply(inflation_data, function(z) tail(z$date, 1)),
  N = sapply(inflation_data, nrow),
  Peak = sapply(inflation_data, function(z) max(z$inflation))
)
print(coverage, row.names = FALSE, digits = 4)

## Historical inflation explorer ----

window_start <- "2000-01-01"
window_end <- "2019-12-31"
full_start <- min(sapply(inflation_data, function(z) min(z$date)))
full_end <- max(sapply(inflation_data, function(z) max(z$date)))

inflation_range <- function(x) {
  z <- range(x, finite = TRUE)
  if (!all(is.finite(z))) return(c(-1, 1))
  pad <- max(0.08 * diff(z), 0.5)
  z + c(-pad, pad)
}

domains <- list(
  c(0.840, 0.965),
  c(0.657, 0.782),
  c(0.474, 0.599),
  c(0.291, 0.416),
  c(0.108, 0.233)
)

p5 <- plotly::plot_ly()
axes <- list()
annotations <- list()

for (i in seq_along(inflation_data)) {
  id <- if (i == 1) "" else as.character(i)
  z <- inflation_data[[i]]
  selected <- z$date >= window_start & z$date <= window_end
  
  p5 <- plotly::add_trace(
    p5,
    x = z$date,
    y = z$inflation,
    type = "scatter",
    mode = "lines",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    name = country_names[i],
    line = list(color = country_colors[i], width = 1.9),
    hovertemplate = paste0(
      "<b>", country_names[i], "</b>",
      "<br>%{x|%b %Y}",
      "<br>YoY inflation: %{y:.2f}%",
      "<extra></extra>"
    ),
    connectgaps = FALSE,
    showlegend = FALSE,
    inherit = FALSE
  )
  
  axes[[paste0("xaxis", id)]] <- c(
    list(
      domain = c(0.04, 0.96),
      anchor = paste0("y", id),
      type = "date",
      range = c(window_start, window_end),
      title = if (i == 5) "Calendar year" else "",
      tickformat = "%Y",
      nticks = 8,
      showticklabels = i == 5,
      fixedrange = TRUE,
      gridcolor = "#E4E6F2",
      zeroline = FALSE
    ),
    if (i < 5) list(matches = "x5") else list(
      rangeslider = list(
        visible = TRUE,
        range = c(full_start, full_end),
        thickness = 0.07,
        bgcolor = "#F0F1F8",
        bordercolor = "#D9DCEB",
        borderwidth = 1,
        yaxis = list(rangemode = "auto")
      )
    )
  )
  
  axes[[paste0("yaxis", id)]] <- list(
    domain = domains[[i]],
    anchor = paste0("x", id),
    range = inflation_range(z$inflation[selected]),
    title = "YoY (%)",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#8E9BAE"
  )
  
  annotations[[i]] <- list(
    x = 0.08,
    y = max(domains[[i]]) + 0.024,
    xref = "paper",
    yref = "paper",
    text = paste0("<b>", country_names[i], "</b>"),
    showarrow = FALSE,
    xanchor = "left",
    yanchor = "bottom",
    font = list(size = 13, color = "#000000")
  )
}

p5 <- do.call(plotly::layout, c(
  list(p5), axes,
  list(
    title = list(
      text = "Is inflation stationary?",
      x = 0.5,
      font = list(size = 21, color = "#000000")
    ),
    annotations = annotations,
    hovermode = "closest",
    autosize = TRUE,
    height = 1000,
    margin = list(l = 80, r = 80, t = 110, b = 140),
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor = "#FFFFFF"
  )
))

p5 <- plotly::config(
  p5,
  responsive = TRUE,
  displaylogo = FALSE,
  scrollZoom = FALSE
)


p5 <- htmlwidgets::onRender(
  p5,
  "
  function(el, x, data) {
    var series = el.data.slice(0, 5).map(function(z) {
      return {
        dates: Array.from(z.x, function(d) {
          return new Date(d).getTime();
        }),
        values: Array.from(z.y, Number)
      };
    });

    function updateRanges(range) {
      var start = new Date(range[0]).getTime();
      var end = new Date(range[1]).getTime();
      if (!Number.isFinite(start) || !Number.isFinite(end)) return;

      var updates = {};

      series.forEach(function(z, i) {
        var values = z.values.filter(function(v, j) {
          return z.dates[j] >= start && z.dates[j] <= end &&
            Number.isFinite(v);
        });

        var lo = values.length ? Math.min.apply(null, values) : -1;
        var hi = values.length ? Math.max.apply(null, values) : 1;
        var pad = Math.max(0.08 * (hi - lo), 0.5);
        var axis = i === 0 ? 'yaxis' : 'yaxis' + (i + 1);

        updates[axis + '.range'] = [lo - pad, hi + pad];
      });

      Plotly.relayout(el, updates);
    }

    el.on('plotly_relayout', function(e) {
      var axes = ['xaxis5', 'xaxis', 'xaxis2', 'xaxis3', 'xaxis4'];
      var range = null;

      for (var i = 0; i < axes.length; i++) {
        var a = axes[i];
        if (e[a + '.range']) {
          range = e[a + '.range'];
          break;
        }
        if (e[a + '.range[0]'] !== undefined &&
            e[a + '.range[1]'] !== undefined) {
          range = [e[a + '.range[0]'], e[a + '.range[1]']];
          break;
        }
      }

      if (range) updateRanges(range);
    });

    updateRanges(['2000-01-01', '2019-12-31']);
  }
  "
)

p5
saveRDS(p5, "figures/02/historical-inflation.rds")



# Autocorrelation ----

## From lag scatterplots to autocorrelation ----

set.seed(2026)

n_acf <- 300
rho_values_acf <- c(-0.9, -0.7, -0.5, -0.2, 0, 0.2, 0.5, 0.8, 0.95)
lag_values_acf <- 1:20
acf_lags <- 0:20
rho_start_acf <- which(rho_values_acf == 0.8)
lag_start_acf <- 1

shocks_acf <- rnorm(n_acf)
initial_acf <- rnorm(1)

acf_paths <- lapply(rho_values_acf, function(rho) {
  x <- numeric(n_acf)
  previous <- initial_acf
  
  for (t in seq_len(n_acf)) {
    previous <- rho * previous + sqrt(1 - rho^2) * shocks_acf[t]
    x[t] <- previous
  }
  
  unname(round(x, 5))
})

acf_theoretical <- lapply(rho_values_acf, function(rho) rho^acf_lags)

path0 <- acf_paths[[rho_start_acf]]
xlag0 <- head(path0, -lag_start_acf)
xnow0 <- tail(path0, -lag_start_acf)
fit0 <- lm(xnow0 ~ xlag0)
line_x <- range(xlag0)
line_y <- unname(coef(fit0)[1] + coef(fit0)[2] * line_x)

value_lim <- max(3.5, 1.08 * max(abs(unlist(acf_paths))))
value_range <- c(-value_lim, value_lim)

p6 <- plotly::plot_ly()

p6 <- plotly::add_trace(
  p6,
  x = seq_len(n_acf),
  y = path0,
  type = "scatter",
  mode = "lines",
  xaxis = "x",
  yaxis = "y",
  line = list(color = "#0100CA", width = 1.8),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p6 <- plotly::add_trace(
  p6,
  x = xlag0,
  y = xnow0,
  type = "scattergl",
  mode = "markers",
  xaxis = "x2",
  yaxis = "y2",
  marker = list(color = "rgba(1,0,202,0.25)", size = 5),
  hovertemplate = "X(t-h): %{x:.2f}<br>X(t): %{y:.2f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p6 <- plotly::add_trace(
  p6,
  x = line_x,
  y = line_y,
  type = "scatter",
  mode = "lines",
  xaxis = "x2",
  yaxis = "y2",
  line = list(color = "#C45B40", width = 2.5),
  hoverinfo = "skip",
  showlegend = FALSE,
  inherit = FALSE
)

p6 <- plotly::add_trace(
  p6,
  x = acf_lags,
  y = acf_theoretical[[rho_start_acf]],
  type = "bar",
  xaxis = "x3",
  yaxis = "y3",
  marker = list(color = "#008C95"),
  hovertemplate = "Lag: %{x}<br>ACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p6 <- plotly::add_trace(
  p6,
  x = lag_start_acf,
  y = acf_theoretical[[rho_start_acf]][lag_start_acf + 1],
  type = "scatter",
  mode = "markers",
  xaxis = "x3",
  yaxis = "y3",
  marker = list(
    color = "#C45B40",
    size = 11,
    line = list(color = "#FFFFFF", width = 1.5)
  ),
  hovertemplate = "Selected lag: %{x}<br>ACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

rho_steps_acf <- lapply(rho_values_acf, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

lag_steps_acf <- lapply(lag_values_acf, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

p6 <- plotly::layout(
  p6,
  title = list(
    text = "From lag scatterplots to autocorrelation",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.09, 0.96),
    anchor = "y",
    range = c(1, n_acf),
    title = "Observation",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.72, 0.93),
    anchor = "x",
    range = value_range,
    title = "Value",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.09, 0.46),
    anchor = "y2",
    range = value_range,
    title = "X(t-h)",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zerolinecolor = "#A0A5B5"
  ),
  yaxis2 = list(
    domain = c(0.13, 0.61),
    anchor = "x2",
    range = value_range,
    title = "X(t)",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis3 = list(
    domain = c(0.59, 0.96),
    anchor = "y3",
    range = c(-0.5, 20.5),
    title = "Lag h",
    dtick = 2,
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis3 = list(
    domain = c(0.13, 0.61),
    anchor = "x3",
    range = c(-1.05, 1.05),
    title = "ACF",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.09, y = 0.975,
      xref = "paper", yref = "paper",
      text = "<b>Stationary AR(1) realisation</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.09, y = 0.665,
      xref = "paper", yref = "paper",
      text = "<b>Lag scatterplot and fitted regression</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.59, y = 0.665,
      xref = "paper", yref = "paper",
      text = "<b>Theoretical autocorrelation function</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    )
  ),
  sliders = list(
    list(
      active = rho_start_acf - 1,
      steps = rho_steps_acf,
      currentvalue = list(prefix = "Persistence rho = "),
      x = 0.14, len = 0.72, y = 0.055,
      pad = list(t = 30)
    ),
    list(
      active = lag_start_acf - 1,
      steps = lag_steps_acf,
      currentvalue = list(prefix = "Scatterplot lag h = "),
      x = 0.14, len = 0.72, y = -0.09,
      pad = list(t = 30)
    )
  ),
  autosize = TRUE,
  height = 850,
  margin = list(l = 75, r = 40, t = 95, b = 190),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p6 <- plotly::config(
  p6,
  responsive = TRUE,
  displaylogo = FALSE
)

p6 <- htmlwidgets::onRender(
  p6,
  "
  function(el, x, data) {
    var rhoIndex = data.rhoStart;
    var lagIndex = data.lagStart;

    function updateLag() {
      var path = data.paths[rhoIndex];
      var h = data.lags[lagIndex];
      var acf = data.acf[rhoIndex];

      var xs = path.slice(0, path.length - h);
      var ys = path.slice(h);
      var n = xs.length;

      var mx = 0, my = 0;
      for (var i = 0; i < n; i++) {
        mx += xs[i];
        my += ys[i];
      }
      mx /= n;
      my /= n;

      var sxx = 0, sxy = 0;
      for (var i = 0; i < n; i++) {
        sxx += (xs[i] - mx) * (xs[i] - mx);
        sxy += (xs[i] - mx) * (ys[i] - my);
      }

      var slope = sxy / sxx;
      var intercept = my - slope * mx;
      var xmin = Math.min.apply(null, xs);
      var xmax = Math.max.apply(null, xs);

      Plotly.restyle(
        el,
        {
          x: [xs, [xmin, xmax], [h]],
          y: [
            ys,
            [intercept + slope * xmin, intercept + slope * xmax],
            [acf[h]]
          ]
        },
        [1, 2, 4]
      );
    }

    function updateRho() {
      Plotly.restyle(
        el,
        {y: [data.paths[rhoIndex], data.acf[rhoIndex]]},
        [0, 3]
      );
      updateLag();
    }

    el.on('plotly_sliderchange', function(e) {
      var label = String(e.step.label);
      var r = data.rhoLabels.indexOf(label);
      var h = data.lagLabels.indexOf(label);

      if (r >= 0) {
        rhoIndex = r;
        updateRho();
      } else if (h >= 0) {
        lagIndex = h;
        updateLag();
      }
    });

    updateRho();
  }
  ",
  data = list(
    paths = acf_paths,
    acf = acf_theoretical,
    lags = lag_values_acf,
    rhoLabels = as.character(rho_values_acf),
    lagLabels = as.character(lag_values_acf),
    rhoStart = rho_start_acf - 1L,
    lagStart = lag_start_acf - 1L
  )
)

p6
saveRDS(p6, "figures/02/lag-scatter-acf.rds")


## Empirical autocorrelation ----

### Simulations ----

set.seed(2026)

n_series_acf <- 300
n_paths_acf <- 100
lag_max_acf <- 20
lags_acf <- 1:lag_max_acf

rho_values_emp <- rho_values_acf
rho_start_emp <- which(rho_values_emp == 0.8)

innov_wn <- matrix(rnorm(n_series_acf * n_paths_acf), nrow = n_series_acf)
innov_ar <- matrix(rnorm(n_series_acf * n_paths_acf), nrow = n_series_acf)
initials_ar <- rnorm(n_paths_acf)

emp_acf <- function(x) {
  as.numeric(acf(x, plot = FALSE, lag.max = lag_max_acf)$acf)[-1]
}

wn_initial <- innov_wn[, 1]
rho_initial <- rho_values_emp[rho_start_emp]
ar_initial <- numeric(n_series_acf)
previous <- initials_ar[1]

for (t in seq_len(n_series_acf)) {
  previous <- rho_initial * previous +
    sqrt(1 - rho_initial^2) * innov_ar[t, 1]
  ar_initial[t] <- previous
}

# Keep the original common vertical scale across all persistence values.
max_abs <- max(abs(innov_wn))

for (rho in rho_values_emp) {
  previous <- initials_ar
  sd_innov <- sqrt(1 - rho^2)
  
  for (t in seq_len(n_series_acf)) {
    previous <- rho * previous + sd_innov * innov_ar[t, ]
    max_abs <- max(max_abs, abs(previous))
  }
}

traj_lim <- max(3.5, 1.08 * max_abs)
traj_range <- c(-traj_lim, traj_lim)

### Binary simulation asset ----

asset_dir <- "figures/02/empirical-acf-assets"
dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

# R matrices are column-major: one complete path follows another.
# Store white-noise innovations, AR(1) innovations, then initial states.
values <- c(innov_wn, innov_ar, initials_ar)
con <- file(file.path(asset_dir, "innovations.bin"), "wb")
writeBin(values, con, size = 4, endian = "little")
close(con)

writeLines(
  'window.drpEmpiricalAcf02Base = new URL(".", document.currentScript.src).href;',
  file.path(asset_dir, "loader.js"),
  useBytes = TRUE
)

emp_acf_dep <- htmltools::htmlDependency(
  name = "drp-empirical-acf-02",
  version = "1.0.0",
  src = c(file = normalizePath(asset_dir, winslash = "/")),
  script = "loader.js",
  all_files = TRUE
)

### Plot ----

p7 <- plotly::plot_ly()

p7 <- plotly::add_trace(
  p7,
  x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x", yaxis = "y",
  line = list(color = "rgba(1,0,202,0.08)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p7 <- plotly::add_trace(
  p7,
  x = seq_len(n_series_acf), y = wn_initial,
  type = "scatter", mode = "lines",
  xaxis = "x", yaxis = "y",
  line = list(color = "#0100CA", width = 1.8),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

p7 <- plotly::add_trace(
  p7,
  x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  line = list(color = "rgba(196,91,64,0.08)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p7 <- plotly::add_trace(
  p7,
  x = seq_len(n_series_acf), y = ar_initial,
  type = "scatter", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  line = list(color = "#C45B40", width = 1.8),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

p7 <- plotly::add_trace(
  p7,
  x = numeric(0), y = numeric(0),
  type = "scatter", mode = "lines",
  xaxis = "x3", yaxis = "y3",
  line = list(color = "rgba(1,0,202,0.10)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p7 <- plotly::add_trace(
  p7,
  x = lags_acf, y = emp_acf(wn_initial),
  type = "scatter", mode = "lines+markers",
  xaxis = "x3", yaxis = "y3",
  line = list(color = "#0100CA", width = 2),
  marker = list(color = "#0100CA", size = 5),
  hovertemplate = "Lag: %{x}<br>Empirical ACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

p7 <- plotly::add_trace(
  p7,
  x = numeric(0), y = numeric(0),
  type = "scatter", mode = "lines",
  xaxis = "x4", yaxis = "y4",
  line = list(color = "rgba(196,91,64,0.10)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p7 <- plotly::add_trace(
  p7,
  x = lags_acf, y = emp_acf(ar_initial),
  type = "scatter", mode = "lines+markers",
  xaxis = "x4", yaxis = "y4",
  line = list(color = "#C45B40", width = 2),
  marker = list(color = "#C45B40", size = 5),
  hovertemplate = "Lag: %{x}<br>Empirical ACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

rho_steps_emp <- lapply(rho_values_emp, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

mode_buttons_emp <- lapply(c("One realisation", "All realisations"), function(z) {
  list(label = z, method = "skip", args = list())
})

next_button_emp <- list(
  list(label = "Next realisation", method = "skip", args = list())
)

p7 <- plotly::layout(
  p7,
  title = list(
    text = "Sampling variability of the empirical autocorrelation function",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.08, 0.46), anchor = "y",
    range = c(1, n_series_acf), title = "",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.62, 0.92), anchor = "x",
    range = traj_range, title = "Value", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.58, 0.96), anchor = "y2",
    range = c(1, n_series_acf), title = "",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis2 = list(
    domain = c(0.62, 0.92), anchor = "x2",
    range = traj_range, title = "Value", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis3 = list(
    domain = c(0.08, 0.46), anchor = "y3",
    range = c(1, lag_max_acf), dtick = 2,
    title = "Lag h", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis3 = list(
    domain = c(0.13, 0.47), anchor = "x3",
    range = c(-1.05, 1.05), title = "Empirical ACF",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis4 = list(
    domain = c(0.58, 0.96), anchor = "y4",
    range = c(1, lag_max_acf), dtick = 2,
    title = "Lag h", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis4 = list(
    domain = c(0.13, 0.47), anchor = "x4",
    range = c(-1.05, 1.05), title = "Empirical ACF",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.08, y = 0.965, xref = "paper", yref = "paper",
      text = "<b>Gaussian white noise</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.58, y = 0.965, xref = "paper", yref = "paper",
      text = "<b>Stationary AR(1)</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.08, y = 0.53, xref = "paper", yref = "paper",
      text = "<b>Empirical autocorrelation</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.58, y = 0.53, xref = "paper", yref = "paper",
      text = "<b>Empirical autocorrelation</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    )
  ),
  sliders = list(list(
    active = rho_start_emp - 1,
    steps = rho_steps_emp,
    currentvalue = list(prefix = "Persistence rho = "),
    x = 0.14, len = 0.72, y = 0.045,
    pad = list(t = 30)
  )),
  updatemenus = list(
    list(
      type = "buttons", direction = "right",
      buttons = mode_buttons_emp, active = 0,
      showactive = TRUE, x = 0.28, xanchor = "center",
      y = -0.2, yanchor = "middle"
    ),
    list(
      type = "buttons", direction = "right",
      buttons = next_button_emp, active = -1,
      showactive = FALSE, x = 0.74, xanchor = "center",
      y = -0.2, yanchor = "middle"
    )
  ),
  autosize = TRUE,
  height = 850,
  margin = list(l = 75, r = 40, t = 95, b = 170),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p7 <- plotly::config(
  p7,
  responsive = TRUE,
  displaylogo = FALSE
)

### Interactivity ----

p7$dependencies <- c(p7$dependencies, list(emp_acf_dep))

p7 <- htmlwidgets::onRender(
  p7,
  "
  function(el, x, data) {
    var n = data.nSeries;
    var nPaths = data.nPaths;
    var lagMax = data.lagMax;
    var blockSize = n * nPaths;
    var rhoIndex = data.rhoStart;
    var highlight = 0;
    var showAll = false;
    var raw = null;
    var wnPaths = null;
    var wnAcf = null;
    var arCache = new Array(data.rhoLabels.length);
    var times = Array.from({length: n}, function(_, i) { return i + 1; });
    var lags = Array.from({length: lagMax}, function(_, i) { return i + 1; });

    function sampleAcf(series) {
      var mean = 0;
      for (var i = 0; i < n; i++) mean += series[i];
      mean /= n;

      var centered = new Array(n);
      var denom = 0;
      for (var i = 0; i < n; i++) {
        var d = series[i] - mean;
        centered[i] = d;
        denom += d * d;
      }

      var result = new Array(lagMax);
      for (var h = 1; h <= lagMax; h++) {
        var numer = 0;
        for (var t = h; t < n; t++) {
          numer += centered[t] * centered[t - h];
        }
        result[h - 1] = denom ? numer / denom : 0;
      }
      return result;
    }

    function buildWhiteNoise() {
      wnPaths = new Array(nPaths);
      wnAcf = new Array(nPaths);
      for (var j = 0; j < nPaths; j++) {
        var path = new Array(n);
        for (var t = 0; t < n; t++) {
          path[t] = raw[j * n + t];
        }
        wnPaths[j] = path;
        wnAcf[j] = sampleAcf(path);
      }
    }

    function getAr(r) {
      if (arCache[r]) return arCache[r];

      var rho = Number(data.rhoLabels[r]);
      var sigma = Math.sqrt(1 - rho * rho);
      var paths = new Array(nPaths);
      var acfs = new Array(nPaths);

      for (var j = 0; j < nPaths; j++) {
        var state = raw[2 * blockSize + j];
        var path = new Array(n);
        var offset = blockSize + j * n;

        for (var t = 0; t < n; t++) {
          state = rho * state + sigma * raw[offset + t];
          path[t] = state;
        }

        paths[j] = path;
        acfs[j] = sampleAcf(path);
      }

      arCache[r] = {paths: paths, acfs: acfs};
      return arCache[r];
    }

    function stackSeries(series, xvals, omit) {
      var xs = [];
      var ys = [];

      for (var j = 0; j < series.length; j++) {
        if (j === omit) continue;
        for (var i = 0; i < xvals.length; i++) {
          xs.push(xvals[i]);
          ys.push(series[j][i]);
        }
        xs.push(null);
        ys.push(null);
      }

      return {x: xs, y: ys};
    }

    function updatePlot() {
      if (!raw) return;

      var ar = getAr(rhoIndex);
      var wnBg = showAll
        ? stackSeries(wnPaths, times, highlight) : {x: [], y: []};
      var arBg = showAll
        ? stackSeries(ar.paths, times, highlight) : {x: [], y: []};
      var wnAcfBg = showAll
        ? stackSeries(wnAcf, lags, highlight) : {x: [], y: []};
      var arAcfBg = showAll
        ? stackSeries(ar.acfs, lags, highlight) : {x: [], y: []};

      Plotly.update(
        el,
        {
          x: [wnBg.x, times, arBg.x, times, wnAcfBg.x, lags, arAcfBg.x, lags],
          y: [
            wnBg.y, wnPaths[highlight],
            arBg.y, ar.paths[highlight],
            wnAcfBg.y, wnAcf[highlight],
            arAcfBg.y, ar.acfs[highlight]
          ]
        },
        {},
        [0, 1, 2, 3, 4, 5, 6, 7]
      );
    }

    el.on('plotly_sliderchange', function(e) {
      var r = data.rhoLabels.indexOf(String(e.step.label));
      if (r >= 0) {
        rhoIndex = r;
        updatePlot();
      }
    });

    el.on('plotly_buttonclicked', function(e) {
      if (e.button.label === 'One realisation') showAll = false;
      if (e.button.label === 'All realisations') showAll = true;
      if (e.button.label === 'Next realisation') {
        highlight = (highlight + 1) % nPaths;
      }
      updatePlot();
    });

    if (!window.drpEmpiricalAcf02Base) {
      console.error('Figure 7: simulation dependency was not loaded.');
      return;
    }

    fetch(new URL('innovations.bin', window.drpEmpiricalAcf02Base))
      .then(function(response) {
        if (!response.ok) throw new Error('HTTP ' + response.status);
        return response.arrayBuffer();
      })
      .then(function(buffer) {
        var expected = 4 * (2 * blockSize + nPaths);
        if (buffer.byteLength !== expected) {
          throw new Error('Unexpected binary size: ' + buffer.byteLength);
        }

        // Explicit little-endian decoding matches writeBin() in R.
        var view = new DataView(buffer);
        raw = new Float32Array(expected / 4);
        for (var i = 0; i < raw.length; i++) {
          raw[i] = view.getFloat32(4 * i, true);
        }

        buildWhiteNoise();
        updatePlot();
      })
      .catch(function(error) {
        console.error('Figure 7: could not load innovations.bin', error);
      });
  }
  ",
  data = list(
    nSeries = n_series_acf,
    nPaths = n_paths_acf,
    lagMax = lag_max_acf,
    rhoLabels = as.character(rho_values_emp),
    rhoStart = rho_start_emp - 1L
  )
)

p7
saveRDS(p7, "figures/02/empirical-autocorrelation.rds")


## Partial autocorrelation ----

rho_values_pacf <- rho_values_acf
rho_start_pacf <- which(rho_values_pacf == 0.8)
rho0 <- rho_values_pacf[rho_start_pacf]

acf_lags_pacf <- 0:20
pacf_lags <- 1:20

acf_values <- rho0^acf_lags_pacf
pacf_values <- c(rho0, rep(0, 19))

stems <- function(h, z) {
  list(
    x = as.vector(rbind(h, h, NA_real_)),
    y = as.vector(rbind(0, z, NA_real_))
  )
}

acf_stems <- stems(acf_lags_pacf, acf_values)
pacf_stems <- stems(pacf_lags, pacf_values)

p8 <- plotly::plot_ly()

p8 <- plotly::add_trace(
  p8,
  x = acf_stems$x,
  y = acf_stems$y,
  type = "scatter",
  mode = "lines",
  xaxis = "x",
  yaxis = "y",
  line = list(color = "#008C95", width = 2.5),
  hoverinfo = "skip",
  showlegend = FALSE,
  inherit = FALSE
)

p8 <- plotly::add_trace(
  p8,
  x = acf_lags_pacf,
  y = acf_values,
  type = "scatter",
  mode = "markers",
  xaxis = "x",
  yaxis = "y",
  marker = list(color = "#008C95", size = 6),
  hovertemplate = "Lag: %{x}<br>ACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

p8 <- plotly::add_trace(
  p8,
  x = pacf_stems$x,
  y = pacf_stems$y,
  type = "scatter",
  mode = "lines",
  xaxis = "x2",
  yaxis = "y2",
  line = list(color = "#C45B40", width = 2.5),
  hoverinfo = "skip",
  showlegend = FALSE,
  inherit = FALSE
)

p8 <- plotly::add_trace(
  p8,
  x = pacf_lags,
  y = pacf_values,
  type = "scatter",
  mode = "markers",
  xaxis = "x2",
  yaxis = "y2",
  marker = list(color = "#C45B40", size = 6),
  hovertemplate = "Lag: %{x}<br>PACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE,
  inherit = FALSE
)

rho_steps_pacf <- lapply(rho_values_pacf, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

p8 <- plotly::layout(
  p8,
  title = list(
    text = "Autocorrelation versus partial autocorrelation",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.10, 0.96),
    anchor = "y",
    range = c(-0.5, 20.5),
    dtick = 2,
    title = "",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.59, 0.93),
    anchor = "x",
    range = c(-1.05, 1.05),
    title = "ACF",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.10, 0.96),
    anchor = "y2",
    range = c(0.5, 20.5),
    tickmode = "array",
    tickvals = c(1, seq(2, 20, by = 2)),
    title = "Lag h",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis2 = list(
    domain = c(0.14, 0.47),
    anchor = "x2",
    range = c(-1.05, 1.05),
    title = "PACF",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.10, y = 0.975,
      xref = "paper", yref = "paper",
      text = "<b>Theoretical autocorrelation function</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.10, y = 0.515,
      xref = "paper", yref = "paper",
      text = "<b>Theoretical partial autocorrelation function</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    )
  ),
  sliders = list(list(
    active = rho_start_pacf - 1,
    steps = rho_steps_pacf,
    currentvalue = list(prefix = "Persistence rho = "),
    x = 0.14,
    len = 0.72,
    y = 0.025,
    pad = list(t = 30)
  )),
  autosize = TRUE,
  height = 780,
  margin = list(l = 80, r = 40, t = 95, b = 145),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p8 <- plotly::config(
  p8,
  responsive = TRUE,
  displaylogo = FALSE
)

p8 <- htmlwidgets::onRender(
  p8,
  "
  function(el, x, data) {

    function updatePlot(rho) {
      var acf = [];
      var pacf = [];
      var acfStems = [];
      var pacfStems = [];

      for (var h = 0; h <= 20; h++) {
        var v = Math.pow(rho, h);
        acf.push(v);
        acfStems.push(0, v, null);
      }

      for (var h = 1; h <= 20; h++) {
        var v = h === 1 ? rho : 0;
        pacf.push(v);
        pacfStems.push(0, v, null);
      }

      Plotly.restyle(
        el,
        {
          y: [acfStems, acf, pacfStems, pacf]
        },
        [0, 1, 2, 3]
      );
    }

    el.on('plotly_sliderchange', function(e) {
      updatePlot(Number(e.step.label));
    });
  }
  "
)

p8
saveRDS(p8, "figures/02/theoretical-acf-pacf.rds")



## Persistent stationarity versus random walk ----

n_values_rwacf <- seq(100, 1000, by = 100)
n_initial_rwacf <- n_values_rwacf[1]
n_paths_rwacf <- ncol(paired_paths[[1]])
lags_rwacf <- 1:20

rwacf_paths <- lapply(paired_paths, function(z) {
  z[seq_len(max(n_values_rwacf)), , drop = FALSE]
})

rwacf_estimates <- lapply(rwacf_paths, function(z) {
  lapply(n_values_rwacf, function(n_i) {
    lapply(seq_len(ncol(z)), function(j) {
      unname(round(
        as.numeric(acf(z[seq_len(n_i), j], lag.max = 20, plot = FALSE)$acf[-1]),
        5
      ))
    })
  })
})

rwacf_series <- lapply(rwacf_paths, function(z) {
  lapply(seq_len(ncol(z)), function(j) {
    unname(round(z[, j], 4))
  })
})

asset_dir <- "figures/02/persistent-rw-acf-assets"
dir.create(asset_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(
  paste0(
    "window.drpPersistentRW02 = ",
    jsonlite::toJSON(
      list(paths = rwacf_series, acfs = rwacf_estimates),
      auto_unbox = TRUE, digits = NA
    ),
    ";"
  ),
  file.path(asset_dir, "persistent-rw-acf-data.js"),
  useBytes = TRUE
)

rwacf_dep <- htmltools::htmlDependency(
  name = "drp-persistent-rw-acf",
  version = "1.0.0",
  src = c(file = normalizePath(asset_dir, winslash = "/")),
  script = "persistent-rw-acf-data.js",
  all_files = FALSE
)

rwacf_colors <- c("#0100CA", "#C45B40")
rwacf_faint <- c(
  "rgba(1,0,202,0.08)",
  "rgba(196,91,64,0.08)"
)

initial_range <- axis_range(c(
  rwacf_paths[[1]][seq_len(n_initial_rwacf), n_paths_rwacf],
  rwacf_paths[[2]][seq_len(n_initial_rwacf), n_paths_rwacf]
))

p9 <- plotly::plot_ly()

for (i in 1:2) {
  id <- if (i == 1) "" else as.character(i)
  
  p9 <- plotly::add_trace(
    p9,
    x = numeric(0), y = numeric(0),
    type = "scattergl", mode = "lines",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    line = list(color = rwacf_faint[i], width = 1),
    visible = FALSE, hoverinfo = "skip",
    showlegend = FALSE, inherit = FALSE
  )
  
  p9 <- plotly::add_trace(
    p9,
    x = seq_len(n_initial_rwacf),
    y = rwacf_series[[i]][[n_paths_rwacf]][seq_len(n_initial_rwacf)],
    type = "scatter", mode = "lines",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    line = list(color = rwacf_colors[i], width = 2),
    hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
    showlegend = FALSE, inherit = FALSE
  )
}

for (i in 1:2) {
  id <- as.character(i + 2)
  
  p9 <- plotly::add_trace(
    p9,
    x = numeric(0), y = numeric(0),
    type = "scatter", mode = "lines",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    line = list(color = rwacf_faint[i], width = 1),
    visible = FALSE, hoverinfo = "skip",
    showlegend = FALSE, inherit = FALSE
  )
  
  p9 <- plotly::add_trace(
    p9,
    x = lags_rwacf,
    y = rwacf_estimates[[i]][[1]][[n_paths_rwacf]],
    type = "scatter", mode = "lines+markers",
    xaxis = paste0("x", id),
    yaxis = paste0("y", id),
    line = list(color = rwacf_colors[i], width = 2),
    marker = list(color = rwacf_colors[i], size = 5),
    hovertemplate = "Lag: %{x}<br>Empirical ACF: %{y:.3f}<extra></extra>",
    showlegend = FALSE, inherit = FALSE
  )
}

sample_steps_rwacf <- lapply(n_values_rwacf, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

mode_buttons_rwacf <- lapply(c("One realisation", "All realisations"), function(z) {
  list(label = z, method = "skip", args = list())
})

p9 <- plotly::layout(
  p9,
  title = list(
    text = "Persistent stationarity versus random walk",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.08, 0.46),
    anchor = "y",
    range = c(1, n_initial_rwacf),
    title = "",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.62, 0.92),
    anchor = "x",
    range = initial_range,
    title = "Value",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.58, 0.96),
    anchor = "y2",
    range = c(1, n_initial_rwacf),
    title = "",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis2 = list(
    domain = c(0.62, 0.92),
    anchor = "x2",
    range = initial_range,
    title = "Value",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis3 = list(
    domain = c(0.08, 0.46),
    anchor = "y3",
    range = c(0.5, 20.5),
    dtick = 2,
    title = "Lag h",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis3 = list(
    domain = c(0.13, 0.47),
    anchor = "x3",
    range = c(-1.05, 1.05),
    title = "Empirical ACF",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis4 = list(
    domain = c(0.58, 0.96),
    anchor = "y4",
    range = c(0.5, 20.5),
    dtick = 2,
    title = "Lag h",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis4 = list(
    domain = c(0.13, 0.47),
    anchor = "x4",
    range = c(-1.05, 1.05),
    title = "Empirical ACF",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.08, y = 0.965,
      xref = "paper", yref = "paper",
      text = "<b>Stationary AR(1), rho = 0.995</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.58, y = 0.965,
      xref = "paper", yref = "paper",
      text = "<b>Random walk, rho = 1</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.08, y = 0.53,
      xref = "paper", yref = "paper",
      text = "<b>Empirical ACF — stationary AR(1)</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.58, y = 0.53,
      xref = "paper", yref = "paper",
      text = "<b>Empirical ACF — random walk</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    )
  ),
  sliders = list(list(
    active = 0,
    steps = sample_steps_rwacf,
    currentvalue = list(prefix = "Sample size n = "),
    x = 0.14, len = 0.72, y = 0.045,
    pad = list(t = 30)
  )),
  updatemenus = list(list(
    type = "buttons",
    direction = "right",
    buttons = mode_buttons_rwacf,
    active = 0,
    showactive = TRUE,
    x = 0.5, xanchor = "center",
    y = -0.20, yanchor = "middle"
  )),
  autosize = TRUE,
  height = 850,
  margin = list(l = 75, r = 40, t = 95, b = 190),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF"
)

p9 <- plotly::config(
  p9,
  responsive = TRUE,
  displaylogo = FALSE
)

p9$dependencies <- c(p9$dependencies, list(rwacf_dep))

p9 <- htmlwidgets::onRender(
  p9,
  "
  function(el, x, data) {
    var shared = window.drpPersistentRW02;

    if (!shared) {
      console.error('Persistent AR(1) / RW data could not be loaded.');
      return;
    }

    var nIndex = 0;
    var showAll = false;
    var highlight = data.nPaths - 1;
    var indices = [0, 1, 2, 3, 4, 5, 6, 7];

    function stackSeries(series, xvals, omit) {
      var xs = [], ys = [];

      for (var j = 0; j < series.length; j++) {
        if (j === omit) continue;

        for (var t = 0; t < xvals.length; t++) {
          xs.push(xvals[t]);
          ys.push(series[j][t]);
        }

        xs.push(null);
        ys.push(null);
      }

      return {x: xs, y: ys};
    }

    function updatePlot() {
      var n = data.nValues[nIndex];
      var times = Array.from(
        {length: n},
        function(_, i) { return i + 1; }
      );

      var xs = [], ys = [], visible = [];
      var lo = Infinity, hi = -Infinity;

      for (var i = 0; i < 2; i++) {
        var paths = shared.paths[i];
        var start = showAll ? 0 : highlight;

        for (var j = start; j < paths.length; j++) {
          for (var t = 0; t < n; t++) {
            var v = paths[j][t];
            if (v < lo) lo = v;
            if (v > hi) hi = v;
          }
        }

        var bg = showAll
          ? stackSeries(
              paths.map(function(p) { return p.slice(0, n); }),
              times,
              highlight
            )
          : {x: [], y: []};

        xs.push(bg.x, times);
        ys.push(bg.y, paths[highlight].slice(0, n));
        visible.push(showAll, true);
      }

      for (var i = 0; i < 2; i++) {
        var estimates = shared.acfs[i][nIndex];

        var bg = showAll
          ? stackSeries(estimates, data.lags, highlight)
          : {x: [], y: []};

        xs.push(bg.x, data.lags);
        ys.push(bg.y, estimates[highlight]);
        visible.push(showAll, true);
      }

      var pad = Math.max(0.08 * (hi - lo), 0.1);
      var yrange = [lo - pad, hi + pad];

      Plotly.update(
        el,
        {x: xs, y: ys, visible: visible},
        {
          'xaxis.range': [1, n],
          'xaxis2.range': [1, n],
          'yaxis.range': yrange,
          'yaxis2.range': yrange
        },
        indices
      );
    }

    el.on('plotly_sliderchange', function(e) {
      var n = data.nValues.indexOf(Number(e.step.label));

      if (n >= 0) {
        nIndex = n;
        updatePlot();
      }
    });

    el.on('plotly_buttonclicked', function(e) {
      showAll = e.button.label === 'All realisations';
      updatePlot();
    });

    updatePlot();
  }
  ",
  data = list(
    nValues = n_values_rwacf,
    nPaths = n_paths_rwacf,
    lags = lags_rwacf
  )
)

p9
saveRDS(p9, "figures/02/persistent-ar1-rw-acf.rds")


## Autocorrelation in economic and financial data ----

uk <- inflation_data[["United Kingdom"]]
uk <- subset(uk, date >= "1984-01-01")
uk <- data.frame(Date = as.Date(uk$date), Value = uk$inflation)

spy_raw <- quantmod::getSymbols(
  "SPY", src = "yahoo", from = "2018-01-02",
  to = Sys.Date(), auto.assign = FALSE
)

spy_returns <- na.omit(100 * diff(log(quantmod::Ad(spy_raw))))
spy <- data.frame(
  Date = as.Date(zoo::index(spy_returns)),
  Value = as.numeric(spy_returns)
)

stopifnot(
  identical(uk$Date, seq(min(uk$Date), max(uk$Date), by = "month")),
  nrow(spy) > 20
)

uk_sq <- transform(uk, Value = (Value - mean(Value))^2)
spy_sq <- transform(spy, Value = (Value - mean(Value))^2)

economic_series <- list(uk, uk_sq, spy, spy_sq)

series_titles <- c(
  "UK inflation",
  "UK inflation — squared deviations",
  "SPY daily log returns",
  "SPY returns — squared deviations"
)

series_units <- c(
  "Inflation (%)",
  "Squared deviation (%²)",
  "Return (%)",
  "Squared deviation (%²)"
)

series_colors <- c("#687589", "#687589", "#0100CA", "#0100CA")
lag_units <- c("Months", "Months", "Trading days", "Trading days")
date_breaks <- c("8 years", "8 years", "2 years", "2 years")

plot_series <- function(z, title, units, color, breaks) {
  ggplot(z, aes(Date, Value)) +
    geom_hline(yintercept = 0, color = "grey75", linewidth = 0.3) +
    geom_line(color = color, linewidth = 0.45) +
    scale_x_date(date_breaks = breaks, date_labels = "%Y") +
    labs(title = title, x = NULL, y = units) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank()
    )
}

plot_acf <- function(x, units, color) {
  estimates <- as.numeric(acf(x, lag.max = 20, plot = FALSE)$acf)[-1]
  z <- data.frame(Lag = 1:20, ACF = estimates)
  bound <- 1.96 / sqrt(length(x))
  
  ggplot(z, aes(Lag, ACF)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
    geom_hline(
      yintercept = c(-bound, bound),
      color = "#C45B40", linetype = "dashed", linewidth = 0.55
    ) +
    geom_segment(aes(xend = Lag, yend = 0), color = color, linewidth = 0.75) +
    geom_point(color = color, size = 1.5) +
    scale_x_continuous(breaks = seq(0, 20, by = 5)) +
    coord_cartesian(xlim = c(0.5, 20.5), ylim = c(-1, 1)) +
    labs(title = "", x = paste("Lag (", units, ")", sep = ""), y = "ACF") +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank()
    )
}

panels <- unlist(lapply(seq_along(economic_series), function(i) {
  z <- economic_series[[i]]
  
  list(
    plot_series(
      z, series_titles[i], series_units[i],
      series_colors[i], date_breaks[i]
    ),
    plot_acf(z$Value, lag_units[i], series_colors[i])
  )
}), recursive = FALSE)

p10 <- patchwork::wrap_plots(
  plotlist = panels,
  ncol = 2,
  widths = c(1.45, 1)
) +
  patchwork::plot_annotation(
    title = "Autocorrelation in economic and financial data",
    subtitle = "Observed series and empirical ACFs | Dashed lines: approximate 95% white-noise reference bounds"
  )

p10
saveRDS(p10, "figures/02/empirical-acf.rds")



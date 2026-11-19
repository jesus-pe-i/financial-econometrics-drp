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
    margin = list(l = 25, r = 35, t = 90, b = 150),
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
  margin = list(l = 45, r = 45, t = 95, b = 80),
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
  margin = list(l = 40, r = 40, t = 95, b = 140),
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
  margin = list(l = 40, r = 45, t = 95, b = 100),
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
    margin = list(l = 75, r = 80, t = 110, b = 90),
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
  margin = list(l = 35, r = 40, t = 95, b = 120),
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
  margin = list(l = 35, r = 40, t = 95, b = 120),
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
  margin = list(l = 35, r = 40, t = 95, b = 120),
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
  margin = list(l = 35, r = 40, t = 95, b = 100),
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

# Mean modelling ----

## Autoregressive models ----

### Simulation setup ----

set.seed(2026)

ar_n <- 1000
ar_burn <- 600
ar_paths <- 100
ar_initial_n <- 300
ar_lag_max <- 20
ar_initial_p <- 1L
ar_initial_strength <- 0.80
ar_initial_structure <- "Recent lags"
ar_structures <- c("Recent lags", "Distributed lags", "Distant lag")

# Signed strength is the AR(1) coefficient for p = 1.
# For p > 1, the structure determines where the dependence is located.
# All three families satisfy sum(abs(phi)) = abs(strength) < 1,
# ensuring causal stationarity without requiring students to tune roots.
ar_coefficients <- function(p, strength, structure) {
  stopifnot(
    p %in% seq_len(10),
    abs(strength) < 1,
    structure %in% ar_structures
  )
  
  if (p == 1L) return(strength)
  
  phi <- numeric(p)
  
  if (structure == "Recent lags") {
    tail_weights <- 0.60^(seq_len(p - 1L) - 1L)
    phi[1] <- 0.94 * strength
    phi[-1] <- 0.06 * strength * tail_weights / sum(tail_weights)
  } else if (structure == "Distributed lags") {
    weights <- 0.90^(seq_len(p) - 1L)
    weights <- weights / sum(weights)
    signs <- if (strength >= 0) rep(1, p) else (-1)^seq_len(p)
    phi <- abs(strength) * weights * signs
  } else {
    phi[p] <- strength
  }
  
  phi
}

ar_innov <- matrix(
  rnorm((ar_n + ar_burn) * ar_paths),
  nrow = ar_n + ar_burn
)

ar_generate <- function(z, phi) {
  p <- length(phi)
  x <- numeric(length(z))
  
  for (t in seq_along(z)) {
    lagged <- if (t > 1L) {
      j <- seq_len(min(p, t - 1L))
      sum(phi[j] * x[t - j])
    } else {
      0
    }
    x[t] <- lagged + z[t]
  }
  
  tail(x, ar_n)
}

ar_initial <- ar_generate(
  ar_innov[, 1],
  ar_coefficients(ar_initial_p, ar_initial_strength, ar_initial_structure)
)[seq_len(ar_initial_n)]

ar_sample_acf <- function(x) {
  as.numeric(stats::acf(x, lag.max = ar_lag_max, plot = FALSE)$acf)[-1]
}

ar_sample_pacf <- function(x) {
  as.numeric(stats::pacf(x, lag.max = ar_lag_max, plot = FALSE)$acf)
}

### Binary simulation asset ----

ar_asset_dir <- "figures/02/ar-explorer-assets"
dir.create(ar_asset_dir, recursive = TRUE, showWarnings = FALSE)

ar_con <- file(file.path(ar_asset_dir, "innovations.bin"), "wb")
writeBin(as.numeric(ar_innov), ar_con, size = 4, endian = "little")
close(ar_con)

writeLines(
  'window.drpARExplorer02Base = new URL(".", document.currentScript.src).href;',
  file.path(ar_asset_dir, "loader.js"),
  useBytes = TRUE
)

ar_dependency <- htmltools::htmlDependency(
  name = "drp-ar-explorer-02",
  version = "1.1.0",
  src = c(file = normalizePath(ar_asset_dir, winslash = "/")),
  script = "loader.js",
  all_files = TRUE
)

### Figure ----

ar_lags <- seq_len(ar_lag_max)

ar_stems <- function(values) {
  list(
    x = as.vector(rbind(ar_lags, ar_lags, NA_real_)),
    y = as.vector(rbind(0, values, NA_real_))
  )
}

ar_acf0 <- ar_sample_acf(ar_initial)
ar_pacf0 <- ar_sample_pacf(ar_initial)
ar_acf_stems <- ar_stems(ar_acf0)
ar_pacf_stems <- ar_stems(ar_pacf0)

p11 <- plotly::plot_ly()

# One faint background trace and one highlighted trace for the trajectory.
p11 <- plotly::add_trace(
  p11, x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x", yaxis = "y",
  line = list(color = "rgba(1,0,202,0.02)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p11 <- plotly::add_trace(
  p11, x = seq_len(ar_initial_n), y = ar_initial,
  type = "scatter", mode = "lines",
  xaxis = "x", yaxis = "y",
  line = list(color = "#0100CA", width = 2),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

# ACF: faint background stems, highlighted stems, highlighted markers.
p11 <- plotly::add_trace(
  p11, x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  line = list(color = "rgba(0,140,149,0.08)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p11 <- plotly::add_trace(
  p11, x = ar_acf_stems$x, y = ar_acf_stems$y,
  type = "scatter", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  line = list(color = "#008C95", width = 2.1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p11 <- plotly::add_trace(
  p11, x = ar_lags, y = ar_acf0,
  type = "scatter", mode = "markers",
  xaxis = "x2", yaxis = "y2",
  marker = list(color = "#008C95", size = 5),
  hovertemplate = "Lag: %{x}<br>ACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

# PACF: faint background stems, highlighted stems, highlighted markers.
p11 <- plotly::add_trace(
  p11, x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x3", yaxis = "y3",
  line = list(color = "rgba(196,91,64,0.08)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p11 <- plotly::add_trace(
  p11, x = ar_pacf_stems$x, y = ar_pacf_stems$y,
  type = "scatter", mode = "lines",
  xaxis = "x3", yaxis = "y3",
  line = list(color = "#C45B40", width = 2.1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p11 <- plotly::add_trace(
  p11, x = ar_lags, y = ar_pacf0,
  type = "scatter", mode = "markers",
  xaxis = "x3", yaxis = "y3",
  marker = list(color = "#C45B40", size = 5),
  hovertemplate = "Lag: %{x}<br>PACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

ar_strength_values <- seq(-0.95, 0.95, by = 0.05)
ar_sample_sizes <- seq(100, ar_n, by = 100)

ar_strength_steps <- lapply(ar_strength_values, function(z) {
  list(label = sprintf("%.2f", z), method = "skip", args = list())
})

ar_size_steps <- lapply(ar_sample_sizes, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

ar_order_buttons <- lapply(seq_len(10), function(p) {
  list(label = paste0("AR(", p, ")"), method = "skip", args = list())
})

ar_structure_buttons <- lapply(ar_structures, function(z) {
  list(label = z, method = "skip", args = list())
})

ar_mode_buttons <- lapply(c("One realisation", "All realisations"), function(z) {
  list(label = z, method = "skip", args = list())
})

ar_initial_range <- axis_range(ar_initial)

p11 <- plotly::layout(
  p11,
  title = list(
    text = "Autoregressive models: trajectories and empirical dependence",
    x = 0.5, font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.09, 0.97), anchor = "y",
    range = c(1, ar_initial_n), title = "Observation",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.59, 0.93), anchor = "x",
    range = ar_initial_range, title = "Value",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.09, 0.47), anchor = "y2",
    range = c(0.5, ar_lag_max + 0.5), dtick = 2,
    title = "Lag h", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis2 = list(
    domain = c(0.14, 0.46), anchor = "x2",
    range = c(-1.05, 1.05), title = "ACF",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  xaxis3 = list(
    domain = c(0.59, 0.97), anchor = "y3",
    range = c(0.5, ar_lag_max + 0.5), dtick = 2,
    title = "Lag h", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis3 = list(
    domain = c(0.14, 0.46), anchor = "x3",
    range = c(-1.05, 1.05), title = "PACF",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.09, y = 0.985, xref = "paper", yref = "paper",
      text = "<b>Simulated trajectory</b>", showarrow = FALSE,
      xanchor = "left", font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.09, y = 0.52, xref = "paper", yref = "paper",
      text = "<b>Empirical ACF</b>", showarrow = FALSE,
      xanchor = "left", font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.59, y = 0.52, xref = "paper", yref = "paper",
      text = "<b>Empirical PACF</b>", showarrow = FALSE,
      xanchor = "left", font = list(size = 13, color = "#000000")
    )
  ),
  sliders = list(
    list(
      active = which(abs(ar_strength_values - ar_initial_strength) < 1e-8) - 1,
      steps = ar_strength_steps,
      currentvalue = list(prefix = "Signed coefficient strength = "),
      x = 0.12, len = 0.77, y = 0.01, pad = list(t = 25)
    ),
    list(
      active = which(ar_sample_sizes == ar_initial_n) - 1,
      steps = ar_size_steps,
      currentvalue = list(prefix = "Sample size n = "),
      x = 0.12, len = 0.77, y = -0.16, pad = list(t = 25)
    )
  ),
  updatemenus = list(
    list(
      type = "dropdown", buttons = ar_order_buttons,
      active = ar_initial_p - 1, showactive = TRUE,
      x = 0.10, xanchor = "left", y = 1.05, yanchor = "middle"
    ),
    list(
      type = "dropdown", buttons = ar_structure_buttons,
      active = match(ar_initial_structure, ar_structures) - 1,
      showactive = TRUE,
      x = 0.24, xanchor = "left", y = 1.05, yanchor = "middle"
    ),
    list(
      type = "buttons", direction = "right",
      buttons = ar_mode_buttons,
      active = 0, showactive = TRUE,
      x = 0.47, xanchor = "center", y = -0.4, yanchor = "middle"
    ),
    list(
      type = "buttons", direction = "right",
      buttons = list(list(label = "Next realisation", method = "skip", args = list())),
      active = -1, showactive = FALSE,
      x = 0.85, xanchor = "center", y = -0.4, yanchor = "middle"
    )
  ),
  autosize = TRUE, height = 900,
  margin = list(l = 30, r = 35, t = 120, b = 100),
  paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF"
)

p11 <- plotly::config(p11, responsive = TRUE, displaylogo = FALSE)
p11$dependencies <- c(p11$dependencies, list(ar_dependency))

### Interactivity ----

p11 <- htmlwidgets::onRender(
  p11,
  "
  function(el, x, data) {
    var nMax = data.nMax;
    var burn = data.burn;
    var nPaths = data.nPaths;
    var lagMax = data.lagMax;
    var pathLength = nMax + burn;
    var order = data.initialOrder;
    var strength = data.initialStrength;
    var structure = data.initialStructure;
    var sampleSize = data.initialSize;
    var highlight = 0;
    var showAll = false;
    var raw = null;
    var modelKey = null;
    var paths = null;
    var statsKey = null;
    var acfs = null;
    var pacfs = null;
    var times = Array.from({length: nMax}, function(_, i) { return i + 1; });
    var lags = Array.from({length: lagMax}, function(_, i) { return i + 1; });

    function coefficients() {
      if (order === 1) return [strength];

      var phi = new Array(order).fill(0);
      var j, total, weights;

      if (structure === 'Recent lags') {
        weights = [];
        total = 0;
        for (j = 1; j < order; j++) {
          weights[j] = Math.pow(0.60, j - 1);
          total += weights[j];
        }
        phi[0] = 0.94 * strength;
        for (j = 1; j < order; j++) {
          phi[j] = 0.06 * strength * weights[j] / total;
        }
      } else if (structure === 'Distributed lags') {
        weights = [];
        total = 0;
        for (j = 0; j < order; j++) {
          weights[j] = Math.pow(0.90, j);
          total += weights[j];
        }
        for (j = 0; j < order; j++) {
          var sign = strength >= 0 ? 1 : (j % 2 === 0 ? -1 : 1);
          phi[j] = Math.abs(strength) * weights[j] * sign / total;
        }
      } else {
        phi[order - 1] = strength;
      }

      return phi;
    }

    function getPaths() {
      var key = order + ':' + strength.toFixed(2) + ':' + structure;
      if (modelKey === key) return paths;

      var phi = coefficients();
      var result = new Array(nPaths);
      for (var j = 0; j < nPaths; j++) {
        var series = new Array(nMax);
        var state = new Array(order).fill(0);
        var offset = j * pathLength;
        for (var t = 0; t < pathLength; t++) {
          var value = raw[offset + t];
          for (var k = 0; k < order; k++) value += phi[k] * state[k];
          for (var k = order - 1; k > 0; k--) state[k] = state[k - 1];
          state[0] = value;
          if (t >= burn) series[t - burn] = value;
        }
        result[j] = series;
      }
      modelKey = key;
      paths = result;
      return paths;
    }

    function sampleAcf(series) {
      var mean = 0;
      for (var i = 0; i < sampleSize; i++) mean += series[i];
      mean /= sampleSize;
      var centered = new Array(sampleSize);
      var denom = 0;
      for (var i = 0; i < sampleSize; i++) {
        centered[i] = series[i] - mean;
        denom += centered[i] * centered[i];
      }
      var result = new Array(lagMax);
      for (var h = 1; h <= lagMax; h++) {
        var numer = 0;
        for (var t = h; t < sampleSize; t++) {
          numer += centered[t] * centered[t - h];
        }
        result[h - 1] = denom > 0 ? numer / denom : 0;
      }
      return result;
    }

    // Durbin-Levinson recursion, matching the Yule-Walker PACF from R.
    function samplePacf(acf) {
      var previous = [];
      var pacf = [];
      for (var m = 1; m <= lagMax; m++) {
        var numer = acf[m - 1];
        var denom = 1;
        for (var j = 1; j < m; j++) {
          numer -= previous[j - 1] * acf[m - j - 1];
          denom -= previous[j - 1] * acf[j - 1];
        }
        var kappa = Math.abs(denom) > 1e-12 ? numer / denom : 0;
        pacf.push(kappa);
        var current = new Array(m);
        for (var j = 1; j < m; j++) {
          current[j - 1] = previous[j - 1] - kappa * previous[m - j - 1];
        }
        current[m - 1] = kappa;
        previous = current;
      }
      return pacf;
    }

    function stackLines(all, omit) {
      var xs = [];
      var ys = [];
      for (var j = 0; j < all.length; j++) {
        if (j === omit) continue;
        for (var i = 0; i < sampleSize; i++) {
          xs.push(i + 1);
          ys.push(all[j][i]);
        }
        xs.push(null);
        ys.push(null);
      }
      return {x: xs, y: ys};
    }

    function stackStems(all, omit) {
      var xs = [];
      var ys = [];
      for (var j = 0; j < all.length; j++) {
        if (j === omit) continue;
        for (var h = 0; h < lagMax; h++) {
          xs.push(h + 1, h + 1, null);
          ys.push(0, all[j][h], null);
        }
      }
      return {x: xs, y: ys};
    }

    function stems(values) {
      var xs = [];
      var ys = [];
      for (var h = 0; h < lagMax; h++) {
        xs.push(h + 1, h + 1, null);
        ys.push(0, values[h], null);
      }
      return {x: xs, y: ys};
    }

    function displayRange(series) {
      var lo = Infinity;
      var hi = -Infinity;
      var first = showAll ? 0 : highlight;
      var last = showAll ? nPaths - 1 : highlight;
      for (var j = first; j <= last; j++) {
        for (var t = 0; t < sampleSize; t++) {
          var v = series[j][t];
          if (v < lo) lo = v;
          if (v > hi) hi = v;
        }
      }
      var pad = Math.max(0.08 * (hi - lo), 0.1);
      return [lo - pad, hi + pad];
    }

    function getStatistics(simulated) {
      var key = modelKey + ':' + sampleSize;
      if (statsKey !== key) {
        statsKey = key;
        acfs = new Array(nPaths);
        pacfs = new Array(nPaths);
      }

      function ensure(j) {
        if (!acfs[j]) {
          acfs[j] = sampleAcf(simulated[j]);
          pacfs[j] = samplePacf(acfs[j]);
        }
      }

      ensure(highlight);
      if (showAll) {
        for (var j = 0; j < nPaths; j++) ensure(j);
      }
    }

    function updatePlot() {
      if (!raw) return;
      var simulated = getPaths();
      getStatistics(simulated);

      var trajBg = showAll ? stackLines(simulated, highlight) : {x: [], y: []};
      var acfBg = showAll ? stackStems(acfs, highlight) : {x: [], y: []};
      var pacfBg = showAll ? stackStems(pacfs, highlight) : {x: [], y: []};
      var acfHi = stems(acfs[highlight]);
      var pacfHi = stems(pacfs[highlight]);

      Plotly.update(
        el,
        {
          x: [trajBg.x, times.slice(0, sampleSize),
              acfBg.x, acfHi.x, lags, pacfBg.x, pacfHi.x, lags],
          y: [trajBg.y, simulated[highlight].slice(0, sampleSize),
              acfBg.y, acfHi.y, acfs[highlight],
              pacfBg.y, pacfHi.y, pacfs[highlight]]
        },
        {
          'xaxis.range': [1, sampleSize],
          'yaxis.range': displayRange(simulated)
        },
        [0, 1, 2, 3, 4, 5, 6, 7]
      );
    }

    el.on('plotly_sliderchange', function(e) {
      var label = String(e.step.label);
      if (label.indexOf('.') >= 0) strength = Number(label);
      else sampleSize = Number(label);
      updatePlot();
    });

    el.on('plotly_buttonclicked', function(e) {
      var label = e.button.label;
      if (label.indexOf('AR(') === 0) order = Number(label.slice(3, -1));
      else if (data.structureLabels.indexOf(label) >= 0) structure = label;
      else if (label === 'One realisation') showAll = false;
      else if (label === 'All realisations') showAll = true;
      else if (label === 'Next realisation') highlight = (highlight + 1) % nPaths;
      updatePlot();
    });

    if (!window.drpARExplorer02Base) {
      console.error('AR explorer: simulation dependency was not loaded.');
      return;
    }

    fetch(new URL('innovations.bin', window.drpARExplorer02Base))
      .then(function(response) {
        if (!response.ok) throw new Error('HTTP ' + response.status);
        return response.arrayBuffer();
      })
      .then(function(buffer) {
        var expected = 4 * pathLength * nPaths;
        if (buffer.byteLength !== expected) {
          throw new Error('Unexpected binary size: ' + buffer.byteLength);
        }
        var view = new DataView(buffer);
        raw = new Float32Array(expected / 4);
        for (var i = 0; i < raw.length; i++) {
          raw[i] = view.getFloat32(4 * i, true);
        }
        updatePlot();
      })
      .catch(function(error) {
        console.error('AR explorer: could not load innovations.bin', error);
      });
  }
  ",
  data = list(
    nMax = ar_n,
    burn = ar_burn,
    nPaths = ar_paths,
    lagMax = ar_lag_max,
    structureLabels = ar_structures,
    initialStructure = ar_initial_structure,
    initialOrder = ar_initial_p,
    initialStrength = ar_initial_strength,
    initialSize = ar_initial_n
  )
)

p11
saveRDS(p11, "figures/02/ar-explorer.rds")

## Moving-average models ----

### Simulation setup ----

set.seed(2026)

ma_n <- 1000
ma_max_order <- 10L
ma_paths <- 100L
ma_initial_n <- 300L
ma_lag_max <- 20L
ma_initial_q <- 1L
ma_initial_strength <- 0.80
ma_initial_structure <- "Recent lags"
ma_structures <- c("Recent lags", "Distributed lags", "Distant lag")

# Every family is an MA(q), hence stationary, and is invertible
# for abs(strength) < 1. The construction differs from the AR zoo:
# - Recent lags:      (1 + s B)(1 + 0.2 s B)^(q-1)
# - Distributed lags: (1 + s B)^q
# - Distant lag:      1 + s B^q
# Normalisation below gives all displayed series unit variance;
# it leaves their population ACF and PACF unchanged.
ma_coefficients <- function(q, strength, structure) {
  stopifnot(
    q %in% seq_len(ma_max_order),
    abs(strength) < 1,
    structure %in% ma_structures
  )
  
  if (structure == "Distant lag") {
    return(c(1, rep(0, q - 1L), strength))
  }
  
  factors <- if (structure == "Recent lags") {
    c(strength, rep(0.20 * strength, q - 1L))
  } else {
    rep(strength, q)
  }
  
  theta <- 1
  for (a in factors) {
    next_theta <- numeric(length(theta) + 1L)
    next_theta[seq_along(theta)] <- next_theta[seq_along(theta)] + theta
    next_theta[seq_along(theta) + 1L] <-
      next_theta[seq_along(theta) + 1L] + a * theta
    theta <- next_theta
  }
  
  theta
}

# Ten preceding innovations ensure that all MA orders have the
# same n=1000 observation window and can share the same shocks.
ma_innov <- matrix(
  rnorm((ma_n + ma_max_order) * ma_paths),
  nrow = ma_n + ma_max_order
)

ma_generate <- function(z, theta) {
  q <- length(theta) - 1L
  scale <- sqrt(sum(theta^2))
  out <- numeric(ma_n)
  
  for (t in seq_len(ma_n)) {
    index <- ma_max_order + t
    out[t] <- sum(theta * z[index - (0:q)]) / scale
  }
  
  out
}

ma_initial <- ma_generate(
  ma_innov[, 1],
  ma_coefficients(
    ma_initial_q, ma_initial_strength, ma_initial_structure
  )
)[seq_len(ma_initial_n)]

ma_sample_acf <- function(x) {
  as.numeric(stats::acf(x, lag.max = ma_lag_max, plot = FALSE)$acf)[-1]
}

ma_sample_pacf <- function(x) {
  as.numeric(stats::pacf(x, lag.max = ma_lag_max, plot = FALSE)$acf)
}

### Binary simulation asset ----

ma_asset_dir <- "figures/02/ma-explorer-assets"
dir.create(ma_asset_dir, recursive = TRUE, showWarnings = FALSE)

ma_con <- file(file.path(ma_asset_dir, "innovations.bin"), "wb")
writeBin(as.numeric(ma_innov), ma_con, size = 4, endian = "little")
close(ma_con)

writeLines(
  'window.drpMAExplorer02Base = new URL(".", document.currentScript.src).href;',
  file.path(ma_asset_dir, "loader.js"),
  useBytes = TRUE
)

ma_dependency <- htmltools::htmlDependency(
  name = "drp-ma-explorer-02",
  version = "1.0.0",
  src = c(file = normalizePath(ma_asset_dir, winslash = "/")),
  script = "loader.js",
  all_files = TRUE
)

### Figure ----

ma_lags <- seq_len(ma_lag_max)

ma_stems <- function(values) {
  list(
    x = as.vector(rbind(ma_lags, ma_lags, NA_real_)),
    y = as.vector(rbind(0, values, NA_real_))
  )
}

ma_acf0 <- ma_sample_acf(ma_initial)
ma_pacf0 <- ma_sample_pacf(ma_initial)
ma_acf_stems <- ma_stems(ma_acf0)
ma_pacf_stems <- ma_stems(ma_pacf0)

p12 <- plotly::plot_ly()

# One faint background trace and one highlighted trace for the trajectory.
p12 <- plotly::add_trace(
  p12, x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x", yaxis = "y",
  line = list(color = "rgba(1,0,202,0.02)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p12 <- plotly::add_trace(
  p12, x = seq_len(ma_initial_n), y = ma_initial,
  type = "scatter", mode = "lines",
  xaxis = "x", yaxis = "y",
  line = list(color = "#0100CA", width = 2),
  hovertemplate = "Observation: %{x}<br>Value: %{y:.2f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

# ACF: faint background stems, highlighted stems, highlighted markers.
p12 <- plotly::add_trace(
  p12, x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  line = list(color = "rgba(0,140,149,0.08)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p12 <- plotly::add_trace(
  p12, x = ma_acf_stems$x, y = ma_acf_stems$y,
  type = "scatter", mode = "lines",
  xaxis = "x2", yaxis = "y2",
  line = list(color = "#008C95", width = 2.1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p12 <- plotly::add_trace(
  p12, x = ma_lags, y = ma_acf0,
  type = "scatter", mode = "markers",
  xaxis = "x2", yaxis = "y2",
  marker = list(color = "#008C95", size = 5),
  hovertemplate = "Lag: %{x}<br>ACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

# PACF: faint background stems, highlighted stems, highlighted markers.
p12 <- plotly::add_trace(
  p12, x = numeric(0), y = numeric(0),
  type = "scattergl", mode = "lines",
  xaxis = "x3", yaxis = "y3",
  line = list(color = "rgba(196,91,64,0.08)", width = 1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p12 <- plotly::add_trace(
  p12, x = ma_pacf_stems$x, y = ma_pacf_stems$y,
  type = "scatter", mode = "lines",
  xaxis = "x3", yaxis = "y3",
  line = list(color = "#C45B40", width = 2.1),
  hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
)

p12 <- plotly::add_trace(
  p12, x = ma_lags, y = ma_pacf0,
  type = "scatter", mode = "markers",
  xaxis = "x3", yaxis = "y3",
  marker = list(color = "#C45B40", size = 5),
  hovertemplate = "Lag: %{x}<br>PACF: %{y:.3f}<extra></extra>",
  showlegend = FALSE, inherit = FALSE
)

ma_strength_values <- seq(-0.95, 0.95, by = 0.05)
ma_sample_sizes <- seq(100, ma_n, by = 100)

ma_strength_steps <- lapply(ma_strength_values, function(z) {
  list(label = sprintf("%.2f", z), method = "skip", args = list())
})

ma_size_steps <- lapply(ma_sample_sizes, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

ma_order_buttons <- lapply(seq_len(10), function(p) {
  list(label = paste0("MA(", p, ")"), method = "skip", args = list())
})

ma_structure_buttons <- lapply(ma_structures, function(z) {
  list(label = z, method = "skip", args = list())
})

ma_mode_buttons <- lapply(c("One realisation", "All realisations"), function(z) {
  list(label = z, method = "skip", args = list())
})

ma_initial_range <- axis_range(ma_initial)

p12 <- plotly::layout(
  p12,
  title = list(
    text = "Moving-average models: trajectories and empirical dependence",
    x = 0.5, font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.09, 0.97), anchor = "y",
    range = c(1, ma_initial_n), title = "Observation",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.59, 0.93), anchor = "x",
    range = ma_initial_range, title = "Value",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.09, 0.47), anchor = "y2",
    range = c(0.5, ma_lag_max + 0.5), dtick = 2,
    title = "Lag h", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis2 = list(
    domain = c(0.14, 0.46), anchor = "x2",
    range = c(-1.05, 1.05), title = "ACF",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  xaxis3 = list(
    domain = c(0.59, 0.97), anchor = "y3",
    range = c(0.5, ma_lag_max + 0.5), dtick = 2,
    title = "Lag h", fixedrange = TRUE,
    gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis3 = list(
    domain = c(0.14, 0.46), anchor = "x3",
    range = c(-1.05, 1.05), title = "PACF",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.09, y = 0.985, xref = "paper", yref = "paper",
      text = "<b>Simulated trajectory</b>", showarrow = FALSE,
      xanchor = "left", font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.09, y = 0.52, xref = "paper", yref = "paper",
      text = "<b>Empirical ACF</b>", showarrow = FALSE,
      xanchor = "left", font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.59, y = 0.52, xref = "paper", yref = "paper",
      text = "<b>Empirical PACF</b>", showarrow = FALSE,
      xanchor = "left", font = list(size = 13, color = "#000000")
    )
  ),
  sliders = list(
    list(
      active = which(abs(ma_strength_values - ma_initial_strength) < 1e-8) - 1,
      steps = ma_strength_steps,
      currentvalue = list(prefix = "Signed shock strength = "),
      x = 0.12, len = 0.77, y = 0.01, pad = list(t = 25)
    ),
    list(
      active = which(ma_sample_sizes == ma_initial_n) - 1,
      steps = ma_size_steps,
      currentvalue = list(prefix = "Sample size n = "),
      x = 0.12, len = 0.77, y = -0.16, pad = list(t = 25)
    )
  ),
  updatemenus = list(
    list(
      type = "dropdown", buttons = ma_order_buttons,
      active = ma_initial_q - 1, showactive = TRUE,
      x = 0.10, xanchor = "left", y = 1.05, yanchor = "middle"
    ),
    list(
      type = "dropdown", buttons = ma_structure_buttons,
      active = match(ma_initial_structure, ma_structures) - 1,
      showactive = TRUE,
      x = 0.25, xanchor = "left", y = 1.05, yanchor = "middle"
    ),
    list(
      type = "buttons", direction = "right",
      buttons = ma_mode_buttons,
      active = 0, showactive = TRUE,
      x = 0.47, xanchor = "center", y = -0.4, yanchor = "middle"
    ),
    list(
      type = "buttons", direction = "right",
      buttons = list(list(label = "Next realisation", method = "skip", args = list())),
      active = -1, showactive = FALSE,
      x = 0.85, xanchor = "center", y = -0.4, yanchor = "middle"
    )
  ),
  autosize = TRUE, height = 900,
  margin = list(l = 30, r = 35, t = 120, b = 120),
  paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF"
)

p12 <- plotly::config(p12, responsive = TRUE, displaylogo = FALSE)
p12$dependencies <- c(p12$dependencies, list(ma_dependency))

### Interactivity ----

p12 <- htmlwidgets::onRender(
  p12,
  "
  function(el, x, data) {
    var nMax = data.nMax;
    var qMax = data.qMax;
    var nPaths = data.nPaths;
    var lagMax = data.lagMax;
    var pathLength = nMax + qMax;
    var order = data.initialOrder;
    var strength = data.initialStrength;
    var structure = data.initialStructure;
    var sampleSize = data.initialSize;
    var highlight = 0;
    var showAll = false;
    var raw = null;
    var modelKey = null;
    var paths = null;
    var statsKey = null;
    var acfs = null;
    var pacfs = null;
    var times = Array.from({length: nMax}, function(_, i) { return i + 1; });
    var lags = Array.from({length: lagMax}, function(_, i) { return i + 1; });

    function coefficients() {
      if (structure === 'Distant lag') {
        var distant = new Array(order + 1).fill(0);
        distant[0] = 1;
        distant[order] = strength;
        return distant;
      }

      var factors = new Array(order);
      for (var j = 0; j < order; j++) {
        factors[j] = structure === 'Recent lags' && j > 0
          ? 0.20 * strength : strength;
      }

      var theta = [1];
      for (var j = 0; j < order; j++) {
        var next = new Array(theta.length + 1).fill(0);
        for (var k = 0; k < theta.length; k++) {
          next[k] += theta[k];
          next[k + 1] += factors[j] * theta[k];
        }
        theta = next;
      }
      return theta;
    }

    function getPaths() {
      var key = order + ':' + strength.toFixed(2) + ':' + structure;
      if (modelKey === key) return paths;

      var theta = coefficients();
      var sumSq = 0;
      for (var k = 0; k < theta.length; k++) {
        sumSq += theta[k] * theta[k];
      }
      var scale = Math.sqrt(sumSq);

      var result = new Array(nPaths);
      for (var j = 0; j < nPaths; j++) {
        var series = new Array(nMax);
        var offset = j * pathLength + qMax;

        for (var t = 0; t < nMax; t++) {
          var value = 0;
          for (var k = 0; k <= order; k++) {
            value += theta[k] * raw[offset + t - k];
          }
          series[t] = value / scale;
        }
        result[j] = series;
      }
      modelKey = key;
      paths = result;
      return paths;
    }

    function sampleAcf(series) {
      var mean = 0;
      for (var i = 0; i < sampleSize; i++) mean += series[i];
      mean /= sampleSize;
      var centered = new Array(sampleSize);
      var denom = 0;
      for (var i = 0; i < sampleSize; i++) {
        centered[i] = series[i] - mean;
        denom += centered[i] * centered[i];
      }
      var result = new Array(lagMax);
      for (var h = 1; h <= lagMax; h++) {
        var numer = 0;
        for (var t = h; t < sampleSize; t++) {
          numer += centered[t] * centered[t - h];
        }
        result[h - 1] = denom > 0 ? numer / denom : 0;
      }
      return result;
    }

    // Durbin-Levinson recursion, matching the Yule-Walker PACF from R.
    function samplePacf(acf) {
      var previous = [];
      var pacf = [];
      for (var m = 1; m <= lagMax; m++) {
        var numer = acf[m - 1];
        var denom = 1;
        for (var j = 1; j < m; j++) {
          numer -= previous[j - 1] * acf[m - j - 1];
          denom -= previous[j - 1] * acf[j - 1];
        }
        var kappa = Math.abs(denom) > 1e-12 ? numer / denom : 0;
        pacf.push(kappa);
        var current = new Array(m);
        for (var j = 1; j < m; j++) {
          current[j - 1] = previous[j - 1] - kappa * previous[m - j - 1];
        }
        current[m - 1] = kappa;
        previous = current;
      }
      return pacf;
    }

    function stackLines(all, omit) {
      var xs = [];
      var ys = [];
      for (var j = 0; j < all.length; j++) {
        if (j === omit) continue;
        for (var i = 0; i < sampleSize; i++) {
          xs.push(i + 1);
          ys.push(all[j][i]);
        }
        xs.push(null);
        ys.push(null);
      }
      return {x: xs, y: ys};
    }

    function stackStems(all, omit) {
      var xs = [];
      var ys = [];
      for (var j = 0; j < all.length; j++) {
        if (j === omit) continue;
        for (var h = 0; h < lagMax; h++) {
          xs.push(h + 1, h + 1, null);
          ys.push(0, all[j][h], null);
        }
      }
      return {x: xs, y: ys};
    }

    function stems(values) {
      var xs = [];
      var ys = [];
      for (var h = 0; h < lagMax; h++) {
        xs.push(h + 1, h + 1, null);
        ys.push(0, values[h], null);
      }
      return {x: xs, y: ys};
    }

    function displayRange(series) {
      var lo = Infinity;
      var hi = -Infinity;
      var first = showAll ? 0 : highlight;
      var last = showAll ? nPaths - 1 : highlight;
      for (var j = first; j <= last; j++) {
        for (var t = 0; t < sampleSize; t++) {
          var v = series[j][t];
          if (v < lo) lo = v;
          if (v > hi) hi = v;
        }
      }
      var pad = Math.max(0.08 * (hi - lo), 0.1);
      return [lo - pad, hi + pad];
    }

    function getStatistics(simulated) {
      var key = modelKey + ':' + sampleSize;
      if (statsKey !== key) {
        statsKey = key;
        acfs = new Array(nPaths);
        pacfs = new Array(nPaths);
      }

      function ensure(j) {
        if (!acfs[j]) {
          acfs[j] = sampleAcf(simulated[j]);
          pacfs[j] = samplePacf(acfs[j]);
        }
      }

      ensure(highlight);
      if (showAll) {
        for (var j = 0; j < nPaths; j++) ensure(j);
      }
    }

    function updatePlot() {
      if (!raw) return;
      var simulated = getPaths();
      getStatistics(simulated);

      var trajBg = showAll ? stackLines(simulated, highlight) : {x: [], y: []};
      var acfBg = showAll ? stackStems(acfs, highlight) : {x: [], y: []};
      var pacfBg = showAll ? stackStems(pacfs, highlight) : {x: [], y: []};
      var acfHi = stems(acfs[highlight]);
      var pacfHi = stems(pacfs[highlight]);

      Plotly.update(
        el,
        {
          x: [trajBg.x, times.slice(0, sampleSize),
              acfBg.x, acfHi.x, lags, pacfBg.x, pacfHi.x, lags],
          y: [trajBg.y, simulated[highlight].slice(0, sampleSize),
              acfBg.y, acfHi.y, acfs[highlight],
              pacfBg.y, pacfHi.y, pacfs[highlight]]
        },
        {
          'xaxis.range': [1, sampleSize],
          'yaxis.range': displayRange(simulated)
        },
        [0, 1, 2, 3, 4, 5, 6, 7]
      );
    }

    el.on('plotly_sliderchange', function(e) {
      var label = String(e.step.label);
      if (label.indexOf('.') >= 0) strength = Number(label);
      else sampleSize = Number(label);
      updatePlot();
    });

    el.on('plotly_buttonclicked', function(e) {
      var label = e.button.label;
      if (label.indexOf('MA(') === 0) order = Number(label.slice(3, -1));
      else if (data.structureLabels.indexOf(label) >= 0) structure = label;
      else if (label === 'One realisation') showAll = false;
      else if (label === 'All realisations') showAll = true;
      else if (label === 'Next realisation') highlight = (highlight + 1) % nPaths;
      updatePlot();
    });

    if (!window.drpMAExplorer02Base) {
      console.error('MA explorer: simulation dependency was not loaded.');
      return;
    }

    fetch(new URL('innovations.bin', window.drpMAExplorer02Base))
      .then(function(response) {
        if (!response.ok) throw new Error('HTTP ' + response.status);
        return response.arrayBuffer();
      })
      .then(function(buffer) {
        var expected = 4 * pathLength * nPaths;
        if (buffer.byteLength !== expected) {
          throw new Error('Unexpected binary size: ' + buffer.byteLength);
        }
        var view = new DataView(buffer);
        raw = new Float32Array(expected / 4);
        for (var i = 0; i < raw.length; i++) {
          raw[i] = view.getFloat32(4 * i, true);
        }
        updatePlot();
      })
      .catch(function(error) {
        console.error('MA explorer: could not load innovations.bin', error);
      });
  }
  ",
  data = list(
    nMax = ma_n,
    qMax = ma_max_order,
    nPaths = ma_paths,
    lagMax = ma_lag_max,
    structureLabels = ma_structures,
    initialStructure = ma_initial_structure,
    initialOrder = ma_initial_q,
    initialStrength = ma_initial_strength,
    initialSize = ma_initial_n
  )
)

p12
saveRDS(p12, "figures/02/ma-explorer.rds")

## Why moving averages? ----

### Shock propagation ----

set.seed(2028)

shock_pre <- 14L
shock_post <- 50L
shock_n <- shock_pre + shock_post + 1L
shock_burn <- 120L
shock_paths <- 100L
shock_t0 <- shock_pre + 1L
shock_initial_phi <- 0.90
shock_initial_size <- 5L

# Both models use the same innovations. The shocked path changes
# only one innovation, so the difference has an exact interpretation.
shock_innov <- matrix(
  rnorm((shock_n + shock_burn) * shock_paths),
  nrow = shock_n + shock_burn
)

shock_generate <- function(z, phi) {
  ar <- numeric(length(z))
  ma <- numeric(length(z))
  previous_ar <- 0
  previous_noise <- 0

  for (t in seq_along(z)) {
    ar[t] <- phi * previous_ar + z[t]
    ma[t] <- z[t] + phi * previous_noise
    previous_ar <- ar[t]
    previous_noise <- z[t]
  }

  list(ar = tail(ar, shock_n), ma = tail(ma, shock_n))
}

shock_effects <- function(phi, amount) {
  h <- 0:shock_post
  list(
    ar = amount * phi^h,
    ma = c(amount, amount * phi, rep(0, shock_post - 1L))
  )
}

shock_initial <- shock_generate(shock_innov[, 1], shock_initial_phi)
shock_initial_effect <- shock_effects(shock_initial_phi, shock_initial_size)
shock_initial_ar <- shock_initial$ar
shock_initial_ma <- shock_initial$ma
shock_initial_ar[shock_t0:shock_n] <-
  shock_initial_ar[shock_t0:shock_n] + shock_initial_effect$ar
shock_initial_ma[shock_t0:shock_n] <-
  shock_initial_ma[shock_t0:shock_n] + shock_initial_effect$ma

shock_range <- function(z) {
  limits <- range(z, finite = TRUE)
  padding <- max(0.08 * diff(limits), 0.3)
  limits + c(-padding, padding)
}

shock_initial_range <- shock_range(c(
  shock_initial$ar, shock_initial_ar,
  shock_initial$ma, shock_initial_ma
))
shock_initial_effect_range <- shock_range(c(
  0, shock_initial_effect$ar, shock_initial_effect$ma
))

### Binary simulation asset ----

shock_asset_dir <- "figures/02/shock-propagation-assets"
dir.create(shock_asset_dir, recursive = TRUE, showWarnings = FALSE)

shock_con <- file(file.path(shock_asset_dir, "innovations.bin"), "wb")
writeBin(as.numeric(shock_innov), shock_con, size = 4, endian = "little")
close(shock_con)

writeLines(
  'window.drpShockPropagation02Base = new URL(".", document.currentScript.src).href;',
  file.path(shock_asset_dir, "loader.js"),
  useBytes = TRUE
)

shock_dependency <- htmltools::htmlDependency(
  name = "drp-shock-propagation-02",
  version = "1.0.0",
  src = c(file = normalizePath(shock_asset_dir, winslash = "/")),
  script = "loader.js",
  all_files = TRUE
)

### Figure ----

shock_times <- seq_len(shock_n)
shock_horizons <- 0:shock_post

p13 <- plotly::plot_ly()

# AR(1): original path and the counterfactual shocked path.
p13 <- plotly::add_trace(
  p13, x = shock_times, y = shock_initial$ar,
  type = "scatter", mode = "lines", xaxis = "x", yaxis = "y",
  line = list(color = "#8A94A4", width = 1.8, dash = "dash"),
  name = "Original", legendgroup = "original", showlegend = TRUE,
  hovertemplate = "t = %{x}<br>Original: %{y:.2f}<extra></extra>",
  inherit = FALSE
)

p13 <- plotly::add_trace(
  p13, x = shock_times, y = shock_initial_ar,
  type = "scatter", mode = "lines", xaxis = "x", yaxis = "y",
  line = list(color = "#0100CA", width = 2.5),
  name = "AR(1) shocked", legendgroup = "ar", showlegend = TRUE,
  hovertemplate = "t = %{x}<br>AR shocked: %{y:.2f}<extra></extra>",
  inherit = FALSE
)

# MA(1): the same innovations and same added shock.
p13 <- plotly::add_trace(
  p13, x = shock_times, y = shock_initial$ma,
  type = "scatter", mode = "lines", xaxis = "x2", yaxis = "y2",
  line = list(color = "#8A94A4", width = 1.8, dash = "dash"),
  name = "Original", legendgroup = "original", showlegend = FALSE,
  hovertemplate = "t = %{x}<br>Original: %{y:.2f}<extra></extra>",
  inherit = FALSE
)

p13 <- plotly::add_trace(
  p13, x = shock_times, y = shock_initial_ma,
  type = "scatter", mode = "lines", xaxis = "x2", yaxis = "y2",
  line = list(color = "#008C95", width = 2.5),
  name = "MA(1) shocked", legendgroup = "ma", showlegend = TRUE,
  hovertemplate = "t = %{x}<br>MA shocked: %{y:.2f}<extra></extra>",
  inherit = FALSE
)

# Difference between the shocked and original trajectories.
p13 <- plotly::add_trace(
  p13, x = shock_horizons, y = shock_initial_effect$ar,
  type = "scatter", mode = "lines+markers", xaxis = "x3", yaxis = "y3",
  line = list(color = "#0100CA", width = 2.5),
  marker = list(color = "#0100CA", size = 4),
  name = "AR(1) effect", legendgroup = "ar", showlegend = FALSE,
  hovertemplate = "h = %{x}<br>AR effect: %{y:.2f}<extra></extra>",
  inherit = FALSE
)

p13 <- plotly::add_trace(
  p13, x = shock_horizons, y = shock_initial_effect$ma,
  type = "scatter", mode = "lines+markers", xaxis = "x3", yaxis = "y3",
  line = list(color = "#008C95", width = 2.5),
  marker = list(color = "#008C95", size = 4),
  name = "MA(1) effect", legendgroup = "ma", showlegend = FALSE,
  hovertemplate = "h = %{x}<br>MA effect: %{y:.2f}<extra></extra>",
  inherit = FALSE
)

shock_phi_values <- seq(-0.95, 0.95, by = 0.05)
shock_size_values <- -10:10

shock_phi_steps <- lapply(shock_phi_values, function(z) {
  list(label = sprintf("%.2f", z), method = "skip", args = list())
})

shock_size_steps <- lapply(shock_size_values, function(z) {
  list(label = sprintf("%+d", z), method = "skip", args = list())
})

p13 <- plotly::layout(
  p13,
  title = list(
    text = "One innovation, two propagation mechanisms",
    x = 0.5, font = list(size = 21, color = "#000000")
  ),
  legend = list(
    orientation = "h", x = 0.5, xanchor = "center",
    y = 1.02, yanchor = "bottom", font = list(size = 11)
  ),
  xaxis = list(
    domain = c(0.09, 0.47), anchor = "y",
    range = c(1, shock_n), title = "Observation t",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.60, 0.90), anchor = "x",
    range = shock_initial_range, title = "Value",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.59, 0.97), anchor = "y2",
    range = c(1, shock_n), title = "Observation t",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis2 = list(
    domain = c(0.60, 0.90), anchor = "x2",
    range = shock_initial_range, title = "Value",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  xaxis3 = list(
    domain = c(0.09, 0.97), anchor = "y3",
    range = c(0, shock_post), dtick = 5,
    title = "Periods after the added innovation (h)",
    fixedrange = TRUE, gridcolor = "#E4E6F2", zeroline = FALSE
  ),
  yaxis3 = list(
    domain = c(0.14, 0.44), anchor = "x3",
    range = shock_initial_effect_range,
    title = "Additional effect",
    fixedrange = TRUE, gridcolor = "#E4E6F2",
    zeroline = TRUE, zerolinecolor = "#A0A5B5"
  ),
  shapes = list(
    list(
      type = "line", xref = "x", yref = "paper",
      x0 = shock_t0, x1 = shock_t0, y0 = 0.60, y1 = 0.90,
      line = list(color = "#C45B40", width = 1.4, dash = "dot")
    ),
    list(
      type = "line", xref = "x2", yref = "paper",
      x0 = shock_t0, x1 = shock_t0, y0 = 0.60, y1 = 0.90,
      line = list(color = "#C45B40", width = 1.4, dash = "dot")
    )
  ),
  annotations = list(
    list(
      x = 0.09, y = 0.958, xref = "paper", yref = "paper",
      text = "<b>AR(1): original versus shocked</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.59, y = 0.958, xref = "paper", yref = "paper",
      text = "<b>MA(1): original versus shocked</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.09, y = 0.45, xref = "paper", yref = "paper",
      text = "<b>Isolated effect: shocked minus original</b>",
      showarrow = FALSE, xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.8, y = 0.55, xref = "paper", yref = "paper",
      text = paste0("<span style='color:#C45B40'>Dotted line: extra shock at t = ", shock_t0, "</span>"),
      showarrow = FALSE, xanchor = "right",
      font = list(size = 11)
    )
  ),
  sliders = list(
    list(
      active = which(abs(shock_phi_values - shock_initial_phi) < 1e-8) - 1L,
      steps = shock_phi_steps,
      currentvalue = list(prefix = "Signed coefficient = "),
      x = 0.12, len = 0.77, y = 0.01, pad = list(t = 25)
    ),
    list(
      active = which(shock_size_values == shock_initial_size) - 1L,
      steps = shock_size_steps,
      currentvalue = list(prefix = "Additional innovation = "),
      x = 0.12, len = 0.77, y = -0.16, pad = list(t = 25)
    )
  ),
  updatemenus = list(
    list(
      type = "buttons", direction = "right",
      buttons = list(list(
        label = "Next realisation", method = "skip", args = list()
      )),
      showactive = FALSE, active = -1,
      x = 0.50, xanchor = "center", y = -0.40, yanchor = "middle"
    )
  ),
  autosize = TRUE, height = 900,
  margin = list(l = 35, r = 35, t = 130, b = 120),
  paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF"
)

p13 <- plotly::config(p13, responsive = TRUE, displaylogo = FALSE)
p13$dependencies <- c(p13$dependencies, list(shock_dependency))

### Interactivity ----

p13 <- htmlwidgets::onRender(
  p13,
  "
  function(el, x, data) {
    var burn = data.burn;
    var n = data.n;
    var nPaths = data.nPaths;
    var shockIndex = data.shockIndex - 1;
    var post = data.post;
    var coefficient = data.initialCoefficient;
    var amount = data.initialAmount;
    var selected = 0;
    var raw = null;
    var cachedKey = null;
    var cachedSeries = null;
    var pathLength = burn + n;

    function originalSeries() {
      var key = selected + ':' + coefficient.toFixed(2);
      if (cachedKey === key) return cachedSeries;

      var ar = new Array(n);
      var ma = new Array(n);
      var offset = selected * pathLength;
      var previousAR = 0;
      var previousNoise = 0;

      for (var t = 0; t < pathLength; t++) {
        var e = raw[offset + t];
        var arValue = coefficient * previousAR + e;
        var maValue = e + coefficient * previousNoise;
        previousAR = arValue;
        previousNoise = e;
        if (t >= burn) {
          ar[t - burn] = arValue;
          ma[t - burn] = maValue;
        }
      }

      cachedKey = key;
      cachedSeries = {ar: ar, ma: ma};
      return cachedSeries;
    }

    function impulseEffects() {
      var ar = new Array(post + 1);
      var ma = new Array(post + 1).fill(0);
      for (var h = 0; h <= post; h++) {
        ar[h] = amount * Math.pow(coefficient, h);
      }
      ma[0] = amount;
      ma[1] = amount * coefficient;
      return {ar: ar, ma: ma};
    }

    function displayRange(values, includeZero) {
      var lo = includeZero ? 0 : Infinity;
      var hi = includeZero ? 0 : -Infinity;
      for (var k = 0; k < values.length; k++) {
        for (var i = 0; i < values[k].length; i++) {
          var v = values[k][i];
          if (v < lo) lo = v;
          if (v > hi) hi = v;
        }
      }
      var pad = Math.max(0.08 * (hi - lo), 0.3);
      return [lo - pad, hi + pad];
    }

    function updatePlot() {
      if (!raw) return;
      var original = originalSeries();
      var effects = impulseEffects();
      var arShocked = original.ar.slice();
      var maShocked = original.ma.slice();
      for (var h = 0; h <= post; h++) {
        arShocked[shockIndex + h] += effects.ar[h];
        maShocked[shockIndex + h] += effects.ma[h];
      }
      var sharedRange = displayRange(
        [original.ar, arShocked, original.ma, maShocked], false
      );
      var effectRange = displayRange([effects.ar, effects.ma], true);

      Plotly.update(
        el,
        {
          y: [original.ar, arShocked, original.ma, maShocked,
              effects.ar, effects.ma]
        },
        {
          'yaxis.range': sharedRange,
          'yaxis2.range': sharedRange,
          'yaxis3.range': effectRange
        },
        [0, 1, 2, 3, 4, 5]
      );
    }

    el.on('plotly_sliderchange', function(e) {
      var label = String(e.step.label);
      if (label.indexOf('.') >= 0) coefficient = Number(label);
      else amount = Number(label);
      updatePlot();
    });

    el.on('plotly_buttonclicked', function(e) {
      if (e.button.label === 'Next realisation') {
        selected = (selected + 1) % nPaths;
        updatePlot();
      }
    });

    if (!window.drpShockPropagation02Base) {
      console.error('Shock propagation: simulation dependency was not loaded.');
      return;
    }

    fetch(new URL('innovations.bin', window.drpShockPropagation02Base))
      .then(function(response) {
        if (!response.ok) throw new Error('HTTP ' + response.status);
        return response.arrayBuffer();
      })
      .then(function(buffer) {
        var expected = 4 * pathLength * nPaths;
        if (buffer.byteLength !== expected) {
          throw new Error('Unexpected binary size: ' + buffer.byteLength);
        }
        var view = new DataView(buffer);
        raw = new Float32Array(expected / 4);
        for (var i = 0; i < raw.length; i++) {
          raw[i] = view.getFloat32(4 * i, true);
        }
        updatePlot();
      })
      .catch(function(error) {
        console.error('Shock propagation: could not load innovations.bin', error);
      });
  }
  ",
  data = list(
    n = shock_n,
    burn = shock_burn,
    nPaths = shock_paths,
    shockIndex = shock_t0,
    post = shock_post,
    initialCoefficient = shock_initial_phi,
    initialAmount = shock_initial_size
  )
)

p13
saveRDS(p13, "figures/02/shock-propagation.rds")

### MA(1) versus AR(p): parsimony ----

parsimony_lag_max <- 20
parsimony_lags <- seq_len(parsimony_lag_max)
parsimony_orders <- 1:10
parsimony_theta_values <- seq(-0.95, 0.95, by = 0.05)
parsimony_initial_order <- 1
parsimony_initial_theta <- 0.90

ma1_population_acf <- function(theta, lag_max) {
  z <- numeric(lag_max)
  if (lag_max >= 1) {
    z[1] <- theta / (1 + theta^2)
  }
  z
}

levinson_ma1_ar <- function(theta, order) {
  acf_target <- ma1_population_acf(theta, order)
  previous <- numeric(0)
  
  for (m in seq_len(order)) {
    numer <- acf_target[m]
    denom <- 1
    
    if (m > 1) {
      for (j in seq_len(m - 1)) {
        numer <- numer - previous[j] * acf_target[m - j]
        denom <- denom - previous[j] * acf_target[j]
      }
    }
    
    kappa <- if (abs(denom) > 1e-12) numer / denom else 0
    current <- numeric(m)
    
    if (m > 1) {
      for (j in seq_len(m - 1)) {
        current[j] <- previous[j] - kappa * previous[m - j]
      }
    }
    
    current[m] <- kappa
    previous <- current
  }
  
  previous
}

ar_population_acf <- function(phi, lag_max) {
  p <- length(phi)
  if (p == 0) {
    return(numeric(lag_max))
  }
  
  A <- matrix(0, nrow = p, ncol = p)
  b <- numeric(p)
  
  for (h in seq_len(p)) {
    A[h, h] <- 1
    
    for (j in seq_len(p)) {
      index <- h - j
      
      if (index == 0) {
        b[h] <- b[h] + phi[j]
      } else {
        A[h, abs(index)] <- A[h, abs(index)] - phi[j]
      }
    }
  }
  
  rho_first <- as.numeric(solve(A, b))
  rho <- c(1, rho_first)
  
  if (lag_max > p) {
    for (h in (p + 1):lag_max) {
      rho[h + 1] <- sum(phi * rev(rho[(h - p + 1):h]))
    }
  }
  
  rho[-1]
}

parsimony_initial_ma <- ma1_population_acf(
  parsimony_initial_theta,
  parsimony_lag_max
)

parsimony_initial_phi <- levinson_ma1_ar(
  parsimony_initial_theta,
  parsimony_initial_order
)

parsimony_initial_ar <- ar_population_acf(
  parsimony_initial_phi,
  parsimony_lag_max
)

parsimony_phi_display <- numeric(max(parsimony_orders))
parsimony_phi_display[seq_len(parsimony_initial_order)] <- parsimony_initial_phi

p14 <- plotly::plot_ly()

p14 <- plotly::add_trace(
  p14,
  x = parsimony_lags,
  y = parsimony_initial_ma,
  type = "scatter",
  mode = "lines+markers",
  xaxis = "x",
  yaxis = "y",
  line = list(color = "#0100CA", width = 2.3),
  marker = list(color = "#0100CA", size = 5),
  hovertemplate = paste0(
    "<b>MA(1)</b>",
    "<br>Lag: %{x}",
    "<br>ACF: %{y:.3f}<extra></extra>"
  ),
  showlegend = FALSE,
  inherit = FALSE
)

p14 <- plotly::add_trace(
  p14,
  x = parsimony_lags,
  y = parsimony_initial_ar,
  type = "scatter",
  mode = "lines+markers",
  xaxis = "x",
  yaxis = "y",
  line = list(color = "#008C95", width = 2.3),
  marker = list(color = "#008C95", size = 5),
  hovertemplate = paste0(
    "<b>AR approximation</b>",
    "<br>Lag: %{x}",
    "<br>ACF: %{y:.3f}<extra></extra>"
  ),
  showlegend = FALSE,
  inherit = FALSE
)

p14 <- plotly::add_trace(
  p14,
  x = parsimony_orders,
  y = parsimony_phi_display,
  type = "bar",
  xaxis = "x2",
  yaxis = "y2",
  marker = list(color = "#C45B40"),
  hovertemplate = paste0(
    "Coefficient: \\phi_%{x}",
    "<br>Value: %{y:.3f}<extra></extra>"
  ),
  showlegend = FALSE,
  inherit = FALSE
)

parsimony_theta_steps <- lapply(parsimony_theta_values, function(z) {
  list(label = sprintf("%.2f", z), method = "skip", args = list())
})

parsimony_order_steps <- lapply(parsimony_orders, function(z) {
  list(label = as.character(z), method = "skip", args = list())
})

p14 <- plotly::layout(
  p14,
  title = list(
    text = "Parsimony in AR and MA representations",
    x = 0.5,
    font = list(size = 21, color = "#000000")
  ),
  xaxis = list(
    domain = c(0.08, 0.96),
    anchor = "y",
    range = c(1, parsimony_lag_max),
    dtick = 2,
    title = "Lag h",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis = list(
    domain = c(0.56, 0.92),
    anchor = "x",
    range = c(-0.6, 0.6),
    title = "Population ACF",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  xaxis2 = list(
    domain = c(0.08, 0.96),
    anchor = "y2",
    range = c(0.5, max(parsimony_orders) + 0.5),
    dtick = 1,
    title = "AR coefficient index",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = FALSE
  ),
  yaxis2 = list(
    domain = c(0.15, 0.40),
    anchor = "x2",
    range = c(-1.05, 1.05),
    title = "AR coefficient value",
    fixedrange = TRUE,
    gridcolor = "#E4E6F2",
    zeroline = TRUE,
    zerolinecolor = "#A0A5B5"
  ),
  annotations = list(
    list(
      x = 0.08,
      y = 1.0,
      xref = "paper",
      yref = "paper",
      text = "<b>Population autocorrelation comparison</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.08,
      y = 0.42,
      xref = "paper",
      yref = "paper",
      text = "<b>AR(p) coefficients required by the approximation</b>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 13, color = "#000000")
    ),
    list(
      x = 0.08,
      y = 0.95,
      xref = "paper",
      yref = "paper",
      text = "<span style='color:#0100CA'><b>MA(1)</b></span>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 12)
    ),
    list(
      x = 0.15,
      y = 0.95,
      xref = "paper",
      yref = "paper",
      text = "<span style='color:#008C95'><b>AR(p) approximation</b></span>",
      showarrow = FALSE,
      xanchor = "left",
      font = list(size = 12)
    )
  ),
  sliders = list(
    list(
      active = which(abs(parsimony_theta_values - parsimony_initial_theta) < 1e-8) - 1,
      steps = parsimony_theta_steps,
      currentvalue = list(prefix = "MA(1) coefficient = "),
      x = 0.12,
      len = 0.77,
      y = 0.03,
      pad = list(t = 25)
    ),
    list(
      active = parsimony_initial_order - 1,
      steps = parsimony_order_steps,
      currentvalue = list(prefix = "AR approximation order p = "),
      x = 0.12,
      len = 0.77,
      y = -0.2,
      pad = list(t = 25)
    )
  ),
  autosize = TRUE,
  height = 820,
  margin = list(l = 55, r = 60, t = 115, b = 120),
  paper_bgcolor = "#FFFFFF",
  plot_bgcolor = "#FFFFFF",
  bargap = 0.18
)

p14 <- plotly::config(
  p14,
  responsive = TRUE,
  displaylogo = FALSE
)

p14 <- htmlwidgets::onRender(
  p14,
  "
  function(el, x, data) {
    var lagMax = data.lagMax;
    var theta = data.initialTheta;
    var order = data.initialOrder;
    var lags = Array.from({length: lagMax}, function(_, i) { return i + 1; });
    var coeffX = Array.from({length: data.maxOrder}, function(_, i) { return i + 1; });

    function maAcf(thetaValue, lagMaxValue) {
      var acf = new Array(lagMaxValue).fill(0);
      if (lagMaxValue >= 1) {
        acf[0] = thetaValue / (1 + thetaValue * thetaValue);
      }
      return acf;
    }

    function levinsonFromAcf(acfTarget, p) {
      var previous = [];
      for (var m = 1; m <= p; m++) {
        var numer = acfTarget[m - 1];
        var denom = 1;

        for (var j = 1; j < m; j++) {
          numer -= previous[j - 1] * acfTarget[m - j - 1];
          denom -= previous[j - 1] * acfTarget[j - 1];
        }

        var kappa = Math.abs(denom) > 1e-12 ? numer / denom : 0;
        var current = new Array(m).fill(0);

        for (var j = 1; j < m; j++) {
          current[j - 1] = previous[j - 1] - kappa * previous[m - j - 1];
        }

        current[m - 1] = kappa;
        previous = current;
      }
      return previous;
    }

    function solveLinearSystem(A, b) {
      var n = A.length;
      var M = A.map(function(row, i) {
        return row.slice().concat([b[i]]);
      });

      for (var k = 0; k < n; k++) {
        var pivot = k;
        var maxAbs = Math.abs(M[k][k]);

        for (var i = k + 1; i < n; i++) {
          var value = Math.abs(M[i][k]);
          if (value > maxAbs) {
            maxAbs = value;
            pivot = i;
          }
        }

        if (pivot !== k) {
          var temp = M[k];
          M[k] = M[pivot];
          M[pivot] = temp;
        }

        var diag = M[k][k];
        if (Math.abs(diag) < 1e-12) {
          return new Array(n).fill(0);
        }

        for (var j = k; j <= n; j++) {
          M[k][j] /= diag;
        }

        for (var i = 0; i < n; i++) {
          if (i === k) continue;
          var factor = M[i][k];
          for (var j = k; j <= n; j++) {
            M[i][j] -= factor * M[k][j];
          }
        }
      }

      return M.map(function(row) { return row[n]; });
    }

    function arAcf(phi, lagMaxValue) {
      var p = phi.length;
      if (!p) return new Array(lagMaxValue).fill(0);

      var A = new Array(p);
      var b = new Array(p).fill(0);

      for (var h = 1; h <= p; h++) {
        A[h - 1] = new Array(p).fill(0);
        A[h - 1][h - 1] = 1;

        for (var j = 1; j <= p; j++) {
          var index = h - j;
          if (index === 0) {
            b[h - 1] += phi[j - 1];
          } else {
            A[h - 1][Math.abs(index) - 1] -= phi[j - 1];
          }
        }
      }

      var rhoFirst = solveLinearSystem(A, b);
      var rho = [1].concat(rhoFirst);

      for (var h = p + 1; h <= lagMaxValue; h++) {
        var value = 0;
        for (var j = 1; j <= p; j++) {
          value += phi[j - 1] * rho[h - j];
        }
        rho[h] = value;
      }

      return rho.slice(1, lagMaxValue + 1);
    }

    function updatePlot() {
      var target = maAcf(theta, lagMax);
      var targetForFit = maAcf(theta, order);
      var phi = levinsonFromAcf(targetForFit, order);
      var approx = arAcf(phi, lagMax);
      var bars = new Array(data.maxOrder).fill(0);

      for (var j = 0; j < order; j++) bars[j] = phi[j];

      var acfLimit = Math.max(
        0.1,
        1.08 * Math.max.apply(null, target.concat(approx).map(Math.abs))
      );

      var phiLimit = Math.max(
        0.1,
        1.08 * Math.max.apply(null, bars.map(Math.abs))
      );

      Plotly.update(
        el,
        {
          x: [lags, lags, coeffX],
          y: [target, approx, bars]
        },
        {
          'yaxis.range': [-acfLimit, acfLimit],
          'yaxis2.range': [-phiLimit, phiLimit]
        },
        [0, 1, 2]
      );
    }

    el.on('plotly_sliderchange', function(e) {
      var prefix = e.step.value !== undefined ? String(e.step.value) : String(e.step.label);
      if (prefix.indexOf('.') >= 0 || prefix.indexOf('-') === 0) {
        theta = Number(e.step.label);
      } else {
        order = Number(e.step.label);
      }
      updatePlot();
    });

    updatePlot();
  }
  ",
  data = list(
    lagMax = parsimony_lag_max,
    maxOrder = max(parsimony_orders),
    initialTheta = parsimony_initial_theta,
    initialOrder = parsimony_initial_order
  )
)

p14
saveRDS(p14, "figures/02/ma-vs-ar-parsimony.rds")



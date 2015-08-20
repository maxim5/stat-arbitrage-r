#!/usr/bin/Rscript

suppressMessages(library(ggplot2))
suppressMessages(require(reshape2))

invisible(Sys.setlocale("LC_TIME", "en_US.UTF-8"))
load("pairs.RData")


Plot.Price = function(symbol1, symbol2) {
  series1 = all.logs[[symbol1]]
  series2 = all.logs[[symbol2]]
  dates = as.Date(rownames(all.logs))

  data.to.plot = data.frame(exp(series1), exp(series2), dates)
  colnames(data.to.plot) = c(symbol1, symbol2, "Date")
  data.to.plot = melt(data.to.plot, id="Date")

  plot = ggplot(data.to.plot, aes(x=Date, y=value, color=variable)) +
    geom_line() +
    labs(title=paste0(symbol1, " vs ", symbol2),
         x="Time", y="Price") +
    scale_colour_discrete(name="Legend")
  print(plot)
}

Plot.Logs = function(symbol1, symbol2) {
  series1 = all.logs[[symbol1]]
  series2 = all.logs[[symbol2]]
  dates = as.Date(rownames(all.logs))

  data.to.plot = data.frame(series1, series2, dates)
  colnames(data.to.plot) = c(symbol1, symbol2, "Date")
  data.to.plot = melt(data.to.plot, id="Date")

  plot = ggplot(data.to.plot, aes(x=Date, y=value, color=variable)) +
    geom_line() +
    labs(title=paste0("Log(", symbol1, ") vs Log(", symbol2, ")"),
         x="Time", y="Log(Price)") +
    scale_colour_discrete(name="Legend")
  print(plot)
}

Plot.Spread = function(spread, x=NULL, title="Spread") {
  mean = mean(spread)
  sd = sd(spread)
  stats = boxplot.stats(spread)
  if (is.null(x)) {
    x = index(spread)
  }

  plot(x=x, y=spread, type="l", lwd=2, col="darkorchid",
       xlab="Time", ylab="Spread", main=title)

  Add.HLine = function(level, color) {
    abline(h=level, col=color)
    text(x[1], level, signif(level, 3), col=color, adj=c(0.5, 0))
  }

  Add.HLine(mean, "aquamarine3")
  Add.HLine(mean+sd, "aquamarine4")
  Add.HLine(mean-sd, "aquamarine4")
  Add.HLine(stats$stats[1], "firebrick1")
  Add.HLine(stats$stats[5], "firebrick1")
}

Plot.Pair = function(symbol1, symbol2) {
  Plot.Price(symbol1, symbol2)
  Plot.Logs(symbol1, symbol2)

  series1 = all.logs[[symbol1]]
  series2 = all.logs[[symbol2]]
  dates = as.Date(rownames(all.logs))
  gamma = as.numeric(cointegrated.pairs[cointegrated.pairs$Symbol1 == symbol1 &
                                        cointegrated.pairs$Symbol2 == symbol2, "Gamma"])
  spread = series1 - gamma * series2
  Plot.Spread(spread=spread, x=dates,
              title=paste0("Log(", symbol1, ") - ", signif(gamma, 3), "*Log(", symbol2, ")"))
}

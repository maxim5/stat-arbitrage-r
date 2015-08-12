#!/usr/bin/Rscript
# Performs the analysis to select possible pairs for cointegration.
# TODO: Make cmd arguments for: index.path, period.start, period.end, corr.threshold.


suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tseries))

invisible(Sys.setlocale("LC_TIME", "en_US.UTF-8"))


########################################################################################################################
# Read the index.
########################################################################################################################


index.path = "_index.csv"
message("Reading index from ", index.path)
index.table = index.path %>%
  read.csv(stringsAsFactors=FALSE) %>%
  tbl_df() %>%
  mutate(From.Date=as.Date(From.Date), To.Date=as.Date(To.Date), Rows.Number=as.numeric(Rows.Number)) %>%
  filter(!is.na(From.Date), !is.na(To.Date), Rows.Number > 0)
message("Read ", nrow(index.table), " non-empty rows")

period.start = as.Date("2015-01-01")
period.end = as.Date("2015-06-30")
message("Filtering rows available from ", period.start, " to ", period.end)
index.table = index.table %>% 
  filter(From.Date <= period.start, To.Date >= period.end) # %>% top_n(2000, Symbol)
message(nrow(index.table), " rows to process")


########################################################################################################################
# Data sanity check.
########################################################################################################################


tmp.row.num = 0
tmp.min.date = 0
tmp.max.date = 0
Check.Sanity = function(symbol, current.frame) {
  row.num = nrow(current.frame)
  if (tmp.row.num > 0 && tmp.row.num != row.num) {
    message(symbol, ". Inconsistent rows number: ", row.num, ", expected: ", tmp.row.num)
    return (FALSE)
  }
  tmp.row.num <<- row.num

  min.date = min(current.frame$Date)
  if (tmp.min.date > 0 && tmp.min.date != min.date) {
    message(symbol, ". Invalid start date: ", min.date)
    return (FALSE)
  }
  tmp.min.date <<- min.date

  max.date = max(current.frame$Date)
  if (tmp.max.date > 0 && tmp.max.date != max.date) {
    message(symbol, ". Invalid end date: ", max.date)
    return (FALSE)
  }
  tmp.max.date <<- max.date
  
  return (TRUE)
}


########################################################################################################################
# Reading individual securities data.
########################################################################################################################


message("Reading individual securities data")

# Quote data is reversed (most recent come first).
# This is a little bit more than needed, because not all days are traded, but it will suffice.
rows.to.read = as.numeric(max(index.table$To.Date) - period.start)

all.logs = NULL
all.returns = NULL

index.size = nrow(index.table)
progress.quantiles = round(quantile(1:index.size, seq(0, 1, by=0.05)))

for (i in 1:index.size) {
  row = index.table[i, ]
  symbol = row$Symbol

  current.frame = row$Path %>%
    read.csv(nrows=rows.to.read) %>%
    tbl_df() %>%
    mutate(Date=as.Date(Date), Price=as.numeric(Adj.Close)) %>%
    filter(Date >= period.start, Date <= period.end) %>%
    select(Date, Price)

  if (Check.Sanity(symbol, current.frame)) {
    if (is.null(all.logs) || is.null(all.returns)) {
        all.logs = data.frame(matrix(, nrow=nrow(current.frame), ncol=0))
        rownames(all.logs) = current.frame$Date
        all.returns = data.frame(matrix(, nrow=nrow(current.frame) - 1, ncol=0))
        rownames(all.returns) = current.frame$Date[-1]
      }
      all.logs[[symbol]] = log(current.frame$Price)
      all.returns[[symbol]] = diff(log(current.frame$Price))
  }

  if (i %in% progress.quantiles) {
    message("...in progress ", round(100 * i / index.size), "%")
  }
}


########################################################################################################################
# Returns analysis: candidates.
########################################################################################################################


# Vidyamurthy (Pairs Trading: Quantitative Methods and Analysis) suggests running risk-factor analysis first,
# then using innovations of factor components. We're using innovations of total log-prices, including a
# specific component, i.e. returns.
# It's much simpler and for daily and lower frequency seems to be a good approx.

# Compute usual stats
message("Computing covariance and correlation matrices of returns")
return.mu = apply(all.returns, 2, mean)
return.sigma2 = apply(all.returns, 2, var)
return.sigma = sqrt(return.sigma2)
return.cov.matrix = var(all.returns)
return.corr.matrix = cor(all.returns)

# Util: creates an empty data frame of a fixed size.
Empty.DF = function(rows, names) {
  cols = length(names)
  result = data.frame(matrix(rep(NA, rows * cols), nrow=rows, ncol=cols))
  colnames(result) = names
  result
}

# Finds the candidates.
Select.Candidates = function(return.corr.matrix, corr.threshold) {
  message("Selecting the candidates from the correletion matrix with threshold=", corr.threshold)

  matrix.size = nrow(return.corr.matrix)
  result.size = (length(return.corr.matrix[abs(return.corr.matrix) > corr.threshold]) - matrix.size) / 2
  message("Estimated number of candidates: ", result.size)
  
  row.names = rownames(return.corr.matrix)
  col.names = colnames(return.corr.matrix)
  
  index = 1
  progress.quantiles = round(quantile(1:result.size, seq(0, 1.0, by=0.05)))
  result.frame = Empty.DF(result.size, c("Symbol1", "Symbol2", "Return.Corr"))
  for (i in 1:(matrix.size-1)) {
    row.i = return.corr.matrix[i, ]
    for (j in (i+1):matrix.size) {
      if (abs(row.i[j]) > corr.threshold) {
        result.frame[index, ] = c(row.names[i], col.names[j], row.i[j])
        index = index + 1
        if (index %in% progress.quantiles) {
          message("...in progress ", round(100 * index / result.size), "%")
        }
      }
    }
  }
  
  result.frame[order(result.frame$Return.Corr, decreasing=TRUE), ]
}

candidates = Select.Candidates(return.corr.matrix, corr.threshold=0.85)
candidates.size = nrow(candidates)

message("Selected candidates: ", candidates.size)
print(head(candidates, n=10))


########################################################################################################################
# Free some memory.
########################################################################################################################


# Some garbage first
rm(rows.to.read, period.start, period.end, index.size, index.table, index.path, tmp.row.num, tmp.min.date, tmp.max.date,
   current.frame, row, symbol)
candidate.symbols = unique(c(candidates$Symbol1, candidates$Symbol2))
all.logs = subset(all.logs, select=candidate.symbols)
rm(all.returns)
return.cov.matrix = return.cov.matrix[candidate.symbols, candidate.symbols]
rm(return.corr.matrix)


########################################################################################################################
# Test for stationarity.
########################################################################################################################


# Counts the number of zero (mean) crossings of the series.
Get.Zero.Count = function(series) {
  zero.series = series - mean(series)
  series.length = length(zero.series)
  sign.change = sign(zero.series[-series.length]) * sign(zero.series[-1])
  return (sum(sign.change < 0) + sum(sign.change == 0) / 2)
}


# Tests for stationarity.
# Ruppert (Statistics and Data Analysis for Financial Engineering) suggests running the augmented Dickey–Fuller (ADF)
# test, Phillips–Perron (PP) test, similar to the ADF test but differs in some details, and 
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. In addition we do the Ljung-Box test and zero-crossing rate.
Stationary.Test = function(series) {
  # KPSS: the null hypothesis means that the series is stationarity, and small p-values suggest it is not.
  # See http://stats.stackexchange.com/questions/13213/how-to-interpret-kpss-results
  kpss.test.result = suppressWarnings(kpss.test(series))
  kpss.p.value = kpss.test.result$p.value
  kpss.statistic = kpss.test.result$statistic
  if (kpss.p.value < 0.05) {
    return (NULL)
  }
  
  # ADF: the null hypothesis is that there is a unit root, and small p-values incicate possible stationarity.
  adf.test.result = suppressWarnings(adf.test(series))
  adf.p.value = adf.test.result$p.value
  
  # PP: similar to ADF: small p-values indicate possible stationarity.
  pp.test.result = suppressWarnings(pp.test(series))
  pp.p.value = pp.test.result$p.value
  
  if (adf.p.value > 0.3 && pp.p.value > 0.3) {
    return (NULL)
  }

  # Zero count: should be high enough to be tradable.
  zero.count = Get.Zero.Count(series)
  zero.rate = zero.count / length(series)

  if (zero.rate < 0.15) {
    return (NULL)
  }

  # The Ljung-Box test examines whether there is significant evidence for non-zero correlations at lags 1-3.
  # If the p-value is small, it indicates that at least one of the first five autocorrelations is nonzero.
  lb.test.result = Box.test(series, lag=3, fitdf=3, type="Ljung-Box")
  lb.p.value = lb.test.result$p.value
  
  return (list(value=kpss.statistic,
               series=series,
               stationarity=c(kpss.statistic, kpss.p.value, adf.p.value, pp.p.value, lb.p.value),
               zero.count=zero.count))
}


########################################################################################################################
# More detailed analysis.
########################################################################################################################


# Finds the best ratio for the gamma coefficient, assuming it's greater than or equal to 1.
# The reason is that it's impossible to buy fractions of shares.
# Note that the more maximum limit is, the more precise is the ratio, but also the bigger bugdet is needed to trade the
# pair. Hence the function tries to find an approximation with smallest values possible.
Best.Ratio = function(gamma, max=50) {
  a = 10000
  b = 2

  sign = sign(gamma)
  gamma = abs(gamma)
  if (gamma > max) {
    return (c(max, sign * 1, gamma - max))
  }

  min = c(1000000, 0, 0)
  for (n in 1:max) {
    for (m in 1:n) {
      error = abs(gamma - (n / m))
      value = a * error + b * n
      if (value < min[1]) {
        min = c(value, n, sign * m, error)
      }
    }
  }

  return (min[-1])
}


# Performs the analysis of the spread series.
Spread.Analysis = function(spread, zero.count=NULL) {
  if (is.null(zero.count)) {
    zero.count = Get.Zero.Count(spread)
  }
  spread.mean = mean(spread)
  spread.sd = sd(spread)
  profitability = zero.count * spread.sd
  return (c(zero.count, profitability, spread.mean, spread.sd))
}


########################################################################################################################
# Testing all candidates for stationarity.
########################################################################################################################


# Prepare covariance and correlation matrix for log-prices.
logs.cov.matrix = var(all.logs)
logs.corr.matrix = cor(all.logs)

message("Testing all candidates for stationarity")

cointegrated.pairs = Empty.DF(candidates.size,
                              names=c("Symbol1", "Symbol2", "Gamma",
                                      "Zero.Count", "Profitability", "Spread.Mean", "Spread.SD",
                                      "KPSS.Stat", "KPSS.P", "ADF.P", "PP.P", "LB.P",
                                      "Return.Corr", "Log.Price.Corr"))

tradable.pairs = Empty.DF(candidates.size,
                          names=c("Symbol1", "Symbol2", "Gamma.Exact", "Gamma.Num", "Gamma.Denom", "Gamma.Error",
                                  "Position.Cost", "Zero.Count", "Profitability", "Spread.Mean", "Spread.SD"))

# Maximum risk allowed for trading.
max.risk.sd = 0.02

index = 1
progress.quantiles = round(quantile(1:candidates.size, seq(0, 1, by=0.05)))
for (i in 1:candidates.size) {
  if (i %in% progress.quantiles) {
    message("...in progress ", round(100 * i / candidates.size), "%")
  }

  candidate = candidates[i, ]
  symbol1 = candidate$Symbol1
  symbol2 = candidate$Symbol2
  logp1 = all.logs[[symbol1]]
  logp2 = all.logs[[symbol2]]

  regression.return = return.cov.matrix[symbol1, symbol2] / return.cov.matrix[symbol2, symbol2]
  regression.log = logs.cov.matrix[symbol1, symbol2] / logs.cov.matrix[symbol2, symbol2]

  # If regression coeff are far off, find the best fit from the range. Note: takes more time!
  gamma.range = c(regression.return, regression.log)
  if (abs(regression.return - regression.log) > 0.1) {
    gamma.range = seq(regression.return, regression.log, length.out=11)
  } else if (abs(regression.return - regression.log) > 0.05) {
    gamma.range = seq(regression.return, regression.log, length.out=5)
  }

  best.fit = NULL
  min.stat = 1000
  for (gamma.coeff in gamma.range) {
    if (abs(gamma.coeff) > 1) {
      pair = c(symbol1, symbol2)
      spread = logp1 - gamma.coeff * logp2
    } else {
      gamma.coeff = 1 / gamma.coeff
      pair = c(symbol2, symbol1)
      spread = logp2 - gamma.coeff * logp1
    }

    test.result = Stationary.Test(spread)
    if (!is.null(test.result) && test.result$value < min.stat) {
      min.stat = test.result$value
      best.fit = list(pair, gamma.coeff, test.result)
    }
  }
  if (is.null(best.fit)) {
    next
  }

  # Fill in the best fitted data.
  pair = best.fit[[1]]
  gamma.coeff = best.fit[[2]]
  test.result = best.fit[[3]]

  # Exact co-cointegrated pairs.
  spread.analysis = Spread.Analysis(spread=test.result$series, zero.count=test.result$zero.count)
  stationarity = test.result$stationarity
  return.corr = candidate$Return.Corr
  logs.corr = logs.corr.matrix[symbol1, symbol2]
  cointegrated.pairs[i, ] = c(pair, gamma.coeff, spread.analysis, stationarity, return.corr, logs.corr)

  # Tradable pairs: some fraction of co-cointegrated pairs.
  if (pair[[1]] != symbol1) {
    tmp = logp1
    logp1 = logp2
    logp2 = tmp
  }

  gamma.ratio = Best.Ratio(gamma.coeff)
  log.spread = logp1 * gamma.ratio[2] - logp2 * gamma.ratio[1]
  price.spread = exp(logp1) * gamma.ratio[2] - exp(logp2) * gamma.ratio[1]
  if (sd(log.spread) >= max.risk.sd || is.null(Stationary.Test(log.spread))) {
    next
  }

  position.cost = max(abs(price.spread))  # Transaction costs and bid-ask spread ignored.
  spread.analysis = Spread.Analysis(spread=log.spread)
  tradable.pairs[i, ] = c(pair, gamma.coeff, gamma.ratio, position.cost, spread.analysis)
}


########################################################################################################################
# Clean and save results.
########################################################################################################################


Clean.DF = function(frame) {
  frame %>%
    tbl_df() %>%
    filter(!is.na(Symbol1), !is.na(Symbol2)) %>%
    mutate_each_(funs(as.numeric), colnames(frame)[-c(1, 2)])
}


Report.DF = function(frame, description, file.name) {
  message("Found ", description, ": ", nrow(frame))
  print(frame, n=10)
  write.csv(file=file.name, frame)
  message("Saved ", description, " to ", file.name)
  frame
}

cointegrated.pairs = cointegrated.pairs %>%
  Clean.DF() %>%
  arrange(KPSS.Stat) %>%
  Report.DF(description="cointegrated pairs", file.name="result-cointegrated-pairs.csv")

tradable.pairs = tradable.pairs %>%
  Clean.DF() %>%
  arrange(desc(Profitability)) %>%
  Report.DF(description="tradable pairs", file.name="result-tradable-pairs.csv")

# Free everything except for important stuff.
suppressMessages(library(gdata))
keep(cointegrated.pairs, tradable.pairs, all.logs, sure=TRUE)


########################################################################################################################
# Plots.
########################################################################################################################


suppressMessages(library(ggplot2))
suppressMessages(require(reshape2))

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

message("Selection completed")

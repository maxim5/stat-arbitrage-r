#!/usr/bin/Rscript
# Performs the analysis to select possible pairs for cointegration.
# TODO: Make cmd arguments for: index.path, period.start, period.end, corr.threshold.


suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(magrittr))
suppressMessages(library(tseries))

invisible(Sys.setlocale("LC_TIME", "en_US.UTF-8"))


########################################################################################################################
# Read the index.
########################################################################################################################


index.path = "../dat/us-stocks-daily-raw/_index.csv"
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
check.sanity = function(symbol, current.frame) {
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
# Reading individual stock data.
########################################################################################################################


message("Reading individual stock data")

# Quote data is reversed (most recent come first).
# This is a little bit more than needed, because not all days are traded, but it will suffice.
rows.to.read = as.numeric(max(index.table$To.Date) - period.start)

all.returns = NULL
all.innovations = NULL

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

  if (check.sanity(symbol, current.frame)) {
    if (is.null(all.returns) || is.null(all.innovations)) {
        all.returns = data.frame(matrix(, nrow=nrow(current.frame), ncol=0))
        rownames(all.returns) = current.frame$Date
        all.innovations = data.frame(matrix(, nrow=nrow(current.frame) - 1, ncol=0))
        rownames(all.innovations) = current.frame$Date[-1]
      }
      all.returns[[symbol]] = log(current.frame$Price)
      all.innovations[[symbol]] = diff(log(current.frame$Price))
  }

  if (i %in% progress.quantiles) {
    message("...in progress ", round(100 * i / index.size), "%")
  }
}


########################################################################################################################
# Returns innovations analysis: candidates.
########################################################################################################################


# Vidyamurthy (Pairs Trading: Quantitative Methods and Analysis) suggests running risk-factor analysis first,
# then using innovations of factor components. We're using innovations of total returns, including a specific component.
# It's much simpler and for daily and lower frequency seems to be a good approx.

# Compute usual stats
message("Computing covariance and correlation matrices of innovations")
innov.mu = apply(all.innovations, 2, mean)
innov.sigma2 = apply(all.innovations, 2, var)
innov.sigma = sqrt(innov.sigma2)
innov.cov.matrix = var(all.innovations)
innov.corr.matrix = cor(all.innovations)

# Util: creates an empty data frame of a fixed size.
empty.df = function(rows, names) {
  cols = length(names)
  result = data.frame(matrix(rep(NA, rows * cols), nrow=rows, ncol=cols))
  colnames(result) = names
  result
}

# Finds the candidates.
select.candidates = function(innov.corr.matrix, corr.threshold) {
  message("Selecting the candidates from the correletion matrix with threshold=", corr.threshold)

  matrix.size = nrow(innov.corr.matrix)
  result.size = (length(innov.corr.matrix[abs(innov.corr.matrix) > corr.threshold]) - matrix.size) / 2
  message("Estimated number of candidates: ", result.size)
  
  row.names = rownames(innov.corr.matrix)
  col.names = colnames(innov.corr.matrix)
  
  index = 1
  progress.quantiles = round(quantile(1:result.size, seq(0, 1.0, by=0.05)))
  result.frame = empty.df(result.size, c("Symbol1", "Symbol2", "Innov.Corr"))
  for (i in 1:(matrix.size-1)) {
    row.i = innov.corr.matrix[i, ]
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
  
  result.frame[order(result.frame$Innov.Corr, decreasing=TRUE), ]
}

candidates = select.candidates(innov.corr.matrix, corr.threshold=0.85)
candidates.size = nrow(candidates)

message("Selected candidates: ", candidates.size)
print(head(candidates, n=10))

# Free some memory
candidate.symbols = unique(c(candidates$Symbol1, candidates$Symbol2))
innov.cov.matrix = innov.cov.matrix[candidate.symbols, candidate.symbols]
rm(innov.corr.matrix)


########################################################################################################################
# Returns analysis: test for stationarity.
########################################################################################################################


# Tests for stationarity.
# Ruppert (Statistics and Data Analysis for Financial Engineering) suggests running the augmented Dickey–Fuller (ADF)
# test, Phillips–Perron (PP) test, similar to the ADF test but differs in some details, and 
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test. In addition we do the Ljung-Box test and zero-crossing rate.
stationary.test = function(series) {
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
  
  zero.series = series - mean(series)
  series.length = length(zero.series)
  sign.change = sign(zero.series[-series.length]) * sign(zero.series[-1])
  zero.count = sum(sign.change < 0) + sum(sign.change == 0) / 2
  
  if (zero.count / series.length < 0.15) {
    return (NULL)
  }

  # The Ljung-Box test examines whether there is significant evidence for non-zero correlations at lags 1-3.
  # If the p-value is small, it indicates that at least one of the first five autocorrelations is nonzero.
  lb.test.result = Box.test(series, lag=3, fitdf=3, type="Ljung-Box")
  lb.p.value = lb.test.result$p.value
  
  return (c(kpss.statistic, zero.count, kpss.p.value, adf.p.value, pp.p.value, lb.p.value))
}


message("Running full test for stationarity")
names = c("Symbol1", "Symbol2", "Gamma", "Innov.Corr", "KPSS.Stat", "Zero.Count", "KPSS.P", "ADF.P", "PP.P", "LB.P")
cointegrated.pairs = empty.df(candidates.size, names)

index = 1
progress.quantiles = round(quantile(1:candidates.size, seq(0, 1, by=0.05)))
for (i in 1:candidates.size) {
  candidate = candidates[i, ]
  symbol1 = candidate$Symbol1
  symbol2 = candidate$Symbol2
  innov.corr = candidate$Innov.Corr
  gamma.coeff = innov.cov.matrix[symbol1, symbol2] / innov.cov.matrix[symbol2, symbol2]
  
  spread = all.returns[[symbol1]] - gamma.coeff * all.returns[[symbol2]]
  test.result = stationary.test(spread)
  if (!is.null(test.result)) {
    cointegrated.pairs[index, ] = c(symbol1, symbol2, gamma.coeff, innov.corr, test.result)
    index = index + 1
  }
  if (i %in% progress.quantiles) {
    message("...in progress ", round(100 * i / candidates.size), "%")
  }
}

cointegrated.pairs = cointegrated.pairs %>%
  tbl_df() %>%
  filter(!is.na(Symbol1), !is.na(Symbol2)) %>%
  mutate_each_(funs(as.numeric), names[-c(1, 2)]) %>%
  arrange(KPSS.Stat)

message("Found cointegrated pairs: ", nrow(cointegrated.pairs))
print(cointegrated.pairs, n=10)

write.csv(file="cointegrated.pairs.csv", cointegrated.pairs)
message("Saved to cointegrated.pairs.csv")


########################################################################################################################
# Plots.
########################################################################################################################


make.plots = function(symbol1, symbol2) {
  gamma = as.numeric(cointegrated.pairs[cointegrated.pairs$Symbol1 == symbol1 & 
                                        cointegrated.pairs$Symbol2 == symbol2, "Gamma"])
  
  series1 = all.returns[[symbol1]]
  series2 = all.returns[[symbol2]]
  dates = as.Date(rownames(all.returns))

  data.to.plot = data.frame(series1, series2, dates)
  colnames(data.to.plot) = c(symbol1, symbol2, "Date")

  plot = ggplot(data.to.plot, aes(x=Date)) +
    geom_line(aes_q(y=as.name(symbol1), colour="red")) +
    geom_line(aes_q(y=as.name(symbol2), colour="blue")) +
    labs(title=paste("Comparison of returns (log prices):", symbol1, symbol2))
  print(plot)
  
  spread = series1 - gamma * series2
  plot(x=dates, y=spread, type="l",
       xlab="Time", ylab="Spread",
       main=paste("Spread of returns:", symbol1, symbol2))
  abline(h = mean(spread))
}

#message("Making the plots")
#for (i in 1:nrow(cointegrated.pairs)) {
#  pair = cointegrated.pairs[i, ]
#  make.plots(pair$Symbol1, pair$Symbol2)
#}

message("Selection completed")

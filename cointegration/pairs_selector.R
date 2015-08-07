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


index.path = "../dat/us-stocks-daily-raw/_index.csv"
message("Reading index from ", index.path)
index.table = index.path %>%
  read.csv() %>%
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
    warning(symbol, ". Inconsistent rows number: ", row.num)
    return (FALSE)
  }
  tmp.row.num <<- row.num

  min.date = min(current.frame$Date)
  if (tmp.min.date > 0 && tmp.min.date != min.date) {
    warning(symbol, ". Invalid start date: ", min.date)
    return (FALSE)
  }
  tmp.min.date <<- min.date

  max.date = max(current.frame$Date)
  if (tmp.max.date > 0 && tmp.max.date != max.date) {
    warning(symbol, ". Invalid end date: ", max.date)
    return (FALSE)
  }
  tmp.max.date <<- max.date
  
  return (TRUE)
}


########################################################################################################################
# Reading individual stock data.
########################################################################################################################


# Quote data is reversed (most recent come first).
# This is a little bit more than needed, because not all days are traded, but it will suffice.
rows.to.read = as.numeric(max(index.table$To.Date) - period.start)

all.returns = NULL
all.innovations = NULL
invisible(apply(index.table, 1, function(row) {
  symbol = row["Symbol"]
  
  current.frame = row["Path"] %>%
    read.csv(nrows=rows.to.read) %>%
    tbl_df() %>%
    mutate(Date=as.Date(Date), Price=as.numeric(Adj.Close)) %>%
    filter(Date >= period.start, Date <= period.end) %>%
    select(Date, Price)

  if (!check.sanity(symbol, current.frame)) {
    return (0)
  }

  if (is.null(all.returns) || is.null(all.innovations)) {
    all.returns <<- data.frame(matrix(, nrow=nrow(current.frame), ncol=0))
    rownames(all.returns) <<- current.frame$Date
    all.innovations <<- data.frame(matrix(, nrow=nrow(current.frame) - 1, ncol=0))
    rownames(all.innovations) <<- current.frame$Date[-1]
  }
  all.returns[[symbol]] <<- log(current.frame$Price)
  all.innovations[[symbol]] <<- diff(log(current.frame$Price))

  message(symbol, " done")
}))


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

# Finds the candidates
select.candidates = function(innov.corr.matrix, corr.threshold) {
  message("Selecting the candidates from the correletion matrix with threshold=", corr.threshold)

  matrix.size = nrow(innov.corr.matrix)
  result.size = (length(innov.corr.matrix[innov.corr.matrix > corr.threshold]) - matrix.size) / 2
  message("Estimated number of candidates: ", result.size)
  
  row.names = rownames(innov.corr.matrix)
  col.names = colnames(innov.corr.matrix)
  
  index = 1
  progress.quantiles = round(quantile(1:result.size, seq(0.1, 0.9, by=0.1)))
  result.frame = data.frame(First.Symbol=rep(NA, result.size),
                            Second.Symbol=rep(NA, result.size),
                            Correlation=rep(NA, result.size))
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
  
  result.frame[order(result.frame$Correlation, decreasing=TRUE), ]
}

candidates = select.candidates(stats.corr.matrix, corr.threshold=0.85)

message("Selected candidates:", nrow(candidates))
print(head(candidates, n=10))


########################################################################################################################
# Returns analysis: test for stationarity.
########################################################################################################################


invisible(apply(candidates, 1, function(column) {
  symbol1 = column["First.Symbol"]
  symbol2 = column["Second.Symbol"]
  gamma.coeff = innov.cov.matrix[symbol1, symbol2] / innov.cov.matrix[symbol2, symbol2]
  
  series1 = all.returns[[symbol1]]
  series2 = gamma.coeff * all.returns[[symbol2]]
  diff = series1 - series2
  
  # Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
  # See http://stats.stackexchange.com/questions/13213/how-to-interpret-kpss-results
  test = suppressWarnings(kpss.test(diff))
  if (test$statistic < 0.1) {
    message("Pair ", symbol1, "/", symbol2, " passed the KPSS test with gamma=", gamma.coeff,
            ". KPSS-statistic=", test$statistic, ". innov-corr=", column["Correlation"])
  }
}))

message("Selection completed")

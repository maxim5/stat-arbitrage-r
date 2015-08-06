#!/usr/bin/Rscript
# Performs the analysis to select possible pairs for cointegration.
# TODO: Make cmd arguments for: index.path, period.start, period.end, corr.threshold.


suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(xts))

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

period.start = as.Date("2010-01-01")
period.end = as.Date("2015-06-30")
message("Filtering rows available from ", period.start, " to ", period.end)
index.table = index.table %>% 
  filter(From.Date <= period.start, To.Date >= period.end) %>% top_n(100, Symbol)
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

  if (is.null(all.returns)) {
    all.returns <<- data.frame(matrix(, nrow=nrow(current.frame) - 1, ncol=0))
    rownames(all.returns) <<- current.frame$Date[-1]
  }
  all.returns[[symbol]] <<- diff(log(current.frame$Price))
  
  message(symbol, " done")
}))


########################################################################################################################
# Returns analysis: candidates.
########################################################################################################################


# Vidyamurthy (Pairs Trading: Quantitative Methods and Analysis) suggests running risk-factor analysis first,
# then using innovations of factor components. We're using innovations of total returns, including a specific component.
# It's much simpler and for daily and lower frequency seems to be a good approx.

# Compute usual stats
stats.mu = apply(all.returns, 2, mean)
stats.sigma2 = apply(all.returns, 2, var)
stats.sigma = sqrt(stats.sigma2)
stats.cov.matrix = var(all.returns)
stats.corr.matrix = cor(all.returns)

# Finds the candidates
select.candidates = function(stats.corr.matrix, corr.threshold) {
  patched.corr.matrix = abs(stats.corr.matrix)
  patched.corr.matrix[row(patched.corr.matrix) >= col(patched.corr.matrix)] = 0
  correlations = patched.corr.matrix[order(patched.corr.matrix, decreasing=TRUE)]
  
  tmp.first = c()
  tmp.second = c()
  tmp.corr = c()
  for (item in correlations) {
    if (item > corr.threshold) {
      dim = which(patched.corr.matrix == item, arr.ind=TRUE)
      tmp.first = c(tmp.first, rownames(stats.corr.matrix)[dim[1]])
      tmp.second = c(tmp.second, colnames(stats.corr.matrix)[dim[2]])
      tmp.corr = c(tmp.corr, stats.corr.matrix[dim])
    }
  }
  data.frame(First.Symbol=tmp.first, Second.Symbol=tmp.second, Correlation=tmp.corr)
}

candidates = select.candidates(stats.corr.matrix, corr.threshold=0.95)
candidates

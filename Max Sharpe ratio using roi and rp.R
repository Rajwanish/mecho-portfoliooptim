library(quantmod)
library(PortfolioAnalytics)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(corrplot)
library(nse2r)

#call the symbols of investment options
symbols <- nse_preopen_nifty(clean_names = TRUE)
tickers <- c(symbols$symbol <- paste0(symbols$symbol, ".NS"))
tickers

portfolioprices <- NULL
#date from when the data to be analyzed
since <- "2018-01-01"

for(ticker in tickers) {
  portfolioprices <- cbind(portfolioprices,
                           getSymbols.yahoo(ticker, from = since,
                                            periodicity = "daily",
                                            auto.assign=FALSE)[,6])
  
}


portfolioret <- na.omit(ROC(portfolioprices))


R <- portfolioret

#plot correlation matrix
portf.ret.cor <- cor(R, method = c("spearman"))
corrplot(
  portf.ret.cor,
  method = c("circle"),
  type = c("upper"),
  add = FALSE,
  col = NULL,
  col.lim = NULL,
  bg = "white",
  title = "Correlation Matrix",
  is.corr = TRUE,
  diag = TRUE,
  outline = FALSE,
  mar = c(0, 0, 0, 0),
  addgrid.col = NULL,
  addCoef.col = NULL,
  addCoefasPercent = FALSE,
  order = c("hclust"),
  hclust.method = c("centroid"),
  addrect = NULL,
  rect.col = "black",
  rect.lwd = 2,
  tl.pos = NULL,
  tl.cex = 1,
  tl.col = "red",
  tl.offset = 0.4,
  tl.srt = 90,
  cl.pos = NULL,
  cl.length = NULL,
  cl.cex = 0.8,
  cl.ratio = 0.15,
  cl.align.text = "c",
  cl.offset = 0.5,
  number.cex = 1,
  number.font = 2,
  number.digits = NULL,
  addshade = c("positive"),
  shade.lwd = 1,
  shade.col = "white",
  p.mat = NULL,
  sig.level = 0.05,
  insig = c("pch"),
  pch = 4,
  pch.col = "black",
  pch.cex = 3,
  plotCI = c("n"),
  lowCI.mat = NULL,
  uppCI.mat = NULL,
  na.label = "?",
  na.label.col = "black",
  win.asp = 1)

palette <- colorRampPalette(c("green", "blue", "red"))(40)
heatmap(x=portf.ret.cor, col=palette, symm = TRUE)

portf.ret.rcorr <- rcorr(as.matrix(R))
tail(portf.ret.rcorr)

funds <- colnames(R)
funds

#optimization of weights using random portfolio solver
init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
init.portf <- add.constraint(portfolio=init.portf, type="long_only")
init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
init.portf

init.portf$constraints[[1]]$min_sum=0.99
init.portf$constraints[[1]]$max_sum=1.01

init.portf <- add.objective(portfolio=init.portf, type="risk", 
                            name="StdDev", multiplier=0)

port1 <- add.constraint(portfolio=init.portf, type="diversification", 
                        min=0, max=1, indexnum=2)
port1 <- add.constraint(portfolio = init.portf, type="risk", name="StdDev")

maxSR.diverse.rp <- optimize.portfolio(R=R, portfolio = port1, 
                                       optimize_method="random", 
                                       search_size=50000, maxSR=TRUE,
                                       trace=TRUE)
maxSR.diverse.rp

#optimization of weights using ROI portfolio solver
init.portf2 <- portfolio.spec(assets=funds)
init.portf2 <- add.constraint(portfolio=init.portf2, type="full_investment")
init.portf2 <- add.constraint(portfolio=init.portf2, type="long_only")
init.portf2 <- add.objective(portfolio=init.portf2, type="return", name="mean")
init.portf2

init.portf2$constraints[[1]]$min_sum=1
init.portf2$constraints[[1]]$max_sum=1

init.portf2 <- add.objective(portfolio=init.portf2, type="risk", 
                            name="StdDev", multiplier=0)

port2 <- add.constraint(portfolio=init.portf2, type="diversification", 
                        min=0, max=1, indexnum=2)
port2 <- add.constraint(portfolio = init.portf2, type="risk", name="StdDev" )

maxSR.diverse.roi <- optimize.portfolio(R=R, portfolio = port2, 
                                       optimize_method="ROI", 
                                       search_size=20000, maxSR=TRUE,
                                       trace=TRUE)
maxSR.diverse.roi

#risk reward ratio chart
chart.RiskReward(maxSR.diverse.roi, risk.col = "StdDev", return.col = "mean")

#efficient frontier
ef <- extractEfficientFrontier(maxSR.diverse.roi, match.col = "StdDev",
                               n.portfolios = 25, risk_aversion = NULL)

chart.EfficientFrontier(ef,
                        match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                        cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier for ROI",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)

#benchmark portfolio with equal weights
equal.weight <- rep(1/ncol(R), ncol(R))
equal.wt.portf <- Return.portfolio(R, weights = equal.weight)
colnames(equal.wt.portf) <- "Equal Weight Portfolio"

#getting performance of nifty50 for comparison
nifty50prices <- getSymbols.yahoo("^NSEI", from = since,
                                  periodicity = "daily",
                                  auto.assign=FALSE)[,6]
nifty50ret <- na.omit(ROC(nifty50prices))

#returns from random optimization weights portfolio
maxSR.weight.rp <- extractWeights(maxSR.diverse.rp)
maxSR.ret.rp <- Return.portfolio(R, weights = maxSR.weight.rp)
colnames(maxSR.ret.rp) <- c("Random Portf Return")

#returns with ROI optimization weights portfolio
maxSR.weight.roi <- extractWeights(maxSR.diverse.roi)
maxSR.ret.roi <- Return.portfolio(R, weights = maxSR.weight.roi)
colnames(maxSR.ret.roi) <- c("ROI Portf Return")

#return table
rets.df <- na.omit(cbind(maxSR.ret.roi, maxSR.ret.rp, equal.wt.portf, nifty50ret))

#chart showing daily and cumulative return of benchmarks and optimized portfolios
charts.PerformanceSummary(rets.df, plot.engine = "plotly", main="P/L over time")

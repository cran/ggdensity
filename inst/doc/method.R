## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center",
  dpi = 160,
  out.width = "80%",
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library("ggdensity"); theme_set(theme_minimal(8))
theme_update(legend.position = "none") # Suppressing legends for readability

## -----------------------------------------------------------------------------
set.seed(1) 
df <- data.frame(x = rnorm(500), y = rnorm(500))
p <- ggplot(df, aes(x, y))
p + geom_point()

## ---- fig.show="hold", out.width="45%", fig.align = "default"-----------------
p + geom_hdr(method = "kde")

p + geom_hdr(method = "mvnorm")

p + geom_hdr(method = "histogram")

p + geom_hdr(method = "freqpoly")

## ---- fig.show="hold", out.width="45%", fig.align = "default"-----------------
p + geom_hdr(method = method_kde())

p + geom_hdr(method = method_mvnorm())

p + geom_hdr(method = method_histogram())

p + geom_hdr(method = method_freqpoly())

## -----------------------------------------------------------------------------
p + geom_hdr(method = method_kde(adjust = 1/2))

## -----------------------------------------------------------------------------
res <- get_hdr(df, method = method_kde(adjust = 1/2))

str(res)

## ---- fig.show="hold", out.width="45%", fig.align = "default"-----------------
p + 
  geom_point() +
  geom_hdr_rug(method = method_kde_1d())

p + 
  geom_point() +
  geom_hdr_rug(method = method_norm_1d())

p + 
  geom_point() +
  geom_hdr_rug(method = method_histogram_1d())

p + 
  geom_point() +
  geom_hdr_rug(method = method_freqpoly_1d())

## ---- fig.show="hold", out.width="45%", fig.align = "default"-----------------
p + 
  geom_point() +
  geom_hdr_rug(method = "kde")

p + 
  geom_point() +
  geom_hdr_rug(method = "norm")

p + 
  geom_point() +
  geom_hdr_rug(method = "histogram")

p + 
  geom_point() +
  geom_hdr_rug(method = "freqpoly")

## ---- fig.show = "hide"-------------------------------------------------------
p + 
  geom_point() +
  geom_hdr_rug(method = method_kde()) 

## -----------------------------------------------------------------------------
res <- get_hdr_1d(df$x, method = method_kde_1d())

str(res)

## ---- collapse = TRUE, comment = ""-------------------------------------------
method_kde

## ---- collapse = TRUE, comment = ""-------------------------------------------
method_mvnorm

## -----------------------------------------------------------------------------
method_mvnorm_ind <- function() {
  
  function(data) {
    
    xbar <- mean(data$x)
    ybar <- mean(data$y)
    
    sx <- sd(data$x)
    sy <- sd(data$y)
    
    # joint pdf is simply the product of the marginals
    function(x, y) dnorm(x, xbar, sx) * dnorm(y, ybar, sy)
  }
  
}

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_hdr(method = method_mvnorm_ind())

## -----------------------------------------------------------------------------
A <- matrix(c(
  2*cos(pi/6), -2*sin(pi/6),
  1*sin(pi/6),  1*cos(pi/6)
), byrow = TRUE, ncol = 2)

df_rot <- as.data.frame(as.matrix(df) %*% A)
colnames(df_rot) <- c("x", "y")

ggplot(df_rot, aes(x, y)) +
  geom_hdr(method = method_mvnorm_ind()) +
  geom_point(size = .4) +
  coord_fixed(xlim = c(-6, 6), ylim = c(-6, 6))

## -----------------------------------------------------------------------------
method_mvnorm_ind <- function(circular = FALSE) {
  
  function(data) {
    
    xbar <- mean(data$x)
    ybar <- mean(data$y)
    
    if (circular) {
      sx <- sd(c(data$x - xbar, data$y - ybar))
      sy <- sx
    } else {
      sx <- sd(data$x)
      sy <- sd(data$y)
    }
    
    function(x, y) dnorm(x, xbar, sx) * dnorm(y, ybar, sy)
  }
  
}

## -----------------------------------------------------------------------------
ggplot(df_rot, aes(x, y)) +
  geom_hdr(method = method_mvnorm_ind(circular = TRUE)) +
  geom_point(size = .4) +
  coord_fixed(xlim = c(-6, 6), ylim = c(-6, 6))

## ---- fig.show="hold", out.width="45%", fig.align = "default"-----------------
ggplot(df_rot, aes(x, y)) +
  geom_hdr(method = method_mvnorm_ind(circular = TRUE), ylim = c(-6, 6)) +
  geom_point(size = .4) +
  coord_fixed(xlim = c(-6, 6), ylim = c(-6, 6))

ggplot(df_rot, aes(x, y)) +
  geom_hdr(method = method_mvnorm_ind(circular = TRUE)) +
  geom_point(size = .4) +
  scale_y_continuous(limits = c(-6, 6)) +
  coord_fixed(xlim = c(-6, 6), ylim = c(-6, 6))

## -----------------------------------------------------------------------------
method_mvnorm_ind_grid <- function() {
  
  function(data, n, rangex, rangey) {
    
    # First, we estimate the density -----------------------------
    
    xbar <- mean(data$x)
    ybar <- mean(data$y)
    
    sx <- sd(data$x)
    sy <- sd(data$y)
    
    f_est <- function(x, y) dnorm(x, xbar, sx) * dnorm(y, ybar, sy)
    
    
    # Return the density evaluated on a grid ---------------------
    
    # df_grid defined by rangex, rangey, and n
    df_grid <- expand.grid(
      x = seq(rangex[1], rangex[2], length.out = n),
      y = seq(rangey[1], rangey[2], length.out = n)
    )
    
    df_grid$fhat <- f_est(df_grid$x, df_grid$y)
    
    df_grid
  }
  
}

## -----------------------------------------------------------------------------
ggplot(df, aes(x, y)) +
  geom_hdr(method = method_mvnorm_ind_grid())

## ---- collapse = TRUE, comment = ""-------------------------------------------
method_kde_1d

## ---- collapse = TRUE, comment = ""-------------------------------------------
method_norm_1d


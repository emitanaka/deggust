# based on chroma formula: https://github.com/gka/chroma.js/tree/main/src/io/hsl
hsl2rgb <- function(h, s, l) {
  res <- c(0, 0, 0)
  if (s == 0) {
    r <- g <- b <- l * 255
  } else {
    t3 <- c(0, 0, 0)
    t2 <- ifelse(l < 0.5, l * (1+s), l+s-l*s)
    t1 <- 2 * l - t2
    h_ <- h / 360
    t3[1] <- h_ + 1/3
    t3[2] <- h_
    t3[3] <- h_ - 1/3
    for(i in 1:3) {
      if (t3[i] < 0) t3[i] <- t3[i] + 1
      if (t3[i] > 1) t3[i] <- t3[i] - 1
      if (6 * t3[i] < 1) {
        res[i] <- t1 + (t2 - t1) * 6 * t3[i]
      } else if (2 * t3[i] < 1) {
        res[i] <- t2
      } else if (3 * t3[i] < 2) {
        res[i] <- t1 + (t2 - t1) * ((2 / 3) - t3[i]) * 6
      } else res[i] <- t1
    }
  }
  round(res * 255)
}

rgb2hsl <- function(r, g, b) {

  r <- r / 255
  g <- g / 255
  b <- b / 255

  min <- min(c(r, g, b))
  max <- max(c(r, g, b))

  l <- (max + min) / 2

  if (max == min){
    s <- 0
    h <- NA
  } else {
    s <- ifelse(l < 0.5, (max - min) / (max + min), (max - min) / (2 - max - min))
  }

  if (r == max) {
    h <- (g - b) / (max - min)
  } else if (g == max) {
    h <- 2 + (b - r) / (max - min)
  } else if (b == max) h <- 4 + (r - g) / (max - min)

  h <- h * 60
  if (h < 0) h <- h + 360
  c(h, s, l)
}


scale_fill_hslh <- function(h = 0, s = 0.7, l = 0.5, aesthetics = "fill",
                                       ...) {
  pal <- function(n) {
    vals <- sapply((seq(0, 360 * (n - 1) / n, length.out = n) + h) %% 360, function(x) hsl2rgb(x, s, l))
    rgb(vals[1,], vals[2,], vals[3,], maxColorValue = 255)
  }
  discrete_scale(aesthetics, "manual", pal, ...)
}

scale_fill_hsls <- function(h = 0, s = 0.7, l = 0.5, aesthetics = "fill",
                            ...) {
  pal <- function(n) {
    # changed to maximum of 0.8 since s = 1 is too much for me
    vals <- sapply((seq(0.05, 0.8 * (n - 1) / n, length.out = n) + s) %% 1, function(x) hsl2rgb(h, x, l))
    rgb(vals[1,], vals[2,], vals[3,], maxColorValue = 255)
  }
  discrete_scale(aesthetics, "manual", pal, ...)
}

scale_fill_hsll <- function(h = 0, s = 0.7, l = 0.5, aesthetics = "fill",
                            ...) {
  pal <- function(n) {
    vals <- sapply((seq(0, 1* (n - 1) / n, length.out = n) + l) %% 1, function(x) hsl2rgb(h, s, x))
    rgb(vals[1,], vals[2,], vals[3,], maxColorValue = 255)
  }
  discrete_scale(aesthetics, "manual", pal, ...)
}

calc_t_score <- function (x, mu, s, n, sig_level = NULL, p_val = FALSE,
                          alternative = "greater", shade = FALSE, digits = 4) {
  #calc_t_score takes some information from a hypothesis test
  # and calculates the appropriate t-score and p-value.
  #calc_t_score returns a sentence (or more than 1) highlighting
  # the sample and population values, the t-score, p-value, and
  # a conclusion for 1 or 2 sided hypothesis of 1 sample mean test.

  ##Arguments:
  #x = the mean or value in question/to be tested.
  #mu = the population mean.
  #s = the population standard deviation.
  #n = the sample size.  n = 1 is the default for the cases where
  # only one value is of interest (not the mean of a sample).
  #sig_level = the significance level for a hypothesis test.
  #p_val = a logical for if the p-value should be included in
  # the response.
  #alternative = the direction of the alternative hypothesis.
  # alternative can take 3 options ("greater", "less", "not equal to").
  # Currently, any value that isn't "greater" or "less" is assumed to be "not equal to".
  #shade = a logical for if the output should include a plot
  # that shades the area of interest and the resulting p-value.
  #digits = the number of decimal places to which the answer
  # should be rounded.  Note: positive numbers refer to the
  # number of digits after the decimal place, negative numbers
  # refer to the number of digits before the decimal place, and
  # 0 refers to the nearest whole number.

  # Need to add this later
  # I need to figure out how to
  # show the plot and the text together
  # if(shade){
  #   #shade function and output here.
  # }

  alternative <- ifelse(toupper(alternative) == "GREATER", 1,
                        ifelse(toupper(alternative) == "LESS", 2, 3))

  df <- ifelse(n == 1, 1, n - 1)
  se <- s/sqrt(n)
  t <- (x - mu) / se
  t <- round(t, digits = digits)

  if (!p_val) {

    return(paste("The t-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", s,
                 " is ", t, ".",
                 sep = "") )

  } else if (p_val & is.null(sig_level)) {
    l_tail <- ifelse(alternative == 1, FALSE, TRUE)
    neq <- ifelse(alternative == 3, TRUE, FALSE)
    if (neq) {
      l_tail <- ifelse(t > 0, FALSE, TRUE)
      p <- 2*pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      p <- ifelse(p < 0.0001, 0.0001, p)
    } else{
      p <- pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      p <- ifelse(p < 0.0001, 0.0001, p)
    } #end not equal to if statement

    return(paste("The t-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", s,
                 " is ", t, ".",
                 "  The associated p-value is ", p, ".",
                 sep = "") )

  } else if (p_val & !is.null(sig_level)) {
    l_tail <- ifelse(alternative == 1, FALSE, TRUE)
    neq <- ifelse (alternative == 3, TRUE, FALSE)
    if (neq) {
      l_tail <- ifelse(t > 0, FALSE, TRUE)
      p <- 2*pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      conc <- p <= sig_level
      p <- ifelse(p < 0.0001, 0.0001, p)
    } else{
      p <- pt(t, df = df, lower.tail = l_tail)
      p <- round(p, digits = digits)
      conc <- p <= sig_level
      p <- ifelse(p < 0.0001, 0.0001, p)
    } #end not equal to if statement

    return(paste("The t-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", s,
                 " is ", t, ".",
                 "  The associated p-value is ", p, ".",
                 "  For a level ", sig_level,
                 " hypothesis test, a p-value of ", p,
                 " suggests that we should ",
                 ifelse(conc, "reject", "not reject"),
                 " the null hypothesis that mu " ,
                 ifelse(alternative == 1,
                        paste("is at most ", mu, sep = ""),
                        ifelse(alternative == 2,
                               paste("is at least ", mu, sep = ""),
                               paste("is equal to ", mu, sep = ""))),
                 ".",
                 sep = "") )
  }
}#end calc_t_score




calc_z_score <- function (x, mu, sigma, n = 1, sig_level = NULL, p_val = FALSE,
                          alternative = "greater", shade = FALSE, digits = 4) {
  #calc_z_score takes some information from a hypothesis test
  # and calculates the appropriate z-score and p-value.
  #calc_z_score returns a sentence (or more than 1) highlighting
  # the sample and population values, the z-score, p-value, and
  # a conclusion for 1 or 2 sided hypothesis of 1 sample mean test.

  ##Arguments:
  #x = the mean or value in question/to be tested.
  #mu = the population mean.
  #sigma = the population standard deviation.
  #n = the sample size.  n = 1 is the default for the cases where
  # only one value is of interest (not the mean of a sample).
  #sig_level = the significance level for a hypothesis test.
  #p_val = a logical for if the p-value should be included in
  # the response.
  #alternative = the direction of the alternative hypothesis.
  # alternative can take 3 options ("greater", "less", "not equal to").
  # Currently, any value that isn't "greater" or "less" is assumed to be "not equal to".
  #shade = a logical for if the output should include a plot
  # that shades the area of interest and the resulting p-value.
  #digits = the number of decimal places to which the answer
  # should be rounded.  Note: positive numbers refer to the
  # number of digits after the decimal place, negative numbers
  # refer to the number of digits before the decimal place, and
  # 0 refers to the nearest whole number.


  # Need to add this later
  # I need to figure out how to
  # show the plot and the text together
  # if(shade){
  #   #shade function and output here.
  # }

  alternative <- ifelse(toupper(alternative) == "GREATER", 1,
                        ifelse(toupper(alternative) == "LESS", 2, 3))

  se <- sigma/sqrt(n)
  z <- (x - mu) / se
  z <- round(z, digits = digits)

  if (!p_val) {

    return(paste("The z-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", sigma,
                 " is ", z, ".",
                 sep = "") )

  } else if (p_val & is.null(sig_level)) {
    l_tail <- ifelse(alternative == 1, FALSE, TRUE)
    neq <- ifelse (alternative == 3, TRUE, FALSE)
    if (neq) {
      l_tail <- ifelse(z > 0, FALSE, TRUE)
      p <- 2*pnorm(z, lower.tail = l_tail)
      p <- round(p, digits = digits)
      p <- ifelse(p < 0.0001, 0.0001, p)
    } else{
      p <- pnorm(z, lower.tail = l_tail)
      p <- round(p, digits = digits)
      p <- ifelse(p < 0.0001, 0.0001, p)
    } #end not equal to if statement

    return(paste("The z-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", sigma,
                 " is ", z, ".",
                 "  The associated p-value is ", p, ".",
                 sep = "") )

  } else if (p_val & !is.null(sig_level)) {
    l_tail <- ifelse(alternative == 1, FALSE, TRUE)
    neq <- ifelse (alternative == 3, TRUE, FALSE)
    if (neq) {
      l_tail <- ifelse(z > 0, FALSE, TRUE)
      p <- 2*pnorm(z, lower.tail = l_tail)
      p <- round(p, digits = digits)
      conc <- p <= sig_level
      p <- ifelse(p < 0.0001, 0.0001, p)
    } else{
      p <- pnorm(z, lower.tail = l_tail)
      p <- round(p, digits = digits)
      conc <- p <= sig_level
      p <- ifelse(p < 0.0001, 0.0001, p)
    } #end not equal to if statement

    return(paste("The z-score for the mean ", x,
                 ", for a sample of size ", n,
                 " from a distribution with mean ", mu,
                 " and standard deviation ", sigma,
                 " is ", z, ".",
                 "  The associated p-value is ", p, ".",
                 "  For a level ", sig_level,
                 " hypothesis test, a p-value of ", p,
                 " suggests that we should ",
                 ifelse(conc, "reject", "not reject"),
                 " the null hypothesis that mu " ,
                 ifelse(alternative == 1,
                        paste("is at most ", mu, sep = ""),
                        ifelse(alternative == 2,
                               paste("is at least ", mu, sep = ""),
                               paste("is equal to ", mu, sep = ""))),
                 ".",
                 sep = "") )
  }

}#end calc_z_score




shade_area <- function(start = NULL, stop = NULL, prob = NULL, mu = 0, sd = 1, n = 1,
                       direction = "greater", test = "z", fill = "orange", digits = 4) {
  #shade_area takes some values of interest and plots a normal or
  # standardized t density with an area shaded corresponding to
  # the information provided.  For example, if a student was hoping
  # to visualize what area of a density function an introductory
  # z-score or hypothesis test question was asking for, they could enter
  # the X value, the mean, sd, and direction of interest.  shade_area
  # will spit back a plot of the normal distribution associated with the
  # mean/sd given, and will shade the tail (or middle) that answers the
  # question.
  #shade_area returns a plot.  (Hope to add text with the plot someday.)
  #shade_area requires that at least one of start or prob is given.



  ##Arguments:
  #start = an X value or z-score that defines the z-score or t-score
  # that is of interest.
  #stop = a value to be used if there is a clearly defined end range
  # Note: stop only needs to be used if the interest is in some range
  # over the middle of the distribution.
  #prob = the area under the curve or the probability that is wanted
  # to be shaded.
  #mu = the mean of the distribution.
  #sd = the standard deviation of the distribution.
  #n = the sample size.
  #direction = the direction of the area to be shaded.
  # direction takes 4 possible values, c("greater", "less",
  # "middle", "both").
  # "greater" = the right tail.
  # "less" = the left tail.
  # "middle" = the middle of the distribution.
  # "both" = both the left and right tails.
  # Note: if "both" is selected with a start value, then
  # the start value should be the lower value given.  If the value
  # provided is greater than the mean, then the entire distribution
  # will be shaded.
  #test = the proper distribution to shade.
  # test takes two values ("z", "t"), corresponding to a z test or
  # a t-test.  Currently only 1 sample mean tests are supported.
  #fill = the color to be used to shade the tails.
  #digits = the number of decimal places to which the axis ticks
  # should be rounded.  Note: positive numbers refer to the
  # number of digits after the decimal place, negative numbers
  # refer to the number of digits before the decimal place, and
  # 0 refers to the nearest whole number.

  #direction can be greater, less, middle, both
  se <- sd/sqrt(n)
  deg_free <- ifelse(n == 1, n, n - 1)
  lims <- 3

  if(toupper(test) != "Z" & toupper(test) != "T"){
    stop(paste("Error: please enter either 'z' or 't' for the test value."))
  }
  if(is.null(start) & is.null(prob)){
    stop("Error: either start or prob is required.")
  }#error invalid test

  if(toupper(test) == "T"){
    start <- (start - mu) / se
    stop <- if(!is.null(stop)) {(stop - mu) / se}
    lims <- min(qt(.999, deg_free), 4)
  }


  if(!is.null(prob)){
    p <- prob

    prob <- ifelse(toupper(direction) == "MIDDLE", prob/2 + .50, prob)
    prob <- ifelse(toupper(direction) == "GREATER", 1 - prob, prob)
    prob <- ifelse(toupper(direction) == "BOTH", prob/2, prob)

    start <- ifelse(toupper(test) == "Z", qnorm(prob), qt(prob, df = deg_free))
    stop <- ifelse(toupper(direction) == "MIDDLE", -start, -1000)
    stop <- ifelse(toupper(direction) == "GREATER", -stop, stop)

    if(toupper(test) == "Z"){
      start <- mu + start * se
      stop <- mu + stop * se
    }
  }#end !is.null(prob)

  if(toupper(test) == "Z"){
    x <- seq(from = mu - lims * se, to = mu + lims * se, by = 0.01)
  } else {
    x <- seq(from = -lims, to = lims, by = 0.01)
  }#end test limits

  if(toupper(test) == "Z"){
    y <- dnorm(x, mean = mu, sd = se)
  }else if(toupper(test) == "T"){
    y <- dt(x, df = deg_free)
  }#end dnorm/dt
  df <- data.frame(x = x, y = y)
  stop <- ifelse(is.null(stop), mu + (-1000)*se, stop)
  if(is.null(prob)){
    stop <- ifelse(toupper(direction) == "GREATER", -stop, stop)
  }#end is.null(prob)

  if(stop < start){
    temp <- start
    start <- stop
    stop <- temp
  }

  if(toupper(direction) == "BOTH" & toupper(test) == "Z"){
    fstart <- -start
    fstop <- (mu - stop) + mu
  }else {
    fstart <- -start
    fstop <- -stop
  }#end start/stop flips

  brks <- round(seq(-lims, lims, length = 7), digits = digits)
  if(toupper(test) == "Z"){brks <- round((-3:3) * se + mu, digits = digits)}
  p <- abs(ifelse(toupper(test) == "Z",
                  pnorm(stop, mean = mu, sd = sd) - pnorm(start, mean = mu, sd = sd),
                  pt(stop, df = deg_free) - pt(start, df = deg_free)))



  if(toupper(direction) == "BOTH"){
    shade_plot <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_area(data = subset(df, x >= start
                              & x <= stop),
                aes(y=y), fill = fill) +
      geom_area(data = subset(df, x >= fstop
                              & x <= fstart),
                aes(y=y), fill = fill) +
      scale_x_continuous(breaks = brks) +
      scale_y_continuous(breaks = NULL) +
      theme(panel.background = element_blank(),
            axis.line.x = element_line(),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      ylab("") +
      xlab("") +
      ggtitle(paste(ifelse(toupper(test) == "Z",
                           "Normal Distribution",
                           paste("T Distribution with ", deg_free,
                                 " Degree(s) of Freedom", sep = "")),
                    sep = ""))

  } else {
    shade_plot <- ggplot(df, aes(x = x, y = y)) +
      geom_line() +
      geom_area(data = subset(df, x >= start
                              & x < stop),
                aes(y = y), fill = "orange") +
      scale_x_continuous(breaks = brks) +
      scale_y_continuous(breaks = NULL) +
      theme(panel.background = element_blank(),
            axis.line.x = element_line(),
            plot.title = element_text(hjust = 0.5, face = "bold")) +
      ylab("") +
      xlab("") +
      ggtitle(paste(ifelse(toupper(test) == "Z",
                           "Normal Distribution",
                           paste("T Distribution with ", deg_free,
                                 " Degrees of Freedom", sep = "")),
                    sep = ""))
  }

  return(shade_plot)


}#end shade_area function





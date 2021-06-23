#### Load libraries and data ####
library(tidyverse)
library(dslabs)
library(ggplot2)
library(dplyr)
data(polls_2008)
data(brexit_polls)
data(polls_us_election_2016)

#### 2008 US Presidential Election ####

# Let's estimate the time trend in Barrack Obama's polling 
# margin over John McCain through 2008. The graphic below is
# a simple scatterplot:
qplot(day, margin, data = polls_2008)

# Don't think of it as a forecasting problem; just focus on the
# trend. Assume that for any given day x, there's a true pre-
# ference abong the electorate f(x), but each polling point is
# obscured by error epsilon.

# The observed poll margin Y_i follows:
# Y_i = f(x_i) + epsilon_i

# To estimate the expected value of Y given x_i, let's start
# with linear regression:
p08_fit <- lm(margin ~ day, polls_2008)
p08_pre <- predict(p08_fit, polls_2008)
polls_2008 %>%
  mutate(resid = ifelse(margin > p08_pre, "+", "-")) %>%
  ggplot(aes(day, margin, color = resid)) +
  geom_point(size = 3) +
  geom_abline(intercept = p08_fit$coefficients[1], 
              slope = p08_fit$coefficients[2],
              size = 1.2)

# Not good! McCain's support genuinely peaked during the RNC,
# but the regression line dismisses it as noise. The points 
# above and below the line are not evenly distributed. We 
# need a more flexible approach.

# Bin smoothing involves grouping data points into strata that
# have constant values of f(x), allowing us to detect curved 
# trends in noisy data. We assume that f(x) changes slowly, 
# being constant within a small time frime.

# Start by assuming that public opinion is constant within a
# week at any given date. Estimating f(x) at each x, we re-
# compute the average over a week for each date.

# Fixing a certain date x_0 to be the center of a week, f(x) is
# assumed constant for any day x ST |x - x_0| <= 3.5. For the
# given week, let f(x) = mu. If |x_i - x_0| <= 3.5, then 
# E[Y_i|X_i = x_i] ~= mu. We call the size of the interval 
# satisfying that condition the window size, bandwith, or span.

# A good estimate of f(x) should be the average Y_i value of the 
# window. Defining A_0 to be the set of indeces i ST |x_i - x_0| <= 3.5, 
# N_0 as the number of indeces in A_0, our estimate follows:
# f_hat(x_0) = (1/N_0)SIG[i=[A_0]]Y_i

# Each point receives a weight. For bin smoothers, the weight
# w_0  falls between 0 (out of window) and 1/N_0 (in-window).
# N_0 is the number of points in a week for our case.

# ksmooth(kernel = "box) uses a bin smoother that resembles a
# step function. The kernel is a function that computes the
# weight. The formula follows:
# f_hat(x_0) = SIG[i=1,N]w_0(x_i)Y_i

# Below, we apply the bin smoother and plot the moving average:

span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points = day,
                                kernel = "box", bandwidth = span))
polls_2008 %>%
  mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")


# We can make the line less jagged by giving central span points
# more weight than fringe points. # ksmooth() lets us find smoother 
# estimates by using the Gaussian (normal) density to assign weight:
fit <- with(polls_2008, ksmooth(day, margin, x.points = day,
                                kernel = "normal", bandwidth = span))
polls_2008 %>%
  mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

# Much better. We can improve via local weighted (loess) regression,
# which derives from Taylor's theorem -- holding that every smooth
# function locally resembles a line at any given point -- and lets us 
# use larger windows. We can assume f(x) to be locally linear rather
# than constant within a given window. 

# Let's try a three-week window:
# E[Y_i|X_i=x_i] = beta_0 + beta_1(x_i - x_0)
# for |x_i - X_0| <= 10.5

# Loess gets us a smoother fit. It varies the bin size while
# using a constant number of points for a local fit. Instead
# of using least squares, it minimizes this figure:
# SIG[i=1, N]w_0(x_i)[Y_i - {beta_0 + beta_1(x_i - x_0)}]^2

# Instead of using Gaussian kernel, loess uses the Tukey tri-
# weight function:
# W(u) = (1 - |u|^3)^3 if |u| <= 1
# W(u) = 0 if |u| > 1
# w_0(x_i) = W((x_i - x_0)/h)

# Taylor's theorem also implies that functions look parabolic
# at a less local level than linear. Expanding our windows,
# we can fit parabola rather than lines.

# Our parabolic local model follows:
# E[Y_i|x_i] = beta_0 + beta_1(x_i - x_0) + 
#                beta_2*(x_i - x_0)^2
# if |x_i - x_0| <= h

total_days <- diff(range(polls_2008$day))
span <- 21/total_days
fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)
polls_2008 %>%
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = 0.5, color = "grey") +
  geom_line(aes(day, smooth), color = "red")

# By default, geom_smooth() uses method='loess' with a polynomial
# degree of 1, letting N be the number of points and span be 0.5, 
# using half N points closest to a given x for the fit:
polls_2008 %>%
  ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()

#### Brexit ####
# Final parameters:
p <- 0.481        # Proportion who voted Remain.
d <- (2*p) - 1    # Spread, or margin of Remain over Leave votes.

# Expected values and standard errors of polls:
N <- 1500         # Sample size.
p*N               # Expected total number of Remain voters.
N*sqrt(p*(1-p)/N) # Standard error for value above.
p                 # Expected proportion voting Remain.
sqrt(p*(1-p)/N)   # Standard error for value above.
(1-p)*N           # Expected total number of Leave voters.
N*sqrt((1-p)*p/N) # Standard error for value above.
d                 # Expected value of proportion spread.
2*sqrt(p*(1-p)/N) # Standard error of spread.


# Actual Brexit poll estimates:
head(brexit_polls)
mean(brexit_polls$leave)  # Average proportion voting Leave.
mean(brexit_polls$remain) # Average proportion voting Remain.
mean(brexit_polls$undecided)
mean(brexit_polls$spread) # Average observed spread.
sd(brexit_polls$spread)   # Standard dev. of spreads.

# Let's analyze one of the last polls:
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)
x_hat <- brexit_polls$x_hat[1]    # 52% vote Remain.
N <- brexit_polls$samplesize[1]   # 4772 respondents.
se_hat <- sqrt(x_hat*(1-x_hat)/N) # Estimated standard error.
x_hat - qnorm(.975)*se_hat        # Low 95% confidence interval.
x_hat + qnorm(.975)*se_hat        # High 95% confidence interval.
!between(0.5, x_hat - qnorm(.975)*se_hat, x_hat + qnorm(.975)*se_hat)   # Does not predict toss-up.
between(0.481, x_hat - qnorm(.975)*se_hat, x_hat + qnorm(.975)*se_hat)  # Remain not predicted to win.

# Let's analyze the June polls:
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         lower = spread - (qnorm(0.975)*se_spread),
         upper = spread + (qnorm(0.975)*se_spread),
         hit = d > lower & d < upper,
         tossup = lower < 0 & upper > 0,
         r_win = lower > 0)

head(june_polls)
str(june_polls)                  # 32 polls in June.
mean(june_polls$hit == TRUE)     # 56.2% allow for real results to occur within 95% CI.
mean(june_polls$tossup == TRUE)  # 62.5% allow a toss-up within a 95% confidence interval. 
mean(june_polls$r_win == TRUE)   # 12.5% were certain that Remain would win.

# Rank the most accurate pollsters:
june_polls %>%
  group_by(pollster) %>%
  summarize(p_hits = mean(hit), N = n()) %>%
  arrange(desc(p_hits))

# The results are consistent with a large general bias that 
# affects all pollsters. Lack of bias does NOT mean that 
# pollsters would cover the correct value 50% of the time.


# Boxplot of Brexit polls by poll type:
june_polls %>%
  ggplot(aes(poll_type, spread)) +
  geom_boxplot() + geom_point()

# Telephone polls tend to show support for Remain.
# Online polls tend to show support for Leave.
# Online polls also have a larger interquartile range,
# indicating more variability.
# Poll type introduces a bias that affects poll results.

# Combined spread across poll type:
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
            spread_lower = spread - qnorm(.975)*se_spread,
            spread_upper = spread + qnorm(.975)*se_spread)
combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(spread_lower)
combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(spread_upper)

## Neither CI covers the true d = -0.038.
## The CIs overlap.
## The CI for telephone polls includes more positive values than does that for online polls.
## The telephone CI is larger than the online CI.
## Neither CI makes a prediction; they both include 0.

# Chi-squared p-values:
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         se_lower = spread - qnorm(0.975)*se_spread,
         se_upper = spread + qnorm(0.975)*se_spread,
         hit = se_lower < d & se_upper > d) %>%
  select(poll_type, hit)

# Online polls are more likely to correctly predict the
# spread, which, at a p-cutoff of 0.05, is statistically
# significant:

brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]
chisq.test(brexit_chisq)$p.value < 0.05

# Let's compute the odds ratios of online and telephone polls:
brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq_df <- as.data.frame(brexit_chisq)
online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]
online_odds <- online_true/online_false
online_odds
phone_true <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "TRUE"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "FALSE"]
phone_odds <- phone_true/phone_false
phone_odds
online_odds/phone_odds

# Let's plot the spread over time by poll platform:
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))


# Let's plot raw percentages over time:
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% 
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3) +
  geom_point(aes(color = vote))

## The percentage of undecided voters declines over time but still hangs around 10% through June.
## CIs for Leave and Remain overlap most of the time. Most of the time, both CIs are below 50%.
## Leave beats remain in first half of June, but CIs still overlap.
## Remain trends upward toward the end of June.

#### 2016 US Presidential Election ####
# Create a table that filters by  state and date, reporting
# Clinton's spread over Trump:
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Generate confidence intervals for polls:
cis <- polls %>%
  mutate(X_hat = (spread+1)/2, 
         se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
         lower = spread - (se*qnorm(0.975)),
         upper = spread + (se*qnorm(0.975))) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set:
add <- results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% 
  mutate(state = as.character(state)) %>%
  left_join(add, by = "state")

# Summarize the proportion of confidence intervals containing the actual value:
p_hits <- ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>%
  summarize(mean(hit == TRUE))
p_hits

# Summarize the proportion of hits for each pollster that has at least 5 polls:
p_hits <- ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit == TRUE), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))
p_hits

# Summarize the proportion of hits for each state that has more than 5 polls:
p_hits <- ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit == TRUE), n = n()) %>%
  arrange(desc(proportion_hits))

# Make a barplot of the proportion of hits for each state:
p_hits %>% ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Calculates the difference between the predicted and actual 
# spread, indicating if the correct winner was predicted:
errors <- ci_data %>%
  mutate(error = spread - actual_spread,
         hit = ifelse(sign(spread)==sign(actual_spread), TRUE, FALSE))

# Examine the last 6 rows:
tail(errors)

# Summarize the proportion of hits for each state that has 5 or more polls:
p_hits <- errors %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit == TRUE), n = n())

# Make a barplot of the proportion of hits for each state:
p_hits %>% ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Generate a histogram of the errors:
hist(errors$error)

# Calculate the median of the errors:
median(errors$error)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point()

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher:
errors %>%
  filter(grade %in% c("A+", "A", "A-", "B+")) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point()

# 加载包
library(QingPackage)

# Q1: 使用 logLikBernoulli 函数
data_q1 <- c(1, 0, 0, 0, 1, 1, 1)
p_max_q1 <- logLikBernoulli(data_q1)
print(paste("Maximum likelihood estimate for p:", p_max_q1))

# Q2: 使用 survCurv 函数
data_q2 <- read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
status_q2 <- data_q2$status
time_q2 <- data_q2$time
survCurv(status_q2, time_q2)
s
# Q3: 使用 unscale 函数
x_q3 <- scale(c(1, 2, 3, 4, 5))
unscaled_x_q3 <- unscale(x_q3)
print("Unscaled values:")
print(unscaled_x_q3)

# Q4: 使用 pcApprox 函数
set.seed(123)
x_q4 <- matrix(rnorm(100), nrow = 10)
approximation_q4 <- pcApprox(x_q4, npc = 2)
print("Approximation using 2 principal components:")
print(approximation_q4)

# Q5: 使用 standardizeNames 函数
data_q5 <- data.frame(
  "First Name" = c("John", "Jane"),
  "Last_Name" = c("Doe", "Smith"),
  "Age." = c(25, 30)
)
standardized_data_q5 <- standardizeNames(data_q5)
print("Standardized column names:")
print(standardized_data_q5)

# Q6: 使用 minimumN 函数
x1_q6 <- c(2.3, 1.9, 2.1, 2.4, 2.2)
min_n_q6 <- minimumN(x1_q6)
print(paste("Minimum sample size needed:", min_n_q6))

# Q7: 使用 downloadRedcapReport 函数 (请提前配置 API 令牌和 URL 或注释掉这部分)
# reportData_q7 <- downloadRedcapReport("MY_REDCAP_TOKEN", "https://redcap.example.com", 1234)
# print("Downloaded RedCap report data:")
# print(reportData_q7)

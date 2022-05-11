#задание 1
income = 1000000
log_income = log(income)
income_pre = 500000
if (income <= income_pre) {print("в прошлом месяце доход больше")}else {print("в текщем месяце доход больше")}

#задание 2
x = 2
y = 4
t = x
x = y
y = t

#задание 3
x = 3.5
y = "2,6"
z =1.78
h = TRUE
class(x)
class(y)
class(z)
class(h)

#задание 4
q = c( 4, 7, -1, 21, 2, 0, 14)
q_sq = q*q
i=1; q_log = c(); while(i < length(q)+1){if (q[i]>0) {q_log[i] = log(q[i])};i=i+1}
#log(x), x > 0, когда x <= 0 пишет NA
i=1; while(i < length(q)+1){if (q[i]>=0) {print(q[i])};i=i+1}
i=1; while(i < length(q)+1){if (i%%7==0) {print(i)};i=i+1}
i=1; while(i < length(q_log)+1){if (is.na(q_log[i])){}else{if (q_log[i]%%2==0 & q_log[i]>5) {print(i)}};i=i+1}

#задание 5
turnout <- c(100, 124, 121, 130, 150, 155, 144, 132, 189, 145, 125, 110, 118, 129, 127)
i=1;    while (i < length(turnout)+1){  if(turnout[i]%%10==0 | turnout[i]%%5==0) {print(turnout[i])} ;i=i+1}
i=1;  len = c() ; while (i < length(turnout)+1){  if(turnout[i]%%10==0 | turnout[i]%%5==0) {len[i] = turnout[i]} ;i=i+1}; print(round(length(len)/length(turnout),2))

#задание 6
z <- c(8, NA, 7, 10, NA, 15, NA, 0, NA, NA, 87)
i=1; while(i < length(z)+1){if (is.na(z[i])){print(i)};i=i+1}

#задание 7
s <- c("4,5", "6,8", "9,2", "1,75")
s
class(s)
i = 1; n =c(); while (i < length(s)+1) {s[i] = sub(",",".",s[i]); n[i]=as.numeric(s[i]); i=i+1}
n
class(n)

#задание 8
#часть1
#до первой трансформации Гаусса
m = matrix(c(1,50,1,75),2,2)
m2 = matrix(c(100,6625),2,1)
x = solve(m,m2)
#трансформация Гаусса
mT = t(m) %*% m
m2T = t(m) %*% m2
xT = solve(mT,m2T)

#часть2
A = matrix(c(1,2,3,4,2,7,6,9,3,6,3,8,4,9,8,2),4,4)
A1 = solve(A)
AT = t(A)
trA = sum(diag(A))
det(A)
A23 = -1*det(A[-2,-3])

# 4.1

n_sim <- 10000
n_odd <- 0
n_five <- 0

for (i in 1:n_sim) {
    trial <- sample(1:6, 1)
    if (trial %% 2 == 1) n_odd <- n_odd + 1
    if (trial == 5) n_five <- n_five + 1
}

n_five / n_odd
p_odd <- n_odd / n_sim
p_five <- n_five / n_sim
p_five / p_odd

# 4.2

n_sim <- 10000      # 검사 횟수
prevalence <- 0.001 # 유병율

# 검사의 민감도, 특이도
sensitivity <- 0.99
specificity <- 0.99

n_total_positive <- 0  # 전체 질환 케이스 수
n_true_positive <- 0   # 실제 환자 수
n_false_positive <- 0  # 오진 케이스 수

for (i in 1:n_sim) {
    # 유병율에 따라 실제 병의 유무를 할당함
    disease <- rbinom(1, 1, prevalence)
    if (disease == 0) { # 실제 병이 없는 경우
        diagnosis <- rbinom(1, 1, 1-specificity)
        if (diagnosis == 1) {
            n_total_positive <- n_total_positive + 1
            n_false_positive <- n_false_positive + 1
        }
    }
    if (disease == 1) { # 실제 병이 있는 경우
        diagnosis <- rbinom(1, 1, sensitivity)
        if (diagnosis == 1) {
            n_total_positive <- n_total_positive + 1
            n_true_positive <- n_false_positive + 1
        }
    }
}

n_total_positive # 양성의 수
n_true_positive  # 진양성의 수
n_false_positive # 위양성의 수

n_false_positive / n_total_positive

# 4.3

n <- 20
p <- 1
for (i in 2:n) {
    p <- p * ((366 - i)/365)
}
1 - p

birthday <- function(n) {
    p <- 1
    for (i in 2:n) {
        p <- p * ((366 - i)/365)
    }
    return (1 - p)
}

for (i in 1:30) {
    cat(i, birthday(i), '\n')
}

# Ex.4.1

TP <- 5539
TN <- 53223
FP <- 512
FN <- 726

accuracy <- (TP+TN)/(TP+TN+FP+FN)
accuracy
precision <- TP/(TP+FP)
precision
sensitivity <-TP/(TP+FN) # recall
sensitivity
specificity <- TN/(TN+FP)
specificity



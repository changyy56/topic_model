#relationship between original citation and SRPL

origin_cita <- read.csv("cita_origin.csv", row.names = 1, stringsAsFactors = FALSE)
srpl_cita <- read.csv("cita_SRPL.csv", row.names = 1, stringsAsFactors = FALSE)

vec_origin_cita <- as.numeric(as.matrix(origin_cita))
vec_srpl_cita <- as.numeric(as.matrix(srpl_cita))
diff_cita1 <- vec_srpl_cita - vec_origin_cita
diff_cita2 <- -vec_srpl_cita + vec_origin_cita

library("dplyr")

par(mfrow=c(1,2))
#1
plot(vec_origin_cita, vec_srpl_cita, log = "xy", type = "p", pch=20 , cex=1, col="black",
     xlab = "#direct citations", ylab = "#SRPL citations", cex.axis=1.2,cex.lab=1.2)
abline(a = 0, b = 1, col="lightgrey")
#2
plot(vec_origin_cita, diff_cita1, log = "xy", type = "p", pch=20 , cex=1, col="red",
     xlab = "#direct citations", ylab = "#citation increases", add = TRUE, cex.axis=1.2,cex.lab=1.2)
#3
df_ci <- data.frame(vec_origin_cita, diff_cita2)
df_ci_f <- filter(df_ci, df_ci$diff_cita2>0)
plot(df_ci_f$vec_origin_cita, df_ci_f$diff_cita2,log = "xy", type = "p", pch=20 , cex=1, col="blue",
     xlab = "#direct citations", ylab = "#citation reductions", add = TRUE, cex.axis=1.2,cex.lab=1.2)

#plot(df_ci_f$vec_origin_cita, df_ci_f$diff_cita2, type = "p", pch=20 , cex=1, col="blue",
#     xlab = "#direct citations", ylab = "#citation reductions", add = TRUE)

#plot(log2(df_ci_f$vec_origin_cita), log2(df_ci_f$diff_cita2), type = "p", pch=20 , cex=1, col="blue",
#     xlab = "#direct citations", ylab = "#citation reductions", add = TRUE)

#reg <- lm(log2(df_ci_f$diff_cita2)~log2(df_ci_f$vec_origin_cita))
#abline(reg,col="grey")
#lines(lowess(log2(logvec_origin_cita),log2(diff_cita2)))


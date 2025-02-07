attach_pop("1.1")
X <- rbind_space(
  sumby(age, treatment , data=data),
  sumby(gender, treatment, data=data)
)
write_table(X)

attach_pop("1.1.1")
X <- rbind_space(
  sumby(age, treatment, data=data, label="aGe (yAars)", text_clean = NULL),
  sumby(gender, treatment, data=data, text_clean = NULL)
)
write_table(X)


attach_pop("1.10")
X <- sumby(age, treatment, data=data)
write_ggplot(attr(X,"fig"))

attach_pop("1.2")
fit <- lm(age~treatment, data=data, na.action = na.omit)
X <- regression_table(fit, labels = c("Mean age reference arm","Treament Effect"))
#X <- sumby(age, treatment, data=data, label="aGe (yAars)", text_clean = NULL)
write_table(X)

attach_pop("1.3")
library(survival)
fit <- survfit(Surv(time,status)~rx, data=colon)
fig <- km_ggplot(fit)
write_plot(fig)

attach_pop("2")
write_text("There were no deaths")



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



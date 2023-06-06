library("tidyverse")
?mutate

library(readr)
insurance_with_date <- read_csv("data/raw/insurance_with_date.csv", 
                                col_types = cols(X = col_number(), age = col_number(), 
                                                 bmi = col_number(), children = col_number(), 
                                                 date = col_date(format = "%Y-%m-%d")))


ins.w.d_read.csv <- read.csv("data/raw/insurance_with_date.csv")
ins.w.d._read_csv <- read_csv("data/raw/insurance_with_date.csv")

str(ins.w.d._read_csv)
str(ins.w.d_read.csv)

reformatted <- insurance_with_date |> 
  mutate(
    across(c(sex, region), factor),
    # sex = factor(sex),
    # region = factor(region),
    gt2_children = children > 2,
    smokes = smoker == "yes",
    date_6m = date + months(6)
    # date_6m = date + 30.4 * 6
  )



library(ggplot2)
library(readr)

ebola <- read_csv("data/raw/ebola.csv", col_types = cols(...1 = col_number(), 
                                                         Date = col_date(format = "%Y-%m-%d"), 
                                                         Cum_conf_cases = col_number(), Cum_susp_cases = col_number(), 
                                                         Cum_conf_death = col_number()))

ebola_31.03.15 <- ebola %>% filter(Date < "2015-04-01")
ebola_31.03.15_country <- ebola_31.03.15 %>% filter(Country %in% c("Spain", "Liberia", "Sierra Leone"))

ggplot(data = ebola_31.03.15_country, mapping = aes(x = Date, y = Cum_conf_cases)) + 
  geom_point(alpha = 0.8, colour = "black", fill = "red", shape = 21, size = 3, stroke = 1.1) 

ggplot(data= ebola_31.03.15_country, mapping = aes(x = Date, y = Cum_conf_cases, colour = Country, fill = Country)) +
  geom_point() + ggtitle("Ebola cases over time") +
  scale_colour_manual(values = c("green", "blue", "purple"),
                      labels = c("Liberia", "Sierra Leone", "Spain")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", 
                                  "2015-02-01","2015-04-01")),
               labels = c("29 August", "1 Oktober", "1 Dezember", "1 Februar", "1 April"),
               limits = as.Date(c("2014-08-29", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2000),
                     limits = c(0, 10000)) +
  xlab(label = "Date") + 
  ylab(label = "cumulative ebola cases") +
  theme_bw() + theme(legend.position="bottom") +
  facet_grid(cols = vars(Country))

library(cowplot)

ggplot(data = insurance_with_date, mapping = aes(x= bmi, colour= sex, fill= sex))+ geom_density(alpha = 0.7) + 
  xlab(label = "BMI (kg/m^2)") + theme_bw() +
  scale_colour_manual(name ="", values = c("female" = "purple", "male" = "red", labels = c("Female", "Male"))) +
  scale_fill_manual(name = "", values = c("female" = "purple", "male" = "red", labels = c("Female", "Male")))

ggplot(data = insurance_with_date, mapping = aes(x=age, y=bmi, colour = smoker)) + geom_point() + geom_quantile() + theme_bw() 

ggplot(data = insurance_with_date, mapping = aes(x= smoker, y=charges)) + geom_violin() + ylab( label = "Charges ($)")


library(usethis)
usethis::use_git_config(user.name = "SamChr34", user.email = "samuelelias.christen@gmail.com")
usethis::create_github_token()
gitcreds::gitcreds_set(3)
gitcreds::gitcreds_set()
0
2
2
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
gitcreds_get()
gitcreds::gitcreds_get()
1



usethis::gh_token_help()

usethis::git_sitrep()

gh::gh_whoami()





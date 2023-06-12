ggplot(test)+
  aes(x, z)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

test <- tibble(z = seq(1, 5, length.out = 1000),
               x = exp(z*2),
               y = exp(z*-2))

ggplot(test)+
  aes(x)+
  geom_density()
  scale_x_log10()+
  scale_y_log10()

ggplot(test)+
  aes(x,y)+
  geom_line()+
  scale_y_log10()+
  scale_x_log10()

lm(log(y) ~ log(x), test)

test <- tibble(x = rexp(n = 1000, rate = 3),
               y = exp(x*3),
               rank = min_rank(y),
               ccdf = round((length(rank)-rank+1)/length(rank), 3))

ggplot(test)+
  aes(x)+
  geom_density()

ggplot(test)+
  aes(y, ccdf)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

my_pl <- conpl$new(test$y)
xmin <- estimate_xmin(my_pl)
my_pl$setXmin(xmin)

my_pl$pars
# here alpha = ca. 2, and both rates are 3
# According to Newman, alpha = 1 - (a/b)
# 1 - (-3/3)
# 1 + 1 = 2. Try another one!

test2 <- tibble(x = rexp(n = 1000, rate = 0.4),
                y = exp(x*2),
                rank = min_rank(y),
                ccdf = round((length(rank)-rank+1)/length(rank), 3))
ggplot(test2)+
  aes(y, ccdf)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

my_pl2 <- conpl$new(test2$y)
xmin2 <- estimate_xmin(my_pl2, xmax = 1e16)
my_pl2$setXmin(xmin2)

my_pl2$pars
# alpha = 1.208
# 1 - (-0.4/2) = 1 + 0.2 = 1.2
#It actually works. Try again.

test3 <- tibble(x = rexp(n = 1000, rate = 2.65),
                y = exp(x*0.025),
                rank = min_rank(y),
                ccdf = round((length(rank)-rank+1)/length(rank), 3))
ggplot(test3)+
  aes(y, ccdf)+
  geom_point()+
  scale_x_log10()+
  scale_y_log10()

my_pl3 <- conpl$new(test3$y)
xmin3 <- estimate_xmin(my_pl3)
my_pl3$setXmin(xmin3)

my_pl3$pars
# alpha = 104.313, this is a very theoretical example!
# 1 - (-2.65/0.025) = 1 + 106 = 107. C'est dingue, j'ai enfin réussi...

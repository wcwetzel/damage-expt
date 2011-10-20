## damage exp't 2010-2011
library(bbmle)
library(ggplot2)
library(RColorBrewer)
d = read.csv("~/Documents/DATA/2010 DATA/FIELD/CLIP/CLIP - 2010 - gall data.csv")

d = d[!is.na(d$galls2010),]

t.test(d$galls2010[d$clip==0], d$galls2010[d$clip==1])
t.test(d$galls2011[d$clip==0], d$galls2011[d$clip==1])
t.test(d$change[d$clip==0], d$change[d$clip==1])



mean(d$galls2011)
var(d$galls2011)

by(d$galls2011, d$clip, mean)
by(d$galls2011, d$clip, sd)/sqrt(by(d$galls2010, d$clip, length))


by(d$galls2010, d$clip, mean)
by(d$galls2010, d$clip, sd)/sqrt(by(d$galls2010, d$clip, length))


m0 = mle2(d$galls2011 ~ dnbinom(mu= a, size=s), data=d, 
	start = list(a = mean(d$galls2011), s=1))

m1 = mle2(d$galls2011 ~ dnbinom(mu = a + r * d$galls2010, size=s), data=d, 
	start = list(a = mean(d$galls2011), r = 1.4, s=1))

m2 = mle2(d$galls2011 ~ dnbinom(mu = a + r * d$galls2010 + b * clip, size=s),
	data=d, start = list(a = mean(d$galls2011), r = 1.4, b = 0, s=1))
	
m3 = mle2(d$galls2011 ~ dnbinom(mu = a + b * d$clip, size=s),
	data=d, start = list(a = mean(d$galls2011), b = 0, s=1))
	
m4 = mle2(d$galls2011 ~ dnbinom(mu = a + b * d$clip + r * d$galls2010
	+ c * d$clip * d$galls2010, size=s),
	data=d, start = list(a = mean(d$galls2011), b = 0, r=1.4, c=0, s=1))
	
m5 = mle2(d$galls2011 ~ dnbinom(mu = a + c * d$clip * d$galls2010, size=s),
	data=d, start = list(a = mean(d$galls2011), c=0, s=1))

AICtab(m0, m1, m2, m3, m4, m5, weights=TRUE)
BICtab(m0, m1, m2, m3, m4, m5, weights=TRUE)

# m1 (#galls depend on last year, not on damage) is the prefered model
# seems as though the relationship between last year's galls and this
# years galls is strong enough that damaging shrubs can't remove it.




cm0 = mle2(d$galls2010 ~ dnbinom(mu= a, size=s), data=d, 
	start = list(a = mean(d$galls2011), s=1))

cm1 = mle2(d$galls2010 ~ dnbinom(mu = a + b * clip, size=s),
	data=d, start = list(a = mean(d$galls2011), b = 0, s=1))

AICtab(cm0, cm1)


plot(d$galls2010 ~ I(d$clip + runif(63,-0.1,0.1)))
points(0:1, by(d$galls2010, d$clip, mean), pch=2, col=2)

plot(d$galls2011 ~ as.factor(d$clip))
points(1:2, by(d$galls2011, d$clip, mean), pch=2, col=2)


p = ggplot(d, aes(x=d$galls2010, y=d$galls2011)) + 
	geom_point(aes(colour = as.factor(d$clip)), alpha=0.8, position='jitter') +
	scale_color_hue('Damage') +
# green is for clip, red is control
# plot with lines for m1 (no damage effect) and m2 (same slope, different intercepts)
 #geom_abline(intercept = coef(m1)['a'], slope = coef(m1)['r']) +
	geom_abline(intercept = coef(m2)['a'], slope = coef(m1)['r'], colour='tomato') +
	geom_abline(intercept = coef(m2)['a'] + coef(m2)['b'], 
		slope = coef(m1)['r'], colour='turquoise') +
	scale_x_continuous('Galls in 2010') +
	scale_y_continuous('Galls in 2011') +
	#scale_colour_discrete(name = "Damage") +
	theme_bw() +
	opts( panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
	axis.title.x = theme_text(vjust = 0))
p
	# add CI for 0 and 1 damage
ggsave('~/Documents/Analysis repos/damage-expt/figs/damage.pdf', p, width=4, height=3)
# # plot with lines for m4 (different slopes and different intercepts)
# p + #geom_abline(intercept = coef(m1)['a'], slope = coef(m1)['r']) +
	# geom_abline(intercept = coef(m4)['a'], slope = coef(m4)['r'], colour='red') +
	# geom_abline(intercept = coef(m4)['a'] + coef(m4)['b'], 
		# slope = coef(m4)['r'] + coef(m4)['c'], colour='turquoise') +
	# theme_bw() +
	# opts( panel.grid.minor=theme_blank(), panel.grid.major=theme_blank(),
	# axis.title.x = theme_text(vjust = 0))



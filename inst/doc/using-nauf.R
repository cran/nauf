## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(nauf)

summary(plosives)

dat <- droplevels(subset(plosives, dialect == "Cuzco" & voicing == "Voiceless"))

item_nas <- is.na(dat$item)
xtabs(~ item_nas + dat$spont)

## ------------------------------------------------------------------------
sdat <- standardize(vdur ~ spont + place + stress +
  (1 + spont + place + stress | speaker) + (1 | item), dat)

mod <- nauf_lmer(sdat$formula, sdat$data)

summary(mod)

## ------------------------------------------------------------------------
anova(mod, method = "S")

## ------------------------------------------------------------------------
rg <- nauf_ref.grid(mod)
nauf_pmmeans(rg, "stress", pairwise = TRUE)

## ------------------------------------------------------------------------
dat <- fricatives

u <- unique(dat[, c("lang", "wordpos", "uvoi")])
u <- u[order(u$lang, u$wordpos, u$uvoi), ]
rownames(u) <- NULL
u

## ------------------------------------------------------------------------
u$uvoi[!(u$lang == "Catalan" & u$wordpos == "Medial")] <- NA
u

dat$uvoi[!(dat$lang == "Catalan" & dat$wordpos == "Medial")] <- NA

## ------------------------------------------------------------------------
dat$c_speaker <- dat$s_speaker <- dat$speaker
dat$c_speaker[dat$lang != "Catalan"] <- NA
dat$s_speaker[dat$lang != "Spanish"] <- NA

## ------------------------------------------------------------------------
s.pvoi <- standardize(pvoi ~ lang * wordpos + uvoi +
  (1 + wordpos + uvoi | c_speaker) + (1 + wordpos | s_speaker),
  dat)

m.pvoi <- nauf_lmer(s.pvoi$formula, s.pvoi$data)

summary(m.pvoi)

## ------------------------------------------------------------------------
nauf_contrasts(m.pvoi)

## ------------------------------------------------------------------------
anova(m.pvoi, method = "S")

rg.pvoi <- nauf_ref.grid(m.pvoi)

## ------------------------------------------------------------------------
nauf_pmmeans(rg.pvoi, "lang", pairwise = TRUE,
  subset = list(
    list(lang = "Catalan", wordpos = "Initial", uvoi = NA),
    list(lang = "Catalan", wordpos = "Medial", uvoi = "Voiceless"),
    list(lang = "Spanish", wordpos = c("Initial", "Medial"), uvoi = NA)
  )
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg.pvoi, "wordpos", pairwise = TRUE,
  subset = list(lang = "Spanish", uvoi = NA)
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg.pvoi, "wordpos", pairwise = TRUE,
  subset = list(
    list(lang = "Catalan", wordpos = "Initial", uvoi = NA),
    list(lang = "Catalan", wordpos = "Medial", uvoi = "Voiceless")
  )
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg.pvoi, "uvoi", pairwise = TRUE,
  subset = list(lang = "Catalan", wordpos = "Medial")
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg.pvoi, c("wordpos", "uvoi"), pairwise = TRUE,
  subset = list(
    list(lang = "Catalan", wordpos = "Medial", uvoi = c("Voiced", "Voiceless")),
    list(lang = "Catalan", wordpos = "Final", uvoi = NA)
  ),
  na_as_level = "uvoi"
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg.pvoi, "lang", pairwise = TRUE,
  subset = list(wordpos = "Final", uvoi = NA)
)

## ------------------------------------------------------------------------
dat <- subset(plosives, voicing == "Voiceless")

xtabs(~ dialect + spont, dat)

## ------------------------------------------------------------------------
dat$spont[dat$dialect == "Valladolid"] <- NA
dat$c_speaker <- dat$l_speaker <- dat$v_speaker <- dat$speaker
dat$c_speaker[dat$dialect != "Cuzco"] <- NA
dat$l_speaker[dat$dialect != "Lima"] <- NA
dat$v_speaker[dat$dialect != "Valladolid"] <- NA

sdat <- standardize(cdur ~ spont * dialect +
  (1 + spont | c_speaker) + (1 + spont | l_speaker) + (1 | v_speaker) +
  (1 + dialect | item),
  dat)

mod <- nauf_lmer(sdat$formula, sdat$data)

summary(mod)

## ------------------------------------------------------------------------
nauf_contrasts(mod)


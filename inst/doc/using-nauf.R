## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(nauf)

summary(plosives)

dat <- droplevels(subset(plosives, dialect == "Cuzco" & voicing == "Voiceless"))

xtabs(~ is.na(item) + spont, dat)

## ------------------------------------------------------------------------
sobj <- standardize(vdur ~ spont + place + stress +
  (1 + spont + place + stress | speaker) + (1 | item), dat)

mod <- nauf_lmer(sobj$formula, sobj$data)

summary(mod)

## ------------------------------------------------------------------------
anova(mod, method = "S")

## ------------------------------------------------------------------------
rg <- nauf_ref.grid(mod)
nauf_pmmeans(rg, "stress", pairwise = TRUE)

## ------------------------------------------------------------------------
summary(fricatives)

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
dat$ca_speaker <- dat$sp_speaker <- dat$speaker
dat$ca_speaker[dat$lang != "Catalan"] <- NA
dat$sp_speaker[dat$lang != "Spanish"] <- NA

## ------------------------------------------------------------------------
sobj <- standardize(dur ~ lang * wordpos + uvoi +
  (1 + wordpos + uvoi | ca_speaker) + (1 + wordpos | sp_speaker), dat)

mod <- nauf_lmer(sobj$formula, sobj$data)

summary(mod)

## ------------------------------------------------------------------------
nauf_contrasts(mod)

## ------------------------------------------------------------------------
anova(mod, method = "S")

rg <- nauf_ref.grid(mod)

## ------------------------------------------------------------------------
nauf_pmmeans(rg, "lang", pairwise = TRUE,
  subset = list(
    list(lang = "Catalan", wordpos = "Initial", uvoi = NA),
    list(lang = "Catalan", wordpos = "Medial", uvoi = "Voiceless"),
    list(lang = "Spanish", wordpos = c("Initial", "Medial"), uvoi = NA)
  )
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg, c("lang", "wordpos"), pairwise = TRUE, by = "lang",
  subset = list(
    list(lang = "Catalan", wordpos = "Initial", uvoi = NA),
    list(lang = "Catalan", wordpos = "Medial", uvoi = "Voiceless"),
    list(lang = "Spanish", uvoi = NA)
  )
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg, "uvoi", pairwise = TRUE,
  subset = list(lang = "Catalan", wordpos = "Medial")
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg, c("wordpos", "uvoi"), pairwise = TRUE, na_as_level = "uvoi",
  subset = list(
    list(lang = "Catalan", wordpos = "Medial", uvoi = c("Voiced", "Voiceless")),
    list(lang = "Catalan", wordpos = "Final", uvoi = NA)
  )
)

## ------------------------------------------------------------------------
nauf_pmmeans(rg, "lang", pairwise = TRUE,
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

sobj <- standardize(cdur ~ spont * dialect +
  (1 + spont | c_speaker) + (1 + spont | l_speaker) + (1 | v_speaker) +
  (1 + dialect | item),
  dat)

mod <- nauf_lmer(sobj$formula, sobj$data)

summary(mod)

## ------------------------------------------------------------------------
nauf_contrasts(mod)


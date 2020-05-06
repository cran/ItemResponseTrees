## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(tibble.print_min = 10, tibble.print_max = 20, pillar.min_title_chars = 16)

## ----data, message = FALSE----------------------------------------------------
library("ItemResponseTrees")

data("jackson")

set.seed(9701)
df1 <- jackson[sample(nrow(jackson), 321), paste0("E", 1:9)]
df1

## ---- out.width="80%", echo = FALSE, out.extra='style="border:0px;display: block;  margin-left: auto; margin-right: auto;"'----

knitr::include_graphics("../tools/ecn-model.png")

## ----model-tree---------------------------------------------------------------
m1 <- "
# IR-tree model for 5-point items (Böckenholt, 2012)

Equations:
1 = (1-m)*(1-t)*e
2 = (1-m)*(1-t)*(1-e)
3 = m
4 = (1-m)*t*(1-e)
5 = (1-m)*t*e

IRT:
t  BY  E1,   E2,   E3,   E4,   E5,   E6,   E7,   E8,   E9;
e  BY  E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1;
m  BY  E1@1, E2@1, E3@1, E4@1, E5@1, E6@1, E7@1, E8@1, E9@1;

Class:
Tree
"

## ----model-grm----------------------------------------------------------------
m2 <- "
# Graded response model

IRT:
t  BY  E1,   E2,   E3,   E4,   E5,   E6,   E7,   E8,   E9;

Class:
GRM
"

## -----------------------------------------------------------------------------
model1 <- irtree_model(m1)
model2 <- irtree_model(m2)

model1$mapping_matrix

## ----fit, cache = TRUE, warning = FALSE, message = FALSE----------------------
# mirt can be used with an EM algorithm (the default) or, for example, with the
# MH-RM algorithm, which seems a little bit faster here.
# See ?mirt::mirt for details.
ctrl <- control_mirt(method = "MHRM")

fit1 <- fit(model1, data = df1, engine = "mirt", control = ctrl)
fit2 <- fit(model2, data = df1, engine = "mirt", control = ctrl)

## -----------------------------------------------------------------------------
glance(fit1)

rbind(glance(fit1), glance(fit2))

## -----------------------------------------------------------------------------
tidy(fit1, par_type = "difficulty")

tail(tidy(fit1, par_type = "difficulty"), 9)

## ----augment, cache = TRUE----------------------------------------------------
augment(fit1)

cor(augment(fit1)$.fitted.t, augment(fit2)$.fitted.t)


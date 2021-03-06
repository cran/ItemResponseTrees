##### Models #####

m1 <- "
# Comment
 IRT:
b BY X1@1, X2@1, X3@1, X4@1, X10@1, X11@1, X101@1;
a BY X1@1, X2@1, X3@1, X4@1, X10@1, X11@1, X101@1;

 Equations:
1 = (1-a)
2 = a*(1-b)
3 = a*b
Class:
Tree

Addendum:
b WITH a@0;
b WITH y1;
"

m2 <- "
IRT:
a BY Work, Comfort@1, Future, Benefit;

Class:
GRM
"

m3 <- "
IRT:
t BY Comfort@1, Work, Future;

Class:
GRM

Addendum:
t WITH Benefit;
"

# m4 <- "
# IRT:
# b BY X1, X2, X3, X4*2, X10;
# a BY X1, X2, X3, X4*2, X10;
#
# Equations:
# 1 = (1-a)
# 2 = a*(1-b)
# 3 = a*b
#
# Class:
# Tree
#
# Addendum:
# a WITH b@0;
# a WITH y1;
# "

model1 <- irtree_model(m1)
model2 <- irtree_model(m2)
model3 <- irtree_model(m3)


##### Data #####

set.seed(3241)

X <- irtree_gen_data(object = model1, N = 100,
                     sigma = diag(model1$S),
                     itempar = list(beta = matrix(rnorm(model1$J*model1$P), model1$J, model1$P),
                                    alpha = matrix(1, model1$J, model1$P)),
                     na_okay = FALSE, skip = TRUE)
for (ii in seq_len(ncol(X$data))) {
    X$data[ii, ii] <- NA
}
df1 <- sample(data.frame(X$data, y1 = rnorm(100)))

data(Science, package = "mirt")

counts <- Science %>%
    lapply(function(x) data.frame(table(factor(x, 1:4)))) %>%
    tibble::enframe(name = "variable") %>%
    tidyr::unnest(value) %>%
    transmute(category = as.integer(Var1),
              variable = toupper(variable), count = Freq)

##### Fit #####

run <- (MplusAutomation::mplusAvailable() == 0)

res1 <- fit(data = df1,
            verbose = FALSE,
            engine = "mplus",
            object = model1,
            control = control_mplus(
                quadpts = 6,
                analysis_list = list(LOGCRITERION = ".01",
                                     COVERAGE = "0"),
                warnings2messages = TRUE,
                run = run))

res2 <- fit(data = Science,
            verbose = FALSE,
            engine = "mplus",
            object = model2,
            control = control_mplus(
                quadpts = "MONTECARLO(500)",
                analysis_list = list(LOGCRITERION = ".01",
                                     COVERAGE = "0"),
                run = run))

res3 <- fit(data = Science,
            verbose = FALSE,
            engine = "mplus",
            object = model3,
            control = control_mplus(
                quadpts = "GAUSS(6)",
                analysis_list = list(LOGCRITERION = ".01",
                                     COVERAGE = "0"),
                run = run))

test_that("Provide starting values",{
    skip("Starting values not yet implemented")

    flag1 <- TRUE
    while (flag1) {
        X4 <- irtree_gen_data(object = model4, N = 100,
                              sigma = diag(model4$S),
                              itempar = list(beta = matrix(rnorm(model4$J*model4$P), model4$J, model4$P),
                                             alpha = matrix(1, model4$J, model4$P)))
        flag1 <- any(!vapply(lapply(X4$data, unique), function(x) length(x) == model4$K, FUN.VALUE = T))
    }
    tmp1 <- names(model4$j_names)
    names(tmp1) <- model4$j_names
    names(X4$data) <- stringr::str_replace_all(names(X4$data), tmp1)
    df4 <- sample(data.frame(X4$data, y1 = rnorm(100)))

    model4 <- irtree_model(m4)
    res4 <- fit(data = df4,
                verbose = FALSE,
                engine = "mplus",
                object = model4,
                control = control_mplus(
                    file = basename(tempfile()),
                    run = TRUE,
                    quadpts = 7))
})

if (run) {
    res1 <- remove_filenames(res1)
    saveRDS(res1, file = test_path("fit-mplus-res1.rds"), version = 2)
    res2 <- remove_filenames(res2)
    saveRDS(res2, file = test_path("fit-mplus-res2.rds"), version = 2)
    res3 <- remove_filenames(res3)
    saveRDS(res3, file = test_path("fit-mplus-res3.rds"), version = 2)
} else {
    # If Mplus is not available, use stored copies of results instead of res1, res2, res3
    res1 <- readRDS(test_path("fit-mplus-res1.rds"))
    res2 <- readRDS(test_path("fit-mplus-res2.rds"))
    res3 <- readRDS(test_path("fit-mplus-res3.rds"))
}

##### Tests #####

test_that("irtree_fit_mplus() works for Tree", {

    expect_s3_class(res1$mplus, "mplus.model")
    n_ipar_1 <- with(model1, J*4 + S + S*(S+1)/2 + S*(S-1)/2)
    checkmate::expect_data_frame(res1$mplus$parameters$unstandardized,
                                 any.missing = FALSE,
                                 nrows = n_ipar_1, ncols = 6)
})

test_that("irtree_fit_mplus() works for GRM", {

    expect_s3_class(res2$mplus, "mplus.model")

    checkmate::expect_data_frame(res2$mplus$parameters$unstandardized,
                                 any.missing = FALSE,
                                 nrows = 17, ncols = 6)
    expect_equal(res2$mplus$sampstat$proportions.counts[, "count"],
                 counts[, "count", drop = TRUE], check.attributes = FALSE)
})

test_that("Methods work for output of irtree_fit_mplus()", {

    expect_condition(capture.output(print(res1)), NA)
    expect_condition(capture.output(summary(res1)), NA)
    expect_condition(capture.output(coef(res1)), NA)
})

# Tidiers -----------------------------------------------------------------

# From vignette at https://broom.tidyverse.org/articles/adding-tidiers.html

test_that("irtree_fit tidier arguments", {

    skip_if_not_installed("modeltests")

    # data(column_glossary, package = "modeltests")
    data(argument_glossary, package = "modeltests")

    modeltests::check_arguments(tidy.irtree_fit)
    modeltests::check_arguments(glance.irtree_fit)
    modeltests::check_arguments(augment.irtree_fit, strict = FALSE)
})

test_that("tidy.irtree_fit()", {

    skip_if_not_installed("modeltests")

    data(column_glossary, package = "modeltests")

    td1 <- tidy(res1)
    td2 <- tidy(res2)
    td3 <- tidy(res3)

    modeltests::check_tidy_output(td1)
    modeltests::check_tidy_output(td2)
    modeltests::check_tidy_output(td3)

    n_ipar_1 <- with(model1, J*4 + S + S*(S+1)/2 + S*(S-1)/2 + 2)

    modeltests::check_dims(td1, n_ipar_1, 6)
    modeltests::check_dims(td2, 17, 6)
    modeltests::check_dims(td3, 17, 6)

    ### Own tests ###

    tmp1 <- tibble::deframe(select(td3, term, estimate))
    tmp2 <- tmp1[["T<->BENEFIT"]]/sqrt(tmp1[["BENEFIT<->BENEFIT"]])/sqrt(tmp1[["T<->T"]])
    expect_equal(tmp2, tmp1[["CORR_T<->BENEFIT"]], tolerance = .002)

    tmp1 <- dplyr::filter(td3, grepl("Thresholds", term)) %>%
        select(4) %>%
        as.data.frame
    tmp2 <- dplyr::filter(td2, grepl("Thresholds", term)) %>%
        select(4) %>%
        slice(1:9) %>%
        as.data.frame
    expect_equal(tmp1, tmp2, tolerance = .1)

    checkmate::expect_numeric(td1$p.value, lower = 0, upper = 1, finite = TRUE)
    checkmate::expect_numeric(td2$p.value, lower = 0, upper = 1, finite = TRUE)
    checkmate::expect_numeric(td3$p.value, lower = 0, upper = 1, finite = TRUE)
    checkmate::expect_numeric(td1$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td2$std.error, lower = 0, finite = TRUE)
    checkmate::expect_numeric(td3$std.error, lower = 0, finite = TRUE)

})

test_that("glance.irtree_fit()", {

    skip_if_not_installed("modeltests")

    data(column_glossary, package = "modeltests")

    gl1 <- glance(res1)
    gl2 <- glance(res2)
    gl3 <- glance(res3)

    modeltests::check_glance_outputs(gl1, gl2, strict = TRUE)

    ### Own tests ###

    expect_equal(pull(gl1, nobs), nrow(df1))
    expect_equal(pull(gl2, nobs), nrow(Science))

})

test_that("augment.irtree_fit()", {

    skip_if_not_installed("modeltests")

    # data(column_glossary, package = "modeltests")

    modeltests::check_augment_function(
        augment.irtree_fit, res1, data = df1, strict = FALSE
    )
    modeltests::check_augment_function(
        augment.irtree_fit, res2, data = Science, strict = FALSE
    )
    modeltests::check_augment_function(
        augment.irtree_fit, res3, data = Science, strict = FALSE
    )

    ### Own tests ###

    ag1 <- augment(res1)
    ag2 <- augment(res2)
    ag3 <- augment(res3)

    modeltests::check_dims(ag1, nrow(df1), ncol(df1) + model1$S*2)
    modeltests::check_dims(ag2, nrow(Science), ncol(Science) + model2$S*2)
    modeltests::check_dims(ag3, nrow(Science), ncol(Science) + model2$S*2)

    checkmate::expect_numeric(ag1$.fitted.B, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag1$.fitted.A, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag2$.fitted.A, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag3$.fitted.T, finite = TRUE, all.missing = FALSE)

    checkmate::expect_numeric(ag1$.se.fit.B, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag1$.se.fit.A, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag2$.se.fit.A, lower = 0, finite = TRUE, all.missing = FALSE)
    checkmate::expect_numeric(ag3$.se.fit.T, lower = 0, finite = TRUE, all.missing = FALSE)

})

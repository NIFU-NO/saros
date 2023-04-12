## Gitt alle variabler, finn alle sett med minst n_min

# library(furrr)
# plan(multisession, workers = availableCores())
# sourcing_items <- 
#     paste0("Q_", c(1:11, 37:46)) %>%
#     combn_upto(n_min = 21L)
# sourcing_cfa_best <-
#     sourcing_df %>%
#     run_multiple_cfa(list_vars=sourcing_items[1], x=., type = "mplus", returns = "wellfitting")

# run_multiple_cfa <- 
#     function(list_vars, x, type=c("mplus", "lavaan", "openmx"), returns = c("wellfitting", "model"),
#     		 cfi_lim=.95, tli_lim=.95, rmsea_lim=.05, srmr_lim=.05) {
#         furrr::future_map(.x = list_vars, .options = furrr::furrr_options(seed = NULL), 
#                           .f = function(manifest_vars) {
#             x <- x[, manifest_vars]
#             
#             switch(type, 
#                    mplus = {
#                        out <-
#                            MplusAutomation::mplusObject(rdata = x, usevariables = manifest_vars,
#                                                         VARIABLE = paste0("CATEGORICAL = ", manifest_vars[1], "-", manifest_vars[length(manifest_vars)], ";"),
#                                                         MODEL = paste0("F BY ", manifest_vars[1], "-", manifest_vars[length(manifest_vars)], ";"))
#                        out <-
#                            MplusAutomation::mplusModeler(object = out,
#                                                          modelout = tempfile(pattern = "mplustmp", fileext = ".inp"),
#                                                          run = 1L, hashfilename = FALSE, writeData = "always", quiet = T)
#                        switch(returns,
#                               wellfitting = {
#                        !is.null(out$results$summaries) &&
#                            out$results$summaries$CFI >= cfi_lim &&
#                            out$results$summaries$TLI >= tli_lim &&
#                            out$results$summaries$RMSEA_Estimate <= rmsea_lim &&
#                            out$results$summaries$SRMR <= srmr_lim
#                               }, 
#                        model = out)
#                    },
#                    lavaan = {
#                        out<-
#                            lavaan::cfa(model = paste0("f =~ ", paste0(manifest_vars, collapse="+")),
#                                        ordered = TRUE, data = x, estimator = "DWLS")
#                        switch(returns, wellfitting= {
#                        if(lavaan::lavInspect(object = out, what = "converged")) {
#                            out <- as.list(lavaan::fitMeasures(out, fit.measures = c("cfi", "tli", "rmsea", "srmr")))
#                            out$cfi >= cfi_lim & 
#                            	out$tli >= tli_lim & 
#                            	out$rmsea <= rmsea_lim & 
#                            	out$srmr <= srmr_lim
#                        } else F
#                        }, model = out)
#                    }, 
#                    openmx = {
#                        
#                        x <- dplyr::mutate(x, across(everything(), 
#                                                               ~OpenMx::mxFactor(x = .x, 
#                                                                                 levels = 0:max(.x, na.rm=TRUE), 
#                                                                                 ordered = T)))
#                        mx_data <- OpenMx::mxData(observed = as.data.frame(x), type = "raw")
#                        mx_latVar <- OpenMx::mxPath(from="F", arrows=2, free=TRUE, values=1, labels ="varF")
#                        mx_resVars <- OpenMx::mxPath(from=manifest_vars, arrows=2, 
#                                                     free=TRUE, 
#                                                     values=rep(1, length(manifest_vars)),
#                                                     labels=paste0("e_", manifest_vars))
#                        mx_facLoads <- OpenMx::mxPath(from="F", to=manifest_vars, arrows=1,
#                                                      free = c(F, rep(T, length(manifest_vars)-1)), 
#                                                      values = rep(1, length(manifest_vars)),
#                                                      labels = paste0("l_", manifest_vars))
#                        mx_means <- OpenMx::mxPath(from="one", to=c(manifest_vars, "F"), arrows=1,
#                                                   free=c(rep(T, length(manifest_vars)), FALSE), 
#                                                   values=c(rep(1, length(manifest_vars)), 0),
#                                                   labels = c(paste0("mean_", manifest_vars), NA))
#                        # n_thres <- purrr::map_int(manifest_vars, ~length(unique(x[,.x]))-1L)
#                        # mx_thres <- OpenMx::mxThreshold(vars = manifest_vars, nThresh = n_thres, 
#                        #                                 free = rep(TRUE, sum(n_thres)),
#                        #                                 values = OpenMx::mxNormalQuantiles(nBreaks = n_thres)
#                        #                                 )
#                        mx_thres <- purrr::map(manifest_vars, 
#                                               ~OpenMx::mxThreshold(vars = .x, 
#                                                                    nThresh = dplyr::n_distinct(x[,.x], na.rm = T)-1L, free = TRUE, 
#                                                                    values = OpenMx::mxNormalQuantiles(
#                                                                        nBreaks = dplyr::n_distinct(x[,.x], na.rm = T)-1L)))
#                        
#                        out <- purrr::lift(..f = OpenMx::mxModel, 
#                                           model = paste0(manifest_vars, collapse="_"), type = "RAM", 
#                                           latentVars = "F", manifestVars = manifest_vars)(
#                                               c(mx_data, mx_latVar, mx_resVars, mx_facLoads, mx_means, mx_thres)
#                                           )
#                        OpenMx::mxRun(out, silent = TRUE)
#                        switch(returns, wellfitting = NA, model=out)
#                    })
#             
#         })
#     }

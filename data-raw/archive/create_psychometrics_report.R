#
#
# analyze_psych <-
#     function(dat, folder=getwd(), title="Psychometric analysis",
#              dif_var=NULL, aux_vars = NULL, cluster_var=NULL, wgt_var= NULL,
#                           plot1b_x="Logit (evnenivå eller (del)oppgavenes vanskelighetsgrad)",
#                           plot1b_colour="Grense mellom deloppgaver",
#                           plot1_label = c("   Elever", "Oppgaver"),
#                           plot1_filename = "Test_dist_1PL.png",
#                           plot2_filename = "Test_dist_2PL.png") {
#         library(tidyverse)
#         library(TAM)
#         library(MplusAutomation)
#         library(lordif)
#
#         title <- gsub("\\[", "(", title)
#         title <- gsub("\\]", ")", title)
#         title <- gsub("\\||\\.|\\*|\\+", "_", title)
#         dir.create(paste0(folder, "/", title), showWarnings = F, recursive = T)
#         setwd(paste0(folder, "/", title))
#
#         ### Data for ConstructMap
#         dir.create(paste0("ConstructMap"), showWarnings = F, recursive = T)
#         dat %>%
#             select(all_of(c(aux_vars, dif_var, wgt_var)), everything()) %>%
#             magrittr::set_colnames(value = c(aux_vars, dif_var, wgt_var, colnames(.)[!colnames(.) %in% c(dif_var, aux_vars, wgt_var)])) %>%
#             data.table::fwrite(x = ., file = paste0("ConstructMap", "/CM.tab"), sep = "\t", col.names = F)
#
#
#         # Internal consistency: Person Separation Reliability / Cronbach's alpha
#         mod_alpha <- psych::alpha(x = dat %>% select(-all_of(c(dif_var, aux_vars, cluster_var, wgt_var))), check.keys = F)
#
#         ### IRT
#         mod_1PL <- TAM::tam.mml(resp = dat %>% select(-all_of(c(dif_var, aux_vars, cluster_var, wgt_var))), irtmodel = "1PL", verbose = F, pweights = if(!is.null(wgt_var)) dat[[wgt_var]])
#         mod_GPCM <- TAM::tam.mml.2pl(resp = dat %>% select(-all_of(c(dif_var, aux_vars, cluster_var, wgt_var))), irtmodel = "GPCM", verbose = F, pweights = if(!is.null(wgt_var)) dat[[wgt_var]])
#         mod_2PL <- TAM::tam.mml.2pl(resp = dat %>% select(-all_of(c(dif_var, aux_vars, cluster_var, wgt_var))), irtmodel = "2PL", verbose = F, pweights = if(!is.null(wgt_var)) dat[[wgt_var]])
#
#         mod_1PL_modfit <- CDM::IRT.modelfit(mod_1PL)
#         mod_GPCM_modfit <- CDM::IRT.modelfit(mod_GPCM)
#         mod_2PL_modfit <- CDM::IRT.modelfit(mod_2PL)
#
#         mod_comparison <- CDM::IRT.compareModels(mod_1PL_modfit, mod_GPCM_modfit, mod_2PL_modfit)
#
#         mod_1PL_abilities <- TAM::tam.wle(mod_1PL, progress = F) # Evnenivåer for hver elev
#         mod_2PL_abilities <- TAM::tam.wle(mod_2PL, progress = F)
#
#         ## CTT
#         mod_ctt <- tam.ctt(resp = dat %>% select(-all_of(c(dif_var, aux_vars, cluster_var, wgt_var))),
#                            wlescore = mod_1PL_abilities[["theta"]], progress = F)
#
#         mod_1PL_thresholds <- tam.threshold(mod_1PL) # Vanskelighetsparametre
#         mod_2PL_thresholds <- tam.threshold(mod_2PL) # Vanskelighetsparametre (NB: Avhengig av diskrimineringsparametere)
#
#         ## Total Test Information Curve
#         mod_1PL_TIC <- TAM::IRT.informationCurves(object = mod_1PL)
#         mod_2PL_TIC <- TAM::IRT.informationCurves(object = mod_2PL)
#
#         ## Outfit and infit
#         mod_1PL_itemfit <- tam.fit(tamobj = mod_1PL, Nsimul = 500, progress = F, useRcpp = T)
#         mod_2PL_itemfit <- tam.fit(tamobj = mod_2PL, Nsimul = 500, progress = F, useRcpp = T)
#         mod_1PL_itemfit_msq <- TAM::msq.itemfit(object = mod_1PL)
#         mod_2PL_itemfit_msq <- TAM::msq.itemfit(object = mod_2PL)
#
#         ## residuals
#         mod_1PL_res <- IRT.residuals(object = mod_1PL)
#         mod_2PL_res <- IRT.residuals(object = mod_2PL)
#
#         ## Item Response Curves: 1PL
#         dir.create(paste0(folder, "/", title, "/1PL/"), showWarnings = F, recursive = T)
#         setwd(paste0(folder, "/", title, "/1PL/"))
#         irc_1pl <-
#             lapply(1:nrow(mod_1PL$item), function(i) {
#             if("AXsi_.Cat2" %in% colnames(mod_1PL$item) && !is.na(mod_1PL$item[i, "AXsi_.Cat2"]) && title != "b_p") {
#                 plot(mod_1PL, items = i, type = "items", low = -4.5, high = 4.5, ngroups = 10, wle=mod_1PL_abilities[["theta"]])
#             } else plot(mod_1PL, items = i, type = "expected", low = -4.5, high = 4.5, ngroups = 10, wle=mod_1PL_abilities[["theta"]])
#
#         })
#
#         plot_1a <- ## Person histogram and test information curve
#             mod_1PL_abilities %>%
#             as.data.frame %>%
#
#             ggplot(data = .) +
#             geom_histogram(aes(theta), bins = 50, colour="gray90", fill="violet", na.rm = T) +
#             geom_vline(xintercept = mean(mod_1PL_abilities[["theta"]]), colour="gray30", na.rm = T) +
#             geom_line(data = data.frame(theta=mod_1PL_TIC[["theta"]][,1],
#                                         tic=mod_1PL_TIC$test_info_curve*600/max(mod_1PL_TIC$test_info_curve)),
#                       mapping = aes(x=theta, y=tic),
#                       colour = "seagreen", size=1, na.rm = T) +
#
#             theme_classic() +
#             scale_x_continuous(limits = c(-4.5, 3.5), expand = c(0,0)) + #position = "top",
#             scale_y_continuous(expand = c(0,0),
#                                sec.axis = sec_axis(trans = ~./nrow(mod_1PL_abilities), labels = scales::percent)) +
#             labs(x=NULL, y=NULL)
#
#         plot_1b <- ## Oppgavene
#             mod_1PL_thresholds %>%
#             as.data.frame() %>%
#             rownames_to_column(var = "Q_ID") %>%
#             gather(key = "Cat", value = "Thresholds", -Q_ID) %>%
#             mutate(Cat = gsub("Cat", "", Cat)) %>%
#             dplyr::filter(!is.na(Thresholds)) %>%
#             left_join(x=., y=mod_1PL$xsi %>%
#                           rownames_to_column(var = "item") %>%
#                           tidyr::separate(col = item, into = c("Q", "Grade", "Q_ID", "Cat"), sep="_") %>%
#                           transmute(Q_ID, Cat = gsub("Cat", "", Cat), se_xsi=se.xsi), by=c("Q_ID", "Cat")) %>%
#             mutate(CI_l = Thresholds - 1.96*se_xsi, CI_h = Thresholds + 1.96*se_xsi,
#                    Q_ID = factor(Q_ID))
#         plot_1b <-
#             ggplot(data = plot_1b, aes(y=Q_ID, x=Thresholds, colour=Cat)) +
#             geom_errorbarh(aes(xmin=CI_l, xmax=CI_h), na.rm = T) +
#             geom_point(na.rm = T) +
#             guides(colour = guide_legend(direction = "horizontal",  title.position = "top", reverse = F)) +
#             theme_classic() +
#             theme(panel.grid.major.y = element_line(colour = "gray90", linetype = "dashed"),
#                   axis.ticks.y = element_blank(),
#                   legend.position = "bottom",
#                   legend.background = element_rect(fill = "gray97", linetype = "dashed", colour="white")) +
#             scale_x_continuous(limits = c(-4.5, 3.5), expand = c(0,0), position = "top") +
#             scale_y_discrete(limits = rev(levels(plot_1b$Q_ID))) +
#             labs(y=NULL, x=plot1b_x, colour=plot1b_colour)
#
#         plot_1 <- cowplot::plot_grid(plot_1a, plot_1b, labels = plot1_label, ncol = 1, align = "v", axis = "lr", rel_heights = c(.3, .7))
#         ggsave(plot_1, filename = paste0("Plots/", plot1_filename), dpi = "retina", width = unit(x = 10, units = "cm"), height = unit(x = 6, units = "cm"))
#
#
#
#
#         ## Item Response Curves: 2PL
#         dir.create(paste0(folder, "/", title, "/2PL/"), showWarnings = F, recursive = T)
#         setwd(paste0(folder, "/", title, "/2PL/"))
#         irc_2pl <-
#             lapply(1:nrow(mod_2PL$item), function(i) {
#             if("AXsi_.Cat2" %in% colnames(mod_2PL$item) && !is.na(mod_2PL$item[i, "AXsi_.Cat2"]) && title != "b_p") {
#                 TAM:::plot.tam(mod_2PL, items = i, type = "items", low = -4.5, high = 4, ngroups = 10, wle=mod_2PL_abilities[["theta"]])
#             } else plot(mod_2PL, items = i, type = "expected", low = -4.5, high = 4, ngroups = 10, wle=mod_2PL_abilities[["theta"]])
#         })
#
#
#         plot_2a <- ## Person histogram and test information curve
#             mod_2PL_abilities %>%
#             as.data.frame %>%
#
#             ggplot(data = .) +
#             geom_histogram(aes(theta), bins = 50, colour="gray90", fill="violet", na.rm = T) +
#             geom_vline(xintercept = mean(mod_2PL_abilities[["theta"]]), colour="gray30", na.rm = T) +
#             geom_line(data = data.frame(theta=mod_2PL_TIC[["theta"]][,1],
#                                         tic=mod_2PL_TIC$test_info_curve*600/max(mod_2PL_TIC$test_info_curve)),
#                       mapping = aes(x=theta, y=tic),
#                       colour = "seagreen", size=1, na.rm = T) +
#
#             theme_classic() +
#             scale_x_continuous(limits = c(-4.5, 4.5), expand = c(0,0)) + #position = "top",
#             scale_y_continuous(expand = c(0,0),
#                                sec.axis = sec_axis(trans = ~./nrow(mod_2PL_abilities), labels = scales::percent)) +
#             labs(x=NULL, y=NULL)
#
#         plot_2b <- ## Oppgavene
#             mod_2PL_thresholds %>%
#             as.data.frame() %>%
#             rownames_to_column(var = "Q_ID") %>%
#             # mutate(Q_ID = gsub("Q_11_|Q_10_", "", Q_ID)) %>%
#             gather(key = "Cat", value = "Thresholds", -Q_ID) %>%
#             mutate(Cat = gsub("Cat", "", Cat)) %>%
#             dplyr::filter(!is.na(Thresholds)) %>%
#             left_join(x=., y=mod_2PL$xsi %>%
#                           rownames_to_column(var = "item") %>%
#                           tidyr::separate(col = item, into = c("Q", "Grade", "Q_ID", "Cat"), sep="_") %>%
#                           transmute(Q_ID, Cat = gsub("Cat", "", Cat), se_xsi=se.xsi), by=c("Q_ID", "Cat")) %>%
#             mutate(CI_l = Thresholds - 1.96*se_xsi, CI_h = Thresholds + 1.96*se_xsi,
#                    Q_ID = factor(Q_ID))
#         plot_2b <-
#             ggplot(data = plot_2b, aes(y=Q_ID, x=Thresholds, colour=Cat)) +
#             geom_errorbarh(aes(xmin=CI_l, xmax=CI_h), na.rm = T) +
#             geom_point(na.rm = T) +
#             guides(colour = guide_legend(direction = "horizontal",  title.position = "top", reverse = F)) +
#             theme_classic() +
#             theme(panel.grid.major.y = element_line(colour = "gray90", linetype = "dashed"),
#                   axis.ticks.y = element_blank(),
#                   legend.position = "bottom",
#                   legend.background = element_rect(fill = "gray97", linetype = "dashed", colour="white")) +
#             scale_x_continuous(limits = c(-4.5, 4.5), expand = c(0,0), position = "top") +
#             scale_y_discrete(limits = rev(levels(plot_1b$Q_ID))) +
#             labs(y=NULL, x=plot1b_x, colour=plot1b_colour)
#
#         plot_2 <- cowplot::plot_grid(plot_2a, plot_2b, labels = plot1_label, ncol = 1, align = "v", axis = "lr", rel_heights = c(.3, .7))
#         ggsave(plot_2, filename = paste0("Plots/", plot2_filename), dpi = "retina", width = unit(x = 10, units = "cm"), height = unit(x = 6, units = "cm"))
#
#
#         ####################################################################
#         ### Mplus
#         mplus_folder <- paste0(folder, "/", title, "/Mplus")
#         dir.create(mplus_folder, showWarnings = F, recursive = T)
#         MplusAutomation::cd(mplus_folder)
#         m_2PL <-
#             dat %>%
#             select(-all_of(dif_var)) %>%
#             MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.), autov = F,
#                                          VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% c(aux_vars, cluster_var, wgt_var)], collapse=" "), 20), ";",
# if(!is.null(wgt_var)) paste0("WEIGHT = ", wgt_var, ";
# "),
# "
# AUXILIARY = ", paste(aux_vars, collapse=" "), ";
# ",
# if(!is.null(cluster_var)) paste0("CLUSTER = ", cluster_var, ";")),
#                                          ANALYSIS = paste0("
# ESTIMATOR = WLSMV;
# PARAMETERIZATION = THETA;
# ",
# if(!is.null(cluster_var)) "TYPE = COMPLEX;"),
# MODEL = paste0("f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% c(aux_vars, cluster_var, wgt_var)], collapse="* "), 40), "*;
# f@1;"),
#                                          OUTPUT = "MOD;",
#                                          PLOT = "TYPE = PLOT3;",
#                                          SAVEDATA = "DIFFTEST = 2PL_diff.dat;
# FILE = fscores_2pl.dat;
# MISSFLAG = .;
# SAVE = FSCORES;") %>%
#             MplusAutomation::mplusModeler(object = ., dataout = "2PL.dat", run = 1L, writeData = "always")
#         ################################################################################################
#         m_1PL <-
#             dat %>%
#             select(-all_of(dif_var)) %>%
#             MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.), autov = F,
#                                          VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% c(aux_vars, cluster_var, wgt_var)], collapse=" "), 40), ";",
# if(!is.null(wgt_var)) paste0("WEIGHT = ", wgt_var, ";
# "),
# "
# AUXILIARY = ", paste(aux_vars, collapse=" "), ";
# ",
# if(!is.null(cluster_var)) paste0("CLUSTER = ", cluster_var, ";")),
#                                          ANALYSIS = paste0("
# ESTIMATOR = WLSMV;
# DIFFTEST = 2PL_diff.dat;
# PARAMETERIZATION = THETA;
# ",
# if(!is.null(cluster_var)) "TYPE = COMPLEX;"),
# MODEL = paste0("f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% c(aux_vars, cluster_var, wgt_var)], collapse="*(l)\n", sep="\n"), 12),
# "*(l);
# f@1;"),
#                                          OUTPUT = "MOD;",
#                                          PLOT = "TYPE = PLOT3;",
#                                          SAVEDATA = "DIFFTEST = 1PL_diff.dat;
# FILE = fscores_1pl.dat;
# MISSFLAG = .;
# SAVE = FSCORES;") %>%
#             MplusAutomation::mplusModeler(object = ., dataout = "1PL.dat", run = 1L, writeData = "always")
#
#         m_efa <-
#             dat %>%
#             select(-all_of(c(dif_var, aux_vars))) %>%
#             MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.), autov = F,
#                                          VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% c(cluster_var, wgt_var)], collapse=" "), 40), ";",
# if(!is.null(wgt_var)) paste0("WEIGHT = ", wgt_var, ";
# "),
# if(!is.null(cluster_var)) paste0("CLUSTER = ", cluster_var, ";")),
#                                          ANALYSIS = paste0("TYPE = EFA 1 5;
# ",
# if(!is.null(cluster_var)) "TYPE = COMPLEX;")) %>%
#             MplusAutomation::mplusModeler(object = ., dataout = "efa.dat", run = 1L, writeData = "always")
#
#         if(!is.null(dif_var)) { ### DIF-analyser
#             dat_dif <- filter_at(dat, vars(all_of(dif_var)),
#                                  .vars_predicate = all_vars(!is.na(.)))  %>%
#                 mutate_all("as.integer")
#
#             m_2PL_DIF_auto <- ## Check MODINDICES
#                 dat_dif %>%
#                 # select() %>% #-all_of(aux_vars)
#                 select(GROUP = all_of(dif_var), everything()) %>%
#                 # magrittr::set_colnames(value = c("GROUP", colnames(.)[!colnames(.) %in% dif_var])) %>%
#                 MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.),
#                                              autov = F,
#                                              VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% c("GROUP", aux_vars, cluster_var, wgt_var)], collapse=" "), 40), ";",
# if(!is.null(wgt_var)) paste0("WEIGHT = ", wgt_var, ";
# "),
# "GROUPING = GROUP (0 = ref 1 = focus);
# AUXILIARY = ", paste(aux_vars, collapse=" "), ";
# ",
# if(!is.null(cluster_var)) paste0("CLUSTER = ", cluster_var, ";")),
#                                              ANALYSIS = paste0("MODEL = CONFIGURAL SCALAR;
# ",
# if(!is.null(cluster_var)) "TYPE = COMPLEX;"),
#                                              MODEL = paste0("
# f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% c("GROUP", aux_vars, cluster_var, wgt_var)], collapse=" "), 40), ";")) %>%
#                 MplusAutomation::mplusModeler(object = ., dataout = paste0("2PL_DIF_", dif_var, "_auto.dat"), run = 1L, writeData = "always")
#             #
#
# #             m_2PL_DIF_weak <-
# #                 dat_dif %>%
# #                 select(-all_of(aux_vars)) %>%
# #                 select(GROUP = all_of(dif_var), everything()) %>%
# #
# #                 MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.),
# #                                              autov = F,
# #                                              VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse=" "), 40), ";
# # GROUPING = GROUP (0 = ref 1 = focus);"),
# #                                              ANALYSIS = "
# # ESTIMATOR = WLSMV;
# # PARAMETERIZATION = THETA;",
# #                                              MODEL = paste0("
# # f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="* "), 40), "*;
# # f@1;
# # [f@0];
# #
# # MODEL focus:
# # f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="* "), 40), "*;
# # f@1;
# # ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="@1 "), 40), "@1;
# # [", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="$1* "), 40), "$1*]; !Must specify intercepts for partial credits
# # [f@0];"),
# #                                              OUTPUT = "MOD;",
# #                                              PLOT = "TYPE = PLOT3;") %>%
# #                 MplusAutomation::mplusModeler(object = ., dataout = paste0("2PL_DIF_", dif_var, "_weak.dat"), run = 1L, writeData = "always")
# #
# #
# #             m_2PL_DIF_strict <- ## Check MODINDICES
# #                 select(dat_dif, -all_of(aux_vars)) %>%
# #                 select(GROUP = all_of(dif_var), everything()) %>%
# #                 # magrittr::set_colnames(value = c("GROUP", colnames(.)[!colnames(.) %in% dif_var])) %>%
# #                 MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.),
# #                                              autov = F,
# #                                              VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse=" "), 40), ";
# # GROUPING = GROUP (0 = ref 1 = focus);"),
# #                                              ANALYSIS = "
# # ESTIMATOR = WLSMV;
# # PARAMETERIZATION = THETA;",
# #                                              MODEL = paste0("
# # f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="* "), 40), "*;
# # f@1;
# # [f@0];
# # ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="@1 "), 40), "@1;
# #
# # MODEL focus:
# #                                                             "),
# #                                              OUTPUT = "MOD;",
# #                                              PLOT = "TYPE = PLOT3;") %>%
# #                 MplusAutomation::mplusModeler(object = ., dataout = paste0("2PL_DIF_", dif_var, "_equal.dat"), run = 1L, writeData = "always")
# #
# #             print("DIF2")
# #
# #             m_2PL_DIF2_weak <-
# #                 dat_dif %>%
# #                 select(-all_of(aux_vars)) %>%
# #                 select(GROUP = all_of(dif_var), everything()) %>%
# #
# #                 MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.),
# #                                              autov = F,
# #                                              VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse=" "), 40), ";
# # GROUPING = GROUP (0 = ref 1 = focus);"),
# #                                              ANALYSIS = "
# # ESTIMATOR = WLSMV;
# # PARAMETERIZATION = DELTA;",
# #                                              MODEL = paste0("
# # f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse=" "), 40), ";
# # {", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="@1 "), 40), "};
# # [f@0];
# #
# # MODEL focus:
# # f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse=" "), 40), ";
# # [", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="$1 "), 40), "$1]; !Must specify intercepts for partial credits
# # [", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="$2 "), 40), "$2]; !Must specify intercepts for partial credits
# # [", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="$3 "), 40), "$3]; !Must specify intercepts for partial credits
# # [", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="$4 "), 40), "$4]; !Must specify intercepts for partial credits
# # "),
# #                                              OUTPUT = "MOD;",
# #                                              PLOT = "TYPE = PLOT3;") %>%
# #                 MplusAutomation::mplusModeler(object = ., dataout = paste0("2PL_DIF2_", dif_var, "_weak.dat"), run = 1L, writeData = "always")
# #             print(3)
# #
# #             m_2PL_DIF2_strict <- ## Check MODINDICES
# #                 select(dat_dif, -all_of(aux_vars)) %>%
# #                 select(GROUP = all_of(dif_var), everything()) %>%
# #                 # magrittr::set_colnames(value = c("GROUP", colnames(.)[!colnames(.) %in% dif_var])) %>%
# #                 MplusAutomation::mplusObject(rdata = ., usevariables = colnames(.),
# #                                              autov = F,
# #                                              VARIABLE = paste0("CATEGORICAL = ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse=" "), 40), ";
# # GROUPING = GROUP (0 = ref 1 = focus);"),
# #                                              ANALYSIS = "
# # ESTIMATOR = WLSMV;
# # PARAMETERIZATION = DELTA;",
# #                                              MODEL = paste0("
# # f by ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="* "), 40), "*;
# # f@1;
# # [f@0];
# # ", str_wrap(paste(colnames(.)[!colnames(.) %in% "GROUP"], collapse="@1 "), 40), "@1;
# #
# # MODEL focus:
# #                                                             "),
# #                                              OUTPUT = "MOD;",
# #                                              PLOT = "TYPE = PLOT3;") %>%
# #                 MplusAutomation::mplusModeler(object = ., dataout = paste0("2PL_DIF2_", dif_var, "_equal.dat"), run = 1L, writeData = "always")
# #
# #
#
#             # dif_check1 <- nrow(do.call("bind_rows", lapply(select(dat_dif, -all_of(c(dif_var, aux_vars))), table))) == 2 # Alle variablene er binære: 2PL
#             #
#             # dif_check2 <- n_distinct(unlist(lapply(dat_dif %>% select(-all_of(c(dif_var, aux_vars))),  # har alle variablene like mange verdier? => RSM?
#             #                                        function(x) nrow(data.frame(table(x)))))) == 1
#
#             # mod_1PL_DIF <- TAM::tam.mml.mfr(resp = dat_dif %>% select(-all_of(c(dif_var, aux_vars))),
#             #                                 irtmodel = if(dif_check1) "PCM" else if(dif_check2) "RSM",
#             #                                 facets = select(dat_dif, group=all_of(dif_var)), verbose = F,
#             #                                 formulaA = if(dif_check1) formula("~ 1+item + group*item") else if(dif_check2) formula("~ 1+item + step + group*item") else formula("~ 1+item + step + group*item + group:step"))
#
#
#             # mod_1PL_DIF_SEX_lordif_df <-
#             #     dat_dif %>%
#             #     select(-all_of(aux_vars))  %>%
#             #     filter(complete.cases(.)) %>%
#             #     # labelled::remove_labels() %>%
#             #     mutate_all("as.integer")
#             # mod_1PL_DIF_SEX_lordif <- lordif(resp.data = dat_dif %>% select(-all_of(c(dif_var, aux_vars))), model = "GPCM",
#             #                                  group = dat_dif %>% select(all_of(dif_var)) %>% pull(1), alpha = .01, nr=500, MonteCarlo = T)
#
#         }
#
#
#         setwd(folder)
#
#         ##### lagre item-parametre i CSV-fil
#         stat <-
#             mod_alpha$item.stats %>%
#             as.data.frame() %>%
#             rownames_to_column(var = "parameter") %>%
#             select(parameter, n, mean)
#         freq <-
#             mod_alpha$response.freq %>%
#             as.data.frame() %>%
#             rownames_to_column(var = "parameter")
#         beta <-
#             mod_1PL_thresholds %>%
#             as.data.frame() %>%
#             rownames_to_column(var = "parameter")
#
#         fit <-
#             mod_1PL_itemfit$itemfit %>%
#             mutate(Outfit_pholm_sig = ifelse(Outfit_pholm <.01, T, F),
#                    Infit_pholm_sig = ifelse(Infit_pholm <.01, T, F)) %>%
#             select(parameter, Infit, Infit_pholm_sig, Outfit, Outfit_pholm_sig)
#         if(any(grepl("Cat1", fit$parameter))) {
#             fit <-
#                 fit %>%
#                 tidyr::separate(parameter, into = c("parameter", "Cat"), sep=c(-4)) %>%
#                 pivot_longer(names_to = "Col", values_to = "Val", cols = Infit:Outfit_pholm_sig) %>%
#                 pivot_wider(id_cols = parameter, values_from = Val, names_from = c(Cat, Col))
#         }
#         x <-
# 			left_join(x=stat, y=beta, by=c("parameter")) %>%
#             left_join(x=., y=freq, by=c("parameter")) %>%
#             left_join(x=., y=fit, by=c("parameter"))
#         dir.create(paste0(folder, "/", title))
#         rio::export(x = x, file = paste0(folder, "/", title, "/items.csv"), format = ";")
#
#         mod_1PL$item %>%
#             rename_all(.funs = ~gsub("([AB])[Xsi\\_]*\\.", "\\1_", gsub("\\.Dim1", "", .))) %>%
#             # rownames_to_column(var = "Q_ID") %>%
#             select(-N, -M, -xsi.item, Q_ID=item) %>%
#             mutate_if(.predicate = "is.numeric", .funs = ~ifelse(.==0, NA_real_, .)) %>%
#             rio::export(x = ., file = paste0(folder, "/", title, "/1PL.csv"), format = ";")
#
#         mod_2PL$item %>%
#             rename_all(.funs = ~gsub("([AB])[Xsi\\_]*\\.", "\\1_", gsub("\\.Dim1", "", .))) %>%
#             # rownames_to_column(var = "Q_ID") %>%
#             select(-N, -M, -xsi.item, Q_ID=item) %>%
#             mutate_if(.predicate = "is.numeric", .funs = ~ifelse(.==0, NA_real_, .)) %>%
#             rio::export(x = ., file = paste0(folder, "/", title, "/2PL.csv"), format = ";")
#
#
#         # Oppsummeringer
#         item_desc <- psych::describe(mod_1PL$item$xsi.item)
#         pers_desc <- psych::describe(mod_1PL_abilities[["theta"]])
#         enframe(x = c(alpha=mod_alpha$total$raw_alpha,
#                       item_loc_mean = item_desc$mean,
#                       item_loc_sd = item_desc$sd,
#                       item_loc_skew = item_desc$skew,
#                       item_loc_kurt = item_desc$kurtosis,
#                       item_1PL_2PL_cor = cor(x = mod_1PL_abilities$PersonScores,
#                                              y = mod_2PL_abilities[["theta"]], use = "complete.obs"),
#                       pers_loc_mean = pers_desc$mean,
#                       pers_loc_sd = pers_desc$sd,
#                       pers_loc_skew = pers_desc$skew,
#                       pers_loc_kurt = pers_desc$kurtosis), name="Stats", value = title) %>%
#             rio::export(x = ., file = paste0(folder, "/", title, "/1PL_summary.csv"), format = ";")
#
#         mget(ls())
# }
#

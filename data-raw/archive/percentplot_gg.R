#### Lage grafer med samme layout som i NIFUmalen. Denne tillater gruppering innenfor hvert spørsmål.
#### Denne sørger for at alle figurene ser helt like ut, bortsett fra at høyden avhenger av antall spørsmål man rapporterer

### df = datasettet.
### batteri = navnet på en kolonne som sier hvilke spørsmål som skal på samme graf.
### mappe = mappen du vil lagre i.
### Bruker pakkene:  ggplot, cowplot og ggfittext
### Dersom den feiler med "Error in if (shades::lightness(text$fill) < 50) { : missing value where TRUE/FALSE needed" å er det fordi kategorier-variabelen er NA. Gjør denne til character.
### En lignende feilmelding skyldes at det er duplikater av spørsmålet i datasettet, innenfor et batteri.
### Error in if ((tgdim$width > xdim | tgdim$height > ydim) | (x$grow & tgdim$width <  : missing value where TRUE/FALSE needed
###### Denne feilmeldingen skyldes alltid at det er duplikater i datasettet som ikke kan være der.

# add_percentplot_gg <- function(df, batteri="spm_gruppe", sporsmol=NULL, frekvenser="p", kategorier="x", mappe="H:/HVL/Grafer/Adm/",
#                        fargerekkefolge=c(2,4,5,3,1,6), n_pa_hoyre_side=NULL, grupper = NULL, dropp_om_singelgruppe=F, fileformat="svg", strip_font_size=10) {
# 
#     dir.create(mappe, showWarnings = F)
#     if(nchar(mappe)>64) rlang::abort("Beklager, mappebanen er for lang. png-funksjonen i R greier ikke mer enn 64? tegn...")
#     maks_bredde <- max(nchar(df[[n_pa_hoyre_side]]))
#     NIFUmal_fargekoder <- c(red="#C82D49",
#                              black = "#363636",
#                              beige="#EDE2D2",
#                              blue="#2D8E9F",
#                              purple="#DBD2E0",
#                              orange="#E8AE63")
# 
#     # if(!is.null(n_pa_hoyre_side) && is.character(n_pa_hoyre_side)) {
#         X <-
#             lapply(unique(df[[batteri]]), function(gruppe) {
#                 df_del <- as.data.frame(df[df[[batteri]] == gruppe, ])
#                 fasett <- enquo(sporsmol)
#                 sporsmol <- quo_name(sporsmol)
# 
#                 df_del[[grupper]] <- factor(df_del[[grupper]], levels = unique(df_del[[grupper]]))
#                 hoyre_side <- df_del[!duplicated(df_del[c(grupper, sporsmol),]), n_pa_hoyre_side]
#                 df_del$facet_var <- df_del[[sporsmol]]
#                 cap <- if(length(unique(df_del[[sporsmol]]))>1) {
#                     cap <- gsub("[\n]{2,}|[[:space:]]{2,}", " ", gruppe)
#                     cap <- strwrap(cap, width = 120, simplify = T) 
#                     paste0(cap, collapse="\n")
#                     } else NULL
#                 p <-
#                     ggplot2::ggplot(data = df_del, mapping = ggplot2::aes_string(y=frekvenser,
#                                                            x=grupper, #paste0("as.integer(", ,")")
#                                                            fill=kategorier), cumulative=TRUE) + #
#                     ggplot2::facet_grid(facet_var ~ ., switch = "y", scales = "free_y", space = "free_y") +
#                     ggplot2::geom_col(width = .8, position = ggplot2::position_fill(reverse = T)) +
#                     ggplot2::coord_flip() +
#                     ggplot2::scale_y_continuous(limits = c(-.003, 1.015), expand=c(0,0), labels = scales::percent_format()) +
#                     ggplot2::scale_fill_manual(name="", values = unname(NIFUmal_fargekoder)[fargerekkefolge]) +
#                     ggplot2::guides(fill=ggplot2::guide_legend(reverse = F, nrow = 1), colour=FALSE) +
#                     ggplot2::theme_classic() +
#                     ggplot2::theme(text = ggplot2::element_text(family = "sans"),
#                           plot.caption = ggplot2::element_text(size = 12),
#                         legend.position = "bottom",
#                         legend.text = ggplot2::element_text(size=12),
#                         strip.placement = "outside",
#                           strip.text.y = ggplot2::element_text(family = "sans", angle=180, hjust = 1, size=strip_font_size),
#                           strip.background = ggplot2::element_rect(colour = NA)) +
#                     ggplot2::labs(x=NULL, y=NULL, 
#                                   caption=cap)
#                 fittext_installed <- requireNamespace("ggfittext", quietly = TRUE)
#                 if(!fittext_installed) {
#                     rlang::warn("Package ggfittext not installed. Using default geom_text instead.") 
#                     p <- p + geom_text(mapping = aes_string(label = paste0("sprintf(", frekvenser, "*100,fmt='%1.0f%%')")), position=position_stack(vjust=.5,reverse = T))
#                 } else {
#                     p <- p + ggfittext::geom_fit_text(mapping = ggplot2::aes_string(label = paste0("sprintf(", frekvenser, "*100,fmt='%1.0f%%')")),
#                                                     position=ggplot2::position_stack(reverse = T), contrast = T, padding.y = unit(0.5, "mm"), size=10, min.size=3, outside = T)
#                 }
#                         
#                 return(p)
#             })
#     X2 <-
#         lapply(unique(df[[batteri]]), function(gruppe) {
#             df_del <- as.data.frame(df[df[[batteri]] == gruppe, ])
#             df_del$const <- 0
#             fasett <- enquo(sporsmol)
#             sporsmol <- quo_name(sporsmol)
# 
#             df_del[[grupper]] <- factor(df_del[[grupper]], levels = unique(df_del[[grupper]]))
#             hoyre_side <- df_del[!duplicated(df_del[c(grupper, sporsmol),]), n_pa_hoyre_side]
#             ggplot2::ggplot(data = df_del, mapping = ggplot2::aes_string(y="const", x=grupper)) + #
#                 ggplot2::facet_grid(variableName2~., switch = "y", scales = "free_y", space = "free_y") +
#                 ggplot2::geom_text(mapping = ggplot2::aes_string(label = n_pa_hoyre_side), size=4, hjust=0, colour="grey30") +
#                 ggplot2::coord_flip(ylim = c(0,1)) +
# 
#                 ggplot2::theme_classic() +
#                 ggplot2::theme(rect = ggplot2::element_rect(fill = "transparent"),
#                       axis.text = ggplot2::element_blank(),
#                       strip.text = ggplot2::element_blank(),
#                       strip.background = ggplot2::element_blank(),
#                       line = ggplot2::element_blank()) +
#                 ggplot2::labs(x=NULL, y=NULL)
#         })
# 
# 
#     names(X) <- unique(df[[batteri]])
#     Y <- cowplot::align_plots(plotlist = X,  align = "hv", axis = "b")
#     Y2 <- cowplot::align_plots(plotlist = X2, align= "v", axis= "b")
#     sapply(1:length(Y), function(i) {
#         if(!(dropp_om_singelgruppe & length(unique(X[[i]]$data[[grupper]]))==1)) {
#             hgt <- 1+.6*sum(!duplicated(X[[i]]$data[,c(grupper, sporsmol)]))
#             name <- gsub("æ|å|Æ|Å", "a", names(X)[[i]]) # For svglite-pakken
#             name <- gsub("ø|Ø", "o", name)
#             name <- gsub(pattern = "[[:punct:]]|[\n]|_{2,}|[[:space:]]", "_", name)
#             name <- substr(start = 1, stop = 50, x = name)
#             name <- paste0(mappe, name, paste0(".", fileformat))
#             print(name)
#             cowplot::save_plot(cowplot::plot_grid(plotlist = list(Y[[i]], Y2[[i]]), align = "h", axis = "bt", rel_widths = c(.94, .06)), base_width = 12, base_height = hgt, filename = name)
#             name
#         }
#     })
# }

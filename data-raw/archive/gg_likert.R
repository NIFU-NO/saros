#### Lage grafer med samme layout som i NIFUmalen.
#### Denne sørger for at alle figurene ser helt like ut, bortsett fra at høyden avhenger av antall spørsmål man rapporterer

### data = datasettet.
### var_group = navnet på en kolonne som sier hvilke spørsmål som skal på samme graf.
### folder = foldern du vil lagre i.
### Bruker pakkene:  ggplot, cowplot og ggfittext
### Dersom den feiler med "Error in if (shades::lightness(text$fill) < 50) { : missing value where TRUE/FALSE needed" å er det fordi category-variabelen er NA. Gjør denne til character.


### Gjøremålsliste forbedringer:
# Rekode totalt til bred format df og

gg_likert <-
    function(data,
             var_group="spm_gruppe",
             var="variableName2",
             frequency_column="p",
             category="x",
             folder = "H:/HVL/Grafer/Adm/",
             colour_order=c(2,4,5,3,1,6), n_pa_hoyre_side=NULL, fileformat="svg") {
    library(ggplot2)
    library(cowplot)
    library(ggfittext)
    dir.create(folder, showWarnings = F)
    if(nchar(folder)>64) stop("Beklager, folderbanen er for lang. png-funksjonen i R greier ikke mer enn 64? tegn...")
    NIFUmal_fargekoder <- c(red="#C82D49",
                             black = "#363636",
                             beige="#EDE2D2",
                             blue="#2D8E9F",
                             purple="#DBD2E0",
                             orange="#E8AE63")

    if(!is.null(n_pa_hoyre_side) && is.character(n_pa_hoyre_side)) {
        X <-
            lapply(unique(data[[var_group]]), function(gruppe) {
                data_del <- as.data.frame(data[data[[var_group]] == gruppe, ])

                data_del[[var]] <- factor(data_del[[var]], levels = unique(data_del[[var]]))
                hoyre_side <- data_del[!duplicated(data_del[[var]]), n_pa_hoyre_side]

                ggplot(data = data_del, mapping = aes_string(y=frequency_column, x=paste0("as.integer(", var,")"),
                                                           fill=category, group=var), cumulative=TRUE) +
                    geom_col(position = "fill", width = .8) +
                    geom_fit_text(mapping = aes_string(label = paste0("sprintf(", frequency_column, "*100,fmt='%1.0f%%')")), position="stack", contrast = T, size=16, min.size=4) +
                    coord_flip() +
                    scale_y_continuous(limits = c(-.003, 1.015), expand=c(0,0), labels = scales::percent_format()) +
                    scale_x_continuous(breaks = 1:length(levels(data_del[[var]])),
                                       expand = c(0, 0),
                                       labels = levels(data_del[[var]]),
                                       sec.axis = sec_axis(~.,
                                                           breaks=1:length(levels(data_del[[var]])),
                                                           labels = hoyre_side)) +
                    scale_fill_manual(name="", values = unname(NIFUmal_fargekoder)[colour_order]) +
                    guides(fill=guide_legend(reverse = F, nrow = 1), colour=FALSE) +
                    theme_classic() +
                    theme(legend.position = "bottom",
                          #axis.text.y = element_text(hjust = 0.5),
                          # plot.caption = element_text(hjust = -1, size = 12),
                          text = element_text(family = "sans", size = 18, colour="black")) +
                    labs(x=NULL, y=NULL, caption=str_wrap(gsub("[\n]{2,}|[[:space:]]{2,}", " ", gruppe), width = 120))
            })
    } else {
    X <-
        lapply(unique(data[[var_group]]), function(gruppe) {
            data_del <- data[data[[var_group]] == gruppe, ]
            ggplot(data = data_del, mapping = aes_string(y=frequency_column, x=var, fill=category, group=var), cumulative=TRUE) +
                geom_col(position = "fill", width = .8) +
                geom_fit_text(mapping = aes_string(label = paste0("sprintf(", frequency_column, "*100,fmt='%1.0f%%')")), position="stack", contrast = T, size=16, min.size=4) +
                coord_flip() +
                scale_y_continuous(limits = c(-.003, 1.015), expand=c(0,0), labels = scales::percent_format()) +
                scale_fill_manual(name="", values = unname(NIFUmal_fargekoder)[colour_order]) +

                guides(fill=guide_legend(reverse = F, nrow = 1), colour=FALSE) +
                theme_classic() +
                theme(legend.position = "bottom",
                      # plot.caption = element_text(hjust = -1, size = 12),
                      text = element_text(family = "sans", size = 18)) +
                labs(x=NULL, y=NULL, caption=str_wrap(gsub("[\n]{2,}|[[:space:]]{2,}", " ", gruppe), width = 120))
        })
    }
    names(X) <- unique(data[[var_group]])
    Y <- align_plots(plotlist = X,  align = "hv", axis = "b")
    sapply(1:length(Y), function(i) {
        hgt <- 1+.8*n_distinct(X[[i]]$data[[var]])
        name <- gsub("æ|å|Æ|Å", "a", names(X)[[i]]) # For svglite-pakken
        name <- gsub("ø|Ø", "o", name)
        name <- gsub(pattern = "[[:punct:]]|[\n]|_{2,}|[[:space:]]", "_", name)
        name <- str_trunc(name, width = 50, side = "left", ellipsis = "")
        name <- paste0(folder, name, paste0(".", fileformat))
        save_plot(ggdraw(Y[[i]]), base_width = 12, base_height = hgt, filename = name)
        name
    })
}

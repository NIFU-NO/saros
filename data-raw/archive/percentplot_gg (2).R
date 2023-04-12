
### Bruker pakkene:  ggplot, cowplot og ggfittext
### Dersom den feiler med "Error in if (shades::lightness(text$fill) < 50) { : missing value where TRUE/FALSE needed" å er det fordi category_var-variabelen er NA. Gjør denne til character.
### En lignende feilmelding skyldes at det er duplikater av spørsmålet i datasettet, innenfor et variable_group.
### Error in if ((tgdim$width > xdim | tgdim$height > ydim) | (x$grow & tgdim$width <  : missing value where TRUE/FALSE needed
###### Denne feilmeldingen skyldes alltid at det er duplikater i datasettet som ikke kan være der.



#' Create barplot
#'
#' @param data data.frame
#' @param y_string_wrap_width Wrap y-labels if longer than this. Default=40.
#' @param reverse_y Reverse order of y-labels (defaults to FALSE).
#' @param base_font_size Base font size. Default=12.
#' @param percent Logical, percentage vs proportion (default).
#' @param decimals Number of decimals for labels.
#' @importFrom dplyr mutate na_if
#' @import ggplot2
#' @importFrom scales label_percent label_number
#' @importFrom ggfittext geom_bar_text
#' @return ggplot2-object
#' @export
#'
#' @examples tab_cat_prop_to_barplot(
#' tab_cat_prop(ex_survey1[,paste0("b_", 1:3)]))
# tab_cat_prop_to_barplot <-
# 	function(data,
# 			 y_string_wrap_width=40, reverse_y=FALSE, base_font_size=12,
# 			 percent=FALSE, decimals=3) {
#
# 		construct <- unique(data$yvar_group)
# 		construct_label <- unique(data$yvar_group_label)
# 		caption_string <-
# 			paste0(if(length(construct_label)==1L) construct_label,
# 				   " (", construct, ")")
#
#
# 		if(length(construct)==1L) {
# 			orig_fact <- unique(data$yvar_cat)
# 			p <-
# 				data %>%
# 				dplyr::mutate(labelled::to_character(.data$yvar_cat),
# 					yvar_cat = factor(.data$yvar_cat, levels=rev(orig_fact)),
# 							  yvar_cat = dplyr::na_if(.data$yvar_cat, "NA"),
# 					   yvar_part_label = center_string(.data$yvar_part_label,
# 					   								maxwidth=.env$y_string_wrap_width),
# 					bar_label = round(.data$.n, decimals),
# 					   bar_label = if(percent) paste0(.data$bar_label*100, "%") else .data$bar_label)
# 			p %>%
# 				ggplot2::ggplot(mapping = ggplot2::aes(x = .data$.n,
# 													   y = .data$yvar_part_label,
# 													   fill = .data$yvar_cat,
# 													   label = .data$bar_label)) +
# 				ggplot2::geom_col(position = "stack") +
# 				ggfittext::geom_bar_text(position = "stack", place = "center", na.rm = FALSE) +
# 				ggplot2::scale_x_continuous(expand = c(0, 0),
# 											limits = c(0, 1.05),
# 											labels = if(percent) scales::label_percent() else scales::label_number()
# 											) +
# 				ggplot2::scale_y_discrete(limits=if(reverse_y) rev) +
# 				ggplot2::scale_fill_brewer(type = "seq", direction = -1, na.value="gray90",
# 										   guide=ggplot2::guide_legend(reverse = F)) +
# 				ggplot2::guides(fill=ggplot2::guide_legend(nrow = 1, reverse = T)) +
# 				ggplot2::theme_classic(base_size = base_font_size) +
# 				ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal") +
# 				ggplot2::labs(x=NULL, y=NULL, title=NULL, fill=NULL,
# 							  caption = caption_string)
#
# 		}
# 	}


#' Turn a list of tabulated data frames into plots to be saved.
#'
#' @param l List of data frames from tab_cat_prop()
#' @param y_string_width y_var strng wrap width
#' @param y_reverse Reverse order of y_var? Defaults to true.
#' @param output_dir String.
#' @param output_file_prefix String
#' @param output_ext Format
#' @param plot_width Plot width
#' @param plot_height Plot height
#' @param plot_scale Plot scale
#' @param plot_units units. defaults to cm
#' @param base_font_size Base font size. Defaults to 12
#' @importFrom cowplot align_plots
#' @importFrom rlang set_names
#' @importFrom purrr map map_chr walk2
#' @importFrom tidyr separate
#' @importFrom dplyr group_by group_map pull
#' @importFrom ggplot2 ggsave
#' @return List of plots
#' @export
#'
#' @examples
# tab_cat_list_to_plots <- function(l, y_string_width=40, y_reverse=TRUE,
# 								  output_dir = getwd(), output_file_prefix="", output_ext="png",
# 								  plot_width=33.8, plot_height=19, plot_scale=1, plot_units="cm",
# 								  base_font_size=12) {
# 	p_list <-
# 		l %>%
# 		purrr::map(.f= ~.x %>%
# 			   	tidyr::separate(col = .data$.variable, into=c(".var_group", ".var_part"), sep = "_", remove = FALSE) %>%
# 			   	dplyr::group_by(.data$.var_group) %>%
# 			   	dplyr::group_map(.keep = T, .f = function(.x, ...) .x)) %>%
# 		unlist(recursive = F) %>%
# 		rlang::set_names(nm = purrr::map_chr(.x = ., .f=~dplyr::pull(.x, .data$.var_group) %>% unique())) %>%
# 		purrr::map(.f=~barplot_tab_cat_percent(.data = .x, y_string_wrap_width = y_string_width, reverse_y = y_reverse)) %>%
# 		cowplot::align_plots(plotlist = ., align = "hv", axis = "lrbt")
# 	purrr::walk2(p_list, .y = names(p_list), .f=function(p, construct) {
# 			ggplot2::ggsave(
# 				filename = paste0(output_dir, "/", output_file_prefix, construct, ".", output_ext),
# 				plot = p, scale = plot_scale, width = plot_width, height = plot_height, units = plot_units)
# 		})
# 		p_list
# }



#
#
#                right_side <- data_part[!duplicated(data_part[c(groups, labels),]), n_right_side]
#                ggplot2::ggplot(data = data_part, mapping = ggplot2::aes_string(y="const", x=groups)) + #
#                  ggplot2::facet_grid(variableName2~., switch = "y", scales = "free_y", space = "free_y") +
#                  ggiraph::geom_text_interactive(mapping = ggplot2::aes(label = {{n_right_side}}), size=4, hjust=0, colour="grey30")



# names(X) <- unique(data[[variable_group]])




# m
# max_width <- max(nchar(data[[n_right_side]]))

# if(!is.null(n_right_side) && is.character(n_right_side)) {
# X <-
# purrr::map(.x = unique(data[[variable_group]]),
#            .f = function(group) {
# data_part <-
#   tibble::as_tibble(x = data) |>
#   dplyr::filter(variable_group == group)
# facet <- enquo(labels)
# labels <- quo_name(labels)

# data_part[[groups]] <-
#   factor(x = data_part[[groups]],
#          levels = unique(data_part[[groups]]))
# right_side <- data_part[!duplicated(data_part[c(groups, labels),]), n_right_side]
# data_part$facet_var <- data_part[[labels]]
# cap <- if(length(unique(data_part[[labels]])) > 1) {
#   cap <- gsub("[\n]{2,}|[[:space:]]{2,}", " ", group)
#   cap <- strwrap(cap, width = 120, simplify = T)
#   paste0(cap, collapse="\n")
# } else NULL
# p <-

# align_plots <- function(x) {
#   Y <- cowplot::align_plots(plotlist = X,  align = "hv", axis = "b")
#   Y2 <- cowplot::align_plots(plotlist = X2, align= "v", axis= "b")
#   sapply(1:length(Y), function(i) {
#     if(!(drop_if_single_group & length(unique(X[[i]]$data[[groups]]))==1)) {
#       hgt <- 1+.6*sum(!duplicated(X[[i]]$data[,c(groups, labels)]))
#       name <- gsub("æ|å|Æ|Å", "a", names(X)[[i]]) # For svglite-pakken
#       name <- gsub("ø|Ø", "o", name)
#       name <- gsub(pattern = "[[:punct:]]|[\n]|_{2,}|[[:space:]]", "_", name)
#       name <- substr(start = 1, stop = 50, x = name)
#       name <- paste0(folder, name, paste0(".", file_format))
#       name
#     }
#   })
# }
#
# save_plot <- function(){
#   dir.create(folder, showWarnings = F)
#   if(nchar(folder)>64) cli::cli_abort("The path is too long. The png-function in R handles only 64 characters.")
#
# }

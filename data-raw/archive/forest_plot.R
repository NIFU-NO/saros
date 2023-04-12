
#' Creates List with Forest Plots from Data Frame
#'
#' @param data Data frame outputed from extract_mplus_reg_coefs()
#' @param caption_suffix Optional suffix string
#' @param y_string_wrap_width Wrap y_labels if wider than this.
#' @param reverse_y Reverse order of y_axis. Defaults to TRUE.
#'
#' @return List with ggplots
#' @export
#'
#' @examples
create_forest_plot <- 
	function(data, 
			 caption_suffix="", y_string_wrap_width=40, reverse_y=TRUE) {

				nm <- unique(dplyr::pull(data, .data$y_label))
				r2_val <- gsub("^0", "", unique(as.numeric(data$r2)))
				
				caption_exp <- 
					bquote(italic(R)^2==.(
						paste0(r2_val, ".", collapse=","))~italic(N)==.(
							paste0(unique(data$n), ". ", caption_suffix)))
				
				ggplot2::ggplot(data = data, 
									mapping = ggplot2::aes(y=.data$y, 
														   x=.data$x, 
														   xmin=.data$xmin, 
														   xmax=.data$xmax, 
														   colour=.data$colour)) +
					ggplot2::facet_grid(rows = vars(.data$facet), 
										scales = "free", 
										space = "free", 
										switch = "y") +
					ggplot2::geom_vline(xintercept = 0, 
										linetype="dashed", 
										colour="gray") +
					ggplot2::geom_point() +
					ggplot2::geom_errorbarh() +
					ggplot2::geom_text(colour="gray40", 
									   mapping = 
									   	ggplot2::aes(
									   		x=max(.data$xmax, na.rm=TRUE)*1.05, 
									   		y=.data$y, 
									   		label=formatC(round(.data$x, 2), 
									   					  format='f',
									   					  digits=2))) +
					ggplot2::theme_classic(base_size = 16) +
					ggplot2::guides(colour = "none") +
					ggplot2::labs(subtitle = unname(nm), 
								  x=NULL, 
								  y=NULL, 
								  caption = caption_exp)
	}


save_forest_plot_list <- 
	function(data, 
			 output_dir = getwd(), output_file_prefix="", output_ext="png",
			 plot_width=33.8, plot_height=19, plot_scale=1, plot_units="cm",
			 ...) {
	data %>%
		dplyr::group_by(.data$y) %>%
		dplyr::group_map(.keep = T, .f = function(.x, ...) .x) %>%
		rlang::set_names(nm = purrr::map_chr(.x = ., 
											 .f=~dplyr::pull(.x, .data$y) %>% 
											 	unique(.))) %>%
		purrr::map(.f=~create_forest_plot(data = .x, ...)) %>%
		cowplot::align_plots(plotlist = ., align = "hv", axis = "lrbt") %>%
		purrr::walk2(., .y = names(.), .f=function(p, construct) {
			ggplot2::ggsave(
				filename = paste0(output_dir, "/", 
								  output_file_prefix, construct, ".", 
								  output_ext), 
				plot = p, scale = plot_scale, 
				width = plot_width, height = plot_height, units = plot_units)
		})
}

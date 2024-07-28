#' @keywords internal
"_PACKAGE"

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
ignore_unused_imports <- function() {
  saros.base::draft_report
  saros.base::ex_survey
  saros.base::ex_survey_ch_overview
  saros.base::filename_sanitizer
  saros.base::get_chunk_template_defaults
  saros.base::get_raw_labels
  saros.base::is_string
  saros.base::refine_chapter_overview
  saros.contents::fig_height_h_barchart
  saros.contents::get_data_label_opts
  saros.contents::get_makeme_types
  saros.contents::ggsaver
  saros.contents::make_content
  saros.contents::make_link
  saros.contents::make_link_global_settings_get
  saros.contents::make_link_global_settings_reset
  saros.contents::make_link_global_settings_set
  saros.contents::makeme
  saros.contents::makeme_global_settings_get
  saros.contents::makeme_global_settings_reset
  saros.contents::makeme_global_settings_set
  saros.contents::n_range
  saros.contents::n_rng
  saros.utils::attach_qualtrics_labels
  saros.utils::col_to_binaries
  saros.utils::combn_upto
  saros.utils::copy_folder_contents_to_dir
  saros.utils::create_directory_structure
  saros.utils::create_email_credentials
  saros.utils::create_r_files
  saros.utils::download_zip_to_folder
  saros.utils::generate_yaml_from_directory
  saros.utils::handpick
  saros.utils::initialize_saros_project
  saros.utils::omitted_recoder_df
  saros.utils::post_render_docx_img_replacer
  saros.utils::read_default_draft_report_args
  saros.utils::recode_checkbox_sets
  saros.utils::remove_special_chars_in_labels
  saros.utils::rename_by_labels
  saros.utils::replace_docx_imgs_with_mscharts
  saros.utils::replace_stata_labels
  saros.utils::sanitize_labels
  saros.utils::setup_access_restrictions
  saros.utils::swap_label_colnames
  saros.utils::write_default_draft_report_args
}

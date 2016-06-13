crossref_rename <- function(x){
  dplyr::rename(x, alternative_id = alternative.id, 
  container_title = container.title,
  reference_count = reference.count,
  license_content_version = license_content.version,
  license_delay_in_days = license_delay.in.days, 
  update_policy = update.policy)          
}
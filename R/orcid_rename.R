#' @title Rename orcid json fields with underscores
#' @description This converts hyphens and stops in column names for results from
#'   rorcid into R friendly underscores. This is a wrapper for dplyr rename for
#'   rorcid. Note that for CRAN use it will probably generate notes on bindings
#'   and a method to fix that (or an alternative approach) may be desirable.
#' @param data A data.frame containing orcid results.
#' @return A data.frame
#' @export
#' @importFrom dplyr rename
#' @examples \dontrun{orcid_rename(data)}
orcid_rename <- function(data){dplyr::rename_(data,
  relevancy_score_value = "`relevancy-score.value`",
  orcid_id = "`orcid-id`",
  orcid_deprecated = "`orcid-deprecated`",
  orcid_preferences = "`orcid-preferences`",
  orcid_history = "`orcid-history`",
  orcid_activities = "`orcid-activities`",
  orcid_internal = "`orcid-internal`",
  group_type = "`group-type`",
  client_type = "`client-type`",
  orcid_identifier_value = "`orcid-identifier.value`",
  orcid_identifier_uri = "`orcid-identifier.uri`",
  orcid_identifier_path = "`orcid-identifier.path`",
  orcid_identifier_host = "`orcid-identifier.host`",
  researcher_urls = "`researcher-urls`",
  contact_details = "`contact-details`",
  personal_details_credit_name = "`personal-details.credit-name`",
  personal_details_other_names = "`personal-details.other-names`",
  personal_details_given_names_value = "`personal-details.given-names.value`",
  personal_details_given_names_visibility = "`personal-details.given-names.visibility`",
  personal_details_family_name_value = "`personal-details.family-name.value`",
  personal_details_family_name_visibility = "`personal-details.family-name.visibility`",
  external_identifiers_external_identifier= "`external-identifiers.external-identifier`",
  external_identifiers_visibility = "`external-identifiers.visibility`",
  personal_details_other_names_other_name = "`personal-details.other-names.other-name`",
  personal_details_other_names_visibility = "`personal-details.other-names.visibility`",
  researcher_urls_researcher_url = "`researcher-urls.researcher-url`",
  researcher_urls_visibility = "`researcher-urls.visibility`",
  contact_details_email = "`contact-details.email`",
  contact_details_address_country_value = "`contact-details.address.country.value`",
  contact_details_address_country_visibility = "`contact-details.address.country.visibility`",
  keywords_keyword = "keywords.keyword",
  keywords_visibility = "keywords.visibility",
  biography_value = "biography.value",
  biography_visibility = "biography.visibility",
  personal_details_credit_name_value = "`personal-details.credit-name.value`",
  personal_details_credit_name_visibility = "`personal-details.credit-name.visibility`",
  external_identifiers = "`external-identifiers`",
  contact_details_address_country = "`contact-details.address.country`",
  personal_details_given_names = "`personal-details.given-names`",
  personal_details_family_name = "`personal-details.family-name`",
  contact_details_address = "`contact-details.address`")
  }


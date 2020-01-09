#' Render HTML version of QA report
#'
#' @param ... Arguments passed down
#'
#' @return R Markdown output format to pass to render
#' @export
qa_html_report <- function(...){
  pkg_resource  <-  function(...) {
    system.file(..., package = "PsNR")
  }
  css <- pkg_resource("reporting/style.css")
  header <- pkg_resource("reporting/header.html")
  navbar <- pkg_resource("reporting/navbar.html")
  knitr::opts_chunk$set(echo = FALSE)
  rmarkdown::html_document(
    number_sections = TRUE,
    css = css,
    includes = rmarkdown::includes(in_header = header, before_body = navbar),
    ...)
}

#' @export
qa_ui_tmode_content <- function(...){
  htmltools::tags$div(class = "qa-tutorial-content collapse", ...)
}

#' @export
qa_ui_manual_panel <- function(title, ...){
  qa_ui_panel(
    title = title, 
    tooltip = "QA user guide",
    icon = "glyphicon-user",
    ... = ...
  )
}

#' @export
qa_ui_technical_panel <- function(title, ...){
  qa_ui_panel(
    title = title,
    tooltip = "Technical details",
    icon = "glyphicon-cog",
    ... = ...
  )
}

#' @export
qa_ui_advice_panel <- function(title, ...){
  qa_ui_panel(
    title = title,
    tooltip = "Actionable advice",
    icon = "glyphicon-flash",
    ... = ...
  )
}

#' @export
qa_ui_panel <- function(title, tooltip, icon, ...){
  heading <- htmltools::tags$div(
    class="panel-heading", 
    htmltools::tags$h4(
      qa_ui_icon(
        icon = icon,
        tooltip = tooltip,
        style="font-size: 15px;"
      ),
      title
    )                    
  )
  body <- htmltools::tags$div(
    class="panel-body",
    ...
  )
  htmltools::tags$div(
    class = "panel panel-default",
    heading,
    body
  )
}

#' @export
qa_ui_element_info <- function(element_id, text){
  htmltools::tags$div(
    style = "position: relative; float:right;",
    htmltools::tags$button(
      class = "qa-element-info-button", 
      `data-element-id` = element_id,
      `data-content` = text,
      qa_ui_icon("glyphicon-info-sign")
    )
  )
}

#' @export
qa_ui_icon <- function(icon, tooltip = NULL, style = NULL){
  tooltip_opts <- list()
  if(!is.null(tooltip)) tooltip_opts <- list(`data-toggle`="tooltip", title=tooltip) 
  htmltools::tags$span(
    class = glue::glue("glyphicon {icon}"),
    `aria-hidden` = "true",
    !!!tooltip_opts,
    style = style)
}

qa_ui_cite <- function(key, bibliography = qa_references()){
  cite(key, bibliography)
}


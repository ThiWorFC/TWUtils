#' Format Table for PowerPoint
#'
#' Transform R tables to pre-formated flextable before export to PowerPoint
#'
#' @param table The table to be re-formated
#'
#' @returns The table as a formatted flextable
#' @export
#'
#' @examples NULL
tableformat <- function(table){

  res <- table %>%
    flextable::flextable() %>%
    flextable::font(fontname="Verdana", part="all") %>%
    flextable::fontsize(size=9, part="all") %>%
    flextable::align(align="center", part="all") %>%
    flextable::bg(bg="#0094D9", part="header") %>%
    flextable::border_outer(border=officer::fp_border(color="#0094D9", style="solid", width=1)) %>%
    flextable::border_inner(border=officer::fp_border(color="#0094D9", style="solid", width=1)) %>%
    flextable::color(color="white", part="header") %>%
    flextable::bold(part="header") %>%
    flextable::align(j=1, align="left") %>%
    flextable::bold(j=1, bold=TRUE) %>%
    flextable::padding(padding.top=1, padding.bottom=1,
            padding.left=1, padding.right=1,
            part="body") %>%
    flextable::autofit()

  return(res)
}

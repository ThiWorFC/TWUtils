tableformat <- function(data){

  require(tidyverse)
  require(flextable)

  res <- data %>%
    flextable() %>%
    font(fontname="Verdana", part="all") %>%
    fontsize(size=9, part="all") %>%
    align(align="center", part="all") %>%
    bg(bg="#0094D9", part="header") %>%
    border_outer(border=fp_border(color="#0094D9", style="solid", width=1)) %>%
    border_inner(border=fp_border(color="#0094D9", style="solid", width=1)) %>%
    color(color="white", part="header") %>%
    bold(part="header") %>%
    align(j=1, align="left") %>%
    bold(j=1, bold=TRUE) %>%
    padding(padding.top=1, padding.bottom=1,
            padding.left=1, padding.right=1,
            part="body") %>%
    autofit()
  return(res)
}

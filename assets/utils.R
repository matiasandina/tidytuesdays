make_caption <- function(accent, bg, data) {
  # borrowing from https://github.com/doehm/tidytues
  #mastodon <- glue::glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
  twitter <- glue::glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
  github <- glue::glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
  floppy <- glue::glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
  #threads <- glue::glue("<span style='font-family:fa-brands; color:{accent}'>&#xe618;</span>")
  space <- glue::glue("<span style='color:{bg};font-size:1px'>'</span>")
  space2 <- glue::glue("<span style='color:{bg}'>-</span>") 
  glue::glue("{twitter} {space2} @NeuroMLA {space2} {github} {space2} matiasandina/tidytuesdays {space2}{space2} {floppy}{space2} {data}")
}

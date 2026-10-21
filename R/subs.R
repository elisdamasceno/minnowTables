#' Unicode subscript characters
#'
#' @description openxlsx has no rich-text API, so subscript characters are
#'   rendered using their dedicated Unicode code points instead. Unicode only
#'   defines subscripts for a subset of lowercase letters (a, e, h, i, j, k, l,
#'   m, n, o, p, r, s, t, u, v, x); digits 0-9 are also included.
#'
#' @format A named character vector mapping lowercase letters and digits to
#'   subscript glyphs
#' @export
subscript_letters <- c(a="ₐ", e="ₑ", h="ₕ", i="ᵢ", j="ⱼ",
                       k="ₖ", l="ₗ", m="ₘ", n="ₙ", o="ₒ",
                       p="ₚ", r="ᵣ", s="ₛ", t="ₜ", u="ᵤ",
                       v="ᵥ", x="ₓ",
                       "0"="₀", "1"="₁", "2"="₂", "3"="₃", "4"="₄",
                       "5"="₅", "6"="₆", "7"="₇", "8"="₈", "9"="₉")

#' Convert letters to Unicode subscript
#'
#' @description Looks up letters or digits in [subscript_letters]. The
#'   subscript counterpart of [sup()].
#'
#' @param letter A character vector of single lowercase letters or digits
#'
#' @return A character vector of subscript glyphs; errors if a character has no
#'   mapping
#' @export
#'
#' @examples
#' paste0("SO", subs("2"))
#' paste0("NO", subs("x"))
#' subs(c("a", "2"))
subs <- function(letter) {
  out <- subscript_letters[letter]
  if (anyNA(out)) stop("No subscript mapping for character(s): ", paste(letter[is.na(out)], collapse=", "))
  unname(out)
}

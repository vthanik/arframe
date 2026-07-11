# .icon pins fontawesome margins and rejects unknown names

    Code
      .icon("nope")
    Condition
      Error:
      ! Unknown icon name "nope".
      i See `.fa_names` for the registered set.

# .glyph renders house SVGs with dashed state classes; unknown names error

    Code
      .glyph("nope")
    Condition
      Error:
      ! Unknown chrome glyph "nope".
      i See `.CHROME_GLYPHS` for the registered set.


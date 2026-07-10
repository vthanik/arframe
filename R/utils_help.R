# The help system (spec 2026-07-10 inspector-consolidation, section 4). A `?`
# icon on every Setup card and inspector section opens an IN-DEPTH modal, in
# the datasetviewer Filter-help style: a bold heading, plain-language prose on
# what the section does and *why it exists in a submission workflow*, and
# worked ADaM examples in bordered code blocks. This registry replaces the old
# inline gray helper-text paragraphs (user call 2026-07-10: "any level of
# user should understand after reading it, not just two words").
#
# Mechanism (small; the writing is the work): `.help_icon(ns, topic)` posts
# `{topic, nonce}` to a per-module-scope `help_open` observer, which calls
# `.show_help(topic)` to open the modal. Each entry is a zero-arg function
# returning a `tagList` so nothing renders until the modal opens.

# ---- content helpers ------------------------------------------------------

#' A modal heading (the bold topic title at the top of every entry).
#' @noRd
.help_h <- function(x) {
  shiny::tags$h3(class = "ar-help-h", x)
}

#' A prose paragraph. Plain language; no jargon left unexplained.
#' @noRd
.help_p <- function(...) {
  shiny::tags$p(class = "ar-help-p", ...)
}

#' An inline code chip (a column name or literal inside a sentence).
#' @noRd
.help_code <- function(x) {
  shiny::tags$code(class = "ar-help-code", x)
}

#' A bordered worked-example block (one expression on its own line). Carries
#' the `ar-help-code` class so every entry provably ships >= 1 example.
#' @noRd
.help_block <- function(x) {
  shiny::tags$div(class = "ar-help-block ar-help-code ar-mono", x)
}

# ---- icon + modal ---------------------------------------------------------

#' The `?` help icon: a small quiet circled button at the right end of a
#' section header or card header. Its onclick MUST `preventDefault()` +
#' `stopPropagation()` so a click opens the modal WITHOUT toggling the
#' surrounding `<details>` accordion or bubbling to the card. Posts
#' `{topic, nonce}` to the module's shared `help_open` observer (the id-less
#' `title_edit`/`cell_edit` idiom -- one observer, not one per section).
#' @param ns *The module namespace function.* From `session$ns`.
#' @param topic *A `.HELP_TOPICS` key.* `<character(1)>`.
#' @noRd
.help_icon <- function(ns, topic) {
  shiny::tags$button(
    type = "button",
    class = "ar-help-btn",
    `aria-label` = paste("Help:", topic),
    title = "What is this?",
    onclick = sprintf(
      "event.preventDefault(); event.stopPropagation(); Shiny.setInputValue('%s', {topic: '%s', nonce: Date.now()}, {priority: 'event'})",
      ns("help_open"),
      topic
    ),
    "?"
  )
}

#' Open the help modal for a topic. Unknown topics are a silent no-op (never
#' errors -- a stale nonce or a topic without an entry just does nothing).
#' Follows `.confirm_delete_modal()`'s pattern: build the dialog, then
#' `tagAppendAttributes()` the scoping class onto the `.modal` root so
#' `.ar-help-modal` never bleeds onto datasetviewer's own dialogs.
#' @param topic *A `.HELP_TOPICS` key.* `<character(1)>`.
#' @noRd
.show_help <- function(topic) {
  entry <- .HELP_TOPICS[[topic]]
  if (is.null(entry)) {
    return(invisible(NULL))
  }
  m <- shiny::modalDialog(
    entry(),
    easyClose = TRUE,
    footer = NULL,
    size = "l"
  )
  shiny::showModal(shiny::tagAppendAttributes(m, class = "ar-help-modal"))
}

# ---- registry -------------------------------------------------------------

# The explicit set of section/card ids that MUST carry a help entry. Derived
# from the code, not the plan's example list:
#   * Setup section tabs are the 9 real ids (mod_setup.R:241-249): there is no
#     `sources`/`preferences` tab (folded into `paths`) and the Page tab's id
#     is `page` (title "Page & Style"), not `page_style`.
#   * Inspector adds the schema-region groups (`.OPT_SECTIONS` labels), the
#     table-layout groups (`.opt_layout_sections`), and every listing editor
#     (`.opt_listing_sections`) -- each is a real rendered section whose old
#     inline helper paragraph was absorbed here.
.HELP_REQUIRED <- c(
  # Setup cards.
  "study",
  "paths",
  "populations",
  "analysis_sets",
  "treatment",
  "page",
  "summaries",
  "footnotes",
  "team",
  # Inspector -- core sections.
  "roles",
  "title",
  "footnotes_out",
  "order",
  "filters",
  "population",
  # Inspector -- Options schema-region groups (.OPT_SECTIONS).
  "options_rows",
  "options_axes",
  "options_series",
  "options_legend",
  "options_options",
  # Inspector -- Options table-layout groups (.opt_layout_sections).
  "options_columns",
  "options_page",
  "options_spans",
  "options_pageby",
  # Inspector -- listing editors (.opt_listing_sections).
  "listing_sort",
  "listing_transpose",
  "listing_formats",
  "listing_stack"
)

.HELP_TOPICS <- list(
  # ==== Setup ==============================================================
  study = function() {
    shiny::tagList(
      .help_h("Study \u2014 the identity stamped on every output"),
      .help_p(
        "These five fields identify the trial: the sponsor, the protocol",
        "number, the study id, an optional indication, and the data",
        "extraction (cut) date. They are entered once and reused everywhere."
      ),
      .help_p(
        "A submission package puts this identity in the running header or",
        "footer of every table, figure, and listing so a reviewer can tell",
        "at a glance which study a page belongs to. The header band writes",
        "these values in through tokens \u2014 a token is a placeholder in",
        "curly braces that the engine replaces with the value you type here:"
      ),
      .help_block("{sponsor} - {protocol}"),
      .help_block("Data cut: {data_date}"),
      .help_p(
        "Sponsor, protocol, study id, and data extraction date are",
        "required: an output whose header references one of them will show",
        "a render error until you fill it in, rather than print a blank",
        "submission header. Indication is optional and resolves to blank."
      )
    )
  },
  paths = function() {
    shiny::tagList(
      .help_h("Paths \u2014 where the project reads and writes"),
      .help_p(
        "A project is a folder on disk, and these settings say which",
        "sub-folders hold each kind of file: the ADaM and SDTM data",
        "directories to read from, and the directories to write programs,",
        "rendered output, datasets, and logs into."
      ),
      .help_p(
        "The folder itself is the unit of collaboration \u2014 a teammate",
        "who opens the same folder sees the same outputs. Keeping the",
        "layout explicit means the package reproduces from the files alone,",
        "the way a SAS study reproduces from its pgms and output folders:"
      ),
      .help_block("programs/l-16-2-7-adverse-event-listing.R"),
      .help_block("output/l-16-2-7-adverse-event-listing.rtf"),
      .help_p(
        "Every output is saved as a program you can run outside the app,",
        "and a run-all.R reproduces the whole package with one command, so",
        "the rendered files are never the only copy of the analysis."
      )
    )
  },
  populations = function() {
    shiny::tagList(
      .help_h("Populations \u2014 what counts as a subject"),
      .help_p(
        "This binds the population dataset (normally ADSL, the",
        "subject-level analysis dataset) and the subject-key variable",
        "(normally USUBJID, the unique subject identifier). It answers one",
        "question: which dataset and which column define a single subject."
      ),
      .help_p(
        "Subject counts \u2014 the N in a column header, the denominator of",
        "every percentage \u2014 are computed against this dataset. Getting",
        "it right is what makes 45 (35.2%) mean 45 of the 128 subjects in",
        "the arm, not 45 of some record count:"
      ),
      .help_block("ADSL keyed by USUBJID"),
      .help_p(
        "The named analysis sets (Safety, FAS, Per-Protocol) that actually",
        "subset the rows live in the separate Analysis sets section \u2014",
        "this section only fixes the subject universe they draw from."
      )
    )
  },
  analysis_sets = function() {
    shiny::tagList(
      .help_h("Analysis sets \u2014 the reusable populations"),
      .help_p(
        "An analysis set is a named, filtered group of subjects: the Safety",
        "population, the Full Analysis Set, the Per-Protocol set, the",
        "Pharmacokinetic set. Each row here has an id, a display label, the",
        "dataset it draws from, and the filter that selects its subjects."
      ),
      .help_p(
        "Defining them once, centrally, is what keeps every output in the",
        "package consistent: 'Safety population' means the same rows on",
        "table 14.3.1 as on listing 16.2.7. The filter is a flag condition",
        "on the population dataset \u2014 flag variables end in FL and hold",
        "\"Y\" or \"N\":"
      ),
      .help_block('SAFFL = "Y"'),
      .help_block('FASFL = "Y"'),
      .help_p(
        "One set is marked the study default (the star). Each output picks",
        "its set in the inspector's Population section, and a running header",
        "can print the active set's label through the {analysis_set} token."
      )
    )
  },
  treatment = function() {
    shiny::tagList(
      .help_h("Treatment \u2014 the arms and how they decode"),
      .help_p(
        "This lists the treatment variables the study analyses by. Each row",
        "is a column plus its estimand basis \u2014 whether the analysis",
        "uses the treatment a subject was Planned to receive or the one",
        "they Actually received. The two are different columns:"
      ),
      .help_block("TRT01P  \u2014 planned treatment, period 1"),
      .help_block("TRT01A  \u2014 actual treatment, period 1"),
      .help_p(
        "Safety tables conventionally use the ACTUAL arm (a subject is",
        "counted where their real exposure was), while efficacy tables use",
        "the PLANNED arm (the randomised assignment). Listing the variable",
        "here fixes the columns \u2014 the arm groups \u2014 that every",
        "output splits its statistics across."
      ),
      .help_p(
        "The first row is the primary variable; its distinct data values",
        "become the arm decode (the column order and labels). An output's",
        "Options > Treatment then just picks a listed variable by name."
      )
    )
  },
  page = function() {
    shiny::tagList(
      .help_h("Page & Style \u2014 the paper the package prints on"),
      .help_p(
        "The study-level page geometry and typography: orientation",
        "(landscape or portrait), paper size, font family and size, the",
        "margins, and the running header and footer bands that print at the",
        "top and bottom of every page."
      ),
      .help_p(
        "These are set once so the whole package looks like one document.",
        "The header and footer bands are the study's identity line; they",
        "carry tokens the engine fills from Setup > Study and the active",
        "analysis set:"
      ),
      .help_block("{sponsor} - {protocol}"),
      .help_block("Page {page} of {npages}"),
      .help_p(
        "{page}/{npages} stay as field codes the renderer numbers per page",
        "\u2014 the RTF is the paginated truth. A band that references a",
        "required study token (sponsor, protocol, study id, data cut date)",
        "left blank in Setup raises an error rather than printing a gap."
      )
    )
  },
  summaries = function() {
    shiny::tagList(
      .help_h("Summaries \u2014 the default statistics and precision"),
      .help_p(
        "The continuous statistics a summary table shows for a numeric",
        "variable \u2014 for example n, Mean, SD, Median, and Min/Max \u2014",
        "and the number of decimal places each is rounded to. This is the",
        "study default every new output inherits."
      ),
      .help_p(
        "Fixing precision centrally is a submission requirement: a reviewer",
        "expects age summarised the same way on every table. The convention",
        "the engine follows is that the spread statistic carries one more",
        "decimal than the location statistic, and percentages are always",
        "one decimal place:"
      ),
      .help_block("AGE: Mean 35.2  SD 8.14  (mean at d, SD at d+1)"),
      .help_block("Percentages: 35.2% (always 1 dp)"),
      .help_p(
        "An individual output can override these in its Options > ROWS",
        "section; until it does, it renders exactly the rows and decimals",
        "you set here, so the package stays uniform by default."
      )
    )
  },
  footnotes = function() {
    shiny::tagList(
      .help_h("Footnotes \u2014 the study-wide footnote register"),
      .help_p(
        "The single list of footnotes reused across the report. Instead of",
        "retyping 'Adverse events are coded using MedDRA version 25.0' on",
        "every safety table, you write it once here under a short key and",
        "reference the key from each output."
      ),
      .help_p(
        "A shared register is what keeps wording identical package-wide: fix",
        "a typo once and it corrects everywhere the key appears. Each entry",
        "has a key and its text; an output cites it by @KEY:"
      ),
      .help_block("@meddra  Adverse events coded using MedDRA v25.0."),
      .help_block("@saf  Safety population: all treated subjects."),
      .help_p(
        "Per-output, one-off footnotes that are not reused live on the",
        "output itself, in the inspector's FOOTNOTES section \u2014 this",
        "register is only for the text shared across many outputs."
      )
    )
  },
  team = function() {
    shiny::tagList(
      .help_h("Team \u2014 who is working in this project"),
      .help_p(
        "The roster of people who have opened this project folder, a feed of",
        "recent edits, and a live presence rail showing who is active right",
        "now. Because the folder is the shared unit, two people can open it",
        "at once \u2014 from a shared drive or a synced folder \u2014 and see",
        "each other."
      ),
      .help_p(
        "This gives a study team the same awareness a shared document does:",
        "you can see that a colleague just edited table 14.3.1 before you",
        "touch it. Team state is written under a hidden folder:"
      ),
      .help_block(".arframe/activity/<name>.jsonl"),
      .help_p(
        "That folder is deliberately excluded when you export the package,",
        "so a sponsor deliverable never carries the team's activity log \u2014",
        "only the outputs, programs, and renders ship."
      )
    )
  },
  # ==== Inspector: core ====================================================
  roles = function() {
    shiny::tagList(
      .help_h("Roles \u2014 assign columns to the table's slots"),
      .help_p(
        "Every generator has named slots \u2014 the parts of the table it",
        "needs filled: the treatment column that becomes the arm headings,",
        "the variables to summarise down the rows, an optional grouping or",
        "by column. Roles is where you drag a dataset column into each slot."
      ),
      .help_p(
        "This is the heart of defining an output: the slots decide the",
        "table's shape, and the columns decide its content. A slot marked",
        "required must be filled before the output can render. For a",
        "demographics summary you might assign:"
      ),
      .help_block("Treatment \u2192 TRT01A"),
      .help_block("Summarise \u2192 AGE, SEX, RACE"),
      .help_p(
        "Each slot shows how many columns it accepts (one, or one-or-more).",
        "Where a slot accepts several, their drag order sets the block order",
        "on the page. Recodes and value labels are edited on the assigned",
        "column; ordering knobs live in Options > ORDER."
      )
    )
  },
  title = function() {
    shiny::tagList(
      .help_h("Title \u2014 the TLF number, kind, and heading"),
      .help_p(
        "The output's number (its place in the submission shell, such as",
        "14.3.1), the kind word (Table, Figure, or Listing), the main",
        "title, and any continuation title lines that print centered",
        "beneath it."
      ),
      .help_p(
        "The number is metadata you set, never something the app derives:",
        "real numbering follows the statistical analysis plan's shell (14.1",
        "for demographics, 14.2 for efficacy, 14.3 for safety, 16.2 for",
        "listings per ICH E3), which only the study team knows. The title",
        "block on the page prints the kind and number together:"
      ),
      .help_block("Table 14.3.1"),
      .help_block("Summary of Treatment-Emergent Adverse Events"),
      .help_p(
        "The contents rail groups outputs by kind but shows this number, so",
        "the number you type is what a reviewer sees in the list. A figure",
        "does not use continuation lines, so that editor only appears for",
        "table outputs."
      )
    )
  },
  footnotes_out = function() {
    shiny::tagList(
      .help_h("Footnotes \u2014 this output's own footnote lines"),
      .help_p(
        "The footnotes printed beneath this specific output, one per line.",
        "Drag the grips to reorder them; the order is part of the output",
        "and prints exactly as shown."
      ),
      .help_p(
        "Footnotes carry the caveats a reviewer needs to read the table",
        "correctly \u2014 the population definition, how a statistic was",
        "computed, what an abbreviation means. The first line conventionally",
        "states the population:"
      ),
      .help_block("Safety population."),
      .help_block(
        "Percentages are based on the number of subjects in each arm."
      ),
      .help_p(
        "For wording reused across many outputs, define it once in Setup >",
        "Footnotes and reference it by @KEY instead of retyping it here, so",
        "a later correction reaches every table at once."
      )
    )
  },
  order = function() {
    shiny::tagList(
      .help_h("Order \u2014 the sequence rows appear on the page"),
      .help_p(
        "Controls the order the table's rows are laid out. What it shows",
        "depends on the generator: a summary or crosstab drags its",
        "row-blocks into order; an occurrence (adverse-event) table chooses",
        "how its terms are ranked; a line or box figure orders the x levels."
      ),
      .help_p(
        "Order is a reporting choice, not a data fact, so it lives with the",
        "output. For an adverse-event table the choice is between two",
        "orderings, and it changes which rows land at the top of the page:"
      ),
      .help_block(
        "Frequency \u2014 most common events first (ties alphabetical)"
      ),
      .help_block("Alphabetical \u2014 body systems and terms in name order"),
      .help_p(
        "Frequency ranks each term by its pooled incidence across all arms,",
        "so the events that affected the most subjects overall rise to the",
        "top \u2014 the usual safety-review order. Where you assign the",
        "variables in Roles first, their drag order sets the block order",
        "here."
      )
    )
  },
  filters = function() {
    shiny::tagList(
      .help_h("Filters \u2014 subset the rows this output analyses"),
      .help_p(
        "A filter keeps only the records that match a condition before any",
        "statistics are computed. The population above applies the study's",
        "analysis set; these ad-hoc filters stack on top of it for a",
        "one-off restriction this output needs."
      ),
      .help_p(
        "Pick a variable, an operator, and a value. Conditions combine with",
        "AND \u2014 every listed condition must hold for a row to survive:"
      ),
      .help_block("AGE >= 18"),
      .help_p(
        "keeps adult subjects only, computed from the dataset's AGE column."
      ),
      .help_block('SAFFL = "Y"'),
      .help_p(
        "keeps rows flagged into the safety population. Flag variables end",
        "in FL and hold \"Y\" or \"N\"."
      ),
      .help_block("AEBODSYS is not na"),
      .help_p(
        "drops rows with a missing body system \u2014 use this to exclude",
        "unmapped events from an occurrence table."
      ),
      .help_p(
        "The telemetry line under the inspector shows how many records",
        "survive the active filters, so you can sanity-check a condition the",
        "moment you add it."
      )
    )
  },
  population = function() {
    shiny::tagList(
      .help_h("Population \u2014 the analysis set this output uses"),
      .help_p(
        "Which named analysis set (defined in Setup > Analysis sets) this",
        "output is computed on. Picking one applies that set's subject",
        "filter and fixes the denominator of every percentage in the table."
      ),
      .help_p(
        "Choosing the right population is a core submission decision:",
        "safety tables run on the Safety population, primary efficacy on the",
        "Full Analysis Set, and so on. Selecting a set here is the same as",
        "applying its filter:"
      ),
      .help_block('Safety \u2192 SAFFL = "Y"'),
      .help_block('Full Analysis Set \u2192 FASFL = "Y"'),
      .help_p(
        "This is the same value the contents table's POPULATION column",
        "shows, so the inspector and the list stay in sync. To add a set",
        "that is not offered here, define it first in Setup > Analysis",
        "sets; ad-hoc, non-population restrictions go in the Filters section."
      )
    )
  },
  # ==== Inspector: Options schema-region groups ============================
  options_rows = function() {
    shiny::tagList(
      .help_h("Rows \u2014 the statistics and how they read down the page"),
      .help_p(
        "Controls that govern the body rows of a summary or crosstab: which",
        "continuous statistics to show, how many decimal places to round",
        "them to, and how a grouping variable's levels are laid out."
      ),
      .help_p(
        "These decide what a reader sees in each cell. The statistics and",
        "decimals default to Setup > Summaries; overriding them here affects",
        "only this output. Decimals follow one rule \u2014 the spread",
        "statistic gets one more place than the mean, percentages always",
        "one:"
      ),
      .help_block("Decimals 1 \u2192 Mean 35.2, SD 8.14, 35.2%"),
      .help_p(
        "The row layout choice changes how a grouping variable prints:",
        "Nested indents each level under a heading row, Column puts the",
        "level labels into columns instead. A per-output statistics override",
        "shows an accent status with a one-click Reset back to the Setup",
        "default."
      )
    )
  },
  options_axes = function() {
    shiny::tagList(
      .help_h("Axes \u2014 how a figure's scales and markers read"),
      .help_p(
        "The figure-specific knobs: axis labels, the confidence-interval",
        "level and error-bar type, whether to draw a risk table or censor",
        "marks, the time-axis break spacing \u2014 whichever apply to the",
        "chosen figure type."
      ),
      .help_p(
        "These translate a statistical result into a readable chart. On a",
        "Kaplan-Meier plot, for instance, the CI level and censor marks are",
        "what a reviewer checks first:"
      ),
      .help_block("CI level 95% \u2192 shaded band around each curve"),
      .help_block("Time breaks every 4 \u2192 x axis at 0, 4, 8, 12 weeks"),
      .help_p(
        "Each control changes only its own element of the plot: raising the",
        "CI level widens the band, turning on the risk table adds the",
        "at-risk counts beneath the axis, and the axis labels replace the",
        "raw column names on the printed figure."
      )
    )
  },
  options_series = function() {
    shiny::tagList(
      .help_h("Series \u2014 the colours a figure draws its arms in"),
      .help_p(
        "The palette that colours the treatment arms (or other series) on a",
        "figure. Each arm gets a distinct, consistent colour across the",
        "whole plot \u2014 the curves, the points, the legend swatches."
      ),
      .help_p(
        "A consistent, legible palette is part of a figure being",
        "submission-ready: the arms must be told apart in colour and stay",
        "the same colour on every figure in the package. Choosing a palette",
        "reassigns every series at once:"
      ),
      .help_block("Placebo \u2192 grey, Active \u2192 blue"),
      .help_p(
        "The palette changes the colour of the plotted series only \u2014 it",
        "does not change which arms exist or their order, which come from",
        "the Treatment role and the arm decode."
      )
    )
  },
  options_legend = function() {
    shiny::tagList(
      .help_h("Legend \u2014 whether and where the key prints"),
      .help_p(
        "Whether a figure shows its legend (the key mapping each colour to",
        "an arm) and, if so, where it sits: to the right, at the bottom,",
        "top, or left of the plot area."
      ),
      .help_p(
        "The legend is how a reader decodes the colours, so on a multi-arm",
        "figure it is normally on; on a single-series plot it is redundant",
        "and can be turned off to give the data more room. The position",
        "changes only where the key sits:"
      ),
      .help_block("Show legend: on \u2192 right"),
      .help_block("Show legend: off \u2192 no key, full-width plot"),
      .help_p(
        "Turning the legend off removes the key entirely; moving it to the",
        "bottom is common when the arm labels are long and would crowd a",
        "right-hand key."
      )
    )
  },
  options_options = function() {
    shiny::tagList(
      .help_h("Options \u2014 the remaining per-output switches"),
      .help_p(
        "Generator-specific switches that do not belong to a more specific",
        "group. For an occurrence (adverse-event) table the most important",
        "is the 'any event' summary row and its label."
      ),
      .help_p(
        "These fine-tune what the table reports. The any-event row adds a",
        "single line counting subjects with at least one event of any kind",
        "\u2014 a standard top-line safety number \u2014 and you can rename",
        "it:"
      ),
      .help_block("Any-event row on \u2192 'Subjects with any TEAE'"),
      .help_p(
        "Turning the row on adds that summary line above the body of the",
        "table; the label editor only appears while the row is on, since a",
        "label for a row the table omits would be dead UI."
      )
    )
  },
  # ==== Inspector: Options table-layout groups ============================
  options_columns = function() {
    shiny::tagList(
      .help_h("Columns \u2014 the stub, the total, the block spacing"),
      .help_p(
        "How the left-hand stub column and the arm columns are laid out: the",
        "stub column's header text, whether to print a blank row between",
        "row-blocks, and whether to add a Total column pooling all arms."
      ),
      .help_p(
        "The stub is the leftmost column that names each row; a clear stub",
        "header and a Total column are common submission conventions:"
      ),
      .help_block("Stub header: Parameter"),
      .help_block("Total column on \u2192 an 'All Patients' column of N (%)"),
      .help_p(
        "The blank row between blocks adds vertical breathing room so a long",
        "table reads in groups. The Total column pools counts across every",
        "arm \u2014 it is a heavy change, because the engine has to",
        "re-collect the pooled numbers, so it takes effect on the next Run",
        "rather than live."
      )
    )
  },
  options_page = function() {
    shiny::tagList(
      .help_h("Page & output \u2014 how the table fills the width"),
      .help_p(
        "How this table uses the available page width. The width mode decides",
        "whether the columns stretch to fill the page or keep their natural",
        "widths."
      ),
      .help_p(
        "This is per-output because a wide many-arm table and a narrow",
        "two-arm table want different treatment on the same paper. The",
        "overall page geometry \u2014 orientation, paper size, margins,",
        "font \u2014 is set once in Setup > Page & Style, not here:"
      ),
      .help_block("Width: fit page \u2192 columns stretch edge to edge"),
      .help_block(
        "Width: natural \u2192 columns only as wide as their content"
      ),
      .help_p(
        "Fit-page spreads a small table across the full text width;",
        "natural width keeps a narrow table compact and left-aligned rather",
        "than stretching a two-column table across a landscape page."
      )
    )
  },
  options_spans = function() {
    shiny::tagList(
      .help_h("Spanning header \u2014 the band over the arm columns"),
      .help_p(
        "The banner that spans across a group of arm columns, printed as a",
        "row above the individual column headings. Each band names a set of",
        "arm columns it sits over."
      ),
      .help_p(
        "Spanning headers group related arms so a reviewer reads the",
        "column structure at a glance \u2014 for example a shared banner",
        "over two active-dose columns, separate from placebo:"
      ),
      .help_block("Band 'Xanomeline' over: Low Dose, High Dose"),
      .help_p(
        "With no band defined, the engine prints one default 'Treatment",
        "Group' banner over every arm. Each band claims its columns",
        "exclusively, so you cannot accidentally place two banners over the",
        "same column. Add a treatment variable in Roles first \u2014 the",
        "bands are built from its arms."
      )
    )
  },
  options_pageby = function() {
    shiny::tagList(
      .help_h("Subgroup / page by \u2014 one table per level"),
      .help_p(
        "Splits the output into one table per level of a chosen column: a",
        "separate page for each sex, each region, each subgroup. A banner",
        "line labels which level each page shows, and Panels lets several",
        "arm-column groups sit side by side."
      ),
      .help_p(
        "Page-by is how a subgroup analysis is presented \u2014 the same",
        "table repeated for each subgroup rather than one crowded table. The",
        "banner labels each split using a token, the page-by column's name",
        "in curly braces:"
      ),
      .help_block("Page by SEX \u2192 one table for M, one for F"),
      .help_block("Banner: Sex: {SEX}  \u2192  'Sex: F'"),
      .help_p(
        "Leaving the banner blank lets the engine label the pages",
        "automatically. Panels groups the arm columns into side-by-side",
        "blocks, useful when many arms would otherwise run off the page",
        "width."
      )
    )
  },
  # ==== Inspector: listing editors ========================================
  listing_sort = function() {
    shiny::tagList(
      .help_h("Sort \u2014 the order rows appear in the listing"),
      .help_p(
        "The sort keys that order a data listing's rows, applied top to",
        "bottom: the first key sorts, the next breaks ties within it, and so",
        "on. Each key can run ascending or descending."
      ),
      .help_p(
        "A listing is raw records, so a sensible sort is what makes it",
        "readable \u2014 usually by subject, then by visit or date within",
        "each subject:"
      ),
      .help_block("USUBJID (asc), then AESTDTC (asc)"),
      .help_p(
        "A sort key may be any column in the dataset, whether or not it is",
        "displayed \u2014 the engine's ORDER BY does not need the column on",
        "the page, so you can sort by a date variable you have chosen not to",
        "show while displaying only its formatted version."
      )
    )
  },
  listing_transpose = function() {
    shiny::tagList(
      .help_h("Transpose \u2014 spread a parameter across columns"),
      .help_p(
        "Turns rows of a parameter/value pair into columns: one column per",
        "level of a chosen parameter, filled with the matching value. You",
        "pick the parameter column, the value column, and how to handle",
        "duplicate rows."
      ),
      .help_p(
        "Lab and vitals data arrive tall \u2014 one row per test per visit",
        "\u2014 but a listing usually reads better wide, one column per",
        "test. Transpose reshapes tall to wide:"
      ),
      .help_block("Parameter PARAMCD, Value AVAL"),
      .help_block("\u2192 columns ALB, ALT, AST filled from AVAL"),
      .help_p(
        "It needs both a parameter and a value column to do anything, and it",
        "can only spread the variables already selected for display. When",
        "two rows land in the same cell, the on-duplicates choice decides",
        "which value wins (for example, keep the first)."
      )
    )
  },
  listing_formats = function() {
    shiny::tagList(
      .help_h("Date formats \u2014 how dates and times print"),
      .help_p(
        "The display format for each date or time column in the listing.",
        "ADaM stores dates as ISO text (2023-04-17); a submission listing",
        "usually prints them in a clinical format instead."
      ),
      .help_p(
        "You can write the format three ways \u2014 a SAS format name, a",
        "picture pattern, or a raw strftime string \u2014 and the engine",
        "resolves whichever you use:"
      ),
      .help_block("date9.        \u2192  17APR2023"),
      .help_block("mm/dd/yyyy    \u2192  04/17/2023"),
      .help_block("%d%b%Y        \u2192  17Apr2023"),
      .help_p(
        "Pattern tokens are yyyy yy mon mm dd hh mi ss. Leaving a column's",
        "format blank prints it exactly as stored, so only the columns you",
        "give a format are reformatted."
      )
    )
  },
  listing_stack = function() {
    shiny::tagList(
      .help_h("Stacked columns \u2014 combine values in one cell"),
      .help_p(
        "Glues several columns into a single listing column, stacking their",
        "values on separate lines inside one cell. You choose the columns",
        "and, optionally, a delimiter, prefix, or suffix that joins them."
      ),
      .help_p(
        "Stacking keeps a wide listing from running off the page: related",
        "fields that a reader wants together \u2014 a term and its verbatim,",
        "a start and stop date \u2014 share one narrow column instead of",
        "several:"
      ),
      .help_block("AEDECOD over AETERM \u2192 coded term above verbatim"),
      .help_block("AESTDTC to AEENDTC \u2192 start and stop stacked"),
      .help_p(
        "Each stack is one printed column; its lines stack within the cell,",
        "and indenting a line steps it two more spaces so a hierarchy reads",
        "clearly. A stack glues displayed columns, never free text."
      )
    )
  }
)

library(shiny)

round_digits <- 2
convert_to_fahrenheit <- function(cent) round(cent*9/5 + 32, round_digits)
convert_to_centigrade <- function(fahr) round((fahr - 32)*5/9, round_digits)

start_cent <- 0
start_fahr <- convert_to_fahrenheit(start_cent)

ui <- fluidPage(
  p(
    "What you type into the Centigrade input is converted to Fahrenheit",
    "rounded to", round_digits, "digits and vice versa for what's typed into",
    "the Fahrenheit input. This illustrates a solution to the classic bidirectional",
    "binding problem (or circular update loop or two-way input synchronisation).",
    "For more details see the",
    a("GitHub repo", href = "https://github.com/banbh/bidirectional-shiny"),
    "."
  ),
  numericInput('cent', 'Centigrade', value = start_cent),
  numericInput('fahr', 'Fahrenheit', value = start_fahr)
)

is_different <- function(x, y) {
  # Need to deal with round-trip problems and with NAs that appear when the field is cleared
  !isTRUE(all.equal(x, y))
}

server <- function(input, output, session) {
  last_fahr <- reactiveVal(start_fahr)
  last_cent <- reactiveVal(start_cent)
  
  observe(
    if (is_different(input$cent, last_cent())) {
      f <- convert_to_fahrenheit(input$cent)
      last_fahr(f)
      updateNumericInput(session, "fahr", value = f)
    }
  ) |> 
    bindEvent(input$cent)
  
  observe(
    if (is_different(input$fahr, last_fahr())) {
      c <- convert_to_centigrade(input$fahr)
      last_cent(c)
      updateNumericInput(session, "cent", value = c)
    }
  ) |> 
    bindEvent(input$fahr)
}

shinyApp(ui, server)
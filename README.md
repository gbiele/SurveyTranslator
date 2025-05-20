# SurveyTranslator

An R package for translating survey items, instructions, and response options using LLMs, with customizable guidelines to ensure conceptual and cultural appropriateness.

An **introductory vignette** is available [here](https://htmlpreview.github.io/?https://raw.githubusercontent.com/gbiele/SurveyTranslator/refs/heads/master/vignettes/Introduction.html?token=GHSAT0AAAAAAC5YUW63V3KYNY5GOFVS7HAY2BMTCUA).

## LLM API Keys

To use the package, you need an API key from a supported provider such as Google Gemini, OpenAI, or Anthropic (Claude).

SurveyTranslator relies on the [chat](https://ellmer.tidyverse.org/reference/Chat.html) function from the [ellmer](https://ellmer.tidyverse.org/) package. This means all LLMs supported by `ellmer`—including locally installed LLMs—can be used with SurveyTranslator.

### How to Obtain API Keys:

- [Google Gemini](https://ai.google.dev/gemini-api/docs/api-key) (free as of this writing)  
- [OpenAI](https://platform.openai.com/api-keys)  
- [Claude by Anthropic](https://console.anthropic.com/dashboard)

### Setting Your API Key in RStudio

To set your API key, open your `.Renviron` file in RStudio by running:

```r
usethis::edit_r_environ()
```

Then, add your key in the following format (replace with your actual key):

```
GEMINI_API_KEY="[your api key]"
GOOGLE_API_KEY="[your api key]"
ANTHROPIC_API_KEY="[your api key]"
```

Save and restart R for the changes to take effect.

## Installation

Install from GitHub:

```r
remotes::install_github("gbiele/SurveyTranslator")
```

## Usage

```r
library(surveyTranslation)

# Basic batch translation
translated_items <- llm_translate(
  source_file  = "Instrument_items.txt",
  example_file = "Example_surveys_translation.xlsx",
  batch_size   = 15
)
```

### Custom Guidelines

```r
my_guidelines <- '
"Age-Appropriate Language: Use terminology for adolescents.",
"Preserve Psychological Meaning: Retain nuance even if rewording.",
"Translator Notes: Include interpretive comments when needed."
'
translated_items_b <- llm_translate(
  source_file  = "Instrument_items.txt",
  example_file = "Example_surveys_translation.xlsx",
  guidelines   = my_guidelines,
  batch_size   = 15
)
```

### Translating Instructions & Response Options

```r
translated_instructions <- llm_translate(
  source_file  = here::here("xdata/BCFPI_instructions.txt"),
  example_file = here::here("xdata/Sample surveys translation.xlsx"),
  example_type = "Instructions"
)

translated_scales <- llm_translate(
  source_file  = here::here("xdata/BCFPI_RespOps.txt"),
  example_file = here::here("xdata/Sample surveys translation.xlsx"),
  example_type = "Response options"
)
```

## Contributing

Contributions are welcome! Please open an issue or pull request on GitHub.

## License

MIT License. See [LICENSE](LICENSE) for details.


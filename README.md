# SurveyTranslator

An R package for translating survey items, instructions, and response options using LLMs, with customizable guidelines to ensure conceptual and cultural appropriateness.

An **introductory vignette** is available [here]([https://htmlpreview.github.io/?https://raw.githubusercontent.com/gbiele/SurveyTranslator/refs/heads/master/vignettes/Introduction.html?token=GHSAT0AAAAAAC5YUW63V3KYNY5GOFVS7HAY2BMTCUA](https://htmlpreview.github.io/?https://raw.githubusercontent.com/gbiele/SurveyTranslator/refs/heads/master/vignettes/Introduction.html).

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

items <- prep_TranslationItems(
  data = items_data,
  examples = here("zdata/Sample surveys translation.xlsx"),
  source_language = "English",
  target_language = "Norwegian",
  domain = "Youth mental health",
  batch_vars = c("Instrument", "Topic")
)

translated_items <- translate_survey(items)
```

## Contributing

Contributions are welcome! Please open an issue or pull request on GitHub.

## License

MIT License. See [LICENSE](LICENSE) for details.


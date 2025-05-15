SurveyTranslator is a barbones package to translate surveys with LLMs by using tailored instructions and example translations.

To use the package, one needs an API key for either google gemini, openai, or anthropic (claude).

How to get API Keys: 

- [Google gemini](https://ai.google.dev/gemini-api/docs/api-key) (is free as of the date of writing)
- [Openai](https://platform.openai.com/api-keys) 
- [Claude](https://console.anthropic.com/dashboard)

To add an API key in Rstudio open the .Renviron file as follows

```{r}
# if needed: install.packages(usethis)
usethis::edit_r_environ()
```
And add one or more of the following:
```
GEMINI_API_KEY="[your api key]"
GOOGLE_API_KEY="[your api key]"
ANTHROPIC_API_KEY="[your api key]"
```


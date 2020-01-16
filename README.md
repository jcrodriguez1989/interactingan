`interactingan`: Interactive Presentation Ninja
================

Create, in your `xaringan` slides, interactions with the audience.

It can also be used with any other RMarkdown document that returns html
results.

## Installation

`interactingan` is currently only available as a GitHub package.

To install it run the following from an R console:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("jcrodriguez1989/interactingan", dependencies = TRUE)
```

## Usage

### Creating a basic `interactingan` presentation

In RStudio click on *File* \> *New File* \> *R Markdown…* \> *From
Template*

Select **Interactive Ninja Presentation** and press **OK**.

This will open an `interactingan` presentation template, if you have
already configured your `rsconnect` account, then this template is ready
to get **Knit**ted.

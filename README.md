`interactingan`: Interactive Presentation Ninja
================

Have you ever tried [slido](https://www.sli.do/) or
[Mentimeter](https://www.mentimeter.com/)? So you know how useful it is having audience interactive slides for teaching or conferences. But,
you also love using your `xaringan` slides.

Now, thanks to `interactingan`, it is possible to incorporate
interactive objects into your `xaringan` presentations\!

(It can also be used with any other RMarkdown document that returns html
results)

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
already [configured your `rsconnect`
account](https://docs.rstudio.com/shinyapps.io/getting-started.html#configure-rsconnect),
then this template is ready to get **Knit**ted.

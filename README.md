# How to use this repository

## R/RStudio and Maize

This shiny app is built with R. To use this repository effectively, I would assume that you have taken [STAT 220](https://www.carleton.edu/math/courses/#stats) at Carleton, or have previous experience developing shiny apps. Carleton provides two servers ([maize](https://maize.mathcs.carleton.edu/)) for students to access Posit Workbench remotely. If you don't already have a machine that runs [RStudio](https://posit.co/download/rstudio-desktop/) locally, you can use the Maize servers to clone and update this repository. Visit the [Carleton Math and Stats Department](https://www.carleton.edu/math/resources/statistics-and-r-studio-help/) website for more help. *Note that this shiny app is memory intensive (it frequently requires over 1 GB of memory), so a local machine is recommended (but not required).*

## Cloning the Repo

We use version control to keep track of changes and eventually collaborate between different students who contribute to this project.To do this, you need to set up version control on either your local machine or maize. Here's a [tutorial](https://stat220-spring24.netlify.app/github_tutorial) on how to do it.

Once you set up version control, you'll be able to pull and push from this [GitHub repository](https://github.com/kwlyu/Event-Data-Analysis). Simply copy the [url](https://github.com/kwlyu/Event-Data-Analysis.git), and start a new project in RStudio: **File \> New Project \> Version Control \> Git**. Paste the repository URL in the `Repository URL`. For more information, here's a detailed tutorial on [how to connect RStudio to Git and GitHub](https://happygitwithr.com/rstudio-git-github#:~:text=In%20RStudio%2C%20start%20a%20new,%2Fjennybc%2Fmyrepo.git%20.).

## Making changes and commit

Now you've set up your RStudio to connect to github. You can finally start the development process. To make any changes and let GitHub remember those changes for you, you'll need to commit and push the changes to this repository. Think of this as saving, undo, and redo in any text editing software, but with code stored in GitHub. Here's a demo.

Change anything in the following chunk:

```         
git status
git add
git commit
```

Now save your changes by pressing `control + s` or `command + s`. Go to the `Git` tab in RStudio, check the staged column of the file that you just changed. In this case, it'll be `README.md`. Hit `Commit`, and it'll prompt you with a new window to enter the commit message. Briefly describe what you just did in commit message and hit `Push`. Now when you refresh the web browser of this repository, you'll see the update readme with the commit message that you sent. **Note that if you're working with a partner, or that there are multiple users committing to this repository, you'll need to *pull* before you push**. Make sure your local version is up to date before you push any changes. Otherwise, this will result in a merge conflict. If you do run into that situation, here's a tutorial on [how to resolve them](https://learning.nceas.ucsb.edu/2023-04-coreR/session_10.html).

# What's in this repository

The root folder contains several files that were used in a development report. The `Rmd` of the same name as this repo was used to generate graphs to present to the [Music Department](https://www.carleton.edu/music/). The individual data sets (`.tsv` and `.csv` files) and the `data` folder were the data sources for that. 

The folder of the most interest to developers would be the `PAC-data-analyzer` folder which contains the shinyapp itself and a duplicate of the data folder, along with other data sets. We pull from [Google Sheets](https://docs.google.com/spreadsheets/d/1a0wHpBMmUMoeKrTK23nHcYvFpQ2djmcYKmjJqEJWX1I/edit?gid=265403245#gid=265403245) as our data source, so if you ever want to go back and edit any entries, you can do so in Google Sheets and upload that as a new data set. Contact [Alexi Carlson](acarlson4@carleton.edu) for administrative access to the Google Sheet.

# Publish the website
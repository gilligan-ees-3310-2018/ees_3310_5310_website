---
title: Tools
---

# Software Tools for the Lab

## Menu {#menu}

* [Installing tools](#installing)
    * [Installing {{% R_LOGO %}}](#installing-r)
    * [Installing {{% GIT_LOGO %}}](#installing-git)
    * [Installing {{% LATEX_LOGO %}}](#installing-latex)
    * [Installing {{% RSTUDIO_LOGO %}}](#installing-rstudio)
    * [Getting a GitHUB {{% GITHUB_LOGO %}} account](#github-account)
* [Resources for learning more](#resources)
    * [{{% R_LOGO %}} and {{% RSTUDIO_LOGO %}}](#r-resources)
    * [{{% GIT_LOGO %}} and GitHUB {{% GITHUB_LOGO %}}](#git-resources)


## Installing Tools {#installing}

### Installing {{% R_LOGO %}} {#installing-r}

* Download R from <https://cran.rstudio.com/>
    * Windows: 
        * Download and install the ["base distribution" of R 3.5.1](https://cran.rstudio.com/bin/windows/base/).
    * MacOS:
        * Download and install [R version 3.5.1](https://cran.rstudio.com/bin/macosx/R-3.5.1.pkg)
    * Linux:
        * You should be able to install R from your Linux distribution's package manager:
            * `sudo apt-get install r-base r-base-dev` for Debian or Ubuntu
            * `sudo yum install R` or `sudo dnf install git` for Fedora, 
              Red Hat, and related distributions.

### Installing {{%GIT_LOGO%}} {#installing-git}

If you have a Mac or Linux you may already have git installed. Test it by 
opening up a terminal window and typing `which git`. If you get a response
like `/usr/bin/git` then it's installed. If there is no response, then you
need to install git.
  
* Windows:
    * Download and install git from <https://git-scm.com>
        * Choose the default options for the installer.
    * Optionally, you might want to also install Tortoise Git, which integrates
      git into the Windows explorer, so you can execute git commands from the 
      context menu when you right-click on files or directories in the explorer.
      You can download Tortoise Git from <https://tortoisegit.org/>
    * Introduce yourself to git (you only need to do this once).
        1. Open a "git bash" window (git will give you the option to do this 
           when it finishes installing) or you can do so from the Windows Start 
           menu, under "Git".
        2. Type the following at the terminal prompt:
        
             ```
             git config --global user.name "Your Name"
             git config --global user.email your.name@vanderbilt.edu
             ```
             
           using your real name and email.
* MacOS:
    * If git is not already installed on your computer, you can download and
      install it from <https://git-scm.com>
    * Introduce yourself to git (you only need to do this once).
        1. Open the Terminal
        2. Type the following at the terminal prompt:
        
             ```
             git config --global user.name "Your Name"
             git config --global user.email your.name@vanderbilt.edu
             ```
             
           using your real name and email.
* Linux:
    * If git is not already installed, you can install it from your 
      distribution's package manager:
            * `sudo apt-get install git` for Debian or Ubuntu
            * `sudo yum install git` or `sudo dnf install git` for Fedora, 
              Red Hat, and related distributions.
    * Introduce yourself to git (you only need to do this once).
        1. Open a terminal window (bash or whatever shell you are using)
        2. Type the following at the terminal prompt:
        
             ```
             git config --global user.name "Your Name"
             git config --global user.email your.name@vanderbilt.edu
             ```

You only need to introduce yourself to git one time after you install it.
Then it will remember who you are every time you use it.
It is important for git to knows your name and email address so it can 
keep track of who is editing files when you are working collaboratively and
so it gives you credit for the files you have authored and edited.

### Installing {{% LATEX_LOGO %}} {#installing-latex}

It is optional to install LaTeX. You will be able to do all the work for the 
labs without it, but if you do install it, it will give you the option to 
produce nicely formatted PDF (Acrobat) output from your RMarkdown files
(for lab reports, presentations, etc.).

* Windows and MacOS: Install MikTeX from <https://miktex.org>. You probably
  want to select the "private TeX installation" option ("only for me").
  The Windows installer will also ask you what paper size you prefer, and
  you probably want to choose "letter" instead of the default "A4" (for 
  European users).
* Linux: Install `texlive` from your distribution's package manager:
    * `sudo apt-get install texlive` for Debian and Ubuntu, 
    * `sudo yum install texlive` or `sudo dfm install texlive` for 
  Fedora, Red Hat, etc.

### Installing {{% RSTUDIO_LOGO %}} {#installing-rstudio}

* Go to the download page for the free desktop edition of RStudio at 
  <https://www.rstudio.com/products/rstudio/download/#download> and
  download the installer for your operating system. Windows, MacOS, 
  and the Debian, Ubuntu, Fedora, RedHat, and openSUSE editions of
  Linux are all supported.
* Run the installer. 
* After the installer finishes running, run RStudio.
    * When RStudio starts up, the lower left part of the screen should have
      a window that displays the R version, saying something like this:
      
      ```
      R version 3.5.1 (2018-07-02) -- "Feather Spray"
      Copyright (C) 2018 The R Foundation for Statistical Computing
      Platform: x86_64-w64-mingw32/x64 (64-bit)

      R is free software and comes with ABSOLUTELY NO WARRANTY.
      You are welcome to redistribute it under certain conditions.
      Type 'license()' or 'licence()' for distribution details.

      R is a collaborative project with many contributors.
      Type 'contributors()' for more information and
      'citation()' on how to cite R or R packages in publications.

      Type 'demo()' for some demos, 'help()' for on-line help, or
      'help.start()' for an HTML browser interface to help.
      Type 'q()' to quit R.
      ```
      The details will be different depending on your operating system, but
      if you see something like this, RStudio correctly found R on your 
      computer.
    * Open the "Tools" menu,   and click on the "Global Options" choice.
        * Go to the "Git/SVN" tab and click "enable version control interface 
          for RStudio projects". If RStudio can find the git program on your
          computer, it will appear in the "git executable" field. If RStudio
          can't find it, you can help it by browsing to the git program.
        * If you have installed LaTeX on your computer (remember that this is
          optional), click on the SWeave tab, and select "knitr" for weaving
          `.Rnw` files, and choose `pdfLaTeX` for typesetting LaTeX files into
          PDF.
  



#### Getting a GitHUB {{% GITHUB_LOGO %}} account {#github-account}

* Go to <https://github.com> and register for a free account
* After you have set up your account, go to <https://education.github.com/students> and register your account for the free extras you can get as a student.
* Send an email to Prof. Gilligan and {{% TA_FORMAL_NAME %}} to let us know your GitHUB account name.
  You can send the email from 
  [this link](mailto:{{% PROF_EMAIL_ADDRESS %}},{{% TA_EMAIL_ADDRESS %}}?subject=EES 3310 Github Account Name)

## Resources for Learning More {#resources}

### {{% R_LOGO %}} and {{% RSTUDIO_LOGO %}} Resources {#r-resources}

* Our principal resource will be the book, 
  _[R for Data Science](https://r4ds.had.co.nz/)_. You can buy
  a printed copy or use the free web version at <https://r4ds.had.co.nz/>
* RStudio also has very useful "Cheat Sheets" that you can access from the
  help menu. These are two-page PDF files that explain the basics of things
  you may want to do with R:
      * Manipulating tibbles and data frames with `dplyr`
      * Visualizing data (making graphs and charts) with `ggplot2`
      * Manipulating lists and vectors with `purrr`
      * Using RMarkdown
      * There is also a cheatsheet for the RStudio IDE (Integrated Development
        Environment), which explains how to do things with RStudio, with a list
        of keyboard shortcuts for many common tasks.
      * There are several additional cheatsheets that aren't listed on the 
        Help menu, but you can see them if you click on 
        "[Browse Cheatsheets...](https://www.rstudio.com/resources/cheatsheets/)"
        at the bottom of the Cheatsheet menu or visit 
        <https://www.rstudio.com/resources/cheatsheets/>

### {{% GIT_LOGO %}} and GitHUB {{% GITHUB_LOGO %}} Resources {#git-resources}

* There is a lot of free documentation about git at the 
  [git-scm](https://git-scm.com) website, including a full 
  [Git reference manual](https://git-scm.com/docs) and
  a free online book, _[Pro Git](https://git-scm.com/book)_
* Professor Jenny Bryan, a professor of statistics at the University of British
  Columbia, has written a lot of helpful tutorial material specifically about
  using git and GitHub with RStudio at 
  [Happy Git and GitHub for the useR](http://happygitwithr.com/).
  
  Professor Bryan has also posted a detailed 
  [video tutorial](https://resources.rstudio.com/wistia-rstudio-conf-2017/happy-git-and-gihub-for-the-user-tutorial-jenny-bryan)
  at the 
  [RStudio Webinars and Videos page](https://resources.rstudio.com/webinars). 
  This tutorial walks you through all the steps of setting up git with RStudio
  and how to use it to keep track of your edits and revisions, and synchronize
  your work with GitHub (this serves three functions: backing up your data to 
  the cloud, sharing your data with other people, and collaborating on writing
  code or documents with other people).

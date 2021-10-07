# PlotXpress
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-3-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->

<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/shiny)](https://CRAN.R-project.org/package=shiny)
[![R build status](https://github.com/rstudio/shiny/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/shiny/actions)
[![RStudio community](https://img.shields.io/badge/community-shiny-blue?style=social&logo=rstudio&logoColor=75AADB)](https://community.rstudio.com/new-topic?category=shiny&tags=shiny)

<!-- badges: end -->

 
PlotXpress is a shiny app that simplifies the analysis and plotting of data from a dual luciferase experiment in 96-well format.

## Input data (expressed as arbitrary luminescence units)
* Excel sheet containing 2 tables in 96-well lay-out (raw output from a Promega GloMax® Navigator). An unprocessed example output file is included: `DualLuc_example_data.xlsx`
* An excel file or CSV file in which conditions are indicated in a 96-well lay-out. The conditions are seperated by underscores (see the example design that is included in this repo: `Design_example.xlsx`)

### Example output

Standard output generated with the example data:

![alt text](https://github.com/ScienceParkStudyGroup/PlotXpress/blob/master/plotXpress_example.png "Output")

### Credits

<p>The plotXpress app is created as a team effort by Elias Brandorff, Marc Galland & Joachim Goedhart</p>

### Contact

Questions related to the shiny app can be addressed to: Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart))

### License
The Apache-2.0 License (see the file LICENSE for full text) applies to the software and the CC-BY-4 license (see the file LICENSE-data for full text) applies to the datafiles (DualLuc_example_data.xlsx & Tidy_design.csv)

## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://huygens.science.uva.nl/"><img src="https://avatars.githubusercontent.com/u/39348589?v=4?s=100" width="100px;" alt=""/><br /><sub><b>Joachim Goedhart</b></sub></a><br /><a href="https://github.com/ScienceParkStudyGroup/PlotXpress/commits?author=JoachimGoedhart" title="Code">💻</a> <a href="https://github.com/ScienceParkStudyGroup/PlotXpress/issues?q=author%3AJoachimGoedhart" title="Bug reports">🐛</a> <a href="#example-JoachimGoedhart" title="Examples">💡</a> <a href="#tool-JoachimGoedhart" title="Tools">🔧</a> <a href="https://github.com/ScienceParkStudyGroup/PlotXpress/commits?author=JoachimGoedhart" title="Tests">⚠️</a></td>
    <td align="center"><a href="https://github.com/ebrando"><img src="https://avatars.githubusercontent.com/u/52273820?v=4?s=100" width="100px;" alt=""/><br /><sub><b>ebrando</b></sub></a><br /><a href="#data-ebrando" title="Data">🔣</a> <a href="#example-ebrando" title="Examples">💡</a> <a href="#ideas-ebrando" title="Ideas, Planning, & Feedback">🤔</a> <a href="#projectManagement-ebrando" title="Project Management">📆</a> <a href="#userTesting-ebrando" title="User Testing">📓</a></td>
    <td align="center"><a href="http://www.mgalland.info"><img src="https://avatars.githubusercontent.com/u/10114186?v=4?s=100" width="100px;" alt=""/><br /><sub><b>Marc Galland</b></sub></a><br /><a href="#ideas-mgalland" title="Ideas, Planning, & Feedback">🤔</a> <a href="#projectManagement-mgalland" title="Project Management">📆</a> <a href="https://github.com/ScienceParkStudyGroup/PlotXpress/commits?author=mgalland" title="Tests">⚠️</a></td>
  </tr>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!

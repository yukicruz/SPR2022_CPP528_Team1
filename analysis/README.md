Analysis
This directory stores the source code that is used to migrate your labs as chapters that do onto be displayed on each website. Each .rmd file that lives within this analysis/ directory serves as a direct input into the eventual .md files that do onto live withing the _post/ directory: that folder that stores the chapters hosted on your website.
Requirements
This is where you will store the .rd files that make up your "Chapters" that will eventually live within the _post/ directory as .md files.
Each .rmd file is a chapter and needs to be saved like this YYY-MM-DD-chXX-short_name.rmd where:
-YY is a 4 digit year (i.e. 2021)
-MM is a two digit month (i.e. 03 for March)
-DD is a two digit day (i.e. 14 for the 14th day)
-chXX references the chapter number (i.e. ch01 for the first chapter)
-short_name is a placeholder and needs to replace with something meaningful (i.e. descriptive statistics)
The title that is displayed back to the user is what you store within the title: section in your YAML header withing the .md file.

YAML header must include a gfm variant of markdown output
Each .rdm file within analysis/ must have YAML header that contains an explicit reference to the GitHub flavored markdown (gfm) variant of markdown as the output that the .rdm knits out. Said differently, rather than only knitting our an HTML, you need to have you .rdm file also knit out an .md file.
Upon closer inspection of the YAML header withing  2022-05-03-ch1-example_page.rdm reveals that the file contains two outputs: an HTML document (courtesy of rmdformats::downcute()) and md_document.


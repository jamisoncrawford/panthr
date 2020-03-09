# Package panthr: Streamlining GSU Data Processing in R

Package `panthr` was created with the express purpose of fluidly streamlining 
common albeit tedious data cleaning and manipulation tasks with field and value 
formats in the [Georgia State University](https://www.gsu.edu/) data warehouse.

Package `panthr` has no package dependencies and is coded entirely in base R.

## Functions

Package `panthr` contains over 35 functions capable of formatting:

* Variable case, such as upper-, lower-, and title case
* Exotic variable conventions, e.g. `CamelCaps`, `snake_case`, `kebab-case`, etc.
* Dates from `DD-MMM-YY` format into: 
    - International Organization for Standardization ([ISO]
    (https://en.wikipedia.org/wiki/International_Organization_for_Standardization)) format, i.e. `YYYY-MM-DD`
    - Full-length and abbreviated days, months, and years in both US and UK notation
    - Academic terms in standard `YYYYMM` format
* GPAs into directionally rounded values and discretized value ranges (e.g. "3.66-4.00")
* Academic terms from `YYYYMM` format into:
    - Dates in ISO format, i.e. `YYYY-MM-DD`
    - Full-length semester names and years
    - Academic, calendar, and fiscal years
    
In addition, `panthr` uses warehouse validation tables to convert codes into human-readable descriptions, including: 
    
* Admission Scores
* Student Attributes
* University Buildings
* University Campuses
* University Colleges
* Countries
* U.S. Counties
* U.S. States
* Admission Decisions
* Conferred Degrees
* College Departments
* Email Categories
* Ethnicity, Race, & Gender
* International Provinces

A number of these decoding functions offer the added ability to abbreviate full-length descriptions 
in order to provide more convenient and concise labeling in tables and visualizations.

Lastly, `panthr` not only decodes ethnicity and race, it formats them per conventions 
established by the Office of Institutional Effectiveness ([OIE](https://oie.gsu.edu/)), allowing:

* Individually parsing racial and ethnic descriptions
* Aggregating races and ethnicities, i.e. "Two or More Races"
* Labeling of "Missing" and "Unreported" races, ethnicities, and genders
* Ethnic precedence, where "Hispanic or Latinx" supersedes all other reported identities

## Installing panthr & Exploring Functions

You can install the `panthr` package directly from GitHub using the `devtools` package.

1. In the R console, run `install.packages("devtools")`
2. Load `devtools` by running `library(devtools)`
3. Install `panthr` with `install_github("jamisoncrawford/panthr")`
4. Load `panthr` by running `library(panthr)`

You can use RStudio's autocomplete feature to quickly scroll through available functions.
Simply type `panthr::` and peruse the scrollable tooltips and function descriptions.

![Peruse panthr functions with RStudio's autocomplete feature.](https://i.imgur.com/JqCua8H.jpg)

You can also read the full documentation for each dataset by typing the bare function name like so:

* `help(function_name)`
* `?function_name`

Documentation will then appear in the "Help" pane.

## Practice Datasets

Package `panthr` includes a sample dataset of 10,000 anonymized and "shuffled" student records: `students`. 
Invoke this dataset by running `data(students)`.

Further details on how these data were shuffled may be accessed by running `help(students)`.

This dataset features variable names and formatting conventions exactly as they 
appear when exported from Oracle SQL Developer and demonstrate how `panthr` was 
created specifically to treat those conventions.

## Development Notes

R CMD Check results currently clock at 20 seconds with 0 errors, 0 warnings, and
1 note regarding compression optimization for internal `sysdata.rda` datasets.

**2019-12-20:** Prototype package `panthr` documentation and functions first published.

**2020-01-06:** Additional decode functions added with "clean" description variables.

**2020-01-07:** Date functions added; visible `sysdata.rda` variable bindings added.

**2020-01-08:** Internal `sysdata.rda` files debugged; field functions added. `README.md` created.

## About the Author

[Jamison R. Crawford, MPA](https://www.linkedin.com/in/jamisoncrawford/) is an 
Institutional Research Associate at [The Graduate School](https://graduate.gsu.edu/) 
and [Center for the Advancement of Students and Alumni (CASA)](https://casa.gsu.edu/) 
at Georgia State University. 

He is an Associate Faculty member at [Arizona State University](https://www.asu.edu/) 
where he teaches Fundamentals of Data Science I to Master of Science candidates in
Program Evaluation & Data Analytics (PEDA) at the [Watts College of Public Service 
and Community Solutions](https://publicservice.asu.edu/) and coauthor of ["Foundations of
Data Science"](https://ds4ps.org/cpp-526-fall-2019/).

## Contributors

The Office of Institutional Effectiveness (OIE) at Georgia State has been instrumental 
in understanding warehouse data, validation tables, and institutional conventions.

Very special thanks to:

* Ben Bond, Assistant Director of Institutional Research, Georgia State University

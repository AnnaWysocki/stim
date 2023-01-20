## Test environments
* locally: R 4.2.1
* remotely: Ubuntu-latest, MacOS-latest and Windows-latest (on github actions-ci)

  
## R CMD check results

### Version 1.0.0
0 errors ✔ | 0 warnings ✔ | 1 notes ✖


* Possibly misspelled words in DESCRIPTION:
  Rhemtulla (7:268)
  Wysocki (7:256)

- Both of these words are author names. Neither are mispelled

## This is a resubmission. See below for admin comments and author response. 

## Comments from CRAN admins:

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file in the form
> authors (year) <doi:...>
> authors (year) <arXiv:...>
> authors (year, ISBN:...)
> or if those are not available: <https:...>
> with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
> auto-linking. (If you want to add a title as well please put it in
> quotes: "Title")

- I added a citation with a link to the description field in the DESCRIPTION file 

> You have examples for unexported functions. Please either omit these
> examples or export these functions.
> Examples for unexported function
>   blueprint() in:
>      blueprint.Rd
>   lavaanEq() in:
>      lavaanEq.Rd
>   modelImpliedEq() in:
>      modelImpliedEq.Rd
>   summary.stim() in:
>      symbMatrix.Rd
>   summary() in:
>      SymbolicMultiplication.Rd

- I removed the examples from all of these functions. 


> \dontrun{} should only be used if the example really cannot be executed?
> (e.g. because of missing additional software, missing API keys, ...) by
> the user. That's why wrapping examples in \dontrun{} adds the comment
> ("# Not run:") as a warning for the user. Does not seem necessary.
> Please replace \dontrun with \donttest.
> Please unwrap the examples if they are executable in < 5 sec, or replace
> dontrun{} with \donttest{}.

- Many of the \dontrun{} calls were wrapping examples that I removed because there were examples for unexported functions. 
- But I changed any remaining \dontrun{} calls to \donttest{}












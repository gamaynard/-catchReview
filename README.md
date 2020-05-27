# -catchReview
Code for analyzing electronic monitoring catch review comparisons between NOAA and EM providers

This branch relies on the NOAA FSB-generated summary files available in the review comparison workbooks.
The summary files come with major caveats, including:
These summary files may be incomplete (e.g., many reported no lengths for White Hake, despite indicating that White Hake were discarded).
Aggregation of the summary files was made difficult by a lack of standardization in species naming conventions. 
I used fuzzy matching to try to assign each species to a standardized (AFS) name, but there may be some that were assigned incorrectly or not assigned at all. 
Some summary files also included multiple rows for a single species, and it is unclear whether that was intentional on the part of the reviewers or not. 
Without knowledge of the protocol for generating the FSB summary files, it is difficult to say whether they are a reliable source of information. 
Barring development of a standardized routine for comparing FSB and TF reviews, however, these hand-generated summary files are the best information available. 

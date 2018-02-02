-   [**Mutating columns: the basics**](#mutating-columns-the-basics)
-   [**Mutating several columns at
    once**](#mutating-several-columns-at-once)
    -   [**Mutate\_all()**](#mutate_all)
    -   [**Mutate\_if()**](#mutate_if)
    -   [**Mutate\_at() to change specifc
        columns**](#mutate_at-to-change-specifc-columns)
-   [**Working with discrete columns**](#working-with-discrete-columns)
    -   [**Recoding discrete columns**](#recoding-discrete-columns)
    -   [**Creating new discrete column (two
        levels)**](#creating-new-discrete-column-two-levels)
    -   [**Creating new discrete column (multiple
        levels)**](#creating-new-discrete-column-multiple-levels)
-   [**Splitting and merging columns**](#splitting-and-merging-columns)
-   [**Bringing in columns from other data
    tables**](#bringing-in-columns-from-other-data-tables)
-   [**Spreading and gathering data**](#spreading-and-gathering-data)
-   [**Turning particular values into
    NA**](#turning-particular-values-into-na)

This is a second post in a series of dplyr functions. The first one
covered basic and advanced ways to select, rename and reorder
columns,and can be found here.

This second one covers tools to manipulate your columns to get them the
way you want them. - content

**The data**  
As per previous blog posts, many of these functions truly shine when you
have a lot of columns, but to make it easy on people to copy paste code
and experiment, I'm using a built-in dataset:

    library(tidyverse)

    glimpse(msleep)

    ## Observations: 83
    ## Variables: 11
    ## $ name         <chr> "Cheetah", "Owl monkey", "Mountain beaver", "Grea...
    ## $ genus        <chr> "Acinonyx", "Aotus", "Aplodontia", "Blarina", "Bo...
    ## $ vore         <chr> "carni", "omni", "herbi", "omni", "herbi", "herbi...
    ## $ order        <chr> "Carnivora", "Primates", "Rodentia", "Soricomorph...
    ## $ conservation <chr> "lc", NA, "nt", "lc", "domesticated", NA, "vu", N...
    ## $ sleep_total  <dbl> 12.1, 17.0, 14.4, 14.9, 4.0, 14.4, 8.7, 7.0, 10.1...
    ## $ sleep_rem    <dbl> NA, 1.8, 2.4, 2.3, 0.7, 2.2, 1.4, NA, 2.9, NA, 0....
    ## $ sleep_cycle  <dbl> NA, NA, NA, 0.1333333, 0.6666667, 0.7666667, 0.38...
    ## $ awake        <dbl> 11.9, 7.0, 9.6, 9.1, 20.0, 9.6, 15.3, 17.0, 13.9,...
    ## $ brainwt      <dbl> NA, 0.01550, NA, 0.00029, 0.42300, NA, NA, NA, 0....
    ## $ bodywt       <dbl> 50.000, 0.480, 1.350, 0.019, 600.000, 3.850, 20.4...

<br>

**Mutating columns: the basics**
--------------------------------

You can make new columns with the `mutate()` function. The options
inside mutate are almost endless: pretty much anything that you can do
to normal vectors, can be done inside a `mutate()` function.  
Anything inside `mutate` can either be a new column (by giving mutate a
new column name), or can replace the current column (by keeping the same
column name).

One of the simplest options is a calculation based on values in other
columns. In the sample code, we're changing the sleep data from data
measured in hours to minutes.

    msleep %>%
      select(name, sleep_total) %>%
      mutate(sleep_total_min = sleep_total * 60)

    ## # A tibble: 83 x 3
    ##    name                       sleep_total sleep_total_min
    ##    <chr>                            <dbl>           <dbl>
    ##  1 Cheetah                          12.1              726
    ##  2 Owl monkey                       17.0             1020
    ##  3 Mountain beaver                  14.4              864
    ##  4 Greater short-tailed shrew       14.9              894
    ##  5 Cow                               4.00             240
    ##  6 Three-toed sloth                 14.4              864
    ##  7 Northern fur seal                 8.70             522
    ##  8 Vesper mouse                      7.00             420
    ##  9 Dog                              10.1              606
    ## 10 Roe deer                          3.00             180
    ## # ... with 73 more rows

New columns can be made with aggregate functions such as average,
median, max, min, sd, ...  
The sample code makes two new columns: one showing the difference of
each row versus the average sleep time, and one showing the difference
versus the animal with the least sleep.

    msleep %>%
      select(name, sleep_total) %>%
      mutate(sleep_total_vs_AVG = sleep_total - round(mean(sleep_total), 1),
             sleep_total_vs_MIN = sleep_total - min(sleep_total))

    ## # A tibble: 83 x 4
    ##    name                       sleep_total sleep_total_vs_AVG sleep_total_~
    ##    <chr>                            <dbl>              <dbl>         <dbl>
    ##  1 Cheetah                          12.1               1.70          10.2 
    ##  2 Owl monkey                       17.0               6.60          15.1 
    ##  3 Mountain beaver                  14.4               4.00          12.5 
    ##  4 Greater short-tailed shrew       14.9               4.50          13.0 
    ##  5 Cow                               4.00             -6.40           2.10
    ##  6 Three-toed sloth                 14.4               4.00          12.5 
    ##  7 Northern fur seal                 8.70             -1.70           6.80
    ##  8 Vesper mouse                      7.00             -3.40           5.10
    ##  9 Dog                              10.1              -0.300          8.20
    ## 10 Roe deer                          3.00             -7.40           1.10
    ## # ... with 73 more rows

The `ifelse()` function deserves a special mention because it is
particularly useful if you don't want to mutate the whole column in the
same way. With `ifelse()`, you first specify a logical statement,
afterwards what needs to happen if the statement returns `TRUE`, and
lastly what needs to happen if it's `FALSE`.

Imagine that we have a database with two large values which we assume
are typos or measurement errors, and we want to exclude them. The below
code will take any `brainwt` value above 4 and return NA. In this case,
the code won't change for anything below 4.

    msleep %>%
      mutate(brainwt2 = ifelse(brainwt > 4, NA, brainwt)) %>%
      select(starts_with("brain")) %>%
      arrange(desc(brainwt))

    ## # A tibble: 83 x 2
    ##    brainwt brainwt2
    ##      <dbl>    <dbl>
    ##  1   5.71    NA    
    ##  2   4.60    NA    
    ##  3   1.32     1.32 
    ##  4   0.655    0.655
    ##  5   0.440    0.440
    ##  6   0.423    0.423
    ##  7   0.419    0.419
    ##  8   0.325    0.325
    ##  9   0.180    0.180
    ## 10   0.180    0.180
    ## # ... with 73 more rows

You can also mutate string columns with stringr's `str_extract()`
function in combination with any character or regex patterns.  
The sample code will return the last word of the animal name and makes
it lower case.

    msleep %>%
      mutate(name_last_word = tolower(str_extract(name, pattern = "\\w+$"))) %>%
      select(name, name_last_word)

    ## # A tibble: 83 x 2
    ##    name                       name_last_word
    ##    <chr>                      <chr>         
    ##  1 Cheetah                    cheetah       
    ##  2 Owl monkey                 monkey        
    ##  3 Mountain beaver            beaver        
    ##  4 Greater short-tailed shrew shrew         
    ##  5 Cow                        cow           
    ##  6 Three-toed sloth           sloth         
    ##  7 Northern fur seal          seal          
    ##  8 Vesper mouse               mouse         
    ##  9 Dog                        dog           
    ## 10 Roe deer                   deer          
    ## # ... with 73 more rows

<br>

**Mutating several columns at once**
------------------------------------

This is where the magic really happens. Just like with the `select()`
functions in part 1, there are variants to `mutate()`: `mutate_all()`,
`mutate_if()`, `mutate_at()`.

### **Mutate\_all()**

The `mutate_all()` version is the easiest to understand, and pretty
nifty when cleaning your data.  
Something easy to start with: turning all the data to lower case:

    msleep %>%
      mutate_all(tolower)

    ## # A tibble: 83 x 11
    ##    name     genus  vore  order  conse~ slee~ slee~ slee~ awake brai~ body~
    ##    <chr>    <chr>  <chr> <chr>  <chr>  <chr> <chr> <chr> <chr> <chr> <chr>
    ##  1 cheetah  acino~ carni carni~ lc     12.1  <NA>  <NA>  11.9  <NA>  50   
    ##  2 owl mon~ aotus  omni  prima~ <NA>   17    1.8   <NA>  7     0.01~ 0.48 
    ##  3 mountai~ aplod~ herbi roden~ nt     14.4  2.4   <NA>  9.6   <NA>  1.35 
    ##  4 greater~ blari~ omni  soric~ lc     14.9  2.3   0.13~ 9.1   0.00~ 0.019
    ##  5 cow      bos    herbi artio~ domes~ 4     0.7   0.66~ 20    0.423 600  
    ##  6 three-t~ brady~ herbi pilosa <NA>   14.4  2.2   0.76~ 9.6   <NA>  3.85 
    ##  7 norther~ callo~ carni carni~ vu     8.7   1.4   0.38~ 15.3  <NA>  20.49
    ##  8 vesper ~ calom~ <NA>  roden~ <NA>   7     <NA>  <NA>  17    <NA>  0.045
    ##  9 dog      canis  carni carni~ domes~ 10.1  2.9   0.33~ 13.9  0.07  14   
    ## 10 roe deer capre~ herbi artio~ lc     3     <NA>  <NA>  21    0.09~ 14.8 
    ## # ... with 73 more rows

But I've also used it to trim whitespaces into an entire table without
issues using: `mutate_all(str_trim)`.  
Or when scraping the web, I've often had plenty off empty spaces and
many `\n` signs across the data.

I'm first going to use `mutate_all()` to screw things up:

    msleep_ohno <- msleep %>%
      mutate_all(~paste(., "  /n  "))

    msleep_ohno[,1:4]

    ## # A tibble: 83 x 4
    ##    name                                genus                vore    order 
    ##    <chr>                               <chr>                <chr>   <chr> 
    ##  1 "Cheetah   /n  "                    "Acinonyx   /n  "    "carni~ "Carn~
    ##  2 "Owl monkey   /n  "                 "Aotus   /n  "       "omni ~ "Prim~
    ##  3 "Mountain beaver   /n  "            "Aplodontia   /n  "  "herbi~ "Rode~
    ##  4 "Greater short-tailed shrew   /n  " "Blarina   /n  "     "omni ~ "Sori~
    ##  5 "Cow   /n  "                        "Bos   /n  "         "herbi~ "Arti~
    ##  6 "Three-toed sloth   /n  "           "Bradypus   /n  "    "herbi~ "Pilo~
    ##  7 "Northern fur seal   /n  "          "Callorhinus   /n  " "carni~ "Carn~
    ##  8 "Vesper mouse   /n  "               "Calomys   /n  "     "NA   ~ "Rode~
    ##  9 "Dog   /n  "                        "Canis   /n  "       "carni~ "Carn~
    ## 10 "Roe deer   /n  "                   "Capreolus   /n  "   "herbi~ "Arti~
    ## # ... with 73 more rows

Let's clean it up again:  
In this code I am assume that not all values show the same amount of
extra white spaces as is often the case with parsed data: it frst
removes all extra white spaces and then removes any `/n`.

    msleep_ohno %>%
      mutate_all(str_trim) %>%
      mutate_all(~str_replace_all(., "/n", "")) %>%
      select(1:4)

    ## # A tibble: 83 x 4
    ##    name                            genus            vore       order      
    ##    <chr>                           <chr>            <chr>      <chr>      
    ##  1 "Cheetah   "                    "Acinonyx   "    "carni   " "Carnivora~
    ##  2 "Owl monkey   "                 "Aotus   "       "omni   "  "Primates ~
    ##  3 "Mountain beaver   "            "Aplodontia   "  "herbi   " "Rodentia ~
    ##  4 "Greater short-tailed shrew   " "Blarina   "     "omni   "  "Soricomor~
    ##  5 "Cow   "                        "Bos   "         "herbi   " "Artiodact~
    ##  6 "Three-toed sloth   "           "Bradypus   "    "herbi   " "Pilosa   "
    ##  7 "Northern fur seal   "          "Callorhinus   " "carni   " "Carnivora~
    ##  8 "Vesper mouse   "               "Calomys   "     "NA   "    "Rodentia ~
    ##  9 "Dog   "                        "Canis   "       "carni   " "Carnivora~
    ## 10 "Roe deer   "                   "Capreolus   "   "herbi   " "Artiodact~
    ## # ... with 73 more rows

Obviously you can use any regex inside `mutate_all()` as well. The
sample code will remove any vowels:

    msleep %>%
      mutate_all(~str_replace_all(., "[aeiou]", ""))

    ## # A tibble: 83 x 11
    ##    name    genus  vore  order  conse~ sleep~ slee~ slee~ awake brai~ body~
    ##    <chr>   <chr>  <chr> <chr>  <chr>  <chr>  <chr> <chr> <chr> <chr> <chr>
    ##  1 Chth    Acnnyx crn   Crnvr  lc     12.1   <NA>  <NA>  11.9  <NA>  50   
    ##  2 Owl mn~ Ats    mn    Prmts  <NA>   17     1.8   <NA>  7     0.01~ 0.48 
    ##  3 Mntn b~ Apldnt hrb   Rdnt   nt     14.4   2.4   <NA>  9.6   <NA>  1.35 
    ##  4 Grtr s~ Blrn   mn    Srcmr~ lc     14.9   2.3   0.13~ 9.1   0.00~ 0.019
    ##  5 Cw      Bs     hrb   Artdc~ dmstc~ 4      0.7   0.66~ 20    0.423 600  
    ##  6 Thr-td~ Brdyps hrb   Pls    <NA>   14.4   2.2   0.76~ 9.6   <NA>  3.85 
    ##  7 Nrthrn~ Cllrh~ crn   Crnvr  v      8.7    1.4   0.38~ 15.3  <NA>  20.49
    ##  8 Vspr ms Clmys  <NA>  Rdnt   <NA>   7      <NA>  <NA>  17    <NA>  0.045
    ##  9 Dg      Cns    crn   Crnvr  dmstc~ 10.1   2.9   0.33~ 13.9  0.07  14   
    ## 10 R dr    Cprls  hrb   Artdc~ lc     3      <NA>  <NA>  21    0.09~ 14.8 
    ## # ... with 73 more rows

### **Mutate\_if()**

Not all cleaning functions can be done with `mutate_all()`. Trying to
round your data will lead to an error if you have both numerical and
character columns.

    msleep %>%
      mutate_all(round)

`Error in mutate_impl(.data, dots) : Evaluation error: non-numeric argument to mathematical function.`

In these cases we have to add the condition that columns need to be
numeric before giving `round()` instructions:  
To note: if you want to round to a few digits you will need to make a
function on the fly by using a tilde: such as
`mutate_if(is.numeric, ~round(. ,2))`.

    msleep %>%
      mutate_if(is.numeric, round) %>%
      select(name, sleep_total:bodywt)

    ## # A tibble: 83 x 7
    ##    name                       sleep_total sleep~ sleep~ awake brai~ bodywt
    ##    <chr>                            <dbl>  <dbl>  <dbl> <dbl> <dbl>  <dbl>
    ##  1 Cheetah                          12.0   NA     NA    12.0     NA  50.0 
    ##  2 Owl monkey                       17.0    2.00  NA     7.00     0   0   
    ##  3 Mountain beaver                  14.0    2.00  NA    10.0     NA   1.00
    ##  4 Greater short-tailed shrew       15.0    2.00   0     9.00     0   0   
    ##  5 Cow                               4.00   1.00   1.00 20.0      0 600   
    ##  6 Three-toed sloth                 14.0    2.00   1.00 10.0     NA   4.00
    ##  7 Northern fur seal                 9.00   1.00   0    15.0     NA  20.0 
    ##  8 Vesper mouse                      7.00  NA     NA    17.0     NA   0   
    ##  9 Dog                              10.0    3.00   0    14.0      0  14.0 
    ## 10 Roe deer                          3.00  NA     NA    21.0      0  15.0 
    ## # ... with 73 more rows

### **Mutate\_at() to change specifc columns**

All columns containing the word sleep are in hours. If I want those in
minutes, I can use `mutate_at()`. To indicate that I want the condition
to be based on the column name I can use `vars(contains("sleep"))`.
Inside `vars()` you can use any of the `select()`helpers.  
The second argument of `mutate_at()` needs to be function, so I have to
wrap my calculation instructions with a tilde to make a function on the
fly.

    msleep %>%
      mutate_at(vars(contains("sleep")), ~(.*60)) %>%
      select(sleep_total:bodywt)

    ## # A tibble: 83 x 6
    ##    sleep_total sleep_rem sleep_cycle awake   brainwt   bodywt
    ##          <dbl>     <dbl>       <dbl> <dbl>     <dbl>    <dbl>
    ##  1         726      NA         NA    11.9  NA         50.0   
    ##  2        1020     108         NA     7.00  0.0155     0.480 
    ##  3         864     144         NA     9.60 NA          1.35  
    ##  4         894     138          8.00  9.10  0.000290   0.0190
    ##  5         240      42.0       40.0  20.0   0.423    600     
    ##  6         864     132         46.0   9.60 NA          3.85  
    ##  7         522      84.0       23.0  15.3  NA         20.5   
    ##  8         420      NA         NA    17.0  NA          0.0450
    ##  9         606     174         20.0  13.9   0.0700    14.0   
    ## 10         180      NA         NA    21.0   0.0982    14.8   
    ## # ... with 73 more rows

<br>

**Working with discrete columns**
---------------------------------

### **Recoding discrete columns**

To rename or reorganize current discrete columns, you can use `recode()`
inside a `mutate()` statement: this enables you to change the current
naming, or to group current levels into less levels. The `.default`
refers to anything that isn't covered by the before groups with the
exception of NA. You can change NA into something other than NA by
adding a `.missing` argument if you want (see next sample code).

    msleep %>%
      mutate(conservation2 = recode(conservation,
                            "en" = "Endangered",
                            "lc" = "Least_Concern",
                            "domesticated" = "Least_Concern",
                            .default = "other")) %>%
      count(conservation2)

    ## # A tibble: 4 x 2
    ##   conservation2     n
    ##   <chr>         <int>
    ## 1 Endangered        4
    ## 2 Least_Concern    37
    ## 3 other            13
    ## 4 <NA>             29

A special version exists to return a factor: `recode_factor()`. By
default the `.ordered` argument is `FALSE`. To return an ordered factor
set the argument to `TRUE`:

    msleep %>%
      mutate(conservation2 = recode_factor(conservation,
                            "en" = "Endangered",
                            "lc" = "Least_Concern",
                            "domesticated" = "Least_Concern",
                            .default = "other",
                            .missing = "no data",
                            .ordered = TRUE)) %>%
      count(conservation2)

    ## # A tibble: 4 x 2
    ##   conservation2     n
    ##   <ord>         <int>
    ## 1 Endangered        4
    ## 2 Least_Concern    37
    ## 3 other            13
    ## 4 no data          29

<br>

### **Creating new discrete column (two levels)**

The `ifelse()` statement can be used to turn a numeric column into a
discrete one. As mentioned above, `ifelse()` takes a logical expression,
then what to do if the expression returns `TRUE` and lastly what to do
when it returns `FALSE`.  
The sample code will divide the current measure `sleep_total` into a
discrete "long" or "short" sleeper.

    msleep %>%
      mutate(sleep_time = ifelse(sleep_total > 10, "long", "short")) %>%
      select(name, contains("sleep"))

    ## # A tibble: 83 x 5
    ##    name                       sleep_total sleep_rem sleep_cycle sleep_time
    ##    <chr>                            <dbl>     <dbl>       <dbl> <chr>     
    ##  1 Cheetah                          12.1     NA          NA     long      
    ##  2 Owl monkey                       17.0      1.80       NA     long      
    ##  3 Mountain beaver                  14.4      2.40       NA     long      
    ##  4 Greater short-tailed shrew       14.9      2.30        0.133 long      
    ##  5 Cow                               4.00     0.700       0.667 short     
    ##  6 Three-toed sloth                 14.4      2.20        0.767 long      
    ##  7 Northern fur seal                 8.70     1.40        0.383 short     
    ##  8 Vesper mouse                      7.00    NA          NA     short     
    ##  9 Dog                              10.1      2.90        0.333 long      
    ## 10 Roe deer                          3.00    NA          NA     short     
    ## # ... with 73 more rows

<br>

### **Creating new discrete column (multiple levels)**

The `ifelse()` can be nested but if you want more than two levels, but
it might be even easier to use `case_when()` which allows as many
statements as you like and is easier to read than many nested `ifelse`
statements.  
The arguments are evaluated in order, so only the rows where the first
statement is not true will continue to be evaluated for the next
statement. For everything that is left at the end just use the
`TRUE ~ "newname"`.  
Unfortunately there seems to be no easy way to get `case_when()` to
return an ordered factor, so you will need to to do that yourself
afterwards, either by using `forcats::fct_relevel()`, or just with a
`factor()` function. If you have a lot of levels I would advice to make
a levels vector upfront to avoid cluttering the piple too much.

    msleep %>%
      mutate(sleep_total_discr = case_when(
        sleep_total > 13 ~ "very long",
        sleep_total > 10 ~ "long",
        sleep_total > 7 ~ "limited",
        TRUE ~ "short")) %>%
      mutate(sleep_total_discr = factor(sleep_total_discr, 
                                        levels = c("short", "limited", 
                                                   "long", "very long"))) %>%
      count(sleep_total_discr)

    ## # A tibble: 4 x 2
    ##   sleep_total_discr     n
    ##   <fctr>            <int>
    ## 1 short                20
    ## 2 limited              19
    ## 3 long                 21
    ## 4 very long            23

The `case_when()` function does not only work inside a column, but can
be used for grouping across columns:

    msleep %>%
      mutate(silly_groups = case_when(
        brainwt < 0.001 ~ "light_headed",
        sleep_total > 10 ~ "lazy_sleeper",
        is.na(sleep_rem) ~ "absent_rem",
        TRUE ~ "other")) %>%
      count(silly_groups)

    ## # A tibble: 4 x 2
    ##   silly_groups     n
    ##   <chr>        <int>
    ## 1 absent_rem       8
    ## 2 lazy_sleeper    39
    ## 3 light_headed     6
    ## 4 other           30

<br>

**Splitting and merging columns**
---------------------------------

Take for example this dataset

    (conservation_expl <- read_csv("conservation_explanation.csv"))

    ## Parsed with column specification:
    ## cols(
    ##   `conservation abbreviation` = col_character()
    ## )

    ## # A tibble: 11 x 1
    ##    `conservation abbreviation`                  
    ##    <chr>                                        
    ##  1 EX = Extinct                                 
    ##  2 EW = Extinct in the wild                     
    ##  3 CR = Critically Endangered                   
    ##  4 EN = Endangered                              
    ##  5 VU = Vulnerable                              
    ##  6 NT = Near Threatened                         
    ##  7 LC = Least Concern                           
    ##  8 DD = Data deficient                          
    ##  9 NE = Not evaluated                           
    ## 10 PE = Probably extinct (informal)             
    ## 11 PEW = Probably extinct in the wild (informal)

You can unmerge any columns by using tidyr's `separate()` function. To
do this, you have to specify the column to be splitted, followed by the
new column names, and which seperator it has to look for.  
The sample code shows seperating into two columns based on '=' as a
separator.

    (conservation_table <- conservation_expl %>%
      separate(`conservation abbreviation`, 
               into = c("abbreviation", "description"), sep = " = "))

    ## # A tibble: 11 x 2
    ##    abbreviation description                            
    ##  * <chr>        <chr>                                  
    ##  1 EX           Extinct                                
    ##  2 EW           Extinct in the wild                    
    ##  3 CR           Critically Endangered                  
    ##  4 EN           Endangered                             
    ##  5 VU           Vulnerable                             
    ##  6 NT           Near Threatened                        
    ##  7 LC           Least Concern                          
    ##  8 DD           Data deficient                         
    ##  9 NE           Not evaluated                          
    ## 10 PE           Probably extinct (informal)            
    ## 11 PEW          Probably extinct in the wild (informal)

The opposite is tidyr's `unite()` function. You specify the new column
name, and then the columns to be united, and lastly what seperator you
want to use.

    #merging currency and goal columns
    conservation_table %>%
      unite(united_col, abbreviation, description, sep=": ") %>%
      select(united_col)

    ## # A tibble: 11 x 1
    ##    united_col                                  
    ##  * <chr>                                       
    ##  1 EX: Extinct                                 
    ##  2 EW: Extinct in the wild                     
    ##  3 CR: Critically Endangered                   
    ##  4 EN: Endangered                              
    ##  5 VU: Vulnerable                              
    ##  6 NT: Near Threatened                         
    ##  7 LC: Least Concern                           
    ##  8 DD: Data deficient                          
    ##  9 NE: Not evaluated                           
    ## 10 PE: Probably extinct (informal)             
    ## 11 PEW: Probably extinct in the wild (informal)

<br>

**Bringing in columns from other data tables**
----------------------------------------------

If you want to add information from another table, you can use the
joining functions from `dplyr`. The msleep data contains abbreviations
for conservation but if you are not familiar with the topic you might
need the description we used in the section above inside the msleep
data.

Joins would be a chapter in itself, but in this particular case you
would do a `left_join()`, i.e. keeping my main table (on the left), and
adding columns from another one to the right. In the `by =` statement
you specify which colums are the same, so the join knows what to add
where.  
The sample code will add the description of the different conservation
states into our main `msleep` table. The main data contained an extra
`domisticated` label which i wanted to keep. This is done in the last
line of the table with an `ifelse()`.

    #joining column data
    msleep %>%
      select(name, conservation) %>%
      mutate(conservation = toupper(conservation)) %>%
      left_join(conservation_table, by = c("conservation" = "abbreviation")) %>%
      mutate(description = ifelse(is.na(description), conservation, description))

    ## # A tibble: 83 x 3
    ##    name                       conservation description    
    ##    <chr>                      <chr>        <chr>          
    ##  1 Cheetah                    LC           Least Concern  
    ##  2 Owl monkey                 <NA>         <NA>           
    ##  3 Mountain beaver            NT           Near Threatened
    ##  4 Greater short-tailed shrew LC           Least Concern  
    ##  5 Cow                        DOMESTICATED DOMESTICATED   
    ##  6 Three-toed sloth           <NA>         <NA>           
    ##  7 Northern fur seal          VU           Vulnerable     
    ##  8 Vesper mouse               <NA>         <NA>           
    ##  9 Dog                        DOMESTICATED DOMESTICATED   
    ## 10 Roe deer                   LC           Least Concern  
    ## # ... with 73 more rows

<br>

**Spreading and gathering data**
--------------------------------

The `gather()` function will gather up many columns into one. In this
case, we have 3 columns that describe a time measure. For some analysis
and graphs, it might be necessary to get them all into one.  
The `gather` function needs you to give a name ("key") for the new
descriptive column, and a another name ("value") for the value column.
The columns that you don't want to gather need to be deselected at the
end. In the sample code I'm deselecting the column `name`.

    msleep %>%
      select(name, contains("sleep")) %>%
      gather(key = "sleep_measure", value = "time", -name)

    ## # A tibble: 249 x 3
    ##    name                       sleep_measure  time
    ##    <chr>                      <chr>         <dbl>
    ##  1 Cheetah                    sleep_total   12.1 
    ##  2 Owl monkey                 sleep_total   17.0 
    ##  3 Mountain beaver            sleep_total   14.4 
    ##  4 Greater short-tailed shrew sleep_total   14.9 
    ##  5 Cow                        sleep_total    4.00
    ##  6 Three-toed sloth           sleep_total   14.4 
    ##  7 Northern fur seal          sleep_total    8.70
    ##  8 Vesper mouse               sleep_total    7.00
    ##  9 Dog                        sleep_total   10.1 
    ## 10 Roe deer                   sleep_total    3.00
    ## # ... with 239 more rows

A useful attribute in gathering is the `factor_key` argument which is
`FALSE`by default. In the previous example the new column
`sleep_measure` is a character vector. If you are going to summarise or
plot afterwards, that column will be ordered alphabetically.  
If you want to preserve the original order, add `factor_key = TRUE`
which will make the new column an ordered factor.

    (msleep_g <- msleep %>%
      select(name, contains("sleep")) %>%
      gather(key = "sleep_measure", value = "time", -name, factor_key = TRUE))

    ## # A tibble: 249 x 3
    ##    name                       sleep_measure  time
    ##    <chr>                      <fctr>        <dbl>
    ##  1 Cheetah                    sleep_total   12.1 
    ##  2 Owl monkey                 sleep_total   17.0 
    ##  3 Mountain beaver            sleep_total   14.4 
    ##  4 Greater short-tailed shrew sleep_total   14.9 
    ##  5 Cow                        sleep_total    4.00
    ##  6 Three-toed sloth           sleep_total   14.4 
    ##  7 Northern fur seal          sleep_total    8.70
    ##  8 Vesper mouse               sleep_total    7.00
    ##  9 Dog                        sleep_total   10.1 
    ## 10 Roe deer                   sleep_total    3.00
    ## # ... with 239 more rows

The opposite of gathering is spreading. Spread will take one column and
make multiple columns out of it. If you would have started with the
previous column, you could get the differrent sleep measures in
different columns:

    msleep_g %>%
      spread(sleep_measure, time)

    ## # A tibble: 83 x 4
    ##    name                      sleep_total sleep_rem sleep_cycle
    ##  * <chr>                           <dbl>     <dbl>       <dbl>
    ##  1 African elephant                 3.30     NA         NA    
    ##  2 African giant pouched rat        8.30      2.00      NA    
    ##  3 African striped mouse            8.70     NA         NA    
    ##  4 Arctic fox                      12.5      NA         NA    
    ##  5 Arctic ground squirrel          16.6      NA         NA    
    ##  6 Asian elephant                   3.90     NA         NA    
    ##  7 Baboon                           9.40      1.00       0.667
    ##  8 Big brown bat                   19.7       3.90       0.117
    ##  9 Bottle-nosed dolphin             5.20     NA         NA    
    ## 10 Brazilian tapir                  4.40      1.00       0.900
    ## # ... with 73 more rows

**Turning particular values into NA**
-------------------------------------

The function `na_if()` turns particular values into `NA`. In most cases
the command probably be `na_if("")` (i.e turn an empty string into NA),
but in principle you can do anything.

The same code will turn any value that reads "omni" into NA

    msleep %>%
      na_if("omni")

    ## # A tibble: 83 x 11
    ##    name     genus  vore  order  conse~ sleep~ sleep~ sleep~ awake  brainwt
    ##    <chr>    <chr>  <chr> <chr>  <chr>   <dbl>  <dbl>  <dbl> <dbl>    <dbl>
    ##  1 Cheetah  Acino~ carni Carni~ lc      12.1  NA     NA     11.9  NA      
    ##  2 Owl mon~ Aotus  <NA>  Prima~ <NA>    17.0   1.80  NA      7.00  1.55e-2
    ##  3 Mountai~ Aplod~ herbi Roden~ nt      14.4   2.40  NA      9.60 NA      
    ##  4 Greater~ Blari~ <NA>  Soric~ lc      14.9   2.30   0.133  9.10  2.90e-4
    ##  5 Cow      Bos    herbi Artio~ domes~   4.00  0.700  0.667 20.0   4.23e-1
    ##  6 Three-t~ Brady~ herbi Pilosa <NA>    14.4   2.20   0.767  9.60 NA      
    ##  7 Norther~ Callo~ carni Carni~ vu       8.70  1.40   0.383 15.3  NA      
    ##  8 Vesper ~ Calom~ <NA>  Roden~ <NA>     7.00 NA     NA     17.0  NA      
    ##  9 Dog      Canis  carni Carni~ domes~  10.1   2.90   0.333 13.9   7.00e-2
    ## 10 Roe deer Capre~ herbi Artio~ lc       3.00 NA     NA     21.0   9.82e-2
    ## # ... with 73 more rows, and 1 more variable: bodywt <dbl>

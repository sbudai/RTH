# RTH
This R package use the **httr** to retrieve data from Refinitiv Tick History REST APIs.    
Originally these functions were part of an educational articles    
    - [RTH in R -part 1](https://developers.refinitiv.com/en/article-catalog/article/tick-history-in-r-language-part-1)    
    - [RTH in R -part 2](https://developers.refinitiv.com/en/article-catalog/article/tick-history-in-r-language-part-2)    
    - [RTH in R -part 3](https://developers.refinitiv.com/en/article-catalog/article/using-tick-history-in-r-language-part-3)    

We encouraged you to visit the REST API Reference tree at the [REST API Help Home Page](https://selectapi.datascope.refinitiv.com/RestApi.Help/Home/Index) as a primary source of reference for all restful functions and supported conditions.

## Prerequisites:
- **devtools** package has to be a priori installed
```
install.packages( 'devtools' )
```
- a github Personal Access Token (GITHUB_PAT) should be created **[this way](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/creating-a-personal-access-token)**
- add your github personal access token to the end of .Renviron file in a new line like this: "**GITHUB_PAT="..."**  
    You can easily open **.Renviron** file on your computer this way:   
```
usethis::edit_file( path = usethis:::scoped_path_r( scope = "user", ".Renviron", envvar = "R_ENVIRON_USER" ) )  
```

- the rth_dss_username and rth_dss_password should be set in your computer's .Renviron file as well.
    If you closed your **.Renviron** file then you should reopen it.

   Add username and password in new lines this way:  
   **rth_dss_username="..."**  
   **rth_dss_password="..."**  

## How to install
You can install it into R by this command:
```
devtools::install_github( repo       = "https://github.com/sbudai/RTH",
                          auth_token = Sys.getenv( "GITHUB_PAT" ), 
                          upgrade    = "ask" )
```

## How to upgrade
You can remove the current version and install the new version into R by these commands:
```
remove.packages( pkgs = 'SFTP' )
devtools::install_github( repo       = "https://github.com/sbudai/RTH",
                          auth_token = Sys.getenv( "GITHUB_PAT" ), 
                          upgrade    = "ask" )
```

# Mapping Museums and Museum Closure Web App

The code in this directory defines an interactive web app based on mapping museums and museum closure data.

## Setup

Running this app requires the following R packages:

- DT
- ggmap
- ggplot2
- ggraph
- htmlwidgets
- igraph
- janitor
- jsonlite
- plotly
- readr
- shiny
- shinyBS
- shinyjs
- shinyWidges
- tidyverse

If you wish to run the app locally, you will need to make sure these are installed on your machine. If you wish to run the app on a shinyapps server, they will be automatically installed during deployment.

If you wish to deploy the app on the worldwide web, setup a user account on [shinyapps.io](https://shinyapps.io) and follow the instructions there.

## Deploying the App

Make sure the `data` directory contains up-to-date copies of the data from the Neo4j database.

Before deploying make sure this directory contains a `users.rds` table containing usernames and sodium hashed passwords. This can be created with the following lines:

```
user_base <- tibble(
  user = c("username"),
  password = purr:map_chr(c("password"), sodium::password_store),
  permissions = c("admin"),
  name = c("User Name")
)

saveRDS(user_base, "users.rds")
```

Within the R console, run the code below to setup authorization. Replace `<TOKEN>` and `<SECRET>` with your token and secret, which you can find in the user area of shinyapps.io.

```
rsconnect::setAccountInfo(name='mappingmuseums',
			  token='<TOKEN>',
			  secret='<SECRET>')
```

Then deploy the app with the following:

```
rsconnect::deployApp('path/to/app')
```

## Troubleshooting

### R Version

Currently, shinyapps.io does not have R 4.4 installed. Use an earlier version of R to deploy, e.g:

```
/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/bin/R*    
```

### Unable to dispatch task

If when deploying the app, you get the error message:

```
Unable to dispatch task for application=xxxxxx as there are 1 tasks already in progress. This is most likely a transient error. Please try again in a bit. If this error persists please contact support.
```

Then delete the `rsconnect` folder and repeat the ```rsconnect::deployApp``` command.

R_CMD = /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/bin/R
PATH_TO_APP = shiny/mappingmuseums

.PHONY: deploy-app-local deploy-app reset-db upload-db dump-db

deploy-app-local:
	Rscript -e "library(methods); shiny::runApp('$(PATH_TO_APP)', launch.browser = TRUE)"

deploy-app:
	@$(R_CMD) --no-save --no-restore --quiet -e "library(rsconnect); rsconnect::deployApp('$(PATH_TO_APP)')"

reset-db:
	@cd sheet_to_graph && pipenv run python reset.py

upload-db:
	@cd sheet_to_graph && pipenv run python upload.py

dump-db:
	@cd sheet_to_graph && pipenv run python dump.py

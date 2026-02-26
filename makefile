R_CMD = /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/bin/R
PATH_TO_APP = shiny/mappingmuseums

.PHONY: deploy-app-local deploy-app install-sheet-to-graph load-mm-data reset-db upload-db dump-db

deploy-app-local: generate-taxonomies
	export PRODUCTION=FALSE
	Rscript -e "library(methods); shiny::runApp('$(PATH_TO_APP)', launch.browser = TRUE)"

deploy-app-broken:
	@$(R_CMD) --no-save --no-restore --quiet -e "library(rsconnect); rsconnect::deployApp('$(PATH_TO_APP)', envVars=c(PRODUCTION=\"TRUE\"))"

deploy-app: generate-taxonomies
	@$(R_CMD) --no-save --no-restore --quiet -e "library(rsconnect); rsconnect::deployApp('$(PATH_TO_APP)', forceUpdate=TRUE)"

install-sheet-to-graph:
	@cd sheet-to-graph && pipenv install

load-mm-data:
	@cd sheet-to-graph && pipenv run python load_mapping_museums_data.py

reset-db:
	@cd sheet-to-graph && pipenv run python reset.py

tests:
	@cd mm-db-manager/apps-script && npm run test
	@cd mm-db-manager/cloud && uv run python -m pytest tests/unit
	@cd sheet-to-graph/cloud && uv run python -m pytest tests/unit

upload-db:
	@cd sheet-to-graph \
	&& pipenv run python anonymize_dispersal_spreadsheet.py \
	&& pipenv run python upload.py

dump-db:
	@cd sheet-to-graph && pipenv run python dump.py

backup-spreadsheet:
	@cd sheet-to-graph && pipenv run python backup_and_anonymize_dispersal_spreadsheet.py

translate-data:
	@cd sheet-to-graph/cloud && pipenv run python translate.py

generate-taxonomies:
	Rscript generate-taxonomies.R

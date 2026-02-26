.PHONY: dump-db reset-db upload-db unit

dump-db:
	pipenv run python dump.py

reset-db:
	pipenv run python reset.py

upload-db:
	pipenv run python upload.py

unit:
	pipenv run python -m pytest tests/unit

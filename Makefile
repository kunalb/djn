.PHONY: fmt fmt-check lint

VENV_BIN=.venv/bin

fmt:
	$(VENV_BIN)/ruff format .
	$(VENV_BIN)/isort --profile black .

fmt-check:
	$(VENV_BIN)/ruff format --check .
	$(VENV_BIN)/isort --profile black --check-only .

lint:
	$(VENV_BIN)/ruff check .

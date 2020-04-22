repl: ## Launch a REPL with pretty-printing enabled
	@stack ghci --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint -fobject-code" --package pretty-simple

build: ## Build the project in --fast mode
	@stack build --fast

test: ## Run the test suite on filewatch mode
	@stack test --fast --file-watch

generate-ci: ## Generate the GitHub Workflow files for the CI
	@dhall-to-yaml-ng --explain \
					  --file=.github/workflows/ci.dhall \
					  --output=.github/workflows/ci.yaml

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help

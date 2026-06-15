.PHONY: build test run repl examples clean restore watch

build:
	dotnet build Goat.slnx

test:
	dotnet test Goat.slnx --verbosity normal

watch:
	dotnet watch test --project tests/Goat.Tests

run:
	dotnet run --project src/Goat.Cli -- run $(FILE)

repl:
	dotnet run --project src/Goat.Repl

examples:
	@for f in examples/*.goat; do \
	  echo "--- $$f ---"; \
	  dotnet run --project src/Goat.Cli -- run $$f; \
	done

clean:
	dotnet clean Goat.slnx
	find . -name "bin" -type d | xargs rm -rf
	find . -name "obj" -type d | xargs rm -rf

restore:
	dotnet restore Goat.slnx

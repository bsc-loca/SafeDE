# CI version of linting script.
docker_spyglass:
	cd ci/ && bash lint_CI.sh
	exit 0

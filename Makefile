
.PHONY: app
app:
	stack --docker --local-bin-path=./bin install

image: app
	docker build -t ${tag} . --build-arg local_bin_path=./bin

test: config/nginx.conf config/mime.types
	@nginx -c `pwd`/config/nginx.conf -t

prod:
	@rm .env
	heroku config:pull -r prod
	make test

staging:
	@rm .env
	heroku config:pull -r staging
	make test

config/nginx.conf: $(wildcard config/*.conf.erb) .env
	@foreman run erb config/nginx.conf.erb > $@

config/mime.types:
	@curl -o $@ https://raw.githubusercontent.com/ryandotsmith/nginx-buildpack/master/config/mime.types
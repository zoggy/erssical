STOG=stog
DEST_DIR=`pwd`/../../erssical-gh-pages
BASE_URL_OPTION=
STOG_OPTIONS=-d $(DEST_DIR) $(BASE_URL_OPTION) --package stog.disqus
LESSC=lessc
CP=cp -f

build:
	$(MAKE) site

style.css: less/*.less
	$(LESSC) less/style.less > $@

site: style.css options
	$(STOG) $(STOG_OPTIONS) .
	$(CP) style.css $(DEST_DIR)/style.css
	mkdir -p $(DEST_DIR)/refdoc
	(cd .. && make depdoc) && cp -r ../html/* $(DEST_DIR)/refdoc/

options: options_erssical.txt options_httpd.txt

options_erssical.txt: ../erssical
	$^ --help 2>&1 | tail -n +3 > $@

options_httpd.txt: ../erssical-httpd
	$^ --help 2>&1 | tail -n +3 > $@

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site

clean:
	rm -fr .stog/cache



	
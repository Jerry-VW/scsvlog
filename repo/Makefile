PROJECT     := scsvlog
MAIN        := ScsvLog

MSG         ?= none

JDK_HOME    ?= ~/work/programs/java/jdk
SCALA_HOME  ?= ~/work/programs/java/scala

DIR_REPO    := ./repo
DIR_DIST    := ./dist
DIR_RELEASE := ./release
DIR_LIB     := ./lib
DIR_LOCALE  := ./locale
DIR_SCRIPTS := ./scripts
DIR_USERSCRIPTS := ./scripts-user
DIR_BIN     := ./bin
DIR_SRC     := ./src
DIR_DOC     := ./doc
DIR_TOOLS   := ./tools
DIR_SETTINGS := ./.settings
DIR_DB      := ./db
DIR_STATIC  := ./static

.PHONY: dist repo commit push release locale run doc scripts

dist:
	rm -rf $(DIR_DIST)/*
	cp -R -L $(DIR_LIB) $(DIR_DIST)/
	cp -R -L $(DIR_LOCALE) $(DIR_DIST)/
	cp -R -L $(DIR_TOOLS)/*.sh $(DIR_DIST)/
	cp -R -L $(DIR_TOOLS)/*.bat $(DIR_DIST)/
	cp -R -L $(DIR_STATIC) $(DIR_DIST)
	cp -R -L $(SCALA_HOME)/lib/scala-library.jar $(DIR_DIST)/lib
	cp -R -L $(SCALA_HOME)/lib/scala-swing_2.11-1.0.1.jar $(DIR_DIST)/lib
	cd $(DIR_BIN) && $(JDK_HOME)/bin/jar -cf ../$(DIR_DIST)/$(PROJECT).jar .

release: dist
	rm -rf $(DIR_RELEASE)/*
	mkdir $(DIR_RELEASE)/$(PROJECT)
	cp -R -L $(DIR_DIST)/* $(DIR_RELEASE)/$(PROJECT)
	cd $(DIR_RELEASE); zip -r $(PROJECT)_`date +%Y.%m.%d-%H.%M`.zip $(PROJECT)
	rm -rf $(DIR_RELEASE)/$(PROJECT)

locale:
	@xgettext -o $(DIR_LOCALE)/$(PROJECT).pot -L Java --from-code=utf-8 --keyword=tr `find -L $(DIR_SRC) -name *.scala`
	#@xgettext -j -o $(DIR_LOCALE)/$(PROJECT).pot -L JavaScript --from-code=utf-8 --keyword=tr `find -L $(DIR_SCRIPTS) -name *.js`

repo:
	rm -rf $(DIR_REPO)/*
	cp -R -L $(DIR_SRC) $(DIR_REPO)/
	cp -R -L $(DIR_LIB) $(DIR_REPO)/
	cp -R -L $(DIR_LOCALE) $(DIR_REPO)/
	cp -R -L $(DIR_STATIC) $(DIR_REPO)/
	#cp -R -L $(DIR_SCRIPTS) $(DIR_REPO)/
	#cp -R -L $(DIR_USERSCRIPTS) $(DIR_REPO)/
	cp -R -L $(DIR_DOC) $(DIR_REPO)/
	cp -R -L $(DIR_TOOLS) $(DIR_REPO)/
	cp -R -L $(DIR_SETTINGS) $(DIR_REPO)/
	cp -L .cache $(DIR_REPO)/
	cp -L .classpath $(DIR_REPO)/
	cp -L .project $(DIR_REPO)/
	cp -L Makefile $(DIR_REPO)/
	cp -L README.md $(DIR_REPO)/
	cp -L LICENSE $(DIR_REPO)/

commit: repo
	git add $(DIR_REPO) LICENSE README.md
	git commit -m "$(MSG)"
	
push: commit
	git push
	
scripts:
	cp -L -f ./$(DIR_SCRIPTS)/* ./$(DIR_DIST)/$(DIR_SCRIPTS)/
	
run: scripts
	cd ./$(DIR_DIST); ./$(PROJECT).sh
	

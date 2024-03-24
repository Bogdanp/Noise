APP_SRC=AppNameHere # FIXME
RKT_SRC=core
RKT_FILES=$(shell find ${RKT_SRC} -name '*.rkt')
RKT_MAIN_ZO=${RKT_SRC}/compiled/main_rkt.zo

RESOURCES_PATH=${APP_SRC}/res
RUNTIME_NAME=runtime
RUNTIME_PATH=${RESOURCES_PATH}/${RUNTIME_NAME}
MANUAL_PATH=${RESOURCES_PATH}/manual

CORE_ZO=${RESOURCES_PATH}/core.zo

.PHONY: all
all: ${CORE_ZO} ${APP_SRC}/Backend.swift

.PHONY: clean
clean:
	rm -fr ${RESOURCES_PATH}

# When building for iOS, replace `raco` with `./bin/pbraco`.

${RKT_MAIN_ZO}: ${RKT_FILES}
	raco make -j 16 -v ${RKT_SRC}/main.rkt

${CORE_ZO}: ${RKT_MAIN_ZO}
	mkdir -p ${RESOURCES_PATH}
	rm -fr ${RUNTIME_PATH}
	raco ctool \
		--runtime ${RUNTIME_PATH} \
		--runtime-access ${RUNTIME_NAME} \
		--mods $@ ${RKT_SRC}/main.rkt

${APP_SRC}/Backend.swift: ${CORE_ZO}
	raco noise-serde-codegen ${RKT_SRC}/main.rkt > $@

${MANUAL_PATH}/index.html: manual/*.scrbl
	raco scribble --html --dest ${MANUAL_PATH} +m manual/index.scrbl

website/manual/index.html: manual/*.scrbl
	make -C website manual/index.html

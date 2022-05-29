.PHONY: all
all: RacketCS.xcframework Tests/NoiseTest/Modules/mods.zo

RacketCS.xcframework: Lib/include/* Lib/libracketcs-*.a
	rm -fr $@
	xcodebuild -create-xcframework \
		-library Lib/libracketcs-x86_64-macos.a \
		-headers Lib/include \
		-output $@

Tests/NoiseTest/Modules/mods.zo:
	make -C Tests/NoiseTest/Modules mods.zo

.PHONY: clean
clean:
	rm -fr RacketCS.xcframework
	make -C Tests/NoiseTest/Modules clean

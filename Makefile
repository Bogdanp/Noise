.PHONY: all
all: RacketCS.xcframework Tests/NoiseTest/Modules/mods.zo

Lib/libracketcs-universal-macos.a:
	make -C Lib libracketcs-universal-macos.a

RacketCS.xcframework: Lib/include/* Lib/libracketcs-universal-macos.a
	rm -fr $@
	xcodebuild -create-xcframework \
		-library Lib/libracketcs-universal-macos.a \
		-headers Lib/include \
		-output $@

Tests/NoiseTest/Modules/mods.zo:
	make -C Tests/NoiseTest/Modules mods.zo

.PHONY: clean
clean:
	rm -fr RacketCS.xcframework
	make -C Lib clean
	make -C Tests/NoiseTest/Modules clean

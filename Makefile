.PHONY: all
all: RacketCS-macos.xcframework RacketCS-ios.xcframework Tests/NoiseTest/Modules/mods.zo

Lib/libracketcs-universal-macos.a:
	make -C Lib libracketcs-universal-macos.a

RacketCS-macos.xcframework: Lib/include/* Lib/libracketcs-universal-macos.a
	rm -fr $@
	xcodebuild -create-xcframework \
		-library Lib/libracketcs-universal-macos.a \
		-headers Lib/include \
		-output $@

RacketCS-ios.xcframework: Lib/include/* Lib/libracketcs-arm64-ios.a
	rm -fr $@
	xcodebuild -create-xcframework \
		-library Lib/libracketcs-arm64-ios.a \
		-headers Lib/include \
		-output $@

Tests/NoiseTest/Modules/mods.zo:
	make -C Tests/NoiseTest/Modules mods.zo

.PHONY: clean
clean:
	rm -fr *.xcframework
	make -C Lib clean
	make -C Tests/NoiseTest/Modules clean

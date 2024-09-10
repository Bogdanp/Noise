.PHONY: all
all: \
	RacketCS-ios.xcframework \
	RacketCS-macos.xcframework \
	Tests/NoiseTest/Modules/mods.zo

Lib/libracketcs-universal-macos.a:
	make -C Lib libracketcs-universal-macos.a

RacketCS-ios.xcframework: Lib/include/* Lib/libracketcs-arm64-ios.a Lib/libracketcs-arm64-iphonesimulator.a
	rm -fr $@
	xcodebuild -create-xcframework \
		-library Lib/libracketcs-arm64-iphonesimulator.a -headers Lib/include \
		-library Lib/libracketcs-arm64-ios.a -headers Lib/include \
		-output $@

RacketCS-macos.xcframework: Lib/include/* Lib/libracketcs-universal-macos.a
	rm -fr $@
	xcodebuild -create-xcframework \
		-library Lib/libracketcs-universal-macos.a \
		-headers Lib/include \
		-output $@

Tests/NoiseTest/Modules/mods.zo:
	make -C Tests/NoiseTest/Modules mods.zo

.PHONY: clean
clean:
	rm -fr *.xcframework
	make -C Lib clean
	make -C Tests/NoiseTest/Modules clean

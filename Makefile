RacketCS.xcframework: Lib/include/* Lib/libracketcs-*.a
	rm -fr $@
	xcodebuild -create-xcframework \
		-library Lib/libracketcs-x86_64-macos.a \
		-headers Lib/include \
		-output $@

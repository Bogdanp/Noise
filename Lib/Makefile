libracketcs-universal-macos.a: libracketcs-arm64-macos.a libracketcs-x86_64-macos.a
	lipo -create -output $@ libracketcs-arm64-macos.a libracketcs-x86_64-macos.a

.PHONY: clean
clean:
	rm libracketcs-universal-*.a

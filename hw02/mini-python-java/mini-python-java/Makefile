
all:
	mkdir -p bin
	javac -cp lib/java-cup-11a-runtime.jar -d bin src/mini_python/*.java
	java -cp lib/java-cup-11a-runtime.jar:bin mini_python.Main test.py

JAVACUP   := java -jar ../../lib/java-cup-11a.jar

.PHONY: tests
tests:
	bash run-tests

src/mini_python/parser.java src/mini_python/sym.java: src/mini_python/Parser.cup
	cd src/mini_python/ && $(JAVACUP) -package mini_python Parser.cup

%.java: %.flex
	rm -f $@
	jflex $<

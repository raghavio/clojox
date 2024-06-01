#!/bin/bash

# Run the JAR file with Java options
java --enable-preview -jar ./target/uberjar/clojox-0.1.0-SNAPSHOT-standalone.jar "$@"

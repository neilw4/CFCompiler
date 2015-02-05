name := "PLDI-CS4201"

version := "1.0"

libraryDependencies ++= Seq(
    "junit" % "junit" % "4.8.1" % "test"
)

libraryDependencies ++= List(
    "com.novocode" % "junit-interface" % "0.9" % "test"
)

instrumentSettings

ScoverageKeys.highlighting := true

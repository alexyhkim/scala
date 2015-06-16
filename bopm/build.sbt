name := "DerivativePricer"

version := "1.0"
scalacOptions += "-target:jvm-1.8"
scalaVersion := "2.11.6"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4-M1",
  "com.typesafe.akka" %% "akka-remote" % "2.4-M1"
)

assemblyJarName in assembly := "myactor.jar"

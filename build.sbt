name := """apolline"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.10.4"

target in Compile in doc := baseDirectory.value / "public/api"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "maven repo" at "http://repo1.maven.org/maven2/"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.reactivemongo" %% "reactivemongo-extensions-json" % "0.10.0.0-SNAPSHOT",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23",
  "com.wordnik" %% "swagger-play2" % "1.3.11",
  "de.flapdoodle.embed" % "de.flapdoodle.embed.mongo" % "1.36",
  "org.seleniumhq.selenium" % "selenium-java" % "2.35.0",
  "net.sourceforge.htmlunit" % "htmlunit" % "2.13",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"
)

fork := false

fork in Test := false

parallelExecution := true

parallelExecution in Test := true

concurrentRestrictions in Test := Seq(
  Tags.limit(Tags.Test, 4)
)

ThisBuild / version := "-SNAPSHOT"
ThisBuild / organization := "com.rayrobdod"

lazy val sharedSettings = Seq(
	libraryDependencies ++= Seq(
		"org.scalatest" %% "scalatest" % "3.1.1" % "test",
	),
	scalacOptions += "-feature",
	scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.10" | "2.11" => Seq("-target:jvm-1.7")
		case _ => Seq("-target:jvm-1.8")
	}),
	scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.10" => Seq("-Yno-predef")
		case "2.11" | "2.12" => Seq("-deprecation", "-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xfuture", "-Xcheckinit", "-Yno-predef")
		case _ => Seq("-Ywarn-unused:_", "-Xlint:_", "-Xcheckinit", "-Yno-predef")
	}),
	unmanagedSourceDirectories in Compile += (scalaBinaryVersion.value match {
		case "2.10" => (Compile / sourceDirectory).value / "scala-2.10-macros"
		case _ => (Compile / sourceDirectory).value / "scala-2.11-macros"
	}),
	scalacOptions in doc in Compile ++= Seq(
		"-doc-title", name.value,
		"-doc-version", version.value,
		"-doc-root-content", ((scalaSource in Compile).value / "rootdoc.txt").toString,
		"-implicits",
		"-groups",
		"-sourcepath", baseDirectory.value.toString,
	),
	Test / testOptions += Tests.Argument(
		"-oS",
	),
)

lazy val base = (projectMatrix in file("Base"))
	.settings(sharedSettings)
	.settings(
		name := "string-context-parser-combinator",
		libraryDependencies ++= Seq(
			"org.scala-lang" % "scala-reflect" % scalaVersion.value,
		),
	)
	.jvmPlatform(scalaVersions = Seq("2.10.7", "2.11.12", "2.12.11", "2.13.1"))

lazy val json = (projectMatrix in file("JsonParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "json",
		publish / skip := true,
		libraryDependencies ++= Seq(
			"org.scala-lang.platform" %% "scalajson" % "1.0.0-M4",
		),
	)
	.jvmPlatform(scalaVersions = Seq("2.10.7", "2.11.12", "2.12.11"))

lazy val time = (projectMatrix in file("TimeParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "time",
		publish / skip := true,
	)
	.jvmPlatform(scalaVersions = Seq("2.10.7", "2.11.12", "2.12.11", "2.13.1"))

lazy val uri = (projectMatrix in file("UriParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "uri",
		publish / skip := true,
	)
	.jvmPlatform(scalaVersions = Seq("2.10.7", "2.11.12", "2.12.11", "2.13.1"))

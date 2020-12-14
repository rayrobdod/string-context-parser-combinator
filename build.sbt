ThisBuild / version := "-SNAPSHOT"
ThisBuild / organization := "com.rayrobdod"

val scala210Ver = "2.10.7"
val scala211Ver = "2.11.12"
val scala212Ver = "2.12.12"
val scala213Ver = "2.13.4"
val scala30Ver = "3.0.0-M2"

lazy val sharedSettings = Seq(
	libraryDependencies ++= Seq(
		"org.scalatest" %% "scalatest" % "3.2.3" % "test",
	),
	scalacOptions += "-feature",
	scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.10" | "2.11" => Seq("-target:jvm-1.7")
		case _ => Seq("-target:jvm-1.8")
	}),
	scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.10" => Seq.empty
		case "2.11" | "2.12" => Seq("-deprecation", "-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xfuture", "-Xcheckinit")
		case "2.13" => Seq("-Ywarn-unused:_", "-Xlint:_", "-Xcheckinit")
		case _ => Seq.empty
	}),
	unmanagedSourceDirectories in Compile += (scalaBinaryVersion.value match {
		case "2.10" => (Compile / sourceDirectory).value / "scala-2.10-macros"
		case "2.11" | "2.12" | "2.13" => (Compile / sourceDirectory).value / "scala-2.11-macros"
		case _ => (Compile / sourceDirectory).value / "scala-3-macros"
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
		libraryDependencies ++= (scalaBinaryVersion.value match {
			case "2.10" | "2.11" | "2.12" | "2.13" => Seq(
				"org.scala-lang" % "scala-reflect" % scalaVersion.value,
			)
			case _ => Seq()
		}),
		libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value)),
	)
	.jvmPlatform(scalaVersions = Seq(
		scala210Ver,
		scala211Ver,
		scala212Ver,
		scala213Ver,
		scala30Ver,
	))

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
	.jvmPlatform(scalaVersions = Seq(
		scala210Ver,
		scala211Ver,
		scala212Ver,
	))

lazy val time = (projectMatrix in file("TimeParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "time",
		publish / skip := true,
	)
	.jvmPlatform(scalaVersions = Seq(
		scala210Ver,
		scala211Ver,
		scala212Ver,
		scala213Ver,
	))

lazy val uri = (projectMatrix in file("UriParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "uri",
		publish / skip := true,
	)
	.jvmPlatform(scalaVersions = Seq(
		scala210Ver,
		scala211Ver,
		scala212Ver,
		scala213Ver,
		scala30Ver,
	))

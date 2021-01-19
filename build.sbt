ThisBuild / version := "-SNAPSHOT"
ThisBuild / organization := "com.rayrobdod"

val scala210Ver = "2.10.7"
val scala211Ver = "2.11.12"
val scala212Ver = "2.12.12"
val scala213Ver = "2.13.4"
val scala30Ver = "3.0.0-M3"

lazy val sharedSettings = Seq(
	libraryDependencies ++= (scalaBinaryVersion.value match {
		case "2.10" | "2.11" | "2.12" | "2.13" => Seq(
			"org.scalatest" %% "scalatest" % "3.2.3" % "test",
		)
		case "3.0.0-M3" => Seq(
			"org.scalatest" % "scalatest_3.0.0-M3" % "3.2.3" % "test",
		)
	}),
	Compile / compile / scalacOptions += "-feature",
	Compile / compile / scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.10" | "2.11" => Seq("-target:jvm-1.7")
		case "2.12" | "2.13" => Seq("-target:jvm-1.8")
		case _ => Seq("-release", "8")
	}),
	Compile / compile / scalacOptions ++= (scalaBinaryVersion.value match {
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
	scalacOptions in doc in Compile ++= (scalaBinaryVersion.value match {
		case "2.10" | "2.11" | "2.12" | "2.13" => Seq(
			"-doc-title", name.value,
			"-doc-version", (if ("-SNAPSHOT" == version.value) {"SNAPSHOT"} else {version.value}),
			"-doc-root-content", ((scalaSource in Compile).value / "rootdoc.txt").toString,
			"-implicits",
			"-groups",
			"-sourcepath", baseDirectory.value.toString,
		)
		case _ => Seq(
			"-project-version", (if ("-SNAPSHOT" == version.value) {"SNAPSHOT"} else {version.value}),
			"-siteroot", ((sourceDirectory).value / "docs" / "public").toString,
			"-sourcepath", baseDirectory.value.toString,
		)
	}),
	Compile / doc / fileInputOptions += "-siteroot",
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
		console / initialCommands := """
			import scala.quoted.{Expr, Quotes}
			import com.rayrobdod.stringContextParserCombinator.{Parser => _, _}
			import com.rayrobdod.stringContextParserCombinator.Parsers._
			import com.rayrobdod.stringContextParserCombinator.typelevel._
		""",
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
		libraryDependencies ++= (scalaBinaryVersion.value match {
			case "2.10" | "2.11" | "2.12" | "2.13" => Seq(
				"org.json4s" %% "json4s-ast" % "3.6.10",
			)
			case _ => Seq(
				"org.json4s" % "json4s-ast_2.13" % "3.6.10",
			)
		}),
		console / initialCommands := """
			import org.json4s.JsonAST._
			import com.rayrobdod.stringContextParserCombinatorExample.json._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala210Ver,
		scala211Ver,
		scala212Ver,
		scala213Ver,
		scala30Ver,
	))

lazy val time = (projectMatrix in file("TimeParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "time",
		publish / skip := true,
		console / initialCommands := """
			import java.time._
			import com.rayrobdod.stringContextParserCombinatorExample.datetime._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala210Ver,
		scala211Ver,
		scala212Ver,
		scala213Ver,
		scala30Ver,
	))

lazy val uri = (projectMatrix in file("UriParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "uri",
		publish / skip := true,
		console / initialCommands := """
			import java.net.URI
			import com.rayrobdod.stringContextParserCombinatorExample.uri._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala210Ver,
		scala211Ver,
		scala212Ver,
		scala213Ver,
		scala30Ver,
	))

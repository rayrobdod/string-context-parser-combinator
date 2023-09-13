val scala212Ver = "2.12.18"
val scala213Ver = "2.13.12"
val scala3Ver = "3.3.1"

val githubId = "rayrobdod/string-context-parser-combinator"

sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / dynverVTagPrefix := false

lazy val sharedSettings = Seq(
	sonatypeRepository := "https://s01.oss.sonatype.org/service/local",

	organization := "name.rayrobdod",
	organizationHomepage := Some(url("https://rayrobdod.name/")),
	versionScheme := Some("early-semver"),
	licenses := Seq(License.Apache2),
	developers := List(
		Developer(
			"rayrobdod",
			"Raymond Dodge",
			"git@rayrobdod.name",
			url("https://rayrobdod.name"),
		),
	),
	autoAPIMappings := true,
	libraryDependencies ++= Seq(
		"org.scalameta" %%% "munit" % "0.7.29" % Test,
	),
	tpolecatExcludeOptions ++= Set(
		org.typelevel.scalacoptions.ScalacOptions.warnUnusedNoWarn,
		org.typelevel.scalacoptions.ScalacOptions.privateWarnUnusedNoWarn,
	),
	Compile / doc / scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.12" | "2.13" => Seq(
			"-doc-title", name.value,
			"-doc-version", (if ("-SNAPSHOT" == version.value) {"SNAPSHOT"} else {version.value}),
			"-doc-root-content", ((Compile / scalaSource).value / "rootdoc.md").toString,
			"-implicits",
			"-groups",
			"-sourcepath", baseDirectory.value.toString,
		)
		case _ => Seq(
			"-doc-root-content", ((Compile / scalaSource).value / "rootdoc.md").toString,
			"-groups",
			"-project-version", (if ("-SNAPSHOT" == version.value) {"SNAPSHOT"} else {version.value}),
			"-revision", git.gitHeadCommit.value.get,
			"-scastie-configuration", Seq(
				"resolvers ++= Resolver.sonatypeOssRepos(\"snapshots\")",
				s"""libraryDependencies += "${organization.value}" %% "${name.value}" % "${version.value}""""
			).mkString("\\n"),
			"-siteroot", ((sourceDirectory).value / "docs").toString,
			"-snippet-compiler:compile",
			s"-social-links:github::https://github.com/${githubId}",
			s"-source-links:github://${githubId}",
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
		description := "A scala library for writing custom string interpolation implementations using parser combinators",
		apiURL := Some(url("https://rayrobdod.github.io/string-context-parser-combinator/")),
		Compile / packageBin / packageOptions += Package.ManifestAttributes(
			"Automatic-Module-Name" -> "name.rayrobdod.stringContextParserCombinator"
		),
		libraryDependencies ++= (scalaBinaryVersion.value match {
			case "2.12" | "2.13" => Seq(
				"org.scala-lang" % "scala-reflect" % scalaVersion.value,
			)
			case _ => Seq()
		}),
		console / initialCommands := """
			import scala.quoted.{Expr, Quotes}
			import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
			import name.rayrobdod.stringContextParserCombinator.typeclass._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))
	.jsPlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))

lazy val json = (projectMatrix in file("JsonParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "json",
		publish / skip := true,
		libraryDependencies ++= Seq(
			"org.json4s" %%% "json4s-ast" % "4.0.6",
		),
		console / initialCommands := """
			import org.json4s._
			import name.rayrobdod.stringContextParserCombinatorExample.json._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))
	.jsPlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))

lazy val time = (projectMatrix in file("TimeParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "time",
		publish / skip := true,
		console / initialCommands := """
			import java.time._
			import name.rayrobdod.stringContextParserCombinatorExample.datetime._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))
	.jsPlatform(scalaVersions = Seq(
			scala212Ver,
			scala213Ver,
			scala3Ver,
		),
		libraryDependencies ++= Seq(
			"io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
		),
	)

lazy val uri = (projectMatrix in file("UriParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "uri",
		publish / skip := true,
		console / initialCommands := """
			import java.net.URI
			import name.rayrobdod.stringContextParserCombinatorExample.uri._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))

autoScalaLibrary := false
publish / skip := true

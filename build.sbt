val scala212Ver = "2.12.19"
val scala213Ver = "2.13.14"
val scala3Ver = "3.3.3"

val githubId = "rayrobdod/string-context-parser-combinator"

sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / dynverVTagPrefix := false
ThisBuild / organization := "name.rayrobdod"

lazy val sharedSettings = Seq(
	sonatypeRepository := "https://s01.oss.sonatype.org/service/local",

	organizationHomepage := Some(url("https://rayrobdod.name/")),
	homepage := Some(url("https://rayrobdod.name/programming/libraries/scala/string-context-parser-combinator/")),
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
	libraryDependencies += {
		"org.scalameta" %%% "munit" % "1.0.1" % Test,
	},
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
lazy val sharedJsSettings = Seq(
	tpolecatScalacOptions ++= {
		val hash = git.gitHeadCommit.value.get
		val local = (LocalRootProject / baseDirectory).value.toURI.toString
		val remote = s"https://raw.githubusercontent.com/${githubId}/${hash}"
		import org.typelevel.scalacoptions.ScalaVersion.V3_0_0
		import scala.Ordering.Implicits._
		Set(
			org.typelevel.scalacoptions.ScalacOption(
				s"-scalajs-mapSourceURI:$local->$remote/",
				version => version >= V3_0_0,
			),
			org.typelevel.scalacoptions.ScalacOption(
				s"-P:scalajs:mapSourceURI:$local->$remote/",
				version => version < V3_0_0,
			),
		)
	},
)

lazy val base = (projectMatrix in file("Base"))
	.settings(sharedSettings)
	.settings(
		name := "string-context-parser-combinator",
		description := "A scala library for writing custom string interpolation implementations using parser combinators",
		apiURL := {
			val suffix = scalaBinaryVersion.value match {
				case "2.12" => "_2.12"
				case "2.13" => "_2.13"
				case "3" => ""
			}
			val v = version.value match {
				case x if x.endsWith("-SNAPSHOT") => "SNAPSHOT"
				case v => v
			}
			Some(url(s"https://rayrobdod.github.io/string-context-parser-combinator/${v}${suffix}/"))
		},
		Compile / packageBin / packageOptions += Package.ManifestAttributes(
			"Automatic-Module-Name" -> "name.rayrobdod.stringContextParserCombinator"
		),
		Compile / doc / scalacOptions ++= (scalaBinaryVersion.value match {
			case "2.12" | "2.13" => Seq(
			)
			case _ => Seq(
				"-versions-dictionary-url", "https://rayrobdod.github.io/string-context-parser-combinator/versions.json",
			)
		}),
		libraryDependencies ++= (scalaBinaryVersion.value match {
			case "2.12" | "2.13" => Seq(
				"org.scala-lang" % "scala-reflect" % scalaVersion.value,
			)
			case _ => Seq()
		}),
		mimaPreviousArtifacts := Set(organization.value %% name.value % "0.1.0"),
		tastyMiMaPreviousArtifacts := mimaPreviousArtifacts.value,
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
	.jsPlatform(
		scalaVersions = Seq(
			scala212Ver,
			scala213Ver,
			scala3Ver,
		),
		sharedJsSettings,
	)
	.nativePlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))

lazy val json = (projectMatrix in file("JsonParser"))
	.dependsOn(base)
	.disablePlugins(MimaPlugin)
	.disablePlugins(TastyMiMaPlugin)
	.settings(sharedSettings)
	.settings(
		name := "json",
		publish / skip := true,
		libraryDependencies ++= Seq(
			"org.json4s" %%% "json4s-ast" % "4.0.7",
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
	.jsPlatform(
		scalaVersions = Seq(
			scala212Ver,
			scala213Ver,
			scala3Ver,
		),
		sharedJsSettings,
	)
	.nativePlatform(scalaVersions = Seq(
		scala212Ver,
		scala213Ver,
		scala3Ver,
	))

lazy val time = (projectMatrix in file("TimeParser"))
	.dependsOn(base)
	.disablePlugins(MimaPlugin)
	.disablePlugins(TastyMiMaPlugin)
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
	.jsPlatform(
		scalaVersions = Seq(
			scala212Ver,
			scala213Ver,
			scala3Ver,
		),
		Seq(
			libraryDependencies ++= Seq(
				"io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
			),
		) ++
		sharedJsSettings,
	)
	.nativePlatform(
		scalaVersions = Seq(
			scala212Ver,
			scala213Ver,
			scala3Ver,
		),
		Seq(
			libraryDependencies ++= Seq(
				"io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
			),
		)
	)

lazy val uri = (projectMatrix in file("UriParser"))
	.dependsOn(base)
	.disablePlugins(MimaPlugin)
	.disablePlugins(TastyMiMaPlugin)
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

lazy val xml = (projectMatrix in file("XmlParser"))
	.dependsOn(base)
	.disablePlugins(MimaPlugin)
	.disablePlugins(TastyMiMaPlugin)
	.settings(sharedSettings)
	.settings(
		name := "xml",
		publish / skip := true,
		libraryDependencies ++= Seq(
			"org.scala-lang.modules" %%% "scala-xml" % "2.3.0",
		),
		console / initialCommands := """
			import name.rayrobdod.stringContextParserCombinatorExample.xml._
		""",
	)
	.jvmPlatform(scalaVersions = Seq(
		scala3Ver,
	))

disablePlugins(MimaPlugin)
disablePlugins(TastyMiMaPlugin)
autoScalaLibrary := false
publish / skip := true

enablePlugins(StageWebsitePlugin)
webSnapshotVariants := Map(
	"2.12" -> (base.jvm(scala212Ver) / Compile / doc).value,
	"2.13" -> (base.jvm(scala213Ver) / Compile / doc).value,
	"3" -> (base.jvm(scala3Ver) / Compile / doc).value,
)
webStage / mappings += ((base.jvm(scala3Ver) / sourceDirectory).value / "docs" / "versions.json") -> "versions.json",

val scala212Ver = "2.12.20"
val scala213Ver = "2.13.16"
val scala3Ver = "3.3.5"

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
		"org.scalameta" %%% "munit" % "1.1.0" % Test,
	},
	tpolecatExcludeOptions ++= Set(
		org.typelevel.scalacoptions.ScalacOptions.warnUnusedNoWarn,
		org.typelevel.scalacoptions.ScalacOptions.privateWarnUnusedNoWarn,
	),
	Compile / ifDefDeclarations ++= (scalaBinaryVersion.value match {
		case "2.12" | "2.13" =>
			Seq("scalaEpochVersion:2")
		case "3" =>
			Nil
	}),
	Test / ifDefDeclarations ++= (scalaBinaryVersion.value match {
		case "2.12" | "2.13" =>
			Seq("scalaEpochVersion:2")
		case "3" =>
			Nil
	}),
	Compile / doc / scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.12" | "2.13" => Seq(
			"-doc-title", name.value,
			"-doc-version", (if ("-SNAPSHOT" == version.value) {"SNAPSHOT"} else {version.value}),
			"-doc-root-content", ((Compile / docRootContentToWikidoc).value).toString,
			"-doc-source-url", s"https://github.com/${githubId}/tree/${if (version.value.endsWith("-SNAPSHOT")) {git.gitHeadCommit.value.get} else {version.value}}€{FILE_PATH}.scala",
			"-implicits",
			"-groups",
			"-sourcepath", baseDirectory.value.getParentFile.getParentFile.getParentFile.toString,
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
		console / initialCommands := """
			import scala.quoted.{Expr, Quotes}
			import name.rayrobdod.stringContextParserCombinator.Interpolator.idInterpolators._
			import name.rayrobdod.stringContextParserCombinator.typeclass._
		""",
		mimaPreviousArtifacts := Set(
			organization.value %% name.value % "0.1.0",
			organization.value %% name.value % "0.1.1",
		),
		tastyMiMaPreviousArtifacts := mimaPreviousArtifacts.value,
		mimaBinaryIssueFilters ++= {
			import com.typesafe.tools.mima.core._
			Seq(
				ProblemFilters.exclude[DirectMissingMethodProblem]("name.rayrobdod.stringContextParserCombinator.typeclass.BiEithered.eitherAnyUnit"),
				ProblemFilters.exclude[DirectMissingMethodProblem]("name.rayrobdod.stringContextParserCombinator.typeclass.BiEithered.eitherUnitAny"),
				ProblemFilters.exclude[IncompatibleMethTypeProblem]("name.rayrobdod.stringContextParserCombinator.ExpectingSet#NonEmpty.*"),
				ProblemFilters.exclude[IncompatibleMethTypeProblem]("name.rayrobdod.stringContextParserCombinator.internal.*"),
				ProblemFilters.exclude[IncompatibleResultTypeProblem]("name.rayrobdod.stringContextParserCombinator.ExpectingSet#NonEmpty.*"),
				ProblemFilters.exclude[IncompatibleResultTypeProblem]("name.rayrobdod.stringContextParserCombinator.internal.package.describeCodepointPredicate"),
				ProblemFilters.exclude[IncompatibleResultTypeProblem]("name.rayrobdod.stringContextParserCombinator.typeclass.*"),
				ProblemFilters.exclude[IncompatibleMethTypeProblem]("name.rayrobdod.stringContextParserCombinator.Parser.interpolate"),
				ProblemFilters.exclude[IncompatibleMethTypeProblem]("name.rayrobdod.stringContextParserCombinator.Interpolator.interpolate"),
				ProblemFilters.exclude[MissingClassProblem]("name.rayrobdod.stringContextParserCombinator.Expecting$package"),
				ProblemFilters.exclude[MissingClassProblem]("name.rayrobdod.stringContextParserCombinator.Expecting$package$"),
				ProblemFilters.exclude[MissingClassProblem]("name.rayrobdod.stringContextParserCombinator.typeclass.VersionSpecific*"),
				ProblemFilters.exclude[MissingClassProblem]("name.rayrobdod.stringContextParserCombinator.VersionSpecific*"),
				ProblemFilters.exclude[MissingTypesProblem]("name.rayrobdod.stringContextParserCombinator.typeclass.*"),
				ProblemFilters.exclude[MissingTypesProblem]("name.rayrobdod.stringContextParserCombinator.Extractor"),
				ProblemFilters.exclude[MissingTypesProblem]("name.rayrobdod.stringContextParserCombinator.Extractor$"),
				ProblemFilters.exclude[MissingTypesProblem]("name.rayrobdod.stringContextParserCombinator.Interpolator"),
				ProblemFilters.exclude[MissingTypesProblem]("name.rayrobdod.stringContextParserCombinator.Interpolator$"),
				ProblemFilters.exclude[MissingTypesProblem]("name.rayrobdod.stringContextParserCombinator.Parser"),
				ProblemFilters.exclude[MissingTypesProblem]("name.rayrobdod.stringContextParserCombinator.Parser$"),
			)
		},
		tastyMiMaConfig ~= { prevConfig =>
			import scala.collection.JavaConverters._
			import tastymima.intf._
			prevConfig
				.withMoreArtifactPrivatePackages(Seq(
					"name.rayrobdod.stringContextParserCombinator.internal",
				).asJava)
				.withMoreProblemFilters(Seq(
					ProblemMatcher.make(ProblemKind.AbstractClass, "name.rayrobdod.stringContextParserCombinator.ExpectingDescription"),
					ProblemMatcher.make(ProblemKind.IncompatibleTypeChange, "name.rayrobdod.stringContextParserCombinator.ExpectingSet.*"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.Extractor"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.Extractor$"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.Expecting.*"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.ExpectingSet.*"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.Parser"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.Parser$"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.typeclass.*"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.Interpolator"),
					ProblemMatcher.make(ProblemKind.InternalError, "name.rayrobdod.stringContextParserCombinator.Interpolator$"),
					ProblemMatcher.make(ProblemKind.MissingClass, "name.rayrobdod.stringContextParserCombinator.Expecting$package$"),
					ProblemMatcher.make(ProblemKind.MissingClass, "name.rayrobdod.stringContextParserCombinator.typeclass.VersionSpecific*"),
					ProblemMatcher.make(ProblemKind.MissingClass, "name.rayrobdod.stringContextParserCombinator.VersionSpecific*"),
					ProblemMatcher.make(ProblemKind.MissingParent, "name.rayrobdod.stringContextParserCombinator.ExpectingDescription"),
					ProblemMatcher.make(ProblemKind.MissingParent, "name.rayrobdod.stringContextParserCombinator.ExpectingDescription$"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.Expecting.*"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.ExpectingDescription.*"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.ExpectingSet.*"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.Input.consume"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.internal.*"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.VersionSpecificParser.interpolate"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.VersionSpecificExtractorModule.*"),
					ProblemMatcher.make(ProblemKind.MissingTermMember, "name.rayrobdod.stringContextParserCombinator.VersionSpecificParserModule.*"),
					ProblemMatcher.make(ProblemKind.MissingTypeMember, "name.rayrobdod.stringContextParserCombinator.VersionSpecificExtractorModule.*"),
					ProblemMatcher.make(ProblemKind.MissingTypeMember, "name.rayrobdod.stringContextParserCombinator.VersionSpecificParserModule.*"),
					ProblemMatcher.make(ProblemKind.RestrictedVisibilityChange, "name.rayrobdod.stringContextParserCombinator.ExpectingDescription.*"),
				).asJava)
		},
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
	.disablePlugins(IfDefPlugin)
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
	.disablePlugins(IfDefPlugin)
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
	.disablePlugins(IfDefPlugin)
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
	.disablePlugins(IfDefPlugin)
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
	"2.12" -> (base.jvm(scala212Ver) / Compile / packageDoc / mappings).value,
	"2.13" -> (base.jvm(scala213Ver) / Compile / packageDoc / mappings).value,
	"3" -> (base.jvm(scala3Ver) / Compile / packageDoc / mappings).value,
)
webStage / mappings += ((base.jvm(scala3Ver) / sourceDirectory).value / "docs" / "versions.json") -> "versions.json"

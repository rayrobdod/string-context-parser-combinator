import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

lazy val sharedSourceDirectory = settingKey[File]("")

ThisBuild / scalaVersion := "2.12.10"
ThisBuild / crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.10")
ThisBuild / version := "-SNAPSHOT"
ThisBuild / organization := "com.rayrobdod"

lazy val sharedSettings = Seq(
	libraryDependencies ++= Seq(
		"org.scalatest" %%% "scalatest" % "3.1.0" % "test",
	),
	scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.10" | "2.11" => Seq("-target:jvm-1.7")
		case _ => Seq("-target:jvm-1.8")
	}),
	scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.10" => Seq("-Yno-predef")
		case "2.11" | "2.12" => Seq("-deprecation", "-Ywarn-unused-import", "-Ywarn-unused", "-Xlint:_", "-Xfuture", "-Xcheckinit", "-Yno-predef")
		case _ => Seq("-Ywarn-unused:_", "-Xlint:_", "-Xcheckinit", "-Yno-predef")
	}),
	Compile / sharedSourceDirectory := (Compile / baseDirectory).value / ".." / "src" / "main",
	unmanagedSourceDirectories in Compile += (scalaBinaryVersion.value match {
		case "2.10" => (Compile / sharedSourceDirectory).value / "scala-2.10-macros"
		case _ => (Compile / sharedSourceDirectory).value / "scala-2.11-macros"
	}),
	scalacOptions in doc in Compile ++= Seq(
		"-doc-title", name.value,
		"-doc-version", version.value,
		"-doc-root-content", ((scalaSource in Compile).value / "rootdoc.txt").toString,
		"-implicits",
		"-groups",
		"-sourcepath", baseDirectory.value.toString,
	),
)

lazy val base = crossProject(JVMPlatform, JSPlatform)
	.crossType(CrossType.Pure)
	.in(file("Base"))
	.settings(sharedSettings)
	.settings(
		name := "string-context-parser-combinator",
		crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.10", "2.13.1"),
		libraryDependencies ++= Seq(
			scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
			scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided",
		),
		Test / sources := (Def.taskDyn[Seq[File]]{
			if (crossProjectPlatform.value == JVMPlatform) {
				Def.task{(Test / sources).value}
			} else {
				/* Attempting to link these tests results in `Referring to non-existent class scala.reflect.api.Exprs$Expr` */
				Def.task{ Seq() }
			}
		}).value,
	)

lazy val json = crossProject(JVMPlatform, JSPlatform)
	.crossType(CrossType.Pure)
	.in(file("JsonParser"))
	.dependsOn(base)
	.settings(sharedSettings)
	.settings(
		name := "json",
		crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.10"),
		libraryDependencies ++= Seq(
			"org.scala-lang.platform" %%% "scalajson" % "1.0.0-M4",
			scalaOrganization.value % "scala-reflect" % scalaVersion.value % "provided",
			scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided",
		),
	)

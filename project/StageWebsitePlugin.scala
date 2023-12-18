package name.rayrobdod.sbtJavafx

import sbt._
import sbt.Keys._

object StageWebsitePlugin extends AutoPlugin {
	object autoImport {
		val webStage = taskKey[Seq[File]]("Create a local directory with all the files laid out as they would be in the website")
		val snapshotVariants = taskKey[Seq[(File, String)]]("snapshot documentations to include in the multi-versioned documentation site")
		val makePagesIndex = taskKey[File]("Generates a root index page for the multi-versioned documentation site")
	}
	import autoImport._

	override val globalSettings = Seq(
		snapshotVariants := Nil,
	)

	private def unscopedSettings = Seq(
		makePagesIndex / target := (target.value / "web" / "makePagesIndex"),
		makePagesIndex := {
			val targetFile = (makePagesIndex / target).value / "index.html"
			val prefix = Seq(
				"<!DOCTYPE html>",
				"<html>",
				" <head>",
				"  <title>String Context Parser Combinator API docs</title>",
				" </head>",
				" <body>",
				"  <h1>String Context Parser Combinator API docs</h1>",
				"  <table>",
			)
			val suffix = Seq(
				"   </tbody>",
				"  </table>",
				" </body>",
				"</html>",
			)

			val scalaVersions = Seq(
				"3" -> "",
				"2.13" -> "_2.13",
				"2.12" -> "_2.12",
			)
			val myVersions = Seq(
				"SNAPSHOT",
			)

			val tableHeader =
				Seq("   <thead>", "    <tr>", "     <th scope='col'/>") ++:
				scalaVersions.map(scalaVersion => s"     <th scope='col'>For Scala ${scalaVersion._1}</th>") ++:
				Seq("    </tr>", "   </thead>", "   <tbody>")

			val table =
				myVersions.flatMap({myVersion =>
					Seq("    <tr>", s"     <th scope='row'>${myVersion}</th>") ++:
					scalaVersions.map({scalaVersion =>
						s"      <td><a href='${myVersion}${scalaVersion._2}/'>API for ${myVersion} for ${scalaVersion._1}</a></td>"
					}) :+
					"    </tr>"
				})

			sbt.io.IO.writeLines(targetFile, prefix ++ tableHeader ++ table ++ suffix)
			targetFile
		},

		webStage / target := (target.value / "web" / "stage"),
		webStage / mappings := Seq.empty,
		webStage / mappings ++= {
			snapshotVariants.value.flatMap({rootdirSuffix =>
				val (rootdir, suffix) = rootdirSuffix
				(rootdir ** AllPassFilter).pair(f => rootdir.relativize(f).map(p => s"SNAPSHOT${suffix}/${p.toString}"))
			})
		},
		webStage / mappings += ((makePagesIndex).value) -> "index.html",
		webStage := {
			import java.nio.file.Files
			import java.nio.file.StandardCopyOption.{COPY_ATTRIBUTES, REPLACE_EXISTING}
			val tarDir = (webStage / target).value
			Files.createDirectories(tarDir.toPath)
			tarDir.listFiles.foreach(f => sbt.io.IO.delete(f))

			(webStage / mappings).value.map{case (srcFile, name) =>
				val tarFile = tarDir / name
				Files.copy(srcFile.toPath, tarFile.toPath, COPY_ATTRIBUTES, REPLACE_EXISTING)
				tarFile
			}
		},
	)

	private def perScopeSettings(config:sbt.librarymanagement.Configuration) = Seq(
	)

	override lazy val projectSettings = unscopedSettings ++ perScopeSettings(Compile) ++ perScopeSettings(Test)
}

package name.rayrobdod.stringContextParserCombinator.build

import sbt._
import sbt.Keys._
import sbt.librarymanagement.ivy.IvyDependencyResolution

object StageWebsitePlugin extends AutoPlugin {
	private def webDirectorySuffixForScalaVersion(v:String) = {
		if ("3" == v) {
			""
		} else {
			s"_$v"
		}
	}

	object autoImport {
		val webStage = taskKey[Seq[File]]("Create a local directory with all the files laid out as they would be in the website")
		val webMakePagesIndex = taskKey[File]("Generates a root index page for the multi-versioned documentation site")
		val webSnapshotVariants = taskKey[Map[String, File]]("snapshot documentations to include in the multi-versioned documentation site")
		val webPublishedVariants = taskKey[Map[String, Map[String, File]]]("published documentations to include in the multi-versioned documentation site")
	}
	import autoImport._

	val webPublishedVariantsTask = Def.task{
		val myVersions = Seq("0.1.0")
		val scalaVersions = Seq("2.12", "2.13", "3")

		val config = InlineIvyConfiguration.apply()
		val resolver = IvyDependencyResolution.apply(config)

		val cacheDir = (webPublishedVariants / target).value
		val logger = (webPublishedVariants / streams).value.log

		myVersions.map({myVersion =>
			myVersion -> scalaVersions.map({scalaVersion =>
				val moduleId = ("name.rayrobdod" % ("string-context-parser-combinator_" + scalaVersion) % myVersion)
					.classifier(Artifact.DocClassifier)
					.notTransitive

				val docjar = resolver.retrieve(moduleId, None, cacheDir / "jars", logger).map(_.distinct) match {
					case Right(Seq(docjar)) => docjar
					case Right(_) => throw new RuntimeException("More than one artifact retrieved: " + moduleId)
					case Left(unresolvedWarning) => throw unresolvedWarning.resolveException
				}

				val unzipdir = cacheDir / "unzip" / scalaVersion / myVersion
				sbt.IO.unzip(docjar, unzipdir)
				scalaVersion -> unzipdir
			}).toMap
		}).toMap
	}.tag(sbt.Tags.Network)

	override val globalSettings = Seq(
	)

	private def unscopedSettings = Seq(
		webSnapshotVariants := Map.empty,
		webPublishedVariants / target := (target.value / "web" / "cache"),
		webPublishedVariants := webPublishedVariantsTask.value,
		webMakePagesIndex / target := (target.value / "web" / "makePagesIndex"),
		webMakePagesIndex := {
			val targetFile = (webMakePagesIndex / target).value / "index.html"
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

			val variants = webPublishedVariants.value + ("SNAPSHOT" -> webSnapshotVariants.value)

			val myVersions = variants.keySet.toSeq.distinct.sorted
			val scalaVersions = variants.values.flatMap(_.keySet).toSeq.distinct.sorted.reverse

			val tableHeader =
				Seq("   <thead>", "    <tr>", "     <th scope='col'/>") ++:
				scalaVersions.map(scalaVersion => s"     <th scope='col'>For Scala ${scalaVersion}</th>") ++:
				Seq("    </tr>", "   </thead>", "   <tbody>")

			val table =
				myVersions.flatMap({myVersion =>
					Seq("    <tr>", s"     <th scope='row'>${myVersion}</th>") ++:
					scalaVersions.map({scalaVersion =>
						val thisVariant = variants.get(myVersion).flatMap(_.get(scalaVersion))
						thisVariant.map({_ =>
							s"      <td><a href='${myVersion}${webDirectorySuffixForScalaVersion(scalaVersion)}/'>API for ${myVersion} for ${scalaVersion}</a></td>"
						}).getOrElse("      <td></td>")
					}) :+
					"    </tr>"
				})

			sbt.io.IO.writeLines(targetFile, prefix ++ tableHeader ++ table ++ suffix)
			targetFile
		},

		webStage / target := (target.value / "web" / "stage"),
		webStage / mappings := Seq.empty,
		webStage / mappings ++= {
			val snapshotVariants = "SNAPSHOT" -> webSnapshotVariants.value
			val publishedVariants = webPublishedVariants.value

			val variants = publishedVariants + snapshotVariants

			variants.toSeq.flatMap({case (myVersion, myVersionVariants) =>
				myVersionVariants.toSeq.flatMap({case (scalaVersion, rootdir) =>
					(rootdir ** AllPassFilter).pair(f => rootdir.relativize(f).map(p => s"${myVersion}${webDirectorySuffixForScalaVersion(scalaVersion)}/${p.toString}"))
				})
			})
		},
		webStage / mappings += ((webMakePagesIndex).value) -> "index.html",
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

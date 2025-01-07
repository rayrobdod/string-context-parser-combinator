package name.rayrobdod.stringContextParserCombinator.build

import sbt._
import sbt.Keys._

object DocRootContentToWikidoc extends AutoPlugin {
	override def requires = sbt.plugins.JvmPlugin
	override def trigger = allRequirements

	object autoImport {
		val markdownSource = settingKey[File]("")
		val docRootContentToWikidoc = taskKey[File]("Convert the doc-root-content file from scala-3's markdown to scala-2's wikidoc")
	}
	import autoImport._

	override val globalSettings = Seq(
	)

	private def unscopedSettings = Seq(
	)

	object HeaderLine {
		def unapply(s:String): Option[(String, Int)] = {
			val (hashes, rest) = s.span(_ == '#')
			if (hashes.size != 0 && rest.head == ' ') {
				Some((rest.tail, hashes.size))
			} else {
				None
			}
		}
	}

	private def perScopeSettings(config:sbt.librarymanagement.Configuration) = Seq(
		config / docRootContentToWikidoc / target := (target.value / s"${config.name}-rootdoc.wikidoc"),
		config / docRootContentToWikidoc / markdownSource := ((config / scalaSource).value / "rootdoc.md"),
		config / docRootContentToWikidoc := {
			val in = (config / docRootContentToWikidoc / markdownSource).value
			val out = (config / docRootContentToWikidoc / target).value

			if (in.exists()) {
				sbt.io.IO.writeLines(
					out,
					sbt.io.IO.readLines(in)	.map{(line) => line match {
						case HeaderLine(title, level) => s"${"=" * level} $title ${"=" * level}"
						case other => other
					}}
				)
			} else {
				sbt.io.IO.delete(out)
			}
			out
		},
	)

	override lazy val projectSettings = unscopedSettings ++ perScopeSettings(Compile) ++ perScopeSettings(Test)
}

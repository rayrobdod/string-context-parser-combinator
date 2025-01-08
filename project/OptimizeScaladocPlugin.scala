package name.rayrobdod.stringContextParserCombinator.build

import sbt._
import sbt.Keys._

object OptimizeScaladocPlugin extends AutoPlugin {
	override def requires = sbt.plugins.JvmPlugin
	override def trigger = allRequirements

	object autoImport {
		val optdocMakeReplacementFontawesomeCss = taskKey[(File, String)]("")
		val optdocMakeReplacementFontawesomeIcons = taskKey[Seq[(File, String)]]("")
	}
	import autoImport._

	override val globalSettings = Seq(
	)

	private def states: Seq[(String, Seq[String], String, String)] = Seq(
		("default", Seq("", ":disabled", ":focus"), "#6F6E77", "#A09FA6"),
		("hover", Seq(":hover", ":active"), "#1A1523", "#CCCCCC"),
		("selected", Seq(".selected"), "#1A1523", "#EDEDEF"),
	)

	private def unscopedSettings = Seq(
		optdocMakeReplacementFontawesomeCss := {
			sbt.IO.createDirectory(target.value / "docopt")
			val outFile = target.value / "docopt" / "fontawesome.css"
			val clockLines = {
				val icon = "clock"
				for {
					theme <- Seq("light", "dark");
					themeClass = if (theme == "light") {""} else {s".theme-$theme "};
					line <- Seq(
						s"${themeClass}.fas.fa-${icon}::before {",
						s"  content: url('../images/fontawesome/${icon}/${theme}/default.svg')",
						"}"
					)
				} yield line
			}
			val playLines = {
				val icon = "play"
				for {
					theme <- Seq("light", "dark");
					themeClass = if (theme == "light") {""} else {s".theme-$theme "};
					(state, stateClasses, _, _) <- states;
					stateClass <- stateClasses;
					line <- Seq(
						s"${themeClass}.icon-button.run-button${stateClass}::before {",
						s"  content: url('../images/fontawesome/${icon}/${theme}/${state}.svg')",
						"}"
					)
				} yield line
			}

			sbt.io.IO.writeLines(outFile, playLines ++ clockLines)
			outFile -> "styles/fontawesome.css"
		},
		optdocMakeReplacementFontawesomeIcons := {
			val outDir = target.value / "docopt" / "icons"
			sbt.IO.createDirectory(outDir)

			for {
				icon <- Seq("play", "clock");
				theme <- Seq("light", "dark");
				(state, _, lightColor, darkColor) <- if (icon == "play") {states} else {states.take(1)}
			} yield {
				val color = if (theme == "light") {lightColor} else {darkColor}
				val lines = icon match {
					case "play" => Seq(
						s"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512" width="11.5" height="15.333" fill="${color}">""",
						"<!--!Font Awesome Free 6.7.2 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2025 Fonticons, Inc.-->",
						"""<path d="M73 39c-14.8-9.1-33.4-9.4-48.5-.9S0 62.6 0 80L0 432c0 17.4 9.4 33.4 24.5 41.9s33.7 8.1 48.5-.9L361 297c14.3-8.7 23-24.2 23-41s-8.7-32.2-23-41L73 39z"/>""",
						"</svg>",
					)
					case "clock" => Seq(
						s"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512" width="13" height="13" fill="${color}">""",
						"<!--!Font Awesome Free 6.7.2 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license/free Copyright 2025 Fonticons, Inc.-->",
						"""<path d="M256 0a256 256 0 1 1 0 512A256 256 0 1 1 256 0zM232 120l0 136c0 8 4 15.5 10.7 20l96 64c11 7.4 25.9 4.4 33.3-6.7s4.4-25.9-6.7-33.3L280 243.2 280 120c0-13.3-10.7-24-24-24s-24 10.7-24 24z"/>""",
						"</svg>",
					)
				}
				val outFile = outDir / s"$icon-$theme-$state.svg"
				sbt.io.IO.writeLines(outFile, lines)
				outFile -> s"images/fontawesome/${icon}/${theme}/${state}.svg"
			}
		},
	)

	private def perScopeSettings(config:sbt.librarymanagement.Configuration) = Seq(
		config / packageDoc / mappings := {
			scalaBinaryVersion.value match {
				case "3" =>
					(config / packageDoc / mappings).value
						.filterNot({filePath =>
							import java.io.File.separator
							val path = filePath._2
							path.startsWith(s"images${separator}icon-buttons${separator}twitter${separator}") ||
							path.startsWith(s"images${separator}twitter-icon-") ||
							path.startsWith(s"images${separator}icon-buttons${separator}gitter${separator}") ||
							path.startsWith(s"images${separator}gitter-icon-") ||
							path.startsWith(s"styles${separator}fontawesome.css") ||
							path.startsWith(s"webfonts${separator}fa-") ||
							false
						})
						.:+(optdocMakeReplacementFontawesomeCss.value)
						.++(optdocMakeReplacementFontawesomeIcons.value)
				case _ =>
					(config / packageDoc / mappings).value
			}
		},
	)

	override lazy val projectSettings = unscopedSettings ++ perScopeSettings(Compile) ++ perScopeSettings(Test)
}

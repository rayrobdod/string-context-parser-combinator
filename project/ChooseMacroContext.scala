package com.rayrobdod.stringContextParserCombinator

import sbt._
import sbt.Keys._
import sbt.nio.Keys._
import java.nio.charset.StandardCharsets.UTF_8
import scala.collection.immutable.{Seq => ISeq}

/**
 * Changes a scala file to use `scala.reflect.macros.Context` instead of
 * `scala.reflect.macros.blackbox.Context`, for Macro-related classes whose
 * only difference between 2.10 and 2.11+ is the Context that they use.
 */
object ChooseMacroContext extends AutoPlugin {
	object autoImport {
		val chooseMacroContext = taskKey[Seq[File]]("")
	}
	import autoImport._
	override def requires = sbt.plugins.JvmPlugin

	private[this] def foldOnTwoTen[A, Z](version:String)(`if`:Function0[Z], `else`:Function0[Z]) = version match {
		case "2.10" => `if`.apply()
		case _ => `else`.apply()
	}

	private[this] def compile1(input:File, outFolder:File):(File, Seq[String]) = {
		val inFileName = input.toPath.getFileName.toString
		val inLines = sbt.io.IO.readLines(input, UTF_8)
		val outLines = inLines.map(_ match {
			case "import scala.reflect.macros.blackbox.Context" => "import scala.reflect.macros.Context"
			case "import scala.reflect.macros.blackbox.Universe" => "import scala.reflect.macros.Universe"
			case x => x
		})
		(outFolder / inFileName, outLines)
	}
	private[this] def compile(inputs:Seq[File], outFolder:File):ISeq[File] = {
		val result = inputs.map({input => compile1(input, outFolder)})
		result.foreach({x =>
			sbt.io.IO.createDirectory(x._1.getParentFile)
			sbt.io.IO.writeLines(x._1, x._2, UTF_8)
		})
		result.map(_._1).toList
	}

	lazy val baseSettings = Seq(
		chooseMacroContext / managedSourceDirectories := Seq.empty,
		chooseMacroContext / unmanagedSourceDirectories := foldOnTwoTen(scalaBinaryVersion.value)(
			{() => Seq((Compile / sourceDirectory).value / "scala-choose-context")},
			{() => Seq.empty},
		),
		unmanagedSourceDirectories ++= foldOnTwoTen(scalaBinaryVersion.value)(
			{() => Seq.empty},
			{() => Seq((Compile / sourceDirectory).value / "scala-choose-context")},
		),
		chooseMacroContext / fileInputs ++= (chooseMacroContext / unmanagedSourceDirectories).value.map(x => Glob(x, "*.scala")),
		chooseMacroContext / target := (crossTarget).value / "chooseMacroContext",
		chooseMacroContext := {
			val inputs = chooseMacroContext.inputFiles.map(_.toFile).toList
			val outFolder = (chooseMacroContext / target).value

			val cacheFactory = (chooseMacroContext / streams).value.cacheStoreFactory
			val inputHashes = inputs.map(x => FileInfo.hash(x))

			import sbt.util.CacheImplicits._
			val tracker = Tracked.inputChanged[(ISeq[java.io.File], java.io.File, Seq[HashFileInfo]), ISeq[File]](cacheFactory.make("input")) {
				case (changed:Boolean, params:(ISeq[java.io.File], java.io.File, Seq[HashFileInfo])) =>
				val tracker = Tracked.lastOutput[(ISeq[java.io.File], java.io.File, Seq[HashFileInfo]), ISeq[File]](cacheFactory.make("last")) {
					case (_, Some(out)) if !changed => out
					case (in, _) => compile(in._1, in._2)
				}
				tracker(params)
			}
			tracker((inputs, outFolder, inputHashes))
		},
		managedSourceDirectories += (chooseMacroContext / target).value,
		sourceGenerators += (chooseMacroContext).taskValue,
	)
	override lazy val projectSettings =
		inConfig(Compile)(baseSettings) ++
		Seq.empty
}

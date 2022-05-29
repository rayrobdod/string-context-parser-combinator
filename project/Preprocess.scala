package com.rayrobdod.stringContextParserCombinator
package gen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.collection.JavaConverters.{seqAsJavaListConverter, asScalaBufferConverter}
import sbt._
import sbt.Keys._
import sbt.librarymanagement.Configuration
import sbt.util.CacheImplicits._

/**
 * Performs some c-style processing on files in the `src/{config}/scala-preprocess` directory
 */
object Preprocess extends AutoPlugin {
	override def trigger = allRequirements
	override def requires = sbt.plugins.JvmPlugin

	object autoImport {
		val preprocess = taskKey[Seq[File]]("")
	}
	import autoImport._

	private def perScopeSettings(config:Configuration) = Seq(
		config / preprocess / sourceDirectory := (config / sourceDirectory).value / "scala-preprocess",
		config / preprocess / includeFilter := "*.preprocess",
		config / preprocess / excludeFilter := HiddenFileFilter,
		config / preprocess / target := (sourceManaged).value / "preprocess",
		config / preprocess / sources := {
			(config / preprocess / sourceDirectory).value **
				((config / preprocess / includeFilter).value --
				(config / preprocess / excludeFilter).value)
		}.get,
		config / preprocess := {
			val baseSrc = (config / preprocess / sourceDirectory).value.toPath
			val baseTar = (config / preprocess / target).value.toPath
			val files = (config / preprocess / sources).value.map{_.toPath}
			val srcDest = files.map{x => ((x, baseTar resolve (baseSrc relativize x)
					resolveSibling (x.getFileName.toString.reverse.split("\\.", 2)(1).reverse)))}

			val cacheFactory = (preprocess / streams).value.cacheStoreFactory
			val logger:sbt.util.Logger = (preprocess / streams).value.log

			srcDest.map({case (src,dest) =>
				java.nio.file.Files.createDirectories(dest.getParent)
				val srcHash = java.util.Base64.getUrlEncoder().encodeToString(sbt.io.Hash(src.toString))

				val tracker = Tracked.inputChanged[HashFileInfo, File](cacheFactory.make(s"input-$srcHash")) {
					case (changed:Boolean, params:HashFileInfo) =>
						val tracker = Tracked.lastOutput[HashFileInfo, File](cacheFactory.make(s"last-$srcHash")) {
							case (_, Some(out)) if !changed && out.exists() => out
							case (in, _) => {
								val srcLines = Files.readAllLines(src, UTF_8)
								val destLines = process(logger, src, srcLines.asScala, Defines(scalaVersion.value))
								Files.write(dest, destLines.asJava, UTF_8)
								dest.toFile
							}
						}
						tracker(params)
				}
				tracker(FileInfo.hash(src.toFile))
			})
		},
		config / sourceGenerators += (config / preprocess).taskValue,
		config / managedSourceDirectories += (config / preprocess / target).value,
	)

	override lazy val projectSettings = perScopeSettings(Compile) ++ perScopeSettings(Test)

	final case class Defines(scalaVersion:String)
	final case class Source(file:Path, lineNo:Int) {
		def logString:String = s"${file}:${lineNo}:1:"
	}

	sealed trait ProcessState {
		def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState)
	}
	object ProcessState {
		object Unconditional extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					val line2 = line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim
					if (line2.startsWith("if ")) {
						val line3 = line2.substring(3).replace("\\s+", " ").trim
						line3 match {
							case "scala=2.x" => ("", ProcessState.If("2." == defines.scalaVersion.substring(0,2)))
							case "scala=3.x" => ("", ProcessState.If("3." == defines.scalaVersion.substring(0,2)))
							case _ => {
								log.error(s"${source.logString} unknown condition: ${line3}")
								("", this)
							}
						}
					} else {
						log.error(s"${source.logString} unexpected preprocessor command in unconditional section: ${line2}")
						("", this)
					}
				} else {
					(line, this)
				}
			}
		}
		final case class If(active:Boolean) extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim match {
						case "else" => ("", Else(active))
						case "endif" => ("", Unconditional)
						case line2 => {
							log.error(s"${source.logString} unexpected preprocessor command in if section: ${line2}")
							("", this)
						}
					}
				} else {
					(if (active) {line} else {""}, this)
				}
			}
		}
		final case class Else(active:Boolean) extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim match {
						case "endif" => ("", Unconditional)
						case line2 => {
							log.error(s"${source.logString} unexpected preprocessor command in else section: ${line2}")
							("", this)
						}
					}
				} else {
					(if (! active) {line} else {""}, this)
				}
			}
		}
	}

	def process(log:sbt.util.Logger, srcPath:Path, src:Seq[String], defines:Defines):Seq[String] = {
		val (result, endState) = src.zipWithIndex.foldLeft[(List[String], ProcessState)]((Nil, ProcessState.Unconditional))({(folding, line) =>
			val (previousLines, state) = folding
			val (lineStr, lineNo) = line

			val (newLine, newState) = state.handle(log, defines, Source(srcPath, lineNo), lineStr)
			(newLine :: previousLines, newState)
		})
		endState match {
			case ProcessState.Unconditional => {}
			case _ => log.error(s"${Source(srcPath, 0).logString} if not terminated by endif")
		}
		result.reverse
	}
}

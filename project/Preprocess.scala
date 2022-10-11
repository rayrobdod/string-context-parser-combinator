package com.rayrobdod.stringContextParserCombinator
package gen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.collection.JavaConverters.{seqAsJavaListConverter, asScalaBufferConverter}
import sbt._
import sbt.Keys._
import sbt.librarymanagement.Configuration
import sbt.nio.Keys._
import sbt.util.CacheImplicits._

/**
 * Performs some c-style processing on files in the `src/{config}/scala-preprocess` directory
 */
object Preprocess extends AutoPlugin {
	override def trigger = allRequirements
	override def requires = sbt.plugins.JvmPlugin

	object autoImport {
		val preprocess = taskKey[Seq[File]]("Applies c-style preprocessing to scala sources")
	}
	import autoImport._

	private def perScopeSettings(config:Configuration) = Seq(
		config / preprocess / sourceDirectory := (config / sourceDirectory).value / "scala-preprocess",
		config / preprocess / target := (sourceManaged).value / "preprocess",
		config / preprocess / fileInputs := Seq((config / preprocess / sourceDirectory).value.toGlob / "**" / "*.preprocess"),
		config / preprocess := {
			val baseSrc = (config / preprocess / sourceDirectory).value.toPath
			val baseTar = (config / preprocess / target).value.toPath
			val files = (config / preprocess / allInputFiles).value
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
		config / packageSrc / mappings ++= {
			val files = (config / preprocess).value
			val dir = (config / preprocess / target).value
			val mapper = sbt.io.Path.relativeTo(dir)
			files.map(f => ((f, mapper(f).getOrElse(f.toString))))
		}
	)

	override lazy val projectSettings = perScopeSettings(Compile) ++ perScopeSettings(Test)

	/** Information that can be used by an `#if` to determine whether to echo a section */
	final case class Defines(scalaVersion:String)

	/** Represents the source location of a line */
	final case class Source(file:Path, lineNo:Int) {
		def logString:String = s"${file}:${lineNo}:1:"
	}

	sealed trait ProcessState {
		/**
		 * Process a line of a source.
		 */
		def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState)
	}
	object ProcessState {
		private val IF_COMMAND = "if (.+)".r

		/** Determines whether the `condition` string is matched by the data in `defines` */
		private def handleIf(log:sbt.util.Logger, defines:Defines, source:Source, condition:String, stack:ProcessState):ProcessState = {
			condition match {
				case "scala=2.x" => ProcessState.If("2." == defines.scalaVersion.substring(0,2), stack)
				case "scala=2.11.x" => ProcessState.If("2.11." == defines.scalaVersion.substring(0,5), stack)
				case "scala=2.12.x" => ProcessState.If("2.12." == defines.scalaVersion.substring(0,5), stack)
				case "scala=2.13.x" => ProcessState.If("2.13." == defines.scalaVersion.substring(0,5), stack)
				case "scala=3.x" => ProcessState.If("3." == defines.scalaVersion.substring(0,2), stack)
				case _ => {
					log.error(s"${source.logString} unknown condition: ${condition}")
					stack
				}
			}
		}

		/** Outside of all if or else sections. Echoes all lines */
		object Unconditional extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					val line2 = line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim
					if (line2.startsWith("if ")) {
						val line3 = line2.substring(3).replace("\\s+", " ").trim
						("", ProcessState.handleIf(log, defines, source, line3, this))
					} else {
						log.error(s"${source.logString} unexpected preprocessor command in unconditional section: ${line2}")
						("", this)
					}
				} else {
					(line, this)
				}
			}
		}

		/**
		 * An if section. `active` is true if the lines in this section should be echoed.
		 * stack is the state to return to upon an endif.
		 */
		final case class If(active:Boolean, stack:ProcessState) extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim match {
						case IF_COMMAND(condition) if active => ("", ProcessState.handleIf(log, defines, source, condition, this))
						case IF_COMMAND(_) => ("", IfChildOfInactive(this))
						case "else" => ("", Else(!active, stack))
						case "endif" => ("", stack)
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

		/**
		 * An else section. `active` is true if the lines in this section should be echoed.
		 * stack is the state to return to upon an endif.
		 */
		final case class Else(active:Boolean, stack:ProcessState) extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim match {
						case IF_COMMAND(condition) if active => ("", ProcessState.handleIf(log, defines, source, condition, this))
						case IF_COMMAND(_) => ("", IfChildOfInactive(this))
						case "endif" => ("", stack)
						case line2 => {
							log.error(s"${source.logString} unexpected preprocessor command in else section: ${line2}")
							("", this)
						}
					}
				} else {
					(if (active) {line} else {""}, this)
				}
			}
		}

		/**
		 * An if section that is inside an if/else that is not active. Not echoed, since the outer
		 * `if` is not echoed. stack is the state to return to upon an endif.
		 */
		final case class IfChildOfInactive(stack:ProcessState) extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim match {
						case IF_COMMAND(condition) => ("", IfChildOfInactive(this))
						case "else" => ("", ElseChildOfInactive(stack))
						case "endif" => ("", stack)
						case line2 => {
							log.error(s"${source.logString} unexpected preprocessor command in if section: ${line2}")
							("", this)
						}
					}
				} else {
					("", this)
				}
			}
		}

		/**
		 * An else section that is inside an if/else that is not active. Not echoed, since the outer
		 * `if` is not echoed. stack is the state to return to upon an endif.
		 */
		final case class ElseChildOfInactive(stack:ProcessState) extends ProcessState {
			override def handle(log:sbt.util.Logger, defines:Defines, source:Source, line:String):(String, ProcessState) = {
				if (line.trim.startsWith("#")) {
					line.trim.substring(1).replaceAll("/\\*.+\\*/", "").trim match {
						case IF_COMMAND(condition) => ("", IfChildOfInactive(this))
						case "endif" => ("", stack)
						case line2 => {
							log.error(s"${source.logString} unexpected preprocessor command in if section: ${line2}")
							("", this)
						}
					}
				} else {
					("", this)
				}
			}
		}
	}

	/**
	 * Process a single source file. Returns a sequence of lines that should be written to the result file.
	 */
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

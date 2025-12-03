package com.wavesplatform.lang

import com.google.common.io
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

import java.io.File
import java.nio.charset.StandardCharsets

object FileCompiler {
  private val estimator = ScriptEstimatorV3.latest

  def main(args: Array[String]): Unit = args
    .foreach { path =>
      val scriptFile = new File(path).getAbsoluteFile
      require(scriptFile.isFile, s"$path is not a file")
      val baseDirectory = scriptFile.getParentFile
      val imports = baseDirectory
        .listFiles({ (pathname: File) =>
          pathname.isFile && pathname.getAbsoluteFile != scriptFile
        })
        .map { f =>
          f.getName -> io.Files.asCharSource(f, StandardCharsets.UTF_8).read()
        }
        .toMap

      API
        .compile(io.Files.asCharSource(scriptFile, StandardCharsets.UTF_8).read(), estimator, libraries = imports)
        .fold(
          error => throw new RuntimeException(s"$error while compiling $path"),
          _ => println(s"successfully compiled $path")
        )
    }
}

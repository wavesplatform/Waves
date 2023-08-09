package com.wavesplatform.io

import java.nio.file.Path
import scala.jdk.CollectionConverters.IteratorHasAsScala

object PathUtils {
  def commonPath(path1: Path, path2: Path): Path =
    if (path1.getRoot == path2.getRoot) {
      val f1Components = path1.iterator().asScala.toSeq
      val f2Components = path2.iterator().asScala.toSeq
      val commonParts  = f1Components.zip(f2Components).takeWhile { case (left, right) => left == right }
      if (commonParts.isEmpty) path1.getRoot
      else {
        val shortest = if (f1Components.size <= f2Components.size) path1 else path2
        shortest.getRoot.resolve(shortest.subpath(0, commonParts.size))
      }
    } else path1.getFileSystem.getPath("")
}

package com.wavesplatform

import java.io.File

import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader

package object settings {
  implicit val optionalPathValueReader: ValueReader[Option[File]] =
    (config: Config, path: String) => config.getString(path).trim match {
      case "" => None
      case nonEmptyPath => Some(new File(nonEmptyPath))
    }
}

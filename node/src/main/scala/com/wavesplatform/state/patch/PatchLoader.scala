package com.wavesplatform.state.patch

import java.nio.file.{Files, Paths}

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json.{Json, Writes}

object PatchLoader extends ScorexLogging {
  def write[T: Writes](name: String, height: Int, value: T): Unit = {
    val chainId = AddressScheme.current.chainId.toChar
    val path = Paths.get(sys.props("user.home"), s"$name-$chainId-$height-patch.json")
    val json = Json.prettyPrint(Json.toJson(value))
    log.info(s"Patch $name at height $height saved to $path")
    Files.write(path, json.getBytes("UTF-8"))
  }
}

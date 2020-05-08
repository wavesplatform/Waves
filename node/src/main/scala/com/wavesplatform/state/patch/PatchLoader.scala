package com.wavesplatform.state.patch

import java.nio.file.{Files, Paths}

import com.wavesplatform.account.AddressScheme
import play.api.libs.json.{Json, Writes}

object PatchLoader {
  def write[T: Writes](name: String, height: Int, value: T): Unit = {
    val chainId = AddressScheme.current.chainId.toChar
    val home = Paths.get(sys.props("user.home"), s"$name-$chainId-$height-patch.json")
    val json = Json.prettyPrint(Json.toJson(value))
    Files.write(home, json.getBytes("UTF-8"))
  }
}

package com.wavesplatform.state.patch

import com.google.common.io.Resources
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.state.{Blockchain, Diff}
import play.api.libs.json.{Json, Reads}

trait PatchDataLoader {
  protected def readPatchData[T: Reads](): T =
    Json.parse(Resources.getResource(s"patches/$this-${AddressScheme.current.chainId.toChar}").openStream()).as[T]
}

trait DiffPatchFactory extends PartialFunction[Blockchain, Diff] with PatchDataLoader

abstract class PatchAtHeight(chainIdToHeight: (Char, Int)*) extends DiffPatchFactory {
  protected lazy val patchHeight: Option[Int] = chainIdToHeight.collectFirst {
    case (chainId, height) if AddressScheme.current.chainId == chainId.toByte => height
  }

  override def isDefinedAt(blockchain: Blockchain): Boolean = patchHeight.contains(blockchain.height)
}

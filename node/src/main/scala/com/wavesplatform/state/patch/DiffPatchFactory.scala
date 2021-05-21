package com.wavesplatform.state.patch

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.state.{Blockchain, Diff}
import play.api.libs.json.{Json, Reads}

import scala.io.Source

trait PatchDataLoader extends {
  protected def readPatchData[T: Reads](): T =
    Json
      .parse(
        Source
          .fromResource(s"patches/${getClass.getSimpleName.replace("$", "")}-${AddressScheme.current.chainId.toChar}.json")
          .mkString
      )
      .as[T]
}

trait DiffPatchFactory extends PartialFunction[Blockchain, Diff]

abstract class PatchAtHeight(chainIdToHeight: (Char, Int)*) extends PatchDataLoader with DiffPatchFactory {
  protected lazy val patchHeight: Option[Int] = chainIdToHeight.collectFirst {
    case (chainId, height) if AddressScheme.current.chainId == chainId.toByte => height
  }

  override def isDefinedAt(blockchain: Blockchain): Boolean = patchHeight.contains(blockchain.height)
}

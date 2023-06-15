package com.wavesplatform.state.patch

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import play.api.libs.json.{Json, Reads}

import scala.io.Source

trait PatchDataLoader {
  protected def readPatchData[T: Reads](): T =
    Json
      .parse(
        Source
          .fromResource(s"patches/${getClass.getSimpleName.replace("$", "")}-${AddressScheme.current.chainId.toChar}.json")
          .mkString
      )
      .as[T]
}

trait DiffPatchFactory extends PartialFunction[Blockchain, StateSnapshot]

abstract class PatchAtHeight(chainIdToHeight: (Char, Int)*) extends PatchDataLoader with DiffPatchFactory {
  private[this] val chainIdToHeightMap   = chainIdToHeight.toMap
  protected def patchHeight: Option[Int] = chainIdToHeightMap.get(AddressScheme.current.chainId.toChar)

  override def isDefinedAt(blockchain: Blockchain): Boolean =
    chainIdToHeightMap.get(blockchain.settings.addressSchemeCharacter).contains(blockchain.height)
}

abstract class PatchOnFeature(feature: BlockchainFeature, networks: Set[Char] = Set.empty) extends PatchDataLoader with DiffPatchFactory {
  override def isDefinedAt(blockchain: Blockchain): Boolean = {
    (networks.isEmpty || networks.contains(blockchain.settings.addressSchemeCharacter)) &&
    blockchain.featureActivationHeight(feature.id).contains(blockchain.height)
  }
}

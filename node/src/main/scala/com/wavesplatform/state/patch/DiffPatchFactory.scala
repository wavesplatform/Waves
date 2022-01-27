package com.wavesplatform.state.patch

import scala.io.Source

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.BlockchainFeature
import com.wavesplatform.state.{Blockchain, Diff}
import play.api.libs.json.{Json, Reads}

trait PatchDataLoader extends {
  protected def readPatchData[T: Reads](chainId: Char = AddressScheme.current.chainId.toChar): T =
    Json
      .parse(
        Source
          .fromResource(s"patches/${getClass.getSimpleName.replace("$", "")}-$chainId.json")
          .mkString
      )
      .as[T]
}

trait DiffPatchFactory extends PartialFunction[Blockchain, Diff]

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

package com.wavesplatform.database.patch

import cats.instances.map._
import cats.syntax.monoid._
import com.google.common.primitives.{Bytes, Ints, Longs, Shorts}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.database.{Keys, LevelDBWriter, RW, ReadOnlyDB}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.AuthorizedTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.utils.ScorexLogging

import scala.collection.mutable

private[database] object ResetInvalidSponsoredInvokes extends ScorexLogging {
  val patchId: Short = 1

  def apply(
      blockchain: LevelDBWriter,
      db: RW,
      assetBalances: Map[BigInt, Map[IssuedAsset, Long]],
      newAddresses: Map[Address, BigInt]
  ): Map[BigInt, Map[IssuedAsset, Long]] =
    if (blockchain.settings.functionalitySettings.resetInvalidInvokeSponsoredFeeHeight <= 0) Map.empty
    else {
      log.info("Collecting invalid sponsored invokes")
      val pfs = createPortfolios(blockchain, db)
      log.info(s"Collected ${pfs.size} deltas: $pfs")

      val diffs = for {
        (address, pf)  <- pfs.toSeq
        (asset, delta) <- pf.assets
      } yield (address, asset, delta)

      diffs.foldLeft(assetBalances) {
        case (map, (address, asset, delta)) =>
          val addressId =
            newAddresses.get(address).orElse(db.get(Keys.addressId(address))).getOrElse(throw new IllegalArgumentException("Address id not found"))
          val addressAssets   = map.getOrElse(addressId, Map.empty)
          val assetBalance    = addressAssets.getOrElse(asset, blockchain.balance(address, asset))
          val newAssetBalance = math.max(assetBalance + delta, 0L)
          // require(newAssetBalance >= 0, s"Negative balance on $address after $this patch")
          map + (addressId -> (addressAssets + (asset -> newAssetBalance)))
      }
    }

  private[this] def createPortfolios(blockchain: LevelDBWriter, db: ReadOnlyDB): Map[Address, Portfolio] = {
    def getIssuer(asset: IssuedAsset): Address =
      blockchain.assetDescription(asset).getOrElse(throw new IllegalArgumentException(s"Asset not found: $asset")).issuer.toAddress

    val sponsoredAssets = mutable.Set.empty[IssuedAsset]
    db.iterateOver(36.toShort) { e => // Keys.sponsorship
      val assetId = IssuedAsset(e.getKey.drop(6))
      val sp      = Longs.fromByteArray(e.getValue)
      if (sp == 0) sponsoredAssets -= assetId
      else sponsoredAssets += assetId
    }

    val sponsoredByIssuer = sponsoredAssets.toVector
      .map(asset => (getIssuer(asset), asset))
      .groupBy(_._1)
      .mapValues(_.map(_._2).toSet)
      .withDefaultValue(Set.empty)

    def feeInSelfSponsoredAsset(tx: AuthorizedTransaction): Boolean = tx.assetFee match {
      case (ia: IssuedAsset, _) => sponsoredByIssuer(tx.sender.toAddress).contains(ia)
      case _                    => false
    }

    var portfolios = Map.empty[Address, Portfolio]

    val r4dHeight = if (blockchain.settings.addressSchemeCharacter == 'W') 1945646 else 1
    db.iterateOver(Shorts.toByteArray(Keys.TransactionInfoPrefix), Ints.toByteArray(r4dHeight)) { e =>
      val transactionBytes = e.getValue
      val isInvokeScript   = transactionBytes.head == 0 && transactionBytes(1) == InvokeScriptTransaction.typeId
      lazy val hasSponsoredAssetFeeHeuristic = {
        val sender       = PublicKey(transactionBytes.slice(4, 36)).toAddress
        val senderAssets = sponsoredByIssuer(sender)
        senderAssets.exists(asset => Bytes.indexOf(transactionBytes, Bytes.concat(Array(1.toByte), asset.id)) != -1)
      }

      if (isInvokeScript && hasSponsoredAssetFeeHeuristic) {
        InvokeScriptTransaction
          .parseBytes(transactionBytes)
          .filter(feeInSelfSponsoredAsset)
          .foreach { tx =>
            val pf = Map(tx.sender.toAddress -> Portfolio(0, LeaseBalance.empty, Map(tx.feeAssetId.asInstanceOf[IssuedAsset] -> -tx.fee)))
            portfolios |+|= pf
          }
      }
    }

    val precomputed =
      if (blockchain.settings.addressSchemeCharacter != 'W') Map.empty[Address, Portfolio]
      else
        Portfolio.combineAllAsset(IssuedAsset(Base58.decode("FiKspxSpkpzT4pMUA9ccZkbJmVXTdu4JhFDXNNXr5noW")))(
          Address.fromString("3PGsboZa7nvTMcAhL8jzPtrXGjsgU8yKWeQ").right.get -> -1000000
        )

    portfolios |+| precomputed
  }
}

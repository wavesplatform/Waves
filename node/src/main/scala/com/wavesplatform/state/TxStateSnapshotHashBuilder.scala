package com.wavesplatform.state

import cats.implicits.catsSyntaxSemigroup
import cats.syntax.either.*
import com.google.common.primitives.{Ints, Longs, UnsignedBytes}
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.BlockDiffer.{CurrentBlockFeePart, maybeApplySponsorship}
import com.wavesplatform.state.diffs.TransactionDiffer
import com.wavesplatform.state.reader.SnapshotBlockchain
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import org.bouncycastle.crypto.digests.Blake2bDigest

import java.nio.charset.StandardCharsets
import scala.collection.mutable

object TxStateSnapshotHashBuilder {
  private implicit val ByteArrayOrdering: Ordering[Array[Byte]] = (x, y) => UnsignedBytes.lexicographicalComparator().compare(x, y)

  val InitStateHash: ByteStr = ByteStr(crypto.fastHash(""))

  final case class Result(txStateSnapshotHash: ByteStr) {
    def createHash(prevHash: ByteStr): ByteStr =
      TxStateSnapshotHashBuilder.createHash(Seq(prevHash.arr, txStateSnapshotHash.arr))
  }

  case class TxStatusInfo(id: ByteStr, status: TxMeta.Status)

  def createHashFromSnapshot(snapshot: StateSnapshot, txStatusOpt: Option[TxStatusInfo]): Result = {
    val changedKeys = mutable.SortedSet.empty[Array[Byte]]

    snapshot.balances.foreach { case ((address, asset), balance) =>
      asset match {
        case Waves              => changedKeys += address.bytes ++ Longs.toByteArray(balance)
        case asset: IssuedAsset => changedKeys += address.bytes ++ asset.id.arr ++ Longs.toByteArray(balance)
      }
    }

    snapshot.leaseBalances.foreach { case (address, balance) =>
      changedKeys += address.bytes ++ Longs.toByteArray(balance.in) ++ Longs.toByteArray(balance.out)
    }

    for {
      (address, data) <- snapshot.accountData
      entry           <- data.values
    } changedKeys += address.bytes ++ entry.key.getBytes(StandardCharsets.UTF_8) ++ entry.valueBytes

    snapshot.aliases.foreach { case (alias, address) =>
      changedKeys += address.bytes ++ alias.name.getBytes(StandardCharsets.UTF_8)
    }

    snapshot.accountScriptsByAddress.foreach { case (address, sv) =>
      changedKeys += address.bytes ++ (sv match {
        case Some(s) => s.script.bytes().arr ++ s.publicKey.arr ++ Longs.toByteArray(s.verifierComplexity)
        case None    => Array.emptyByteArray
      })
    }

    for {
      (asset, scriptInfo) <- snapshot.assetScripts
    } changedKeys += asset.id.arr ++ scriptInfo.script.bytes().arr

    snapshot.leaseStates.foreach { case (leaseId, details) =>
      changedKeys += leaseId.arr ++ booleanToBytes(details.isActive)
      if (details.isActive) {
        changedKeys += leaseId.arr ++ details.sender.arr ++ details.recipientAddress.bytes ++ Longs.toByteArray(details.amount.value)
      }
    }

    snapshot.sponsorships.foreach { case (asset, sponsorship) =>
      changedKeys += asset.id.arr ++ Longs.toByteArray(sponsorship.minFee)
    }

    snapshot.orderFills.foreach { case (orderId, fillInfo) =>
      changedKeys += orderId.arr ++ Longs.toByteArray(fillInfo.volume) ++ Longs.toByteArray(fillInfo.fee)
    }

    snapshot.assetStatics.foreach { case (asset, assetInfo) =>
      changedKeys += asset.id.arr ++ assetInfo.issuer.arr ++ Array(assetInfo.decimals.toByte) ++ booleanToBytes(assetInfo.nft)
    }

    snapshot.assetVolumes.foreach { case (asset, volume) =>
      changedKeys += asset.id.arr ++ booleanToBytes(volume.isReissuable) ++ volume.volume.toByteArray
    }

    snapshot.assetNamesAndDescriptions.foreach { case (asset, assetInfo) =>
      changedKeys += asset.id.arr ++
        assetInfo.name.toByteArray ++
        assetInfo.description.toByteArray ++
        Ints.toByteArray(assetInfo.lastUpdatedAt.toInt)
    }

    txStatusOpt.foreach(txInfo =>
      txInfo.status match {
        case Status.Failed    => changedKeys += txInfo.id.arr ++ Array(1: Byte)
        case Status.Elided    => changedKeys += txInfo.id.arr ++ Array(2: Byte)
        case Status.Succeeded =>
      }
    )

    Result(createHash(changedKeys))
  }

  def createGenesisStateHash(txs: Seq[GenesisTransaction]): ByteStr =
    ByteStr(
      txs
        .foldLeft(InitStateHash.arr -> Map.empty[Address, Long]) { case ((prevStateHash, balances), tx) =>
          val newBalance = balances.getOrElse(tx.recipient, 0L) + tx.amount.value
          val tsh =
            crypto.fastHash(tx.recipient.bytes ++ Longs.toByteArray(newBalance))
          val newStateHash = crypto.fastHash(prevStateHash ++ tsh)
          newStateHash -> balances.updated(tx.recipient, newBalance)
        }
        ._1
    )

  def computeStateHash(
      txs: Seq[Transaction],
      initStateHash: ByteStr,
      initSnapshot: StateSnapshot,
      signer: KeyPair,
      prevBlockTimestamp: Option[Long],
      currentBlockTimestamp: Long,
      isChallenging: Boolean,
      blockchain: Blockchain
  ): TracedResult[ValidationError, ByteStr] = {
    val txDiffer = TransactionDiffer(prevBlockTimestamp, currentBlockTimestamp) _

    txs
      .foldLeft[TracedResult[ValidationError, (ByteStr, StateSnapshot)]](TracedResult.wrapValue(initStateHash -> initSnapshot)) {
        case (TracedResult(Right((prevStateHash, accSnapshot)), _, _), tx) =>
          val accBlockchain  = SnapshotBlockchain(blockchain, accSnapshot)
          val txDifferResult = txDiffer(accBlockchain, tx)
          txDifferResult.resultE match {
            case Right(txSnapshot) =>
              val (feeAsset, feeAmount) =
                maybeApplySponsorship(accBlockchain, accBlockchain.height >= Sponsorship.sponsoredFeesSwitchHeight(blockchain), tx.assetFee)
              val minerPortfolio = Map(signer.toAddress -> Portfolio.build(feeAsset, feeAmount).multiply(CurrentBlockFeePart))

              val txSnapshotWithBalances = txSnapshot.addBalances(minerPortfolio, accBlockchain).explicitGet()
              val txInfo                 = txSnapshot.transactions.head._2
              val stateHash =
                TxStateSnapshotHashBuilder
                  .createHashFromSnapshot(txSnapshotWithBalances, Some(TxStatusInfo(txInfo.transaction.id(), txInfo.status)))
                  .createHash(prevStateHash)

              txDifferResult.copy(resultE = Right((stateHash, accSnapshot |+| txSnapshotWithBalances)))
            case Left(_) if isChallenging =>
              txDifferResult.copy(resultE =
                Right(
                  (
                    TxStateSnapshotHashBuilder
                      .createHashFromSnapshot(StateSnapshot.empty, Some(TxStatusInfo(tx.id(), TxMeta.Status.Elided)))
                      .createHash(prevStateHash),
                    accSnapshot.bindElidedTransaction(accBlockchain, tx)
                  )
                )
              )

            case Left(err) => txDifferResult.copy(resultE = err.asLeft[(ByteStr, StateSnapshot)])
          }
        case (err @ TracedResult(Left(_), _, _), _) => err
      }
      .map(_._1)
  }

  private def booleanToBytes(flag: Boolean): Array[Byte] =
    if (flag) Array(1: Byte) else Array(0: Byte)

  private def createHash(bs: Iterable[Array[Byte]]): ByteStr = {
    val digestFn: Blake2bDigest = new Blake2bDigest(crypto.DigestLength * 8)
    bs.foreach(bs => digestFn.update(bs, 0, bs.length))
    val result = new Array[Byte](crypto.DigestLength)
    digestFn.doFinal(result, 0)
    ByteStr(result)
  }

}

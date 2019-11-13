package com.wavesplatform.api
import com.google.common.primitives.{Longs, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.database.{DBExt, Keys, ReadOnlyDB}
import com.wavesplatform.state.{AddressId, Diff, Height, InvokeScriptResult, Portfolio, TransactionId}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Authorized, CreateAliasTransaction, Transaction}
import org.iq80.leveldb.DB

import scala.collection.mutable

package object common {
  def aliasesOfAddress(db: DB, maybeDiff: => Option[(Height, Diff)])(address: Address): Seq[(Height, CreateAliasTransaction)] = {
    val hijackedAliases = db.get(Keys.disabledAliases)
    addressTransactions(db, maybeDiff, address, None, Set(CreateAliasTransaction.typeId), Int.MaxValue, None)
      .collect {
        case (h, t: CreateAliasTransaction) if !hijackedAliases.contains(t.alias) => h -> t
      }
  }

  def nftList(
      db: DB,
      diff: Diff,
      balance: (Address, IssuedAsset) => Long,
      address: Address,
      count: Int,
      fromId: Option[ByteStr]
  ): Seq[IssueTransaction] = {
    val received = diff.portfolios.getOrElse(address, Portfolio.empty).assets.collect { case (aid, v) if v > 0 => aid }.toSet
    val issuedInDiff = diff.transactions
      .collect {
        case (tx: IssueTransaction, _) if tx.isNFT && received(IssuedAsset(tx.id())) => tx
      }
      .dropWhile(it => fromId.isDefined && !fromId.contains(it.id()))

    if (issuedInDiff.length >= count) issuedInDiff.take(count)
    else if (issuedInDiff.nonEmpty) {
      issuedInDiff ++ loadNftList(db, address, count - issuedInDiff.length, None, received, balance)
    } else {
      loadNftList(db, address, count, fromId, received, balance)
    }
  }

  def addressTransactions(
      db: DB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      count: Int,
      fromId: Option[ByteStr]
  ): Seq[(Height, Transaction)] = db.readOnly(ro => addressTransactions(ro, maybeDiff, subject, sender, types, count, fromId))

  def invokeScriptResults(
      db: DB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      count: Int,
      fromId: Option[ByteStr]
  ): Seq[(Height, Either[Transaction, (InvokeScriptTransaction, Option[InvokeScriptResult])])] = db.readOnly { ro =>
    addressTransactions(ro, maybeDiff, subject, sender, types, count, fromId)
      .map {
        case (height, ist: InvokeScriptTransaction) =>
          height -> Right(ist -> maybeDiff.flatMap(_._2.scriptResults.get(ist.id())).orElse(loadInvokeScriptResult(ro, ist.id())))
        case (height, tx) =>
          height -> Left(tx)
      }
  }

  def invokeScriptResults(db: DB, maybeDiff: Option[(Height, Diff)], txId: ByteStr): Option[(Height, InvokeScriptTransaction, InvokeScriptResult)] = {
    maybeDiff
      .flatMap {
        case (h, diff) =>
          diff
            .transactionMap()
            .get(txId)
            .collect {
              case (ist: InvokeScriptTransaction, _) => diff.scriptResults.get(txId).map(r => (h, ist, r))
            }
            .flatten
      }
      .orElse(db.readOnly { ro =>
        loadTransaction(ro, txId) match {
          case Some((h, ist: InvokeScriptTransaction)) => loadInvokeScriptResult(ro, txId).map(isr => (h, ist, isr))
          case _                                       => None
        }
      })
  }

  def portfolio(db: DB, address: Address, includeNFT: Boolean): Map[IssuedAsset, Long] = db.readOnly { ro =>
    ro.get(Keys.addressId(address)).fold(Map.empty[IssuedAsset, Long]) { addressId =>
      val addressIdBytes = addressId.toByteArray
      val offset         = 2 + addressIdBytes.length

      val balances = mutable.AnyRefMap.empty[IssuedAsset, Long]
      ro.iterateOver(Shorts.toByteArray(Keys.AssetBalancePrefix) ++ addressIdBytes) { e =>
        val asset = IssuedAsset(ByteStr(e.getKey.slice(offset, offset + crypto.DigestLength)))
        balances += asset -> Longs.fromByteArray(e.getValue)
      }

      if (includeNFT) {
        balances.filter(_._2 > 0).toMap
      } else {
        (for {
          (asset, balance) <- balances
          if balance > 0
          _ <- loadTransaction(ro, asset.id).collect { case (_, it: IssueTransaction) if !it.isNFT => it.id() }
        } yield asset -> balance).toMap
      }
    }
  }

  def activeLeases(db: DB, maybeDiff: Option[(Height, Diff)], address: Address): Seq[(Height, LeaseTransaction)] = db.readOnly { ro =>
    def isCancelled(id: ByteStr): Boolean = maybeDiff.exists(_._2.leaseState.get(id).contains(true))

    addressTransactions(db, maybeDiff, address, None, Set(LeaseTransaction.typeId), Int.MaxValue, None)
      .collect { case (h, lt: LeaseTransaction) if !isCancelled(lt.id()) => h -> lt }
  }

  private def addressTransactions(
      db: ReadOnlyDB,
      maybeDiff: Option[(Height, Diff)],
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      count: Int,
      fromId: Option[ByteStr]
  ): Seq[(Height, Transaction)] = {
    maybeDiff.map {
      case (height, diff) =>
        height -> diff.transactions.dropWhile { case (tx, _) => fromId.isDefined && !fromId.contains(tx.id()) }
    } match {
      case Some((height, matchingFromId)) if matchingFromId.nonEmpty =>
        val filteredFromDiff = matchingFromId
          .collect {
            case (tx: Authorized, addresses) if sender.forall(_ == tx.sender.toAddress) && addresses(subject) =>
              Height(height) -> tx
          }

        if (filteredFromDiff.length < count) {
          filteredFromDiff ++ loadAddressTransactions(db, subject, sender, types, count - filteredFromDiff.length, None)
        } else {
          filteredFromDiff.take(count)
        }
      case _ =>
        loadAddressTransactions(db, subject, sender, types, count, fromId)
    }
  }

  private def loadAddressTransactions(
      db: ReadOnlyDB,
      subject: Address,
      sender: Option[Address],
      types: Set[Transaction.Type],
      count: Int,
      fromId: Option[ByteStr]
  ): Seq[(Height, Transaction)] = {
    def matchesSender(tx: Transaction): Boolean =
      sender.forall(
        senderAddress =>
          tx match {
            case a: Authorized => a.sender.toAddress == senderAddress
            case _             => false
          }
      )

    val txIds = (for {
      addressId <- db.get(Keys.addressId(subject)).view
      maxSeqNr = db.get(Keys.addressTransactionSeqNr(AddressId(addressId)))
      seqNr       <- (maxSeqNr to 1 by -1).view
      (h, txNums) <- db.get(Keys.addressTransactionHN(AddressId(addressId), seqNr)).view
      txNum       <- txNums
    } yield h -> txNum).dropWhile { case (_, (_, _, id)) => fromId.isDefined && !fromId.contains(id) }

    (for {
      (h, (txType, txNum, _)) <- txIds
      if types.isEmpty || types.contains(txType)
      tx <- db.get(Keys.transactionAt(h, txNum))
      if matchesSender(tx)
    } yield h -> tx).take(count).toSeq
  }

  private def loadNftList(
      writableDB: DB,
      address: Address,
      count: Int,
      fromId: Option[ByteStr],
      inclusions: Set[IssuedAsset],
      balance: (Address, IssuedAsset) => Long
  ): Seq[IssueTransaction] = writableDB.readOnly { db =>
    val includedTransactions = inclusions.view.flatMap(asset => loadTransaction(db, asset.id)).map(_._2)

    val transactions = for {
      addressId <- db.get(Keys.addressId(address)).toSeq.view
      assetList = db.get(Keys.assetList(addressId)).view.collect {
        case ia if balance(address, ia) > 0 && !inclusions(ia) => ia.id
      }
      id      <- assetList
      (_, tx) <- loadTransaction(db, id)
    } yield tx

    (includedTransactions ++ transactions)
      .dropWhile(t => fromId.isDefined && !fromId.contains(t.id()))
      .collect { case t: IssueTransaction if t.isNFT => t }
      .take(count)
      .toSeq
  }

  private def loadTransaction(db: ReadOnlyDB, id: ByteStr): Option[(Height, Transaction)] =
    for {
      (h, n) <- db.get(Keys.transactionHNById(TransactionId(id)))
      tx     <- db.get(Keys.transactionAt(h, n))
    } yield h -> tx

  private def loadInvokeScriptResult(db: ReadOnlyDB, id: ByteStr): Option[InvokeScriptResult] =
    for {
      (h, txNum) <- db.get(Keys.transactionHNById(TransactionId(id)))
    } yield db.get(Keys.invokeScriptResult(h, txNum))
}

package com.wavesplatform.api
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, Keys, ReadOnlyDB}
import com.wavesplatform.state.{AddressId, Diff, Height, Portfolio, TransactionId}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{CreateAliasTransaction, Transaction}
import org.iq80.leveldb.DB

package object common {
  def addressTransactions(db: DB, maybeDiff: => Option[(Height, Diff)])(
      address: Address,
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
            case (tx, addresses) if addresses.contains(address) && (types.isEmpty || types.contains(tx.typeId)) =>
              Height(height) -> tx
          }

        if (filteredFromDiff.length < count) {
          filteredFromDiff ++ loadAddressTransactions(db, address, types, count - filteredFromDiff.length, None)
        } else {
          filteredFromDiff.take(count)
        }
      case _ =>
        loadAddressTransactions(db, address, types, count, fromId)
    }
  }

  private def loadAddressTransactions(
      writableDB: DB,
      address: Address,
      types: Set[Transaction.Type],
      count: Int,
      fromId: Option[ByteStr]
  ): Seq[(Height, Transaction)] =
    writableDB.readOnly { db =>
      val txIds = (for {
        addressId <- db.get(Keys.addressId(address)).view
        maxSeqNr = db.get(Keys.addressTransactionSeqNr(AddressId(addressId)))
        seqNr       <- (maxSeqNr to 1 by -1).view
        (h, txNums) <- db.get(Keys.addressTransactionHN(AddressId(addressId), seqNr)).view
        txNum       <- txNums
      } yield h -> txNum).dropWhile { case (_, (_, _, id)) => fromId.isDefined && !fromId.contains(id) }

      (for {
        (h, (txType, txNum, _)) <- txIds
        if types.isEmpty || types.contains(txType)
        tx <- db.get(Keys.transactionAt(h, txNum))
      } yield h -> tx).take(count).toSeq
    }

  def aliasesOfAddress(db: DB, maybeDiff: => Option[(Height, Diff)])(address: Address): Seq[(Height, CreateAliasTransaction)] = {
    val hijackedAliases = db.get(Keys.disabledAliases)
    addressTransactions(db, maybeDiff)(address, Set(CreateAliasTransaction.typeId), Int.MaxValue, None)
      .collect {
        case (h, t: CreateAliasTransaction) if !hijackedAliases.contains(t.alias) => h -> t
      }
  }

  def nftList(db: DB, maybeDiff: => Option[(Height, Diff)], balance: (Address, IssuedAsset) => Long)(
      address: Address,
      count: Int,
      fromId: Option[ByteStr]
  ): Seq[IssueTransaction] = maybeDiff match {
    case None => loadNftList(db, address, count, fromId, Set.empty, balance)
    case Some((_, diff)) =>
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

  private def loadTransactions(db: ReadOnlyDB, ids: Seq[ByteStr]) =
    for {
      id     <- ids.view
      (h, n) <- db.get(Keys.transactionHNById(TransactionId(id)))
      tx     <- db.get(Keys.transactionAt(h, n))
    } yield tx

  private def loadNftList(
      writableDB: DB,
      address: Address,
      count: Int,
      fromId: Option[ByteStr],
      inclusions: Set[IssuedAsset],
      balance: (Address, IssuedAsset) => Long
  ): Seq[IssueTransaction] = writableDB.readOnly { db =>
    val includedTransactions = loadTransactions(db, inclusions.map(_.id).toSeq)

    val transactions = for {
      addressId <- db.get(Keys.addressId(address)).toSeq.view
      assetList = db.get(Keys.assetList(addressId)).view.collect {
        case ia if balance(address, ia) > 0 && !inclusions(ia) => ia.id
      }
      tx <- loadTransactions(db, assetList)
    } yield tx

    val result = (includedTransactions ++ transactions)
      .dropWhile(t => fromId.isDefined && !fromId.contains(t.id()))
      .collect { case t: IssueTransaction if t.isNFT => t }
      .take(count)
      .force

    result
  }
}

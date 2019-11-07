package com.wavesplatform.api.common

import com.google.common.base.Charsets
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{DBExt, Keys}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.{Blockchain, BlockchainExt, DataEntry, Diff, Height}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import monix.reactive.Observable
import org.iq80.leveldb.DB

import scala.collection.mutable

class CommonAccountApi(diff: => Diff, db: DB, blockchain: Blockchain) {
  import CommonAccountApi._

  def balance(address: Address, confirmations: Int = 0): Long = {
    blockchain.balance(address, blockchain.height, confirmations)
  }

  def effectiveBalance(address: Address, confirmations: Int = 0): Long = {
    blockchain.effectiveBalance(address, confirmations)
  }

  def balanceDetails(address: Address): BalanceDetails = {
    val portfolio = blockchain.wavesPortfolio(address)
    BalanceDetails(
      portfolio.balance,
      blockchain.generatingBalance(address),
      portfolio.balance - portfolio.lease.out,
      portfolio.effectiveBalance,
      portfolio.lease.in,
      portfolio.lease.out
    )
  }

  def assetBalance(address: Address, asset: IssuedAsset): Long = blockchain.balance(address, asset)

  def portfolio(address: Address): Map[Asset, Long] = ???

  def nftPortfolio(address: Address, from: Option[IssuedAsset]): Observable[IssueTransaction] = ???

  def script(address: Address): AddressScriptInfo = {
    val script: Option[(Script, Long)] = blockchain.accountScript(address)

    AddressScriptInfo(
      script = script.map(_._1.bytes()),
      scriptText = script.map(_._1.expr.toString),
      complexity = script.map(_._2).getOrElse(0),
      extraFee = if (script.isEmpty) 0 else FeeValidation.ScriptExtraFee
    )
  }

  def data(address: Address, key: String): Option[DataEntry[_]] = {
    blockchain.accountData(address, key)
  }

  def dataStream(address: Address, regex: Option[String]): Observable[DataEntry[_]] = {
    val entriesFromDiff = diff.accountData.get(address).fold[Map[String, DataEntry[_]]](Map.empty)(_.data)
    val entries = mutable.ArrayBuffer[DataEntry[_]](entriesFromDiff.values.toSeq: _*)

    db.readOnly { ro =>
      val addressId = db.get(Keys.addressId(address)).get
      db.iterateOver(Keys.DataHistoryPrefix) { e =>
        val key = new String(e.getKey.drop(2), Charsets.UTF_8)
        if (regex.forall(_.r.pattern.matcher(key).matches()) && !entriesFromDiff.contains(key)) {
          ro.get(Keys.data(addressId, key)(ro.get(Keys.dataHistory(addressId, key)).head)).foreach(entries += _)
        }
      }
    }
    Observable.fromIterable(entries)
  }

  def activeLeases(address: Address): Observable[(Height, LeaseTransaction)] = ???
}

object CommonAccountApi {
  final case class BalanceDetails(regular: Long, generating: Long, available: Long, effective: Long, leaseIn: Long, leaseOut: Long)
  final case class AddressScriptInfo(script: Option[ByteStr], scriptText: Option[String], complexity: Long, extraFee: Long)
}

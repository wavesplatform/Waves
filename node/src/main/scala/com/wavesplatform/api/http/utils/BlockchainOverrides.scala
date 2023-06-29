package com.wavesplatform.api.http.utils

import com.wavesplatform.account.Address
import com.wavesplatform.api.http.utils.BlockchainOverrides.AccountOverrides
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset
import play.api.libs.json.*
import supertagged.TaggedType

// TODO move to another place?
case class BlockchainOverrides(accounts: Map[Address, AccountOverrides] = Map.empty) {
  def balance(address: Address, mayBeAssetId: Asset): Option[Long] = accounts.get(address).flatMap { account =>
    mayBeAssetId.fold(account.regularBalance)(account.assetBalances.get)
  }
}

object BlockchainOverrides {

  object Balance extends TaggedType[Long] {
    private val LongStringMaxLength = 20 // Long.MinValue.toString

    implicit val reads: Reads[Type] = Reads {
      case JsString(s) =>
        if (s.length > LongStringMaxLength) JsError("error.expected.numberdigitlimit")
        else
          s.toLongOption match {
            case Some(r) => JsSuccess(Balance(r))
            case None    => JsError(JsonValidationError("error.expected.numberformatexception"))
          }

      case JsNumber(d) =>
        if (d.isValidLong) JsSuccess(Balance(d.toLongExact))
        else JsError(JsonValidationError("error.invalid.long"))

      case _ => JsError(JsonValidationError("error.expected.jsnumberorjsstring"))
    }
  }

  type Balance = Balance.Type

  // TODO test errors, JsPath?
  implicit val accountsMapReads: Reads[Map[Address, AccountOverrides]] =
    Reads.mapReads[Address, AccountOverrides](x => Address.fromString(x).fold(e => JsError(s"Can't parse Address in accounts: $e"), JsSuccess(_)))

  implicit val reads: Reads[BlockchainOverrides] = Json.using[Json.WithDefaultValues].reads[BlockchainOverrides]

  case class AccountOverrides(assetBalances: Map[IssuedAsset, Balance] = Map.empty, regularBalance: Option[Balance] = None)

  object AccountOverrides {
    implicit val assetBalancesMapReads: Reads[Map[IssuedAsset, Balance]] =
      Reads.mapReads[IssuedAsset, Balance](x => Asset.assetReads.reads(JsString(x)))

    implicit val reads: Reads[AccountOverrides] = Json.using[Json.WithDefaultValues].reads[AccountOverrides]
  }
}

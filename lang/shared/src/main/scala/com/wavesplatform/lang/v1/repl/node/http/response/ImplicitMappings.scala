package com.wavesplatform.lang.v1.repl.node.http.response

import com.wavesplatform.lang.v1.repl.node.http.response.model._
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, ScriptAssetInfo, Tx}

case class ImplicitMappings(chainId: Byte) {
  private val chainDependentMapper = new ChainDependentMapper(chainId)

  implicit val heightM: HeightResponse => Long =
    _.height

  implicit val transferTxM: TransferTransaction => Tx =
    chainDependentMapper.toRideModel

  implicit val assetInfoM: AssetInfoResponse => ScriptAssetInfo =
    chainDependentMapper.toRideModel

  implicit val blockInfoM: BlockInfoResponse => BlockInfo =
    chainDependentMapper.toRideModel

  implicit val dataEntryM: DataEntry => Any =
    _.value

  implicit val addressM: AddressResponse => Address =
    a => Address(a.address.byteStr)

  implicit val balanceM: BalanceResponse => Long =
    _.balance
}

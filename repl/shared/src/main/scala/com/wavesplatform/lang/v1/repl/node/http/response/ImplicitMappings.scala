package com.wavesplatform.lang.v1.repl.node.http.response

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.repl.node.http.response.model.*
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, ScriptAssetInfo, Tx}

case class ImplicitMappings(chainId: Byte) {
  private val chainDependentMapper = new ChainDependentMapper(chainId)

  implicit val heightO: HeightResponse => Option[Long] =
    (r: HeightResponse) =>
      if (r.succeed) {
        Some(r.height)
      } else {
        None
      }

  implicit val heightM: HeightResponse => Long =
    _.height

  implicit val transferTxO: TransferTransaction => Option[Tx.Transfer] =
    chainDependentMapper.toRideModelO

  implicit val transferTxM: TransferTransaction => Tx.Transfer =
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

  implicit val balancesM: List[BalanceResponse] => Long =
    _.headOption.fold(0L)(_.balance)

  implicit val addressFromString: String => Either[String, Address] =
    chainDependentMapper.addressFromString

  implicit val addressFromPublicKey: ByteStr => Either[String, Address] =
    b => Right(Address(chainDependentMapper.pkToAddress(ByteString(b.arr))))
}

package com.wavesplatform.transaction.assets.exchange

import java.nio.ByteBuffer
import java.util

import scala.jdk.CollectionConverters._

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import org.bouncycastle.util.encoders.Hex
import org.web3j.abi.datatypes.generated.Bytes32
import org.web3j.crypto.Sign.SignatureData
import org.web3j.crypto.StructuredDataEncoder

object EthOrders {
  private[this] lazy val encoder = new StructuredDataEncoder(orderDomainJson)

  def encodeAsset(asset: Asset): String = asset match {
    case IssuedAsset(id) => Hex.toHexString(id.arr)
    case Waves           => Hex.toHexString(Bytes32.DEFAULT.getValue)
  }

  def encodeOrderType(orderType: OrderType): Boolean = orderType match {
    case OrderType.BUY  => false
    case OrderType.SELL => true
  }

  def encodeAsEthStruct(order: Order): Array[Byte] = {
    encoder.encodeData(
      "Order",
      new util.HashMap(
        Map(
          "version"           -> order.version.toInt,
          "matcherPublicKey"  -> Hex.toHexString(order.matcherPublicKey.arr),
          "amountAsset"       -> encodeAsset(order.assetPair.amountAsset),
          "priceAsset"        -> encodeAsset(order.assetPair.priceAsset),
          "orderType"         -> encodeOrderType(order.orderType),
          "amount"            -> order.amount,
          "price"             -> order.price,
          "timestamp"         -> order.timestamp,
          "expiration"        -> order.expiration,
          "matcherFee"        -> order.matcherFee,
          "matcherFeeAssetId" -> encodeAsset(order.matcherFeeAssetId)
        ).asJava
      ).asInstanceOf[util.HashMap[String, AnyRef]]
    )
  }

  def decodeSignature(signature: Array[Byte]): SignatureData = {
    val buffer = ByteBuffer.wrap(signature)
    val R = new Array[Byte](64)
    val S = new Array[Byte](64)
    buffer.get(R) // 0-64
    buffer.get(S) // 64-128
    val V = buffer.get() // Last byte
    new SignatureData(V, R, S)
  }

  private[this] def orderDomainJson: String =
    s"""
      |{
      |  "types": {
      |    "EIP712Domain": [
      |      {
      |        "name": "name",
      |        "type": "string"
      |      },
      |      {
      |        "name": "version",
      |        "type": "string"
      |      },
      |      {
      |        "name": "chainId",
      |        "type": "uint256"
      |      },
      |      {
      |        "name": "verifyingContract",
      |        "type": "address"
      |      }
      |    ],
      |    "Order": [
      |      {
      |        "name": "version",
      |        "type": "int32"
      |      },
      |      {
      |        "name": "senderPublicKey",
      |        "type": "bytes32"
      |      },
      |      {
      |        "name": "matcherPublicKey",
      |        "type": "bytes32"
      |      },
      |      {
      |        "name": "amountAsset",
      |        "type": "bytes32"
      |      },
      |      {
      |        "name": "priceAsset",
      |        "type": "bytes32"
      |      },
      |      {
      |        "name": "orderType",
      |        "type": "boolean"
      |      },
      |      {
      |        "name": "amount",
      |        "type": "int64"
      |      },
      |      {
      |        "name": "price",
      |        "type": "int64"
      |      },
      |      {
      |        "name": "timestamp",
      |        "type": "int64"
      |      },
      |      {
      |        "name": "expiration",
      |        "type": "int64"
      |      },
      |      {
      |        "name": "matcherFee",
      |        "type": "int64"
      |      },
      |      {
      |        "name": "matcherFeeAssetId",
      |        "type": "bytes32"
      |      }
      |    ]
      |  },
      |  "primaryType": "Order",
      |  "domain": {
      |    "name": "Waves Exchange",
      |    "version": "1",
      |    "chainId": ${AddressScheme.current.chainId},
      |    "verifyingContract": "0x${Hex.toHexString(Array.fill[Byte](32)(AddressScheme.current.chainId))}"
      |  },
      |  "message": {}
      |}
      |""".stripMargin
}

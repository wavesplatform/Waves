package com.wavesplatform.transaction.assets.exchange

import java.math.BigInteger
import java.nio.ByteBuffer
import java.util

import scala.jdk.CollectionConverters._

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import org.bouncycastle.util.encoders.Hex
import org.web3j.abi.datatypes.generated.Bytes32
import org.web3j.crypto.{ECDSASignature, Hash, Sign, StructuredDataEncoder}
import org.web3j.crypto.Sign.SignatureData

object EthOrders extends App {
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

  def recoverEthSignerKey(order: Order, signature: Array[Byte]): PublicKey = {
    val bytes = encodeAsEthStruct(order)
    recoverEthSignerKey(bytes, signature)
  }

  def recoverEthSignerKey(message: Array[Byte], signature: Array[Byte]): PublicKey = {
    val signatureData = EthOrders.decodeSignature(signature)
    val signerKey = Sign
      .recoverFromSignature(
        0,
        new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS)),
        Hash.sha3(message)
      )
      .toByteArray
      .takeRight(64)
    PublicKey(ByteStr(signerKey))
  }

  def decodeSignature(signature: Array[Byte]): SignatureData = {
    val buffer = ByteBuffer.wrap(signature)
    val paramSize = buffer.remaining() match {
      case 129 => 64
      case 65  => 32
      case _   => ???
    }
    val R = new Array[Byte](paramSize)
    val S = new Array[Byte](paramSize)
    buffer.get(R)
    buffer.get(S)
    val V = buffer.get()
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
      |    "verifyingContract": "0x${Hex.toHexString(Array.fill[Byte](20)(AddressScheme.current.chainId))}"
      |  },
      |  "message": {}
      |}
      |""".stripMargin
}

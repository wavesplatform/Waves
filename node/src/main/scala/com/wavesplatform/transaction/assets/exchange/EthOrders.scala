package com.wavesplatform.transaction.assets.exchange

import java.math.BigInteger
import java.nio.ByteBuffer

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import org.bouncycastle.util.encoders.Hex
import org.web3j.abi.datatypes.generated.Bytes32
import org.web3j.crypto.{ECDSASignature, Sign, StructuredDataEncoder}
import org.web3j.crypto.Sign.SignatureData
import org.web3j.utils.Numeric
import play.api.libs.json.{JsObject, Json}

object EthOrders extends App {
  def encodeAsset(asset: Asset): String = asset match {
    case IssuedAsset(id) => Numeric.toHexString(id.arr)
    case Waves           => Numeric.toHexString(Bytes32.DEFAULT.getValue)
  }

  def encodeOrderType(orderType: OrderType): Boolean = orderType match {
    case OrderType.BUY  => false
    case OrderType.SELL => true
  }

  def hashOrderStruct(order: Order): Array[Byte] = {
    val message = Json.obj(
      "version"           -> order.version.toInt,
      "matcherPublicKey"  -> Numeric.toHexString(order.matcherPublicKey.arr),
      "amountAsset"       -> encodeAsset(order.assetPair.amountAsset),
      "priceAsset"        -> encodeAsset(order.assetPair.priceAsset),
      "orderType"         -> encodeOrderType(order.orderType),
      "amount"            -> order.amount,
      "price"             -> order.price,
      "timestamp"         -> order.timestamp,
      "expiration"        -> order.expiration,
      "matcherFee"        -> order.matcherFee,
      "matcherFeeAssetId" -> encodeAsset(order.matcherFeeAssetId)
    )

    val json = Json.parse(orderDomainJson).as[JsObject] ++ Json.obj("message" -> message)
    val encoder = new StructuredDataEncoder(json.toString)
    encoder.hashStructuredData()
  }

  def recoverEthSignerKey(order: Order, signature: Array[Byte]): PublicKey = {
    val bytes = hashOrderStruct(order)
    recoverEthSignerKey(bytes, signature)
  }

  def recoverEthSignerKey(message: Array[Byte], signature: Array[Byte]): PublicKey = {
    val signatureData = EthOrders.decodeSignature(signature)
    val signerKey = Sign
      .recoverFromSignature(
        signatureData.getV.head - 27,
        new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS)),
        message
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
      |        "type": "bool"
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

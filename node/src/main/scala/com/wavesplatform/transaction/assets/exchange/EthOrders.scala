package com.wavesplatform.transaction.assets.exchange

import java.math.BigInteger
import java.nio.ByteBuffer
import com.google.common.base.CaseFormat
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import org.web3j.crypto.Sign.SignatureData
import org.web3j.crypto.{ECDSASignature, ECKeyPair, Sign, StructuredDataEncoder}
import play.api.libs.json.{JsObject, Json}

object EthOrders extends App {
  private[this] lazy val toSnakeCase = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.UPPER_UNDERSCORE)

  def toEip712Json(order: Order): JsObject = {
    def encodeAsset(asset: Asset): String = asset match {
      case IssuedAsset(id) => id.toString
      case Waves           => "WAVES"
    }

    def encodeOrderType(orderType: OrderType): String = orderType match {
      case OrderType.BUY  => "BUY"
      case OrderType.SELL => "SELL"
    }

    val priceMode = order.priceMode match {
      case OrderPriceMode.Default if order.version < Order.V4 => OrderPriceMode.AssetDecimals
      case OrderPriceMode.Default                             => OrderPriceMode.FixedDecimals
      case other                                              => other
    }

    val message = Json.obj(
      "version"           -> order.version.toInt,
      "matcherPublicKey"  -> order.matcherPublicKey.toString,
      "amountAsset"       -> encodeAsset(order.assetPair.amountAsset),
      "priceAsset"        -> encodeAsset(order.assetPair.priceAsset),
      "orderType"         -> encodeOrderType(order.orderType),
      "amount"            -> order.amount.value,
      "price"             -> order.price.value,
      "timestamp"         -> order.timestamp,
      "expiration"        -> order.expiration,
      "matcherFee"        -> order.matcherFee.value,
      "matcherFeeAssetId" -> encodeAsset(order.matcherFeeAssetId),
      "priceMode"         -> toSnakeCase.convert(priceMode.toString)
    )

    Json.parse(orderDomainJson).as[JsObject] ++ Json.obj("message" -> message)
  }

  def hashOrderStruct(order: Order): Array[Byte] = {
    val json    = toEip712Json(order)
    val encoder = new StructuredDataEncoder(json.toString)
    encoder.hashStructuredData()
  }

  def recoverEthSignerKey(order: Order, signature: Array[Byte]): PublicKey = {
    val bytes = hashOrderStruct(order)

    val signerKey = org.web3j.utils.Numeric.toBytesPadded(
      recoverEthSignerKeyBigInt(bytes, signature),
      EthereumKeyLength
    )

    PublicKey(ByteStr(signerKey))
  }

  def recoverEthSignerKeyBigInt(order: Order, signature: Array[Byte]): BigInteger = {
    val bytes = hashOrderStruct(order)
    recoverEthSignerKeyBigInt(bytes, signature)
  }

  def signOrder(order: Order, key: ECKeyPair): Array[Byte] = {
    val message   = hashOrderStruct(order)
    val signature = Sign.signMessage(message, key, false)
    val buffer    = ByteBuffer.allocate(signature.getR.length + signature.getS.length + signature.getV.length)
    buffer.put(signature.getR)
    buffer.put(signature.getS)
    buffer.put(signature.getV)
    buffer.array()
  }

  def decodeSignature(signature: Array[Byte]): SignatureData = {
    val buffer = ByteBuffer.wrap(signature)
    val paramSize = buffer.remaining() match {
      case 129   => 64
      case 65    => 32
      case other => throw new IllegalArgumentException(s"Unexpected signature length: $other")
    }
    val R = new Array[Byte](paramSize)
    val S = new Array[Byte](paramSize)
    buffer.get(R)
    buffer.get(S)
    val V = buffer.get()
    new SignatureData(V, R, S)
  }

  def orderDomainJson: String =
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
       |      }
       |    ],
       |    "Order": [
       |      {
       |        "name": "version",
       |        "type": "int32"
       |      },
       |      {
       |        "name": "matcherPublicKey",
       |        "type": "string"
       |      },
       |      {
       |        "name": "amountAsset",
       |        "type": "string"
       |      },
       |      {
       |        "name": "priceAsset",
       |        "type": "string"
       |      },
       |      {
       |        "name": "orderType",
       |        "type": "string"
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
       |        "type": "string"
       |      },
       |      {
       |        "name": "priceMode",
       |        "type": "string"
       |      }
       |    ]
       |  },
       |  "primaryType": "Order",
       |  "domain": {
       |    "name": "Waves Order",
       |    "version": "1",
       |    "chainId": ${AddressScheme.current.chainId}
       |  },
       |  "message": {}
       |}
       |""".stripMargin

  private def recoverEthSignerKeyBigInt(message: Array[Byte], signature: Array[Byte]): BigInteger = {
    val signatureData = EthOrders.decodeSignature(signature)
    val v             = BigInt(1, signatureData.getV)
    val recId         = if (v == 0 || v == 1) v else if (v > 28) v - AddressScheme.current.chainId * 2 - 35 else v - 27
    Sign
      .recoverFromSignature(
        recId.intValue,
        new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS)),
        message
      )
  }
}

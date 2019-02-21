package com.wavesplatform.serialization.protobuf.utils
import java.io.ByteArrayOutputStream

import com.google.protobuf.{ByteString, CodedOutputStream}
import com.google.protobuf.descriptor.FieldDescriptorProto.Type
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.protobuf.Transaction
import com.wavesplatform.common.utils._
import com.wavesplatform.transaction.protobuf.PBTransaction._
import play.api.libs.json.Json
import scalapb.GeneratedMessage
import scalapb.descriptors._

import scala.annotation.switch

object PBUtils extends App {
  def encodeDeterministic(msg: GeneratedMessage): Array[Byte] = {
    val outArray     = new Array[Byte](msg.serializedSize)
    val outputStream = CodedOutputStream.newInstance(outArray)
    outputStream.useDeterministicSerialization() // Adds this
    msg.writeTo(outputStream)
    outputStream.checkNoSpaceLeft()
    outArray
  }

  def encodeForSignature(msg: GeneratedMessage): Array[Byte] = {
    val bs           = new ByteArrayOutputStream(msg.serializedSize * 2)
    val outputStream = CodedOutputStream.newInstance(bs)
    outputStream.useDeterministicSerialization()

    def writeMsgTo(outputStream: CodedOutputStream, msg: GeneratedMessage) = {
      def writeTagAndValue(field: FieldDescriptor, value: PValue): Unit = {
        value match {
          case PRepeated(seq) =>
            seq.foreach(writeTagAndValue(field, _))

          case _ =>
            outputStream.writeFixed32NoTag(field.index)

            (field.protoType: @switch) match {
              case Type.TYPE_DOUBLE | Type.TYPE_FLOAT | Type.TYPE_GROUP | Type.Unrecognized(_) =>
                throw new IllegalArgumentException(s"Not supported: $field/$value")

              case Type.TYPE_INT64 | Type.TYPE_UINT64 | Type.TYPE_FIXED64 | Type.TYPE_SFIXED64 | Type.TYPE_SINT64 =>
                outputStream.writeFixed64NoTag(value.as[Long])

              case Type.TYPE_FIXED32 | Type.TYPE_INT32 | Type.TYPE_SFIXED32 | Type.TYPE_UINT32 | Type.TYPE_SINT32 =>
                outputStream.writeFixed32NoTag(value.as[Int])

              case Type.TYPE_BOOL =>
                outputStream.writeBoolNoTag(value.as[Boolean])

              case Type.TYPE_STRING =>
                val bytes = value.as[String].toCharArray.map(_.toByte)
                outputStream.writeFixed32NoTag(bytes.length)
                outputStream.write(bytes, 0, bytes.length)

              case Type.TYPE_BYTES =>
                val bytes = value.as[ByteString]
                outputStream.writeFixed32NoTag(bytes.size())
                outputStream.write(bytes.toByteArray, 0, bytes.size())

              case Type.TYPE_ENUM =>
                outputStream.writeFixed32NoTag(value.as[EnumValueDescriptor].index)

              case Type.TYPE_MESSAGE =>
                (value: @unchecked) match {
                  case PEmpty =>
                    outputStream.write(0: Byte)

                  case PMessage(fields) =>
                    val sortedFields = fields.toIndexedSeq.sortBy(_._1.number)
                    outputStream.write(1: Byte)
                    outputStream.writeFixed32NoTag(sortedFields.head._1.containingMessage.index)
                    outputStream.writeFixed32NoTag(sortedFields.length)
                    sortedFields.foreach(kv => writeTagAndValue(kv._1, kv._2))
                }
            }
        }
      }

      msg.companion.scalaDescriptor.fields.foreach(f => writeTagAndValue(f, msg.getField(f)))
    }

    writeMsgTo(outputStream, msg)
    outputStream.flush()
    bs.toByteArray
  }

  val transaction = {
    val js = Json.parse("""{
         "version": 2,
         "type":7,
         "id":"5KUDbPKjAoNHTMyae9zJZpFjYFAbeSQMQ9rzgkDEEUx6",
         "sender":"3N22UCTvst8N1i1XDvGHzyqdgmZgwDKbp44",
         "senderPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
         "fee":1,
         "timestamp":1526992336241,
         "proofs":["5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa"],
         "order1":{
            "version": 2,
            "id":"EcndU4vU3SJ58KZAXJPKACvMhijTzgRjLTsuWxSWaQUK",
            "sender":"3MthkhReCHXeaPZcWXcT3fa6ey1XWptLtwj",
            "senderPublicKey":"BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"buy",
            "price":6000000000,
            "amount":2,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":1,
            "signature":"2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs",
            "proofs":["2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs"]
         },
         "order2":{
            "version": 1,
            "id":"DS9HPBGRMJcquTb3sAGAJzi73jjMnFFSWWHfzzKK32Q7",
            "sender":"3MswjKzUBKCD6i1w4vCosQSbC8XzzdBx1mG",
            "senderPublicKey":"7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV",
            "matcherPublicKey":"Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP",
            "assetPair":{"amountAsset":null,"priceAsset":"9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy"},
            "orderType":"sell",
            "price":5000000000,
            "amount":3,
            "timestamp":1526992336241,
            "expiration":1529584336241,
            "matcherFee":2,
            "signature":"2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq",
            "proofs":["2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq"]
         },
         "price":5000000000,
         "amount":2,
         "buyMatcherFee":1,
         "sellMatcherFee":1
      }
      """)

    val buy = OrderV2(
      PublicKeyAccount.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").explicitGet(),
      PublicKeyAccount.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.BUY,
      2,
      6000000000L,
      1526992336241L,
      1529584336241L,
      1,
      Proofs(Seq(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get))
    )

    val sell = OrderV1(
      PublicKeyAccount.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").explicitGet(),
      PublicKeyAccount.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").explicitGet(),
      AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
      OrderType.SELL,
      3,
      5000000000L,
      1526992336241L,
      1529584336241L,
      2,
      Base58.decode("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get
    )

    val tx = ExchangeTransactionV2
      .create(
        buy,
        sell,
        2,
        5000000000L,
        1,
        1,
        1,
        1526992336241L,
        Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))
      )
      .explicitGet()

    tx.toPB
  }

  val defaultBytes: ByteStr = encodeDeterministic(transaction)
  val canonicalBytes: ByteStr = encodeForSignature(transaction)
  println(defaultBytes) // 426Xcwtdm8kpWH2Gzoy93t6aUvsxkTJFbkHDusQ8EAKKsYEpiRg283uHkfJPdthBQfnWi5XGkBuTZhTBdmDytnBHnkuJLRhVUi2RSaxE2Tx6gGvmsCKkMwksfgqSqNZLHWzuv12rTtXGUe4FHdSmVJDUKPB6nP5ScfFWfd5KapfoBE92fg7gLSg361GoyxCej4Z6yduD8ro92yoBZYap2LMvG4hGuRNRpqq58DVVUye2hiPKvhbsDgBf1sRhVuGgj2i5KG2Awg1TYCfStbBn3p5ZwthBWhVMK2BmwgRWqttM2mT3xcqfyucsXLbKBbTQq3XoRS6fxeUCiYGLxZpKsF97cbvuz9ox2wrFCaknjjt8YuSEZT3jbCGnGcHsaTs3syCQF1qPhTC8PmHyvNSMtaCuLUAaAjX6YpZSG6XcqP9auwJRtDzxrbSykcBAa8VbWYpfoP6KSpagGAptdKWVpVL55Jc1uGbL2JTdaag3dEmPA4vcEi56CG9xor6AKBB597FXETSfPGLDzfuy1ktaaSz6N3ceAW7dBHynaeiF7SDw3Y3Kwc81LvmKXLKLyvgbvU3fqocQcdhdyVeRYXnzWWTacRxWTVsn8SHhCdJu4hK8oyDhcoPYZUKvpt4G7XABStFvZ7ADhM4Hpq2cMQNYep2obS9yU8GD1myh5r8fDRbCvrKvz21uH5oAVzxbEbpcYrZdrwiwBvb7xDDDN
  println(canonicalBytes) // 11114EoghMf6svmunv8F5QwWorKfoHGWrVo37KdDaeKwiGQW8bPSwgKMeoakZqoBTBc3Zn5An663NEMjGZ47dPcNhfDuwaieBtS9XSyxQoU9LGi5p69VBvy8JJWN1p73ZKtTyBPxKMHyEzrygL7sXhbuJ2B1tZBVXjiySA5yd5CS2SSqPTehW7o5RZFwZzjBx2zRKfRbjGH7K5XgjkcoUCvPD5TNYb7pBQM5h9QRX41gUBWJaFvdWj3MS1poGCqouRaq3WYhctqdxezdagRaNdwQdKUHRNUroskNQC2HR1jPtKM9BNMYbTKGgXywhj3v6uv1VbqNPKXXN3zirzb3ELneTd89zYTyF7jA6NyPjpq1KeCmZpyETdsugGgse3d9h7ALvHM1zngKU5xkppexwxRcN3rh32wYEPPi2pXDN5WnuGxE84Zyorgyeps8t7Re1ZeY2iwfrjNidMBPwaqikBN5N4KB8797vZhU8dthxyyvFQbUhgHpEio8q7SnrFYeHhA1XkfChhBESCvQsng23x6R3eRfvXiWSjR8ryBptcGpS7SBHeKpuYDYYScmr7SwdYcPeowcU783Yfjcvdbh5Lje8VHSBs9Cdyc6ySFvXUtJBFnQdsKUFU195kZYDhUzD45HPFSuvNcr7gRzV1Pf67r8qWas1wqJvVQqBv2qtUW46kuZmNdC77acjwyNbQ2Xxu6nL65LgmBGgFMi95t7VxUGAxhgaybraF2GiGNZme8boX4xL7CzcbtqSnL5jFk4692LrCQwTiufJKxsgCkmNMpGvCybZXf9owu7k2ca7hok4gutBUNGpT4Sfjs8gr8n7ob59u7L3pxtRbBnGHqVsLCGofnCn2zmaroTdqTpY2oTv9bZrxdWxbYGqyvdre7gkGE2dE2KegmFV7rnVmaWykwHTf3f4NuzNCd6f6C7EgsnTUGFo63qCDVx4PMSx3RJq9FkjWGBpL7XLnHonyBJ8jpwMKZTT2spu6rXLKeQfL4pLUQw2w17BmHhzsMNGQdvWSfZsdfEDMxfTxfWALieRGyMvDbS3nLm82fD5QQKoFHLLAV47ejHrQiwCW8wCZNqV4FUq6BB9jnanqzGFRCc2DPPdX3h74dwHMakq4BaHW1LK2wYdTGJ62mDCBidb7Z4ALkfULTTAu8Xw5PAR2g6c6mZq51cdtSVaHDuHwtCkD3CiqviH7mcWPZWXnAx6LwsC2joa
}

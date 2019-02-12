package com.wavesplatform.transaction.protobuf
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.exchange._
import play.api.libs.json.Json

// TODO: Remove test object
private[protobuf] object PBTest extends App {
  import PBTransactionImplicits._

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

  val vanillaTx = ExchangeTransactionV2
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

  val tx = vanillaTx.toPB
  println(Json.toJson(tx))

  println(s"Original size: ${vanillaTx.bytes().length}")
  println(s"Protobuf size: ${tx.toByteArray.length}")
}

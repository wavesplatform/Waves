package com.wavesplatform.it.sync.utils

import com.wavesplatform.account.{Address, AddressScheme, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseCancelTransactionV2, LeaseTransactionV1, LeaseTransactionV2}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransactionV1, TransferTransactionV2}
import com.wavesplatform.transaction.{CreateAliasTransactionV1, CreateAliasTransactionV2, DataTransaction, Proofs}
import org.scalatest.prop.TableDrivenPropertyChecks
import scorex.crypto.encode.Base64

class TransactionSerializeSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {
  private val publicKey         = PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get
  private val chainId: Byte     = AddressScheme.current.chainId
  private val ts: Long          = 1526287561757L
  private val tsOrderFrom: Long = 1526992336241L
  private val tsOrderTo: Long   = 1529584336241L

  private val buyV2 = OrderV2(
    PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").right.get,
    PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").right.get,
    AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
    OrderType.BUY,
    2,
    60.waves,
    tsOrderFrom,
    tsOrderTo,
    1,
    Proofs(Seq(ByteStr.decodeBase58("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get))
  )

  val buyV1 = OrderV1(
    PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").right.get,
    PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").right.get,
    AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
    OrderType.BUY,
    2,
    60.waves,
    tsOrderFrom,
    tsOrderTo,
    1,
    Base58.tryDecodeWithLimit("2bkuGwECMFGyFqgoHV4q7GRRWBqYmBFWpYRkzgYANR4nN2twgrNaouRiZBqiK2RJzuo9NooB9iRiuZ4hypBbUQs").get
  )

  private val sell = OrderV1(
    PublicKey.fromBase58String("7E9Za8v8aT6EyU1sX91CVK7tWUeAetnNYDxzKZsyjyKV").right.get,
    PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").right.get,
    AssetPair.createAssetPair("WAVES", "9ZDWzK53XT5bixkmMwTJi2YzgxCqn5dUajXFcT2HcFDy").get,
    OrderType.SELL,
    3,
    50.waves,
    tsOrderFrom,
    tsOrderTo,
    2,
    Base58.tryDecodeWithLimit("2R6JfmNjEnbXAA6nt8YuCzSf1effDS4Wkz8owpCD9BdCNn864SnambTuwgLRYzzeP5CAsKHEviYKAJ2157vdr5Zq").get
  )

  private val exV1 = ExchangeTransactionV1
    .create(
      buyV1,
      sell,
      2,
      50.waves,
      1,
      1,
      1,
      tsOrderFrom,
      ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get
    )
    .right
    .get

  private val exV2 = ExchangeTransactionV2
    .create(
      buyV2,
      sell,
      2,
      50.waves,
      1,
      1,
      1,
      tsOrderFrom,
      Proofs(Seq(ByteStr.decodeBase58("5NxNhjMrrH5EWjSFnVnPbanpThic6fnNL48APVAkwq19y2FpQp4tNSqoAZgboC2ykUfqQs9suwBQj6wERmsWWNqa").get))
    )
    .right
    .get

  private val burnV1 = BurnTransactionV1
    .create(
      publicKey,
      IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
      10000000000L,
      burnFee,
      ts,
      ByteStr.decodeBase58("uapJcAJQryBhWThU43rYgMNmvdT7kY747vx5BBgxr2KvaeTRx8Vsuh4yu1JxBymU9LnAoo1zjQcPrWSuhi6dVPE").get
    )
    .right
    .get

  private val burnV2 = BurnTransactionV2
    .create(
      chainId,
      publicKey,
      IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
      10000000000L,
      burnFee,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("3NcEv6tcVMuXkTJwiqW4J3GMCTe8iSLY7neEfNZonp59eTQEZXYPQWs565CRUctDrvcbtmsRgWvnN7BnFZ1AVZ1H").get))
    )
    .right
    .get

  private val aliasV1 = CreateAliasTransactionV1
    .create(
      publicKey,
      Alias.create("myalias").right.get,
      minFee,
      ts,
      ByteStr.decodeBase58("CC1jQ4qkuVfMvB2Kpg2Go6QKXJxUFC8UUswUxBsxwisrR8N5s3Yc8zA6dhjTwfWKfdouSTAnRXCxTXb3T6pJq3T").get
    )
    .right
    .get

  private val aliasV2 = CreateAliasTransactionV2
    .create(
      publicKey,
      Alias.create("myalias").right.get,
      minFee,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("26U7rQTwpdma5GYSZb5bNygVCtSuWL6DKet1Nauf5J57v19mmfnq434YrkKYJqvYt2ydQBUT3P7Xgj5ZVDVAcc5k").get))
    )
    .right
    .get

  private val data = DataTransaction
    .create(
      publicKey,
      List(IntegerDataEntry("int", 24), BooleanDataEntry("bool", true), BinaryDataEntry("blob", ByteStr(Base64.decode("YWxpY2U=")))),
      minFee,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
    )
    .right
    .get

  private val issueV1 = IssueTransactionV1
    .create(
      publicKey,
      "Gigacoin".getBytes("UTF-8"),
      "Gigacoin".getBytes("UTF-8"),
      someAssetAmount,
      8,
      true,
      issueFee,
      ts,
      ByteStr.decodeBase58("28kE1uN1pX2bwhzr9UHw5UuB9meTFEDFgeunNgy6nZWpHX4pzkGYotu8DhQ88AdqUG6Yy5wcXgHseKPBUygSgRMJ").get
    )
    .right
    .get

  private val issueV2 = IssueTransactionV2
    .create(
      chainId,
      publicKey,
      "Gigacoin".getBytes("UTF-8"),
      "Gigacoin".getBytes("UTF-8"),
      someAssetAmount,
      8,
      true,
      None,
      issueFee,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("43TCfWBa6t2o2ggsD4bU9FpvH3kmDbSBWKE1Z6B5i5Ax5wJaGT2zAvBihSbnSS3AikZLcicVWhUk1bQAMWVzTG5g").get))
    )
    .right
    .get

  private val leasecancelV1 = LeaseCancelTransactionV1
    .create(
      publicKey,
      ByteStr.decodeBase58("EXhjYjy8a1dURbttrGzfcft7cddDnPnoa3vqaBLCTFVY").get,
      minFee,
      ts,
      ByteStr.decodeBase58("4T76AXcksn2ixhyMNu4m9UyY54M3HDTw5E2HqUsGV4phogs2vpgBcN5oncu4sbW4U3KU197yfHMxrc3kZ7e6zHG3").get
    )
    .right
    .get

  private val leasecancelV2 = LeaseCancelTransactionV2
    .create(
      'I',
      publicKey,
      ByteStr.decodeBase58("DJWkQxRyJNqWhq9qSQpK2D4tsrct6eZbjSv3AH4PSha6").get,
      minFee,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("3h5SQLbCzaLoTHUeoCjXUHB6qhNUfHZjQQVsWTRAgTGMEdK5aeULMVUfDq63J56kkHJiviYTDT92bLGc8ELrUgvi").get))
    )
    .right
    .get

  private val leaseV1 = LeaseTransactionV1
    .create(
      publicKey,
      10000000,
      minFee,
      ts,
      Address.fromString(sender.address).right.get,
      ByteStr.decodeBase58("iy3TmfbFds7pc9cDDqfjEJhfhVyNtm3GcxoVz8L3kJFvgRPUmiqqKLMeJGYyN12AhaQ6HvE7aF1tFgaAoCCgNJJ").get
    )
    .right
    .get

  private val leaseV2 = LeaseTransactionV2
    .create(
      publicKey,
      10000000,
      minFee,
      ts,
      Address.fromString(sender.address).right.get,
      Proofs(Seq(ByteStr.decodeBase58("5Fr3yLwvfKGDsFLi8A8JbHqToHDojrPbdEGx9mrwbeVWWoiDY5pRqS3rcX1rXC9ud52vuxVdBmGyGk5krcgwFu9q").get))
    )
    .right
    .get

  private val transfers = MassTransferTransaction
    .parseTransfersList(List(Transfer(firstAddress, 1.waves), Transfer(secondAddress, 2.waves)))
    .right
    .get

  val mass = MassTransferTransaction
    .create(
      Waves,
      publicKey,
      transfers,
      ts,
      2.waves,
      Base58.tryDecodeWithLimit("59QuUcqP6p").get,
      Proofs(Seq(ByteStr.decodeBase58("FXMNu3ecy5zBjn9b69VtpuYRwxjCbxdkZ3xZpLzB8ZeFDvcgTkmEDrD29wtGYRPtyLS3LPYrL2d5UM6TpFBMUGQ").get))
    )
    .right
    .get

  private val reissueV1 = ReissueTransactionV1
    .create(
      publicKey,
      IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
      100000000L,
      true,
      1.waves,
      ts,
      ByteStr.decodeBase58("3LnRMrjkk7RoV35PTwcdB4yW2rqUqXaKAh8DnPk5tNWABvhVQ9oqdTk3zM8b9AbGtry7WEcQZtevfK92DCFaa6hA").get
    )
    .right
    .get

  private val reissueV2 = ReissueTransactionV2
    .create(
      chainId,
      publicKey,
      IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
      100000000L,
      true,
      1.waves,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("4DFEtUwJ9gjMQMuEXipv2qK7rnhhWEBqzpC3ZQesW1Kh8D822t62e3cRGWNU3N21r7huWnaty95wj2tZxYSvCfro").get))
    )
    .right
    .get

  private val setasset = SetAssetScriptTransaction
    .create(
      chainId,
      publicKey,
      IssuedAsset(ByteStr.decodeBase58("DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n").get),
      Some(Script.fromBase64String("base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==").right.get),
      1.waves,
      ts,
      Proofs(
        Seq("5sRtXKcdDa",
            "9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn",
            "",
            "3C",
            "24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",
            "").map(ByteStr.decodeBase58(_).get))
    )
    .right
    .get

  private val setscript = SetScriptTransaction
    .create(
      publicKey,
      None,
      setScriptFee,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("tcTr672rQ5gXvcA9xCGtQpkHC8sAY1TDYqDcQG7hQZAeHcvvHFo565VEv1iD1gVa3ZuGjYS7hDpuTnQBfY2dUhY").get))
    )
    .right
    .get

  private val sponsor = SponsorFeeTransaction
    .create(
      publicKey,
      IssuedAsset(ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get),
      Some(100000),
      1.waves,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("3QrF81WkwGhbNvKcwpAVyBPL1MLuAG5qmR6fmtK9PTYQoFKGsFg1Rtd2kbMBuX2ZfiFX58nR1XwC19LUXZUmkXE7").get))
    )
    .right
    .get

  private val transferV1 = TransferTransactionV1
    .create(
      Waves,
      publicKey,
      Address.fromString(sender.address).right.get,
      1900000,
      ts,
      Waves,
      minFee,
      Base58.tryDecodeWithLimit("").get,
      ByteStr.decodeBase58("eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz").get
    )
    .right
    .get

  private val transferV2 = TransferTransactionV2
    .create(
      Waves,
      publicKey,
      Address.fromString(sender.address).right.get,
      100000000,
      ts,
      Waves,
      minFee,
      Base58.tryDecodeWithLimit("").get,
      Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get))
    )
    .right
    .get

  private val invokeScript = InvokeScriptTransaction
    .create(
      PublicKey.fromBase58String("BqeJY8CP3PeUDaByz57iRekVUGtLxoow4XxPvXfHynaZ").right.get,
      PublicKey.fromBase58String("Fvk5DXmfyWVZqQVBowUBMwYtRAHDtdyZNNeRrwSjt6KP").right.get,
      Some(
        Terms.FUNCTION_CALL(
        function = FunctionHeader.User("testfunc"),
        args = List(TRUE)
      )),
      Seq(InvokeScriptTransaction.Payment(7, IssuedAsset(ByteStr.decodeBase58("73pu8pHFNpj9tmWuYjqnZ962tXzJvLGX86dxjZxGYhoK").get))),
      smartMinFee,
      Waves,
      ts,
      Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get))
    )
    .right
    .get

  forAll(
    Table(
      ("tx", "name"),
      (exV1, "exchangeV1"),
      (exV2, "exchangeV2"),
      (burnV1, "burnV1"),
      (burnV2, "burnV2"),
      (aliasV1, "aliasV1"),
      (aliasV2, "aliasV2"),
      (data, "data"),
      (issueV1, "issueV1"),
      (issueV2, "issueV2"),
      (leasecancelV1, "leasecancelV1"),
      (leasecancelV2, "leasecancelV2"),
      (leaseV1, "leaseV1"),
      (leaseV2, "leaseV2"),
      (mass, "mass"),
      (reissueV1, "reissueV1"),
      (reissueV2, "reissueV2"),
      (setasset, "setasset"),
      (setscript, "setscript"),
      (sponsor, "sponsor"),
      (transferV1, "transferV1"),
      (transferV2, "transferV2"),
      (invokeScript, "invokeScript")
    )) { (tx, name) =>
    test(s"Serialize check of $name transaction") {
      val r = sender.transactionSerializer(tx.json()).bytes.map(_.toByte)

      r shouldBe tx.bodyBytes()

    }

  }

}

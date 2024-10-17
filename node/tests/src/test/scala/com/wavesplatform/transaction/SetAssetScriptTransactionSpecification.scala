package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import org.scalacheck.Gen
import play.api.libs.json._
import com.wavesplatform.test._

class SetAssetScriptTransactionSpecification extends GenericTransactionSpecification[SetAssetScriptTransaction] {
  property("issuer can`t make SetAssetScript tx when Script is Contract") {
    val accountA = PublicKey.fromBase58String("5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr").explicitGet()

    SetAssetScriptTransaction
      .create(
        1.toByte,
        accountA,
        IssuedAsset(ByteStr.decodeBase58("DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n").get),
        Some(ContractScript(V3, DApp(DAppMeta(), List.empty, List.empty, None)).explicitGet()),
        1222,
        System.currentTimeMillis(),
        Proofs.empty
      ) should produce("not Contract")
  }

  property("can't be created with empty script") {
    val gen = for {
      acc   <- accountGen
      asset <- bytes32gen
      fee   <- smallFeeGen
      ts    <- timestampGen
      txEi = SetAssetScriptTransaction.selfSigned(TxVersion.V2, acc, IssuedAsset(ByteStr(asset)), None, fee, ts)
    } yield txEi

    forAll(gen)(_ should produce("Cannot set empty script"))
  }

  override def transactionParser: TransactionParser = SetAssetScriptTransaction

  override def updateProofs(tx: SetAssetScriptTransaction, p: Proofs): SetAssetScriptTransaction = tx.copy(1.toByte, proofs = p)

  override def generator: Gen[(Seq[Transaction], SetAssetScriptTransaction)] = setAssetScriptTransactionGen
  override def assertTxs(first: SetAssetScriptTransaction, second: SetAssetScriptTransaction): Unit = {
    first.sender shouldEqual second.sender
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.asset shouldEqual second.asset
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }

  def jsonRepr: Seq[(JsValue, SetAssetScriptTransaction)] =
    Seq(
      (
        Json.parse(
          s"""{"type":15,"id":"3GdaFxG3JDdUnGDoFNs2MoGwZYivYkkAHFcZe3T2yu72","sender":"3NBKqNonmitNjGJNS3HRKxAhJVRKiFw4PLu","senderPublicKey":"5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr","fee":78311891,"feeAssetId":null,"timestamp":1868142423132802425,"proofs":["5sRtXKcdDa","9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn","","3C","24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",""],"version":1,"chainId":${AddressScheme.current.chainId},"assetId":"DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n","script":"base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg=="}"""
        ),
        SetAssetScriptTransaction
          .create(
            1.toByte,
            PublicKey.fromBase58String("5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr").explicitGet(),
            IssuedAsset(ByteStr.decodeBase58("DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n").get),
            Some(Script.fromBase64String("base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==").explicitGet()),
            78311891L,
            1868142423132802425L,
            Proofs(
              Seq(
                "5sRtXKcdDa",
                "9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn",
                "",
                "3C",
                "24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",
                ""
              ).map(ByteStr.decodeBase58(_).get)
            )
          )
          .explicitGet()
      )
    )

  def transactionName: String = "SetAssetScriptTransaction"

  override def preserBytesJson: Option[(Array[TxVersion], JsValue)] =
    Some(
      Base64.decode(
        "AA8BVNudMMPvBFz21sC5/y/8Q7+G7X+93FmjkMrMxC0kYHB9Fmm/WOmkNhAP6ZOOCa9DmXBZoOwh0jiYyZlV0kVC1CYAAAAABfv7gAz//7LZa4zuAQCNAQMDCQAAZAAAAAIAAAAAAAAAAAEAAAAAAAAAAAEDCQAAZAAAAAIAAAAAAAAAAAEAAAAAAAAAAAEDBgYGCgAAAAABaAYKAAAAAAF2BgYKAAAAAAFzBgoAAAAAAWEGCgAAAAABbwYKAAAAAAFsBgYHCQAAZAAAAAIAAAAAAAAAAAEAAAAAAAAAAAFHzRMGAQABAECI3o9yZX8SkC+oxUmNDQJ7ChMiyJS2d5RhAo5i802Z1ZBx/KU36GwmVxorUlErByaubY6hrlkmK+gmsEprWHCL"
      ) ->
        Json.parse(
          """
          |{
          |  "senderPublicKey" : "FnHJjy1pBYrUY3KFdNH6uTJBscSAF5UwzrUJzqasEGEg",
          |  "sender" : "3N7wSJNF2JzHRTURTTnDpGC3k862354135G",
          |  "feeAssetId" : null,
          |  "chainId" : 84,
          |  "proofs" : [ "3jiSeBNS2Ggc98F4W9BoPJGaiKXCXNskKqhNtQy4EEGdHGnkBJMTGUzTHzukusR9guiaK7KTBd7MLFva4J95TjCN" ],
          |  "assetId" : "2WVVbDwzjg4bSBJq3ykcKu2L1TmpChzygECpqUy6fFKT",
          |  "fee" : 100400000,
          |  "id" : "BZ5MPGipw7F9ZwsGwrrwNaMDbKcJh8VzywZqY8MNoAHE",
          |  "type" : 15,
          |  "version" : 1,
          |  "script" : "base64:AQMDCQAAZAAAAAIAAAAAAAAAAAEAAAAAAAAAAAEDCQAAZAAAAAIAAAAAAAAAAAEAAAAAAAAAAAEDBgYGCgAAAAABaAYKAAAAAAF2BgYKAAAAAAFzBgoAAAAAAWEGCgAAAAABbwYKAAAAAAFsBgYHCQAAZAAAAAIAAAAAAAAAAAEAAAAAAAAAAAFHzRMG",
          |  "timestamp" : 936748391133318382
          |}
          |""".stripMargin
        )
    )
}

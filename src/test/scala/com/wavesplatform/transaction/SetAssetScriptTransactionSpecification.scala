package com.wavesplatform.transaction

import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction
import com.wavesplatform.transaction.smart.script.Script
import org.scalacheck.Gen
import play.api.libs.json._

class SetAssetScriptTransactionSpecification extends GenericTransactionSpecification[SetAssetScriptTransaction] {
  def transactionParser: com.wavesplatform.transaction.TransactionParserFor[SetAssetScriptTransaction] = SetAssetScriptTransaction
  def updateProofs(tx: SetAssetScriptTransaction, p: Proofs): SetAssetScriptTransaction = {
    tx.copy(proofs = p)
  }
  def generator: Gen[((Seq[com.wavesplatform.transaction.Transaction], SetAssetScriptTransaction))] = setAssetScriptTransactionGen
  def assertTxs(first: SetAssetScriptTransaction, second: SetAssetScriptTransaction): Unit = {
    first.sender.address shouldEqual second.sender.address
    first.timestamp shouldEqual second.timestamp
    first.fee shouldEqual second.fee
    first.version shouldEqual second.version
    first.assetId shouldEqual second.assetId
    first.proofs shouldEqual second.proofs
    first.bytes() shouldEqual second.bytes()
    first.script shouldEqual second.script
  }
  def jsonRepr: Seq[(JsValue, SetAssetScriptTransaction)] =
    Seq(
      (Json.parse(
         s"""{"type":15,"id":"3GdaFxG3JDdUnGDoFNs2MoGwZYivYkkAHFcZe3T2yu72","sender":"3NBKqNonmitNjGJNS3HRKxAhJVRKiFw4PLu","senderPublicKey":"5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr","fee":78311891,"timestamp":1868142423132802425,"proofs":["5sRtXKcdDa","9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn","","3C","24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",""],"version":1,"chainId":${AddressScheme.current.chainId},"assetId":"DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n","script":"base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg=="}"""),
       SetAssetScriptTransaction
         .create(
           1: Byte,
           AddressScheme.current.chainId,
           PublicKeyAccount.fromBase58String("5k3gXC486CCFCwzUAgavH9JfPwmq9CbBZvTARnFujvgr").explicitGet(),
           ByteStr.decodeBase58("DUyJyszsWcmZG7q2Ctk1hisDeGBPB8dEzyU8Gs5V2j3n").get,
           Some(Script.fromBase64String("base64:AQkAAGcAAAACAHho/EXujJiPAJUhuPXZYac+rt2jYg==").explicitGet()),
           78311891L,
           1868142423132802425L,
           Proofs(
             Seq("5sRtXKcdDa",
                 "9Zfe5aw9D7rRR3nvU3QuAjCNT7pdwRXwvBFxHmdt2WtWwiEwffn",
                 "",
                 "3C",
                 "24jboCkAEFrsBKNh6z8FFyJP8YhejsrBwt7JdHVhiCk7DCc3Zxsc4g6PYG8tsLXmK",
                 "").map(ByteStr.decodeBase58(_).get))
         )
         .explicitGet()))
  def transactionName: String = "SetAssetScriptTransaction"
}

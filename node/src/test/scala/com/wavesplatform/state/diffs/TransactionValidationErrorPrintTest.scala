package com.wavesplatform.state.diffs

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Expression, V1}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.compilerContext
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.ScriptExecutionError
import com.wavesplatform.transaction.assets.{IssueTransaction, SetAssetScriptTransaction}
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{GenesisTransaction, TxValidationError, TxVersion}
import com.wavesplatform.utils._
import org.scalatest.{EitherValues, Inside, PropSpec}

class TransactionValidationErrorPrintTest extends PropSpec with Inside with WithState with EitherValues {
  property("output transaction error should be easy to read") {
    val assetScript =
      s"""
        | let NETWORKBYTE = takeRight(toBytes(87), 1)
        |
        | match (tx) {
        |     # Only allow transfer transactions
        |     case t:TransferTransaction => {
        |         let txWithoutAttachment = dropRight(t.bodyBytes, 97) + takeRight(toBytes(0), 1)
        |
        |         let recipientPublicKeyAndSignature = t.attachment
        |         let recipientPublicKey = take(recipientPublicKeyAndSignature, 32)
        |         let recipientSignature = takeRight(recipientPublicKeyAndSignature, 64)
        |
        |         let recipientPublicKeyHash = take(keccak256(blake2b256(recipientPublicKey)), 20)
        |         let rpkWithVersionAndByte = takeRight(toBytes(1), 1) + NETWORKBYTE + recipientPublicKeyHash
        |         let checksum = take(keccak256(blake2b256(rpkWithVersionAndByte)), 4)
        |         let recipientAddressFromPublicKey = rpkWithVersionAndByte + checksum
        |         let recipientAddressFromTx = addressFromRecipient(t.recipient).bytes
        |         let recipientAddressStr = toBase58String(recipientAddressFromPublicKey)
        |         let big = base64'${"a" * 2048}'
        |
        |         if (big == big && recipientAddressFromPublicKey != recipientAddressFromTx) then throw(
        |             "Recipient address error:" + recipientAddressStr
        |             ) else {
        |           if (!sigVerify(txWithoutAttachment, recipientSignature, recipientPublicKey))
        |             then true
        |             else false
        |         }
        |     }
        |     case _ => throw("unexpected")
        | }
      """.stripMargin

    val untypedScript = Parser.parseExpr(assetScript).get.value

    val typedScript = ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untypedScript).explicitGet()._1)
      .explicitGet()

    val preTypedScript =
      ExprScript(ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), Parser.parseExpr("true").get.value).explicitGet()._1)
        .explicitGet()

    val seed     = Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet()
    val master   = Address.fromString("3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu").explicitGet()
    val genesis1 = GenesisTransaction.create(master, 1000000000, 0).explicitGet()
    val genesis2 = GenesisTransaction.create(KeyPair(master.bytes).toAddress, 1000000000, 0).explicitGet()

    val issueTransaction = IssueTransaction(
      TxVersion.V2,
      KeyPair(seed.bytes).publicKey,
      "name".utf8Bytes,
      "description".utf8Bytes,
      100,
      0.toByte,
      false,
      Some(preTypedScript),
      10000000,
      0
    ).signWith(KeyPair(seed.bytes).privateKey)

    val preTransferTransaction = TransferTransaction
      .selfSigned(
        version = 2.toByte,
        sender = KeyPair(seed.bytes),
        recipient = KeyPair(master.bytes).toAddress,
        asset = IssuedAsset(issueTransaction.id()),
        amount = 1,
        feeAsset = Waves,
        fee = 10000000,
        attachment = ByteStr.empty,
        timestamp = 0
      )
      .explicitGet()

    val preSetAssetScriptTransaction = SetAssetScriptTransaction
      .selfSigned(
        version = 1.toByte,
        sender = KeyPair(seed.bytes),
        asset = IssuedAsset(issueTransaction.id()),
        script = Some(typedScript),
        fee = 10000000,
        timestamp = 0
      )
      .explicitGet()

    val transferTransaction = TransferTransaction
      .selfSigned(
        version = 2.toByte,
        sender = KeyPair(master.bytes),
        recipient = master,
        asset = IssuedAsset(issueTransaction.id()),
        amount = 1,
        feeAsset = Waves,
        fee = 10000000,
        attachment = ByteStr.empty,
        timestamp = 0
      )
      .explicitGet()

    assertDiffEi(
      Seq(TestBlock.create(Seq(genesis1, genesis2, issueTransaction, preTransferTransaction, preSetAssetScriptTransaction))),
      TestBlock.create(Seq(transferTransaction))
    ) { error =>
      inside(error) {
        case Left(TransactionValidationError(see: ScriptExecutionError, _)) =>
          val expected = //regex because of changeable proof
            """
             |	\$match0 = TransferTransaction\(
             |		recipient = Address\(
             |			bytes = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |		\)
             |		timestamp = 0
             |		bodyBytes = base58'ZFDBCm7WGpX1zYwdAbbbk2XHyDz2urZGfPHjeiPWuGuemeYUAswXmdLfPhXamrydNQwFDR9QKFELsMaZDwneo16LGifGX71dUtdqfRtzzr3KvjVYD1uysyghj3KfWNDSriC3E1vKR6SWa91rqdzXhynrNZXHu9EJpud'
             |		assetId = base58'6Jro1D97trbypmc4HDckkiy2qYtU2JoU7ZUjtwEPi32o'
             |		feeAssetId = Unit
             |		amount = 1
             |		version = 2
             |		id = base58'FsqB36ighMWLbGS1te7gh9DRFbrCRVjvumgSKwdYAwxi'
             |		senderPublicKey = base58'EbxDdqXBhj3TEd1UFoi1UE1vm1k7gM9EMYAuLr62iaZF'
             |		attachment = base58''
             |		sender = Address\(
             |			bytes = base58'3Mrt6Y1QweDrKRRNuhhHGdHpu2kXLXq2QK5'
             |		\)
             |		fee = 10000000
             |	\)
             |	big = base64'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
             |	@xs = base58'11111112'
             |	@number = 1
             |	@xs = base58'11111112W'
             |	@number = 1
             |	NETWORKBYTE = base58'2W'
             |	t = TransferTransaction\(
             |		recipient = Address\(
             |			bytes = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |		\)
             |		timestamp = 0
             |		bodyBytes = base58'ZFDBCm7WGpX1zYwdAbbbk2XHyDz2urZGfPHjeiPWuGuemeYUAswXmdLfPhXamrydNQwFDR9QKFELsMaZDwneo16LGifGX71dUtdqfRtzzr3KvjVYD1uysyghj3KfWNDSriC3E1vKR6SWa91rqdzXhynrNZXHu9EJpud'
             |		assetId = base58'6Jro1D97trbypmc4HDckkiy2qYtU2JoU7ZUjtwEPi32o'
             |		feeAssetId = Unit
             |		amount = 1
             |		version = 2
             |		id = base58'FsqB36ighMWLbGS1te7gh9DRFbrCRVjvumgSKwdYAwxi'
             |		senderPublicKey = base58'EbxDdqXBhj3TEd1UFoi1UE1vm1k7gM9EMYAuLr62iaZF'
             |		attachment = base58''
             |		sender = Address\(
             |			bytes = base58'3Mrt6Y1QweDrKRRNuhhHGdHpu2kXLXq2QK5'
             |		\)
             |		fee = 10000000
             |	\)
             |	recipientPublicKeyAndSignature = base58''
             |	recipientPublicKey = base58''
             |	recipientPublicKeyHash = base58'3aDy5kHaDeXWfQwMrBCRvd6r7gzg'
             |	rpkWithVersionAndByte = base58'N8tNz9vAHAwFpa4A8Rgk45q8tNjeC'
             |	checksum = base58'2U8tZq'
             |	recipientAddressFromPublicKey = base58'3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs'
             |	recipientAddressFromTx = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |	@a = base58'3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs'
             |	@b = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |	@p = false
             |	recipientAddressStr = "3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs"
             |""".stripMargin.r
          TxValidationError.logToString(see.log) should fullyMatch regex expected
      }
    }
  }
}

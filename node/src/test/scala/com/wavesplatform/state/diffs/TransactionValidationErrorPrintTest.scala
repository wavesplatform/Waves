package com.wavesplatform.state.diffs

import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values.{Expression, V1}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.compilerContext
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxValidationError.ScriptExecutionError
import com.wavesplatform.transaction.{TxHelpers, TxValidationError}
import org.scalatest.Inside

class TransactionValidationErrorPrintTest extends PropSpec with Inside with WithState {
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
    val genesis1 = TxHelpers.genesis(master, 1000000000, timestamp = 0)
    val genesis2 = TxHelpers.genesis(KeyPair(master.bytes).toAddress, 1000000000, timestamp = 0)

    val issueTransaction = TxHelpers.issue(
      issuer = KeyPair(seed.bytes),
      amount = 100,
      name = "name",
      reissuable = false,
      script = Some(preTypedScript),
      fee = 10000000,
      timestamp = 0
    )

    val preTransferTransaction = TxHelpers.transfer(
      from = KeyPair(seed.bytes),
      to = KeyPair(master.bytes).toAddress,
      amount = 1,
      asset = issueTransaction.asset,
      fee = 10000000,
      timestamp = 0
    )

    val preSetAssetScriptTransaction = TxHelpers.setAssetScript(
      acc = KeyPair(seed.bytes),
      asset = issueTransaction.asset,
      script = typedScript,
      fee = 10000000,
      timestamp = 0
    )

    val transferTransaction = TxHelpers.transfer(KeyPair(master.bytes), master, 1, issueTransaction.asset, fee = 10000000, timestamp = 0)

    assertDiffEi(
      Seq(TestBlock.create(Seq(genesis1, genesis2, issueTransaction, preTransferTransaction, preSetAssetScriptTransaction))),
      TestBlock.create(Seq(transferTransaction))
    ) { error =>
      inside(error) { case Left(TransactionValidationError(see: ScriptExecutionError, _)) =>
        val expected = // regex because of changeable proof
          f"""
             |	\\$$match0 = TransferTransaction\\(
             |		recipient = Address\\(
             |			bytes = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |		\\)
             |		timestamp = 0
             |		bodyBytes = base58'ZFDBCm7WGpX1zYwdAbbbk2XHyDz2urZGfPHjeiPWuGuemeYUAswXmdLfPhXamrydNQwFDR9QKFELsMaZDwneo16LGifGX71dUtdqfRtzzr3KvjVYD1uysyghj3KfWNDSriC3E1vKR6SWa91rqdzXhynrNZXHu9EJpud'
             |		assetId = base58'6Jro1D97trbypmc4HDckkiy2qYtU2JoU7ZUjtwEPi32o'
             |		feeAssetId = Unit
             |		amount = 1
             |		version = 2
             |		id = base58'FsqB36ighMWLbGS1te7gh9DRFbrCRVjvumgSKwdYAwxi'
             |		senderPublicKey = base58'EbxDdqXBhj3TEd1UFoi1UE1vm1k7gM9EMYAuLr62iaZF'
             |		attachment = base58''
             |		sender = Address\\(
             |			bytes = base58'3Mrt6Y1QweDrKRRNuhhHGdHpu2kXLXq2QK5'
             |		\\)
             |		fee = 10000000
             |	\\)
             |	\\$$isInstanceOf\\.@args = \\[
             |		TransferTransaction\\(
             |			recipient = Address\\(
             |				bytes = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |			\\)
             |			timestamp = 0
             |			bodyBytes = base58'ZFDBCm7WGpX1zYwdAbbbk2XHyDz2urZGfPHjeiPWuGuemeYUAswXmdLfPhXamrydNQwFDR9QKFELsMaZDwneo16LGifGX71dUtdqfRtzzr3KvjVYD1uysyghj3KfWNDSriC3E1vKR6SWa91rqdzXhynrNZXHu9EJpud'
             |			assetId = base58'6Jro1D97trbypmc4HDckkiy2qYtU2JoU7ZUjtwEPi32o'
             |			feeAssetId = Unit
             |			amount = 1
             |			version = 2
             |			id = base58'FsqB36ighMWLbGS1te7gh9DRFbrCRVjvumgSKwdYAwxi'
             |			senderPublicKey = base58'EbxDdqXBhj3TEd1UFoi1UE1vm1k7gM9EMYAuLr62iaZF'
             |			attachment = base58''
             |			sender = Address\\(
             |				bytes = base58'3Mrt6Y1QweDrKRRNuhhHGdHpu2kXLXq2QK5'
             |			\\)
             |			fee = 10000000
             |		\\),
             |		"TransferTransaction"
             |	\\]
             |	big = base64'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
             |	==\\.@args = \\[
             |		base64'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
             |		base64'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
             |	\\]
             |	toBytes\\.@args = \\[
             |		1
             |	\\]
             |	takeRight\\.@args = \\[
             |		base58'11111112',
             |		1
             |	\\]
             |	@xs = base58'11111112'
             |	size\\.@args = \\[
             |		base58'11111112'
             |	\\]
             |	@number = 1
             |	-\\.@args = \\[
             |		8,
             |		1
             |	\\]
             |	drop\\.@args = \\[
             |		base58'11111112',
             |		7
             |	\\]
             |	toBytes\\.@args = \\[
             |		87
             |	\\]
             |	takeRight\\.@args = \\[
             |		base58'11111112W',
             |		1
             |	\\]
             |	@xs = base58'11111112W'
             |	size\\.@args = \\[
             |		base58'11111112W'
             |	\\]
             |	@number = 1
             |	-\\.@args = \\[
             |		8,
             |		1
             |	\\]
             |	drop\\.@args = \\[
             |		base58'11111112W',
             |		7
             |	\\]
             |	NETWORKBYTE = base58'2W'
             |	\\+\\.@args = \\[
             |		base58'2',
             |		base58'2W'
             |	\\]
             |	t = TransferTransaction\\(
             |		recipient = Address\\(
             |			bytes = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |		\\)
             |		timestamp = 0
             |		bodyBytes = base58'ZFDBCm7WGpX1zYwdAbbbk2XHyDz2urZGfPHjeiPWuGuemeYUAswXmdLfPhXamrydNQwFDR9QKFELsMaZDwneo16LGifGX71dUtdqfRtzzr3KvjVYD1uysyghj3KfWNDSriC3E1vKR6SWa91rqdzXhynrNZXHu9EJpud'
             |		assetId = base58'6Jro1D97trbypmc4HDckkiy2qYtU2JoU7ZUjtwEPi32o'
             |		feeAssetId = Unit
             |		amount = 1
             |		version = 2
             |		id = base58'FsqB36ighMWLbGS1te7gh9DRFbrCRVjvumgSKwdYAwxi'
             |		senderPublicKey = base58'EbxDdqXBhj3TEd1UFoi1UE1vm1k7gM9EMYAuLr62iaZF'
             |		attachment = base58''
             |		sender = Address\\(
             |			bytes = base58'3Mrt6Y1QweDrKRRNuhhHGdHpu2kXLXq2QK5'
             |		\\)
             |		fee = 10000000
             |	\\)
             |	recipientPublicKeyAndSignature = base58''
             |	take\\.@args = \\[
             |		base58'',
             |		32
             |	\\]
             |	recipientPublicKey = base58''
             |	blake2b256\\.@args = \\[
             |		base58''
             |	\\]
             |	keccak256\\.@args = \\[
             |		base58'xyw95Bsby3s4mt6f4FmFDnFVpQBAeJxBFNGzu2cX4dM'
             |	\\]
             |	take\\.@args = \\[
             |		base58'DRtdYbxMg7YHw4acvDP6xQrvmsRAz3K7gSkH3xBJ5CTL',
             |		20
             |	\\]
             |	recipientPublicKeyHash = base58'3aDy5kHaDeXWfQwMrBCRvd6r7gzg'
             |	\\+\\.@args = \\[
             |		base58'6v',
             |		base58'3aDy5kHaDeXWfQwMrBCRvd6r7gzg'
             |	\\]
             |	rpkWithVersionAndByte = base58'N8tNz9vAHAwFpa4A8Rgk45q8tNjeC'
             |	blake2b256\\.@args = \\[
             |		base58'N8tNz9vAHAwFpa4A8Rgk45q8tNjeC'
             |	\\]
             |	keccak256\\.@args = \\[
             |		base58'CSJhGcnZPNCcHG5gCZuHKArEg8MUy9ridbKZsryV8FEw'
             |	\\]
             |	take\\.@args = \\[
             |		base58'4sAbTTxFgWFkHC5EutjwtRYgM3Q8V6aBD9EjDKVJ7byk',
             |		4
             |	\\]
             |	checksum = base58'2U8tZq'
             |	\\+\\.@args = \\[
             |		base58'N8tNz9vAHAwFpa4A8Rgk45q8tNjeC',
             |		base58'2U8tZq'
             |	\\]
             |	recipientAddressFromPublicKey = base58'3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs'
             |	addressFromRecipient\\.@args = \\[
             |		Address\\(
             |			bytes = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |		\\)
             |	\\]
             |	recipientAddressFromTx = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |	!=\\.@args = \\[
             |		base58'3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs',
             |		base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |	\\]
             |	@a = base58'3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs'
             |	@b = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |	==\\.@args = \\[
             |		base58'3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs',
             |		base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
             |	\\]
             |	!\\.@args = \\[
             |		false
             |	\\]
             |	@p = false
             |	toBase58String\\.@args = \\[
             |		base58'3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs'
             |	\\]
             |	recipientAddressStr = "3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs"
             |	\\+\\.@args = \\[
             |		"Recipient address error:",
             |		"3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs"
             |	\\]
             |	throw\\.@args = \\[
             |		"Recipient address error:3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs"
             |	\\]
             |""".stripMargin.r
        TxValidationError.logToString(see.log) should fullyMatch regex expected
      }
    }
  }
}

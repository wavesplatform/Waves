package com.wavesplatform.state.diffs

import java.nio.charset.StandardCharsets

import com.wavesplatform.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.{Expression, V1}
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils.compilerContext
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransactionV2
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import org.scalatest.{Inside, PropSpec}

class TransactionValidationErrorPrintTest extends PropSpec with Inside {
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

    val seed    = Address.fromString("3MydsP4UeQdGwBq7yDbMvf9MzfB2pxFoUKU").explicitGet()
    val master  = Address.fromString("3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu").explicitGet()
    val genesis = GenesisTransaction.create(master, 1000000000, 0).explicitGet()

    val issueTransaction = IssueTransactionV2.selfSigned(
      chainId     = AddressScheme.current.chainId,
      sender      = KeyPair(seed.bytes),
      name        = "name".getBytes(StandardCharsets.UTF_8),
      description = "description".getBytes(StandardCharsets.UTF_8),
      quantity    = 100,
      decimals    = 0,
      reissuable  = false,
      script      = Some(typedScript),
      fee         = 10000000,
      timestamp   = 0
    ).explicitGet()

    val transferTransaction = TransferTransactionV2.selfSigned(
        assetId    = IssuedAsset(issueTransaction.id()),
        sender     = KeyPair(master.bytes),
        recipient  = master,
        amount     = 1,
        timestamp  = 0,
        feeAssetId = Waves,
        feeAmount  = 10000000,
        attachment = Array[Byte]()
      )
      .explicitGet()

    assertDiffEi(
      Seq(TestBlock.create(Seq(genesis, issueTransaction))),
      TestBlock.create(Seq(transferTransaction))
    ) {
      error =>
        val expected = //regex because of changeable proof
          """Left\(TransactionValidationError\(cause = ScriptExecutionError\(error = Recipient address error:3PJmMnHHVTTkzvF67HYFjrm5Vj96mM3UtLs, type = Asset, log =
            |	\$match0 = TransferTransaction\(
            |		recipient = Address\(
            |			bytes = base58'3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu'
            |		\)
            |		timestamp = 0
            |		bodyBytes = base58'ZFDBCm7WGpX1zYwdAbbbk2XHyDz2urZGfPHjeiPWuGuemeZBqU4GzZ97iV2a1pt5X7YZttv41PSM3T2h1yMMNBrV6TaCHQK7U738Rs9nNPG241u6zzSCxyh2c7NQmfwq9YmZDb1mPcWXaoVCkCgYqnMk9v895vg4SGF'
            |		assetId = base58'FDdeECUS9tKTjMNyRU8Z5E7dseqBsRWiE92vcsTT8o1i'
            |		feeAssetId = Unit
            |		amount = 1
            |		version = 2
            |		id = base58'4hhZQRZWCabo2JK2cdYnrV9w7u7LhvcdSpf3GYpn6TQA'
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
            |		bodyBytes = base58'ZFDBCm7WGpX1zYwdAbbbk2XHyDz2urZGfPHjeiPWuGuemeZBqU4GzZ97iV2a1pt5X7YZttv41PSM3T2h1yMMNBrV6TaCHQK7U738Rs9nNPG241u6zzSCxyh2c7NQmfwq9YmZDb1mPcWXaoVCkCgYqnMk9v895vg4SGF'
            |		assetId = base58'FDdeECUS9tKTjMNyRU8Z5E7dseqBsRWiE92vcsTT8o1i'
            |		feeAssetId = Unit
            |		amount = 1
            |		version = 2
            |		id = base58'4hhZQRZWCabo2JK2cdYnrV9w7u7LhvcdSpf3GYpn6TQA'
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
            |\),
            |tx = \{
            |  "senderPublicKey" : "EbxDdqXBhj3TEd1UFoi1UE1vm1k7gM9EMYAuLr62iaZF",
            |  "amount" : 1,
            |  "fee" : 10000000,
            |  "type" : 4,
            |  "version" : 2,
            |  "attachment" : "",
            |  "sender" : "3Mrt6Y1QweDrKRRNuhhHGdHpu2kXLXq2QK5",
            |  "feeAssetId" : null,
            |  "proofs" : \[ "\w+" ],
            |  "assetId" : "FDdeECUS9tKTjMNyRU8Z5E7dseqBsRWiE92vcsTT8o1i",
            |  "recipient" : "3N1w8y9Udv3k9NCSv9EE3QvMTRnGFTDQSzu",
            |  "feeAsset" : null,
            |  "id" : "4hhZQRZWCabo2JK2cdYnrV9w7u7LhvcdSpf3GYpn6TQA",
            |  "timestamp" : 0
            |}\)\)""".stripMargin.r

        error.toString should fullyMatch regex expected
    }
  }
}

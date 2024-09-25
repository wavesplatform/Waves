package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{DataTransaction, Proofs, TxPositiveAmount}

class SerContextFunctionsTest extends PropSpec {
  property("check serialization of script with all functions") {
    val entry1 = IntegerDataEntry("int", 24)
    val entry2 = BooleanDataEntry("bool", true)
    val entry3 = BinaryDataEntry("blob", ByteStr.decodeBase64("YWxpY2U=").get)
    val entry4 = StringDataEntry("str", "test")

    val dtx = DataTransaction
      .create(
        1.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        List(entry1, entry2, entry3, entry4),
        100000,
        1526911531530L,
        Proofs(Seq(ByteStr.decodeBase58("32mNYSefBTrkVngG5REkmmGAVv69ZvNhpbegmnqDReMTmXNyYqbECPgHgXrX2UwyKGLFS45j7xDFyPXjF8jcfw94").get))
      )
      .explicitGet()

    val recipient = Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet()
    val ttx =
      TransferTransaction(
        2.toByte,
        PublicKey.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        recipient,
        Waves,
        TxPositiveAmount.unsafeFrom(100000000),
        Waves,
        TxPositiveAmount.unsafeFrom(100000000),
        ByteStr.decodeBase58("4t2Xazb2SX").get,
        1526641218066L,
        Proofs(Seq(ByteStr.decodeBase58("4bfDaqBcnK3hT8ywFEFndxtS1DTSYfncUqd4s5Vyaa66PZHawtC73rDswUur6QZu5RpqM7L9NFgBHT1vhCoox4vi").get)),
        recipient.chainId
      )

    val untypedScript  = Parser.parseExpr(scriptWithAllV1Functions(dtx, ttx)).get.value
    val compiledScript = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), V1, untypedScript).explicitGet()._1
    val bytes = Base64.decode(
      "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"
    )
    SerdeV1.serialize(compiledScript) shouldBe bytes
  }

}

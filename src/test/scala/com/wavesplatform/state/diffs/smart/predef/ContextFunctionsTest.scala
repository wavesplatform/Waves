package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.smart.smartEnabledFS
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState}
import com.wavesplatform.utils.{Base58, dummyCompilerContext}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.transaction.GenesisTransaction
import org.scalacheck.Gen

class ContextFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def compactDataTransactionGen(sender: PrivateKeyAccount) =
    for {
      long <- longEntryGen(dataAsciiKeyGen)
      bool <- booleanEntryGen(dataAsciiKeyGen).filter(_.key != long.key)
      bin  <- binaryEntryGen(40, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key)
      str  <- stringEntryGen(40, dataAsciiKeyGen).filter(e => e.key != long.key && e.key != bool.key && e.key != bin.key)
      tx   <- dataTransactionGenP(sender, List(long, bool, bin, str))
    } yield tx

  val preconditionsAndPayments = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis1 = GenesisTransaction.create(master, ENOUGH_AMT * 3, ts).explicitGet()
    genesis2 = GenesisTransaction.create(recipient, ENOUGH_AMT * 3, ts).explicitGet()
    dataTransaction <- compactDataTransactionGen(recipient)
    transfer        <- transferGeneratorP(ts, master, recipient.toAddress, 100000000L)

    untypedScript = {
      val r = Parser(scriptWithAllFunctions(dataTransaction, transfer)).get.value
      assert(r.size == 1)
      r.head
    }

    typedScript = {
      val compilerScript = CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1
      ScriptV1(compilerScript).explicitGet()
    }
    setScriptTransaction: SetScriptTransaction = SetScriptTransaction.selfSigned(1, recipient, Some(typedScript), 100000000L, ts).explicitGet()

  } yield (Seq(genesis1, genesis2), setScriptTransaction, dataTransaction, transfer)

  property("validation of all functions from contexts") {
    forAll(preconditionsAndPayments) {
      case ((genesis, setScriptTransaction, dataTransaction, transfer)) =>
        assertDiffAndState(smartEnabledFS) { append =>
          append(genesis).explicitGet()
          append(Seq(setScriptTransaction, dataTransaction)).explicitGet()
          append(Seq(transfer)).explicitGet()
        }
    }
  }

  property("reading from data transaction array by key") {
    forAll(preconditionsAndPayments) {
      case ((_, _, tx, _)) =>
        val int  = tx.data(0)
        val bool = tx.data(1)
        val bin  = tx.data(2)
        val str  = tx.data(3)
        val result = runScript[Boolean](
          s"""
               |match tx {
               | case tx: DataTransaction => {
               |  let d = tx.data
               |
               |  let int  = extract(getInteger(d, "${int.key}"))
               |  let bool = extract(getBoolean(d, "${bool.key}"))
               |  let bin  = extract(getBinary(d, "${bin.key}"))
               |  let str  = extract(getString(d, "${str.key}"))
               |
               |  let okInt  = int  == ${int.value}
               |  let okBool = bool == ${bool.value}
               |  let okBin  = bin  == base58'${Base58.encode(bin.asInstanceOf[BinaryDataEntry].value.arr)}'
               |  let okStr  = str  == "${str.value}"
               |
               |  let badInt  = isDefined(getInteger(d, "${bool.key}"))
               |  let badBool = isDefined(getBoolean(d, "${bin.key}"))
               |  let badBin  = isDefined(getBinary(d, "${str.key}"))
               |  let badStr  = isDefined(getString(d, "${int.key}"))
               |
               |  let noSuchKey = isDefined(getInteger(d, "\u00a0"))
               |
               |  let positives = okInt && okBool && okBin && okStr
               |  let negatives = badInt || badBool || badBin || badStr || noSuchKey
               |  positives && ! negatives
               | }
               | case _ => throw()
               |}
               |""".stripMargin,
          tx
        )
        result shouldBe Right(true)
    }
  }

  property("reading from data transaction array by index") {
    forAll(preconditionsAndPayments, Gen.choose(4, 40)) {
      case ((_, _, tx, _), badIndex) =>
        val int  = tx.data(0)
        val bool = tx.data(1)
        val bin  = tx.data(2)
        val str  = tx.data(3)
        val ok = runScript[Boolean](
          s"""
               |match tx {
               | case tx: DataTransaction => {
               |  let d = tx.data
               |
               |  let int  = extract(getInteger(d, 0))
               |  let bool = extract(getBoolean(d, 1))
               |  let bin  = extract(getBinary(d, 2))
               |  let str  = extract(getString(d, 3))
               |
               |  let okInt  = int  == ${int.value}
               |  let okBool = bool == ${bool.value}
               |  let okBin  = bin  == base58'${Base58.encode(bin.asInstanceOf[BinaryDataEntry].value.arr)}'
               |  let okStr  = str  == "${str.value}"
               |
               |  let badInt  = isDefined(getInteger(d, 1))
               |  let badBool = isDefined(getBoolean(d, 2))
               |  let badBin  = isDefined(getBinary(d, 3))
               |  let badStr  = isDefined(getString(d, 0))
               |
               |  let positives = okInt && okBool && okBin && okStr
               |  let negatives = badInt || badBool || badBin || badStr
               |  positives && ! negatives
               | }
               | case _ => throw()
               |}
               |""".stripMargin,
          tx
        )
        ok shouldBe Right(true)

        val outOfBounds = runScript[Boolean](
          s"""
             |match tx {
             | case tx: DataTransaction => isDefined(getInteger(tx.data, $badIndex))
             | case _ => false
             |}
             |""".stripMargin,
          tx
        )
        outOfBounds shouldBe Left(s"java.lang.IndexOutOfBoundsException: $badIndex")
    }
  }
}

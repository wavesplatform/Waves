package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.lang.v1.evaluator.ctx.Obj
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState, produce}
import com.wavesplatform.{NoShrink, TransactionGen}
import fastparse.core.Parsed
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.smart.BlockchainContext
import scorex.transaction.transfer._
import scorex.transaction.{CreateAliasTransaction, GenesisTransaction, Transaction}

class AddressFromRecipientScenarioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndAliasCreations: Gen[(Seq[GenesisTransaction], CreateAliasTransaction, TransferTransactionV1, TransferTransactionV1)] = for {
    master                   <- accountGen
    ts                       <- timestampGen
    other: PrivateKeyAccount <- accountGen
    genesis1: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(other, ENOUGH_AMT, ts).right.get
    alias              <- aliasGen
    fee                <- smallFeeGen
    aliasTx            <- createAliasGen(other, alias, fee, ts)
    transferViaAddress <- transferGeneratorP(master, other, None, None)
    transferViaAlias   <- transferGeneratorP(master, AddressOrAlias.fromBytes(alias.bytes.arr, 0).right.get._1, None, None)
  } yield (Seq(genesis1, genesis2), aliasTx, transferViaAddress, transferViaAlias)

  def evalScript(tx: Transaction, blockchain: Blockchain): Either[com.wavesplatform.lang.ExecutionError, Obj] = {
    val context =
      BlockchainContext.build(AddressScheme.current.chainId, Coeval.evalOnce(tx), Coeval.evalOnce(blockchain.height), blockchain)

    val Parsed.Success(expr, _) = Parser("addressFromRecipient(tx.recipient)")
    assert(expr.size == 1)
    val Right(typedExpr) = CompilerV1(CompilerContext.fromEvaluationContext(context), expr.head)
    EvaluatorV1[Obj](context, typedExpr).left.map(_._3)
  }

  property("Script can resolve AddressOrAlias") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, aliasTx, transferViaAddress, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(aliasTx))) {
          case (_, state) =>
            val Right(addressBytes: ByteVector) =
              evalScript(transferViaAddress, state).explicitGet().fields("bytes").value.value()

            addressBytes.toArray.sameElements(transferViaAddress.recipient.bytes.arr) shouldBe true

            val Right(resolvedAddressBytes: ByteVector) =
              evalScript(transferViaAlias, state).explicitGet().fields("bytes").value.value()

            resolvedAddressBytes.toArray.sameElements(transferViaAddress.recipient.bytes.arr) shouldBe true
        }
    }
  }

  property("Script can't resolve alias that doesn't exist") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, _, _, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq())) {
          case (_, state) =>
            evalScript(transferViaAlias, state) should produce("AliasDoesNotExist")
        }
    }
  }
}

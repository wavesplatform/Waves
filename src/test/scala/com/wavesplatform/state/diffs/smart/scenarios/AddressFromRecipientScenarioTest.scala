package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.lang.v1.ctx.Obj
import com.wavesplatform.lang.v1.{EvaluatorV1, Parser, TypeChecker}
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState, produce}
import com.wavesplatform.{NoShrink, OldTransactionGen}
import fastparse.core.Parsed
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.smart.BlockchainContext
import scorex.transaction.{CreateAliasTransaction, GenesisTransaction, Transaction}

class AddressFromRecipientScenarioTest extends PropSpec with PropertyChecks with Matchers with OldTransactionGen with NoShrink {

  val preconditionsAndAliasCreations: Gen[(Seq[GenesisTransaction], CreateAliasTransaction, TransferTransaction, TransferTransaction)] = for {
    master                   <- accountGen
    ts                       <- timestampGen
    other: PrivateKeyAccount <- accountGen
    genesis1: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(other, ENOUGH_AMT, ts).right.get
    alias <- aliasGen
    fee   <- smallFeeGen
    aliasTx = CreateAliasTransaction.create(other, alias, fee, ts).right.get
    transferViaAddress <- transferGeneratorP(master, other, None, None)
    transferViaAlias   <- transferGeneratorP(master, AddressOrAlias.fromBytes(alias.bytes.arr, 0).right.get._1, None, None)
  } yield (Seq(genesis1, genesis2), aliasTx, transferViaAddress, transferViaAlias)

  def evalScript(tx: Transaction, blockchain: Blockchain): Either[com.wavesplatform.lang.ExecutionError, Obj] = {
    val context =
      BlockchainContext.build(AddressScheme.current.chainId, Coeval.evalOnce(tx), Coeval.evalOnce(blockchain.height), blockchain)

    val Parsed.Success(expr, _) = Parser("addressFromRecipient(tx.recipient)")
    val Right(typedExpr)        = TypeChecker(TypeChecker.TypeCheckerContext.fromContext(context), expr)
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

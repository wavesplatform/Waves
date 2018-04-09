package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.{NoShrink, TransactionGen}
import com.wavesplatform.lang.{Evaluator, Parser, TypeChecker}
import com.wavesplatform.lang.ctx.Obj
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.{assertDiffAndState, produce, ENOUGH_AMT}
import com.wavesplatform.state.reader.SnapshotStateReader
import fastparse.core.Parsed
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scodec.bits.ByteVector
import scorex.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{CreateAliasTransaction, GenesisTransaction, Transaction}
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.smart.BlockchainContext

class AddressFromRecipientScenarioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

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

  def evalScript(tx: Transaction, state: SnapshotStateReader): Either[com.wavesplatform.lang.ExecutionError, Obj] = {
    val context =
      new BlockchainContext(AddressScheme.current.chainId, Coeval.evalOnce(tx), Coeval.evalOnce(state.height), state)
        .build()

    val Parsed.Success(expr, _) = Parser("addressFromRecipient(tx.recipient)")
    val Right(typedExpr)        = TypeChecker(TypeChecker.TypeCheckerContext.fromContext(context), expr)
    Evaluator[Obj](context, typedExpr)
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

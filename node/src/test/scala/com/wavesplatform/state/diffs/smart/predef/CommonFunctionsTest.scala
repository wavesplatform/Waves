package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Testing._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.evaluator.ctx.impl._
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Assertions, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import shapeless.Coproduct

class CommonFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("extract should transaction transfer assetId if exists") {
    forAll(transferV1Gen) {
      case transfer =>
        val result = runScript(
          """
            |match tx {
            | case ttx : TransferTransaction  =>  extract(ttx.assetId)
            | case other => throw()
            | }
            |""".stripMargin,
          Coproduct(transfer)
        )
        transfer.assetId match {
          case IssuedAsset(v) => result.explicitGet().asInstanceOf[CONST_BYTESTR].bs.arr sameElements v.arr
          case Waves          => result should produce("extract() called on unit")
        }
    }
  }

  property("isDefined should return true if transfer assetId exists") {
    forAll(transferV1Gen) {
      case transfer =>
        val result = runScript(
          """
                                          |match tx {
                                          | case ttx : TransferTransaction  =>  isDefined(ttx.assetId)
                                          | case other => throw()
                                          | }
                                          |""".stripMargin,
          Coproduct(transfer)
        )
        result shouldEqual evaluated(transfer.assetId != Waves)
    }
  }

  property("Some/None/extract/isDefined") {
    val some3 = "if true then 3 else unit"
    val none  = "if false then 3 else unit"
    runScript(some3) shouldBe evaluated(3L)
    runScript(none) shouldBe evaluated(unit)
    runScript(s"isDefined($some3)") shouldBe evaluated(true)
    runScript(s"isDefined($none)") shouldBe evaluated(false)
    runScript(s"extract($some3)") shouldBe evaluated(3L)
    runScript(s"extract($none)") should produce("extract() called on unit")
  }

  property("size()") {
    val arr = Array(1: Byte, 2: Byte, 3: Byte)
    runScript("size(base58'')".stripMargin) shouldBe evaluated(0L)
    runScript(s"size(base58'${ByteStr(arr).base58}')".stripMargin) shouldBe evaluated(3L)
  }

  property("getTransfer should extract MassTransfer transfers") {

    forAll(massTransferGen.retryUntil(tg => tg.transfers.nonEmpty && tg.transfers.map(_.address).forall(_.isInstanceOf[Address]))) {
      case massTransfer =>
        val resultAmount = runScript(
          """
            |match tx {
            | case mttx : MassTransferTransaction  =>  mttx.transfers[0].amount
            | case other => throw()
            | }
            |""".stripMargin,
          Coproduct(massTransfer)
        )
        resultAmount shouldBe evaluated(massTransfer.transfers(0).amount)
        val resultAddress = runScript(
          """
                                                      |match tx {
                                                      | case mttx : MassTransferTransaction  =>
                                                      |       match mttx.transfers[0].recipient {
                                                      |           case address : Address => address.bytes
                                                      |           case other => throw()
                                                      |       }
                                                      | case other => throw()
                                                      | }
                                                      |""".stripMargin,
          Coproduct(massTransfer)
        )
        resultAddress shouldBe evaluated(massTransfer.transfers(0).address.bytes)
        val resultLen = runScript(
          """
                                           |match tx {
                                           | case mttx : MassTransferTransaction  =>  size(mttx.transfers)
                                           | case other => throw()
                                           | }
                                           |""".stripMargin,
          Coproduct(massTransfer)
        )
        resultLen shouldBe evaluated(massTransfer.transfers.size.toLong)
    }
  }

  property("+ should check overflow") {
    runScript("2 + 3") shouldBe evaluated(5L)
    runScript(s"1 + ${Long.MaxValue}") should produce("long overflow")
  }

  property("general shadowing verification") {
    forAll(Gen.oneOf(transferV1Gen, transferV2Gen, issueGen, massTransferGen(10))) {
      case (transfer) =>
        val result = runScript(
          s"""
            |match tx {
            | case tx : TransferTransaction  => tx.id == base58'${transfer.id().base58}'
            | case tx : IssueTransaction => tx.fee == ${transfer.assetFee._2}
            | case tx : MassTransferTransaction => tx.timestamp == ${transfer.timestamp}
            | case other => throw()
            | }
            |""".stripMargin,
          Coproduct(transfer)
        )
        result shouldBe evaluated(true)
    }
  }

  property("negative shadowing verification") {
    forAll(Gen.oneOf(transferV2Gen, issueGen, massTransferGen(10))) {
      case (transfer) =>
        try {
          runScript(
            s"""
               |let t = 100
               |match tx {
               | case t: TransferTransaction  => t.id == base58'${transfer.id().base58}'
               | case t: IssueTransaction => t.fee == ${transfer.assetFee._2}
               | case t: MassTransferTransaction => t.timestamp == ${transfer.timestamp}
               | case other => throw()
               | }
               |""".stripMargin,
            Coproduct(transfer)
          )
        } catch {
          case ex: MatchError =>
            Assertions.assert(ex.getMessage().contains("Compilation failed: Value 't' already defined in the scope"))
          case _: Throwable => Assertions.fail("Some unexpected error")
        }
    }
  }

  property("shadowing of empty ref") {
    try {
      runScript(
        s"""
               |match p {
               | case tx: TransferTransaction  => true
               | case other => throw()
               | }
               |""".stripMargin
      )
    } catch {
      case ex: MatchError => Assertions.assert(ex.getMessage().contains("Compilation failed: A definition of 'p' is not found"))
      case _: Throwable   => Assertions.fail("Some unexpected error")
    }
  }

  property("shadowing of inner pattern matching") {
    forAll(Gen.oneOf(transferV2Gen, issueGen)) {
      case transfer =>
        val result =
          runScript(
            s"""
               |match tx {
               | case tx: TransferTransaction | IssueTransaction => {
               |  match tx {
               |    case tx: TransferTransaction  => tx.id == base58'${transfer.id().base58}'
               |    case tx: IssueTransaction => tx.fee == ${transfer.assetFee._2}
               |  }
               |  }
               | case other => throw()
               |}
               |""".stripMargin,
            Coproduct(transfer)
          )
        result shouldBe evaluated(true)
    }
  }

  property("shadowing of variable considered external") {
    runScript(
      s"""
           |match {
           |  let aaa = 1
           |  tx
           |} {
           |     case tx: TransferTransaction  => true
           |     case other => throw()
           | }
           |""".stripMargin
    ) should produce("already defined")
  }

  property("data constructors") {
    forAll(transferV2Gen, longEntryGen(dataAsciiKeyGen)) { (t, entry) =>
      val compareClause = t.recipient match {
        case addr: Address => s"tx.recipient == Address(base58'${addr.stringRepr}')"
        case alias: Alias  => s"""tx.recipient == Alias("${alias.name}")"""
      }
      val transferResult = runScript(
        s"""
           |match tx {
           |  case tx: TransferTransaction =>
           |    let goodEq = $compareClause
           |    let badAddressEq = tx.recipient == Address(base58'Mbembangwana')
           |    let badAddressNe = tx.recipient != Address(base58'3AfZaKieM5')
           |    let badAliasEq = tx.recipient == Alias("Ramakafana")
           |    let badAliasNe = tx.recipient != Alias("Nuripitia")
           |    goodEq && !badAddressEq && badAddressNe && !badAliasEq && badAliasNe
           |  case _ => throw()
           |}
           |""".stripMargin,
        Coproduct(t)
      )
      transferResult shouldBe evaluated(true)

      val dataTx = DataTransaction.create(t.sender, List(entry), 100000L, t.timestamp, Proofs(Seq.empty)).explicitGet()
      val dataResult = runScript(
        s"""
           |match tx {
           |  case tx: DataTransaction =>
           |    let intEq = tx.data[0] == DataEntry("${entry.key}", ${entry.value})
           |    let intNe = tx.data[0] != DataEntry("${entry.key}", ${entry.value})
           |    let boolEq = tx.data[0] == DataEntry("${entry.key}", true)
           |    let boolNe = tx.data[0] != DataEntry("${entry.key}", true)
           |    let binEq = tx.data[0] == DataEntry("${entry.key}", base64'WROOooommmmm')
           |    let binNe = tx.data[0] != DataEntry("${entry.key}", base64'FlapFlap')
           |    let strEq = tx.data[0] == DataEntry("${entry.key}", "${entry.value}")
           |    let strNe = tx.data[0] != DataEntry("${entry.key}", "Zam")
           |    intEq && !intNe && !boolEq && boolNe && !binEq && binNe && !strEq && strNe
           |  case _ => throw()
           |}
         """.stripMargin,
        Coproduct(dataTx)
      )
      dataResult shouldBe evaluated(true)
    }
  }

  property("data constructors bad syntax") {
    val realAddr = "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8"
    val cases = Seq(
      (s"""Address(\"$realAddr\")""", "Compilation failed: Non-matching types"),
      ("Address(base58'GzumLunBoK', 4)", "Function 'Address' requires 1 arguments, but 2 are provided"),
      ("Address()", "Function 'Address' requires 1 arguments, but 0 are provided"),
      (s"Addr(base58'$realAddr')", "Can't find a function 'Addr'")
    )
    for ((clause, err) <- cases) {
      try {
        runScript(
          s"""
             |match tx {
             |  case tx: TransferTransaction =>
             |    let dza = $clause
             |    throw()
             |  case _ => throw()
             |}
             |""".stripMargin
        )
      } catch {
        case ex: MatchError => Assertions.assert(ex.getMessage().contains(err))
        case e: Throwable   => Assertions.fail("Unexpected error", e)
      }
    }
  }
}

package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Testing.*
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BYTESTR
import com.wavesplatform.lang.v1.evaluator.ctx.impl.*
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import org.scalatest.Assertions
import shapeless.Coproduct

import scala.util.Try

class CommonFunctionsTest extends PropSpec {

  property("extract should transaction transfer assetId if exists") {
    val transfer = TxHelpers.transfer(version = TxVersion.V1)

    val result = runScript(
      """
        |match tx {
        | case ttx : TransferTransaction  =>  extract(ttx.assetId)
        | case _ => throw()
        | }
        |""".stripMargin,
      Coproduct(transfer)
    )
    transfer.assetId match {
      case IssuedAsset(v) => result.explicitGet().asInstanceOf[CONST_BYTESTR].bs.arr sameElements v.arr
      case Waves          => result should produce("extract() called on unit")
    }
  }

  property("isDefined should return true if transfer assetId exists") {
    val transfer = TxHelpers.transfer(version = TxVersion.V1)

    val result = runScript(
      """
        |match tx {
        | case ttx : TransferTransaction  =>  isDefined(ttx.assetId)
        | case _ => throw()
        | }
        |""".stripMargin,
      Coproduct(transfer)
    )
    result shouldEqual evaluated(transfer.assetId != Waves)
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
    runScript(s"size(base58'${ByteStr(arr).toString}')".stripMargin) shouldBe evaluated(3L)
  }

  property("getTransfer should extract MassTransfer transfers") {
    val massTransfer = createMassTransfer()

    val resultAmount = runScript(
      """
        |match tx {
        | case mttx : MassTransferTransaction  =>  mttx.transfers[0].amount
        | case _ => throw()
        | }
        |""".stripMargin,
      Coproduct(massTransfer)
    )
    resultAmount shouldBe evaluated(massTransfer.transfers(0).amount.value)
    val resultAddress = runScript(
      """
        |match tx {
        | case mttx : MassTransferTransaction  =>
        |       match mttx.transfers[0].recipient {
        |           case address : Address => address.bytes
        |           case _ => throw()
        |       }
        | case _ => throw()
        | }
        |""".stripMargin,
      Coproduct(massTransfer)
    )
    resultAddress shouldBe evaluated(ByteStr(massTransfer.transfers(0).address.bytes))
    val resultLen = runScript(
      """
        |match tx {
        | case mttx : MassTransferTransaction  =>  size(mttx.transfers)
        | case _ => throw()
        | }
        |""".stripMargin,
      Coproduct(massTransfer)
    )
    resultLen shouldBe evaluated(massTransfer.transfers.size.toLong)
  }

  property("+ should check overflow") {
    runScript("2 + 3") shouldBe evaluated(5L)
    runScript(s"1 + ${Long.MaxValue}") should produce("long overflow")
  }

  property("general shadowing verification") {
    Seq(
      TxHelpers.transfer(version = TxVersion.V1),
      TxHelpers.transfer(),
      TxHelpers.issue(version = TxVersion.V1),
      createMassTransfer()
    ).foreach { tx =>
      val result = runScript(
        s"""
           |match tx {
           | case tx : TransferTransaction  => tx.id == base58'${tx.id().toString}'
           | case tx : IssueTransaction => tx.fee == ${tx.assetFee._2}
           | case tx : MassTransferTransaction => tx.timestamp == ${tx.timestamp}
           | case _ => throw()
           | }
           |""".stripMargin,
        Coproduct(tx)
      )
      result shouldBe evaluated(true)
    }
  }

  property("negative shadowing verification") {
    Seq(
      TxHelpers.transfer(),
      TxHelpers.issue(version = TxVersion.V1),
      createMassTransfer()
    ).foreach { tx =>
      Try {
        runScript(
          s"""
             |let t = 100
             |match tx {
             | case t: TransferTransaction  => t.id == base58'${tx.id().toString}'
             | case t: IssueTransaction => t.fee == ${tx.assetFee._2}
             | case t: MassTransferTransaction => t.timestamp == ${tx.timestamp}
             | case _ => throw()
             | }
             |""".stripMargin,
          Coproduct(tx)
        )
      }.recover[Any] {
        case ex: MatchError =>
          Assertions.assert(ex.getMessage().contains("Compilation failed: Value 't' already defined in the scope"))
        case _: Throwable => Assertions.fail("Some unexpected error")
      }
    }
  }

  property("shadowing of empty ref") {
    Try {
      runScript(
        s"""
           |match p {
           | case _: TransferTransaction  => true
           | case _ => throw()
           | }
           |""".stripMargin
      )
    }.recover[Any] {
      case ex: MatchError => Assertions.assert(ex.getMessage().contains("Compilation failed: A definition of 'p' is not found"))
      case _: Throwable   => Assertions.fail("Some unexpected error")
    }
  }

  property("shadowing of inner pattern matching") {
    Seq(
      TxHelpers.transfer(),
      TxHelpers.issue(version = TxVersion.V1)
    ).foreach { tx =>
      val result =
        runScript(
          s"""
             |match tx {
             | case tx: TransferTransaction | IssueTransaction => {
             |  match tx {
             |    case tx: TransferTransaction  => tx.id == base58'${tx.id().toString}'
             |    case tx: IssueTransaction => tx.fee == ${tx.assetFee._2}
             |  }
             |  }
             | case _ => throw()
             |}
             |""".stripMargin,
          Coproduct(tx)
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
         |     case tx: TransferTransaction => tx == tx
         |     case _ => throw()
         | }
         |""".stripMargin
    ) should produce("already defined")
  }

  property("data constructors") {
    val sender    = TxHelpers.signer(1)
    val recipient = TxHelpers.signer(2)

    val transfer = TxHelpers.transfer(from = sender, to = recipient.toAddress)
    val entry    = IntegerDataEntry("key", 123L)

    val compareClause = (transfer.recipient: @unchecked) match {
      case addr: Address => s"tx.recipient == Address(base58'${addr.toString}')"
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
      Coproduct(transfer)
    )
    transferResult shouldBe evaluated(true)

    val dataTx = TxHelpers.data(sender, Seq(entry))
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

  property("data constructors bad syntax") {
    val realAddr = "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8"
    val cases = Seq(
      (s"""Address(\"$realAddr\")""", "Compilation failed: Non-matching types"),
      ("Address(base58'GzumLunBoK', 4)", "Function 'Address' requires 1 arguments, but 2 are provided"),
      ("Address()", "Function 'Address' requires 1 arguments, but 0 are provided"),
      (s"Addr(base58'$realAddr')", "Can't find a function 'Addr'")
    )
    for ((clause, err) <- cases) {
      Try {
        runScript(
          s"""
             |match tx {
             |  case _: TransferTransaction =>
             |    let dza = $clause
             |    throw()
             |  case _ => throw()
             |}
             |""".stripMargin
        )
      }.recover[Any] {
        case ex: MatchError => Assertions.assert(ex.getMessage().contains(err))
        case e: Throwable   => Assertions.fail("Unexpected error", e)
      }
    }
  }

  private def createMassTransfer(): MassTransferTransaction = {
    val sender     = TxHelpers.signer(1)
    val recipients = (1 to 10).map(idx => TxHelpers.address(idx + 1))
    TxHelpers.massTransfer(
      from = sender,
      to = recipients.map(addr => (addr, 1.waves)),
      version = TxVersion.V1
    )
  }
}

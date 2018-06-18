package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertions, Matchers, PropSpec}
import scodec.bits.ByteVector
import scorex.account.Address
import org.scalacheck.Gen

class CommonFunctionsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("extract should transaction transfer assetId if exists") {
    forAll(transferV1Gen) {
      case (transfer) =>
        val result = runScript[ByteVector](
          """
            |match tx {
            | case ttx : TransferTransaction  =>  extract(ttx.transferAssetId)
            | case other => throw
            | }
            |""".stripMargin,
          transfer
        )
        transfer.assetId match {
          case Some(v) => result.explicitGet().toArray sameElements v.arr
          case None    => result should produce("from empty option")
        }
    }
  }

  property("isDefined should return true if transfer assetId exists") {
    forAll(transferV1Gen) {
      case (transfer) =>
        val result = runScript[Boolean](
          """
                                          |match tx {
                                          | case ttx : TransferTransaction  =>  isDefined(ttx.transferAssetId)
                                          | case other => throw
                                          | }
                                          |""".stripMargin,
          transfer
        )
        transfer.assetId.isDefined shouldEqual result.explicitGet()
    }
  }

  property("Some/None/extract/isDefined") {
    runScript[Any]("Some(3)".stripMargin) shouldBe Right(3L)
    runScript[Any]("None".stripMargin) shouldBe Right(())
    runScript[Boolean]("isDefined(Some(3))".stripMargin) shouldBe Right(true)
    runScript[Boolean]("isDefined(None)".stripMargin) shouldBe Right(false)
    runScript[Long]("extract(Some(3))".stripMargin) shouldBe Right(3L)
    runScript[Long]("extract(None)".stripMargin) should produce("Extract from empty option")
  }

  property("size()") {
    val arr = Array(1: Byte, 2: Byte, 3: Byte)
    runScript[Long]("size(base58'')".stripMargin) shouldBe Right(0L)
    runScript[Long](s"size(base58'${ByteStr(arr).base58}')".stripMargin) shouldBe Right(3L)
  }

  property("getTransfer should extract MassTransfer transfers") {
    import scodec.bits.ByteVector
    forAll(massTransferGen.retryUntil(tg => tg.transfers.size > 0 && tg.transfers.map(_.address).forall(_.isInstanceOf[Address]))) {
      case (massTransfer) =>
        val resultAmount = runScript[Long](
          """
            |match tx {
            | case mttx : MassTransferTransaction  =>  mttx.transfers[0].amount
            | case other => throw
            | }
            |""".stripMargin,
          massTransfer
        )
        resultAmount shouldBe Right(massTransfer.transfers(0).amount)
        val resultAddress = runScript[ByteVector](
          """
                                                      |match tx {
                                                      | case mttx : MassTransferTransaction  =>
                                                      |       match mttx.transfers[0].recipient {
                                                      |           case address : Address => address.bytes
                                                      |           case other => throw
                                                      |       }
                                                      | case other => throw
                                                      | }
                                                      |""".stripMargin,
          massTransfer
        )
        resultAddress shouldBe Right(ByteVector(massTransfer.transfers(0).address.bytes.arr))
        val resultLen = runScript[Long](
          """
                                           |match tx {
                                           | case mttx : MassTransferTransaction  =>  size(mttx.transfers)
                                           | case other => throw
                                           | }
                                           |""".stripMargin,
          massTransfer
        )
        resultLen shouldBe Right(massTransfer.transfers.size.toLong)
    }
  }

  property("+ should check overflow") {
    runScript[Long]("2 + 3") shouldBe Right(5L)
    runScript[Long](s"1 + ${Long.MaxValue}") should produce("long overflow")
  }

  property("general shadowing verification") {
    forAll(Gen.oneOf(transferV1Gen, transferV2Gen, issueGen, massTransferGen(10))) {
      case (transfer) =>
        val result = runScript[Boolean](
          s"""
            |match tx {
            | case tx : TransferTransaction  => tx.id == base58'${transfer.id().base58}'
            | case tx : IssueTransaction => tx.fee == ${transfer.assetFee._2}
            | case tx : MassTransferTransaction => tx.timestamp == ${transfer.timestamp}
            | case other => throw
            | }
            |""".stripMargin,
          transfer
        )
        result shouldBe Right(true)
    }
  }

  property("negative shadowing verification") {
    forAll(Gen.oneOf(transferV2Gen, issueGen, massTransferGen(10))) {
      case (transfer) =>
        try {
          runScript[Boolean](
            s"""
               |let t = 100
               |match tx {
               | case t: TransferTransaction  => t.id == base58'${transfer.id().base58}'
               | case t: IssueTransaction => t.fee == ${transfer.assetFee._2}
               | case t: MassTransferTransaction => t.timestamp == ${transfer.timestamp}
               | case other => throw
               | }
               |""".stripMargin,
            transfer
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
      runScript[Boolean](
        s"""
               |match p {
               | case tx: TransferTransaction  => true
               | case other => throw
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
      case (transfer) =>
        val result =
          runScript[Boolean](
            s"""
               |match tx {
               | case tx: TransferTransaction | IssueTransaction => {
               |  match tx {
               |    case tx: TransferTransaction  => tx.id == base58'${transfer.id().base58}'
               |    case tx: IssueTransaction => tx.fee == ${transfer.assetFee._2}
               |  }
               |  }
               | case other => throw
               |}
               |""".stripMargin,
            transfer
          )
        result shouldBe Right(true)
    }
  }

  property("shadowing of external variable") {
    //TODO: script can be simplified after NODE-837 fix
    try {
      runScript[Boolean](
        s"""
           |match {
           |  let aaa = 1
           |  tx
           |} {
           |     case tx: TransferTransaction  => true
           |     case other => throw
           | }
           |""".stripMargin
      )

    } catch {
      case ex: MatchError => Assertions.assert(ex.getMessage().contains("Compilation failed: Value 'tx' already defined in the scope"))
      case _: Throwable   => Assertions.fail("Some unexpected error")
    }
  }

}

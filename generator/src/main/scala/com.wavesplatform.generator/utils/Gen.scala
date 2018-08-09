package com.wavesplatform.generator.utils

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.generator.utils.Implicits._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.transaction.smart.script.{Script, ScriptCompiler}
import com.wavesplatform.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{Proofs, Transaction}
import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519._

object Gen {
  private def random = ThreadLocalRandom.current

  def multiSigScript(owners: Seq[PrivateKeyAccount], requiredProofsCount: Int): Script = {
    val accountsWithIndexes = owners.zipWithIndex
    val keyLets =
      accountsWithIndexes map {
        case (acc, i) =>
          s"let accountPK$i = base58'${ByteStr(acc.publicKey).base58}'"
      } mkString "\n"

    val signedLets =
      accountsWithIndexes map {
        case (_, i) =>
          s"let accountSigned$i = if(sigVerify(tx.bodyBytes, tx.proofs[$i], accountPK$i)) then 1 else 0"
      } mkString "\n"

    val proofSum = accountsWithIndexes map {
      case (_, ind) =>
        s"accountSigned$ind"
    } mkString ("let proofSum = ", " + ", "")

    val finalStatement = s"proofSum >= $requiredProofsCount"

    val src =
      s"""
       |$keyLets
       |
       |$signedLets
       |
       |$proofSum
       |
       |$finalStatement
      """.stripMargin

    val (script, _) = ScriptCompiler(src)
      .explicitGet()

    script
  }

  def txs(minFee: Long, maxFee: Long, senderAccounts: Seq[PrivateKeyAccount], recipientGen: Iterator[Address]): Iterator[Transaction] = {
    val senderGen = Iterator.randomContinually(senderAccounts)
    val feeGen    = Iterator.continually(minFee + random.nextLong(maxFee - minFee))
    transfers(senderGen, recipientGen, feeGen)
  }

  def transfers(senderGen: Iterator[PrivateKeyAccount], recipientGen: Iterator[Address], feeGen: Iterator[Long]): Iterator[Transaction] = {
    senderGen
      .zip(recipientGen)
      .zip(feeGen)
      .map {
        case ((src, dst), fee) =>
          TransferTransactionV1.selfSigned(None, src, dst, fee, System.currentTimeMillis(), None, fee, Array.emptyByteArray)
      }
      .collect { case Right(x) => x }
  }

  def massTransfers(senderGen: Iterator[PrivateKeyAccount], recipientGen: Iterator[Address], amountGen: Iterator[Long]): Iterator[Transaction] = {
    val transferCountGen = Iterator.continually(random.nextInt(MassTransferTransaction.MaxTransferCount + 1))
    senderGen
      .zip(transferCountGen)
      .map {
        case (sender, count) =>
          val transfers = List.tabulate(count)(_ => ParsedTransfer(recipientGen.next(), amountGen.next()))
          val fee       = 100000 + count * 50000
          MassTransferTransaction.selfSigned(Proofs.Version, None, sender, transfers, System.currentTimeMillis, fee, Array.emptyByteArray)
      }
      .collect { case Right(tx) => tx }
  }

  val address: Iterator[Address] = Iterator.continually {
    val pk = Array.fill[Byte](KeyLength)(random.nextInt(Byte.MaxValue).toByte)
    Address.fromPublicKey(pk)
  }

  def address(uniqNumber: Int): Iterator[Address] = Iterator.randomContinually(address.take(uniqNumber).toSeq)

  def address(limitUniqNumber: Option[Int]): Iterator[Address] = limitUniqNumber.map(address(_)).getOrElse(address)

  def setScriptAllowedScript(i: Int): Script = {
    val expr = BLOCK(
      LET("A", CONST_BYTEVECTOR(ByteVector.fromHex("0x30dfd681a026cba7ae2193f291398ba390062187d52954022891cadb9d1ac87d").get)),
      BLOCK(
        LET("B", CONST_BYTEVECTOR(ByteVector.fromHex("0x9e6a3565145298ffb6042410579fc8745a48a7cb690afae8b9e8aaba35e25b4c").get)),
        BLOCK(
          LET("C", CONST_BYTEVECTOR(ByteVector.fromHex("0x4196cb134edfd03197a1c6de649ee0b8a3308ca2d84c87e56f186acd4de28f2a").get)),
          BLOCK(
            LET("proofs", GETTER(REF("tx"), "proofs")),
            BLOCK(
              LET(
                "AC",
                IF(
                  FUNCTION_CALL(Native(500),
                                List(GETTER(REF("tx"), "bodyBytes"), FUNCTION_CALL(Native(401), List(REF("proofs"), CONST_LONG(0))), REF("A"))),
                  CONST_LONG(1),
                  CONST_LONG(0)
                )
              ),
              BLOCK(
                LET(
                  "BC",
                  IF(
                    FUNCTION_CALL(Native(500),
                                  List(GETTER(REF("tx"), "bodyBytes"), FUNCTION_CALL(Native(401), List(REF("proofs"), CONST_LONG(1))), REF("B"))),
                    CONST_LONG(1),
                    CONST_LONG(0)
                  )
                ),
                BLOCK(
                  LET(
                    "CC",
                    IF(
                      FUNCTION_CALL(Native(500),
                                    List(GETTER(REF("tx"), "bodyBytes"), FUNCTION_CALL(Native(401), List(REF("proofs"), CONST_LONG(2))), REF("C"))),
                      CONST_LONG(1),
                      CONST_LONG(0)
                    )
                  ),
                  IF(
                    FUNCTION_CALL(Native(103),
                                  List(FUNCTION_CALL(Native(100), List(FUNCTION_CALL(Native(100), List(REF("AC"), REF("BC"))), REF("CC"))),
                                       CONST_LONG(2))),
                    TRUE,
                    FUNCTION_CALL(Native(0), List(CONST_LONG(3), CONST_LONG(3)))
                  )
                )
              )
            )
          )
        )
      )
    )
    ???
  }

}

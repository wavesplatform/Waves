package com.wavesplatform.it.sync

import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state._
import com.wavesplatform.utils.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber
import scorex.account.PrivateKeyAccount
import scorex.transaction.Proofs
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._

import scala.util.Random
import scala.concurrent.duration._

/*
Scenario:
1. Alice's and swapBC1's balances initialisation
2. Create and setup smart contract for swapBC1
3. Alice funds swapBC1t
4. Alice can't take money from swapBC1
5.1 Bob takes funds because he knows secret hash OR 5.2 Wait height and Alice takes funds back

 */

class AtomicSwapSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val BobBC1: String   = sender.createAddress()
  private val AliceBC1: String = sender.createAddress()
  private val swapBC1: String  = sender.createAddress()

  private val transferAmount: Long = 1.waves
  private val fee: Long            = 0.001.waves

  private val AlicesPK = PrivateKeyAccount.fromSeed(sender.seed(AliceBC1)).right.get

  private val secretText = "some secret message from Alice"
  private val shaSecret  = "BN6RTYGWcwektQfSFzH8raYo9awaLgQ7pLyWLQY4S4F5"

  private val sc1 = {
    val untyped = Parser(s"""
    let Bob = extract(addressFromString("${BobBC1}")).bytes
    let Alice = extract(addressFromString("${AliceBC1}")).bytes
    let AlicesPK = base58'${ByteStr(AlicesPK.publicKey)}'

    let txRecipient = addressFromRecipient(tx.recipient).bytes
    let txSender = addressFromPublicKey(tx.senderPk).bytes

    let txToBob = ((txRecipient == Bob) && (sha256(tx.proofs[0]) == base58'$shaSecret') && (20 >= height) && sigVerify(tx.bodyBytes,tx.proofs[1],AlicesPK))
    let backToAliceAfterHeight = ((height >= 21) && (txRecipient == Alice))

    txToBob || backToAliceAfterHeight
      """.stripMargin).get.value
    CompilerV1(dummyTypeCheckerContext, untyped).explicitGet()
  }

  test("step1: Balances initialization") {
    val toAliceBC1TxId = sender.transfer(sender.address, AliceBC1, 10 * transferAmount, fee).id
    nodes.waitForHeightAriseAndTxPresent(toAliceBC1TxId)

    val toSwapBC1TxId = sender.transfer(sender.address, swapBC1, fee, fee).id
    nodes.waitForHeightAriseAndTxPresent(toSwapBC1TxId)
  }

  test("step2: Create and setup smart contract for swapBC1") {
    val pkSwapBC1 = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get
    val script    = ScriptV1(sc1).explicitGet()
    val sc1SetTx = SetScriptTransaction
      .selfSigned(version = SetScriptTransaction.supportedVersions.head,
                  sender = pkSwapBC1,
                  script = Some(script),
                  fee = fee,
                  timestamp = System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(sc1SetTx.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val swapBC1ScriptInfo = sender.addressScriptInfo(swapBC1)

    swapBC1ScriptInfo.script.isEmpty shouldBe false
    swapBC1ScriptInfo.scriptText.isEmpty shouldBe false
  }

  test("step3: Alice makes transfer to swapBC1") {
    val txToSwapBC1 =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(sender.seed(AliceBC1)).right.get,
          recipient = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get,
          amount = transferAmount + fee + 0.004.waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = fee + 0.004.waves,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(txToSwapBC1.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("step4: Alice cannot make transfer from swapBC1") {
    val txToSwapBC1 =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get,
          recipient = PrivateKeyAccount.fromSeed(sender.seed(AliceBC1)).right.get,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = fee + 0.004.waves,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    assertBadRequest(sender.signedBroadcast(txToSwapBC1.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))))
  }

  test("step5: Bob makes transfer OR Alice takes funds back") {
    val isBobTakesFunds = Random.nextBoolean()

    val signed = if (isBobTakesFunds) {
      val unsigned =
        TransferTransactionV2
          .create(
            version = 2,
            assetId = None,
            sender = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get,
            recipient = PrivateKeyAccount.fromSeed(sender.seed(BobBC1)).right.get,
            amount = transferAmount,
            timestamp = System.currentTimeMillis(),
            feeAssetId = None,
            feeAmount = fee + 0.004.waves,
            attachment = Array.emptyByteArray,
            proofs = Proofs.empty
          )
          .explicitGet()

      val proof    = ByteStr(secretText.getBytes())
      val sigAlice = ByteStr(crypto.sign(AlicesPK, unsigned.bodyBytes()))
      unsigned.copy(proofs = Proofs(Seq(proof, sigAlice)))
    } else {
      sender.waitForHeight(sender.height + 11, 2.minutes)

      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get,
          recipient = PrivateKeyAccount.fromSeed(sender.seed(AliceBC1)).right.get,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = fee + 0.004.waves,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    }

    val versionedTransferId =
      sender.signedBroadcast(signed.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)
  }

  protected def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      fee * (dataSize / 1024 + 1)
    } else fee
  }
}

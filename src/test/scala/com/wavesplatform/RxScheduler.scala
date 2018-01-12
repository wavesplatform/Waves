package com.wavesplatform

import com.wavesplatform.state2._
import monix.execution.Ack
import monix.reactive.subjects.PublishSubject
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock, SignerData}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.TransactionParser
import scorex.transaction.assets.TransferTransaction

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._


trait RxScheduler {

  implicit val scheduler = monix.execution.Scheduler.singleThread("rx-scheduler")

  def test[A](f: => Future[A]): A = Await.result(f, 10.seconds)

  def send[A](p: PublishSubject[A])(a: A): Future[Ack] = p.onNext(a).map(ack => {
    Thread.sleep(500)
    ack
  })

  def byteStr(id: Int): ByteStr = ByteStr(Array.concat(Array.fill(TransactionParser.SignatureLength - 1)(0), Array(id.toByte)))

  val signer: PrivateKeyAccount = TestBlock.defaultSigner

  def block(id: Int): Block = TestBlock.create(Seq.empty).copy(signerData = SignerData(signer, byteStr(id)))

  def microBlock(total: Int, prev: Int): MicroBlock = {
    val tx = TransferTransaction.create(None, signer, signer.toAddress, 1, 1, None, 1, Array.emptyByteArray).explicitGet()
    MicroBlock.buildAndSign(signer, Seq(tx), byteStr(prev), byteStr(total)).explicitGet()
  }
}

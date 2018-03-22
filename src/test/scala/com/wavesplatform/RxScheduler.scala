package com.wavesplatform

import com.wavesplatform.state2._
import monix.execution.schedulers.SchedulerService
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import org.scalatest.{BeforeAndAfterAll, Suite}
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock, SignerData}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.TransactionParser
import scorex.transaction.assets.TransferTransaction

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


trait RxScheduler extends BeforeAndAfterAll { _: Suite =>
  implicit val implicitScheduler: SchedulerService = Scheduler.singleThread("rx-scheduler")

  def testSchedulerName: String
  lazy val testScheduler = Scheduler.singleThread(testSchedulerName)

  def test[A](f: => Future[A]): A = Await.result(f, 10.seconds)

  def send[A](p: Observer[A])(a: A): Future[Ack] = p.onNext(a).map(ack => {
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

  override protected def afterAll(): Unit = {
    super.afterAll()
    implicitScheduler.shutdown()
    testScheduler.shutdown()
  }
}

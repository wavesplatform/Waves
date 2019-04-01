package com.wavesplatform

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock, SignerData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer._
import monix.execution.schedulers.SchedulerService
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait RxScheduler extends BeforeAndAfterAll { _: Suite =>
  implicit val implicitScheduler: SchedulerService = Scheduler.singleThread("rx-scheduler")

  def testSchedulerName: String
  lazy val testScheduler: SchedulerService = Scheduler.singleThread(testSchedulerName)

  def test[A](f: => Future[A]): A = Await.result(f, 10.seconds)

  def send[A](p: Observer[A])(a: A): Future[Ack] =
    p.onNext(a)
      .map(ack => {
        Thread.sleep(500)
        ack
      })

  def byteStr(id: Int): ByteStr = ByteStr(Array.concat(Array.fill(SignatureLength - 1)(0), Array(id.toByte)))

  val signer: KeyPair = TestBlock.defaultSigner

  def block(id: Int): Block = TestBlock.create(Seq.empty).copy(signerData = SignerData(signer, byteStr(id)))

  def microBlock(total: Int, prev: Int): MicroBlock = {
    val tx = TransferTransactionV1.selfSigned(Waves, signer, signer.toAddress, 1, 1, Waves, 1, Array.emptyByteArray).explicitGet()
    MicroBlock.buildAndSign(signer, Seq(tx), byteStr(prev), byteStr(total)).explicitGet()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    implicitScheduler.shutdown()
    testScheduler.shutdown()
  }
}

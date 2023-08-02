package com.wavesplatform

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, MicroBlock}
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

  def block(id: Int): Block = TestBlock.create(Seq.empty).block.copy(signature = byteStr(id))

  def microBlock(total: Int, prev: Int): MicroBlock = {
    val tx = TransferTransaction.selfSigned(1.toByte, signer, signer.toAddress, Waves, 1, Waves, 1, ByteStr.empty, 1).explicitGet()
    MicroBlock.buildAndSign(3.toByte, signer, Seq(tx), byteStr(prev), byteStr(total), None).explicitGet()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    implicitScheduler.shutdown()
    testScheduler.shutdown()
  }
}

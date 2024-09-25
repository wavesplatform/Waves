package com.wavesplatform.generator

import java.net.{InetSocketAddress, URL}
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import cats.Show
import cats.effect.concurrent.Ref
import cats.syntax.flatMap.*
import com.wavesplatform.generator.Worker.{EmptyState, Settings, SkipState, State}
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import monix.eval.Task
import monix.execution.Scheduler
import org.asynchttpclient.AsyncHttpClient
import play.api.libs.json.Json

import scala.compat.java8.FutureConverters
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

class Worker(
    settings: Settings,
    transactionSource: Iterator[Transaction],
    networkSender: NetworkSender,
    node: InetSocketAddress,
    nodeRestAddress: URL,
    canContinue: () => Boolean,
    initial: Seq[Transaction],
    richAccountAddresses: Seq[String],
    tailInitial: Seq[Transaction] = Seq()
)(implicit httpClient: AsyncHttpClient, ec: ExecutionContext)
    extends ScorexLogging {

  def run(): Future[Unit] =
    task.runAsyncLogErr(Scheduler(ec))

  private[this] val task =
    for {
      state <- Ref.of[Task, State](EmptyState(settings.warmUp))
      initState <- settings.initialWarmUp match {
        case Some(warmUp) => Ref.of[Task, State](EmptyState(warmUp))
        case None         => Ref.of[Task, State](SkipState(settings.utxLimit))
      }
      channel <- withReconnect(getChannel)
      _       <- logInfo("INITIAL PHASE")
      channel <- withReconnect(writeInitial(channel, initState))
      _       <- logInfo("GENERAL PHASE")
      channel <- withReconnect(pullAndWrite(channel, state))
      _       <- closeChannel(channel)
    } yield ()

  private[this] val nodeUTXTransactionsToSendCount: Task[Int] = Task.defer {
    import org.asynchttpclient.Dsl.*
    val request = get(s"$nodeRestAddress/transactions/unconfirmed/size").build()
    Task
      .fromFuture(FutureConverters.toScala(httpClient.executeRequest(request).toCompletableFuture))
      .map(r => math.max(settings.utxLimit - (Json.parse(r.getResponseBody) \ "size").as[Int], 0))
  }

  private[this] val balanceOfRichAccount: Task[Map[String, Long]] =
    Task
      .defer {
        import org.asynchttpclient.Dsl.*
        val results = richAccountAddresses.map { address =>
          val request = get(s"$nodeRestAddress/addresses/balance/$address").build()
          Task
            .fromFuture(FutureConverters.toScala(httpClient.executeRequest(request).toCompletableFuture))
            .map(r => address -> (Json.parse(r.getResponseBody) \ "balance").as[Long])
        }
        Task.parSequence(results).map(_.toMap)
      }
      .onErrorFallbackTo(Task.now(Map()))

  private[this] val retrieveBalances: Task[Unit] =
    if (!canContinue())
      Task.unit
    else
      for {
        balances <- balanceOfRichAccount
        _        <- if (balances.nonEmpty) logInfo(s"Balances: ${balances.mkString("(", ", ", ")")}") else Task.unit
      } yield ()

  private[this] def writeInitial(channel: Channel, state: Ref[Task, State], txs: Seq[Transaction] = initial): Task[Channel] =
    if (!canContinue())
      Task.now(channel)
    else
      for {
        validChannel <- validateChannel(channel)
        _            <- logInfo(s"Sending initial transactions to $validChannel")
        cntToSend    <- calcAndSaveCntToSend(state)
        _            <- Task.deferFuture(networkSender.send(validChannel, txs.take(cntToSend)*))
        r <-
          if (cntToSend >= txs.size) sleepOrWaitEmptyUtx(settings.tailInitialDelay) *> writeTailInitial(validChannel, state)
          else sleep(settings.delay) *> Task.defer(writeInitial(channel, state, txs.drop(cntToSend)))
      } yield r

  private[this] def sleepOrWaitEmptyUtx(strategy: Either[FiniteDuration, FiniteDuration]): Task[Unit] =
    strategy match {
      case Left(duration) => sleep(duration)
      case Right(duration) =>
        for {
          _ <- sleep(duration)
          _ <- nodeUTXTransactionsToSendCount >>= (cnt => if (cnt == settings.utxLimit) Task.unit else Task.defer(sleepOrWaitEmptyUtx(strategy)))
        } yield ()
    }

  private[this] def writeTailInitial(channel: Channel, state: Ref[Task, State], txs: Seq[Transaction] = tailInitial): Task[Channel] =
    if (!canContinue())
      Task.now(channel)
    else
      for {
        validChannel <- validateChannel(channel)
        _            <- logInfo(s"Sending tail initial transactions to $validChannel")
        cntToSend    <- calcAndSaveCntToSend(state)
        _            <- Task.deferFuture(networkSender.send(validChannel, txs.take(cntToSend)*))
        r <-
          if (cntToSend >= txs.size) sleepOrWaitEmptyUtx(settings.initialDelay) *> Task.now(validChannel)
          else sleep(settings.delay) *> Task.defer(writeTailInitial(validChannel, state, txs.drop(cntToSend)))
      } yield r

  private[this] def pullAndWrite(channel: Channel, state: Ref[Task, State], cnt: Int = 0): Task[Channel] =
    if (!canContinue())
      Task.now(channel)
    else
      for {
        _            <- if (cnt % 10 == 0) retrieveBalances.executeAsync else Task.unit
        validChannel <- validateChannel(channel)
        cntToSend    <- calcAndSaveCntToSend(state)
        _            <- logInfo(s"Sending $cntToSend transactions to $validChannel")
        txs          <- Task(transactionSource.take(cntToSend).to(LazyList))
        _            <- txs.headOption.fold(Task.unit)(tx => logInfo(s"Head transaction id: ${tx.id()}"))
        _            <- Task.deferFuture(networkSender.send(validChannel, txs*))
        _            <- sleep(settings.delay)
        r            <- Task.defer(pullAndWrite(validChannel, state, (cnt + 1) % 10))
      } yield r

  private[this] def calcAndSaveCntToSend(stateRef: Ref[Task, State]): Task[Int] =
    for {
      utxCnt <- nodeUTXTransactionsToSendCount
      state  <- stateRef.get
      nextState = state.next(utxCnt)
      _ <- logTrace(s"Prev state: $state, new state: $nextState, tx number to utx limit: $utxCnt")
      _ <- stateRef.set(nextState)
    } yield nextState.cnt

  private[this] def withReconnect[A](baseTask: Task[A]): Task[A] =
    baseTask.onErrorHandleWith {
      case error if settings.autoReconnect && canContinue() =>
        logError(s"[$node] An error during sending transactions, reconnect", error) *>
          sleep(settings.reconnectDelay) *>
          Task.defer(withReconnect(baseTask))
      case error =>
        logError("Stopping because autoReconnect is disabled", error) *>
          Task.raiseError(error)
    }

  private[this] def getChannel: Task[Channel]                        = Task.deferFuture(networkSender.connect(node))
  private[this] def closeChannel(channel: Channel): Task[Unit]       = Task(channel.close())
  private[this] def validateChannel(channel: Channel): Task[Channel] = if (channel.isOpen) Task.now(channel) else getChannel

  private[this] def logError(msg: => String, err: Throwable): Task[Unit] = Task(log.error(msg, err))
  private[this] def logInfo(msg: => String): Task[Unit]                  = Task(log.info(msg))
  private[this] def logTrace(msg: => String): Task[Unit]                 = Task(log.trace(msg))

  private[this] def sleep(delay: FiniteDuration): Task[Unit] = logInfo(s"Sleeping for $delay") *> Task.sleep(delay)
}

object Worker {
  case class Settings(
      utxLimit: Int,
      delay: FiniteDuration,
      tailInitialDelay: Either[FiniteDuration, FiniteDuration],
      initialDelay: Either[FiniteDuration, FiniteDuration],
      workingTime: FiniteDuration,
      autoReconnect: Boolean,
      reconnectDelay: FiniteDuration,
      warmUp: WarmUp,
      initialWarmUp: Option[WarmUp]
  )

  case class WarmUp(
      start: Int,
      end: Int,
      step: Int,
      duration: Option[FiniteDuration],
      once: Boolean
  )

  sealed trait State {
    def cnt: Int
    def next(utxToSendCnt: Int): State =
      this match {
        case SkipState(_) => SkipState(utxToSendCnt)
        case EmptyState(warmUp) =>
          WorkState(warmUp.start, false, warmUp.duration.map(d => LocalDateTime.now.plus(d.toMillis, ChronoUnit.MILLIS)), warmUp)
        case s @ WorkState(cnt, raised, endAfter, warmUp) =>
          if (raised) s.copy(cnt = utxToSendCnt)
          else {
            endAfter match {
              case Some(ldt) if ldt.isBefore(LocalDateTime.now) => s.copy(cnt = utxToSendCnt, raised = true)
              case _ =>
                val mayBeNextCnt = math.min(cnt + warmUp.step, warmUp.end)
                val nextCnt      = math.min(mayBeNextCnt, utxToSendCnt)
                val nextRaised   = nextCnt == warmUp.end && warmUp.once
                WorkState(nextCnt, nextRaised, endAfter, warmUp)
            }
          }
      }
  }

  final case class EmptyState(warmUp: WarmUp) extends State {
    val cnt: Int                  = 0
    override def toString: String = "EmptyState"
  }

  final case class WorkState(cnt: Int, raised: Boolean, endAfter: Option[LocalDateTime], warmUp: WarmUp) extends State {
    require(cnt >= 0)
    override def toString: String = s"State(cnt=$cnt, raised=$raised, endAfter=$endAfter)"
  }

  final case class SkipState(cnt: Int) extends State {
    override def toString: String = "SkipState"
  }

  implicit val toPrintable: Show[Settings] = { x =>
    import x.*

    s"""initial delay: $initialDelay
       |delay between iterations: $delay
       |auto reconnect: ${if (autoReconnect) "enabled" else "disabled"}
       |reconnect delay: $reconnectDelay
       |warm-up: (start=${warmUp.start}, end=${warmUp.end}, step=${warmUp.step}, once=${warmUp.once})
       |""".stripMargin
  }
}

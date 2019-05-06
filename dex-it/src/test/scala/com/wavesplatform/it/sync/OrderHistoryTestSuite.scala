package com.wavesplatform.it.sync

import java.sql.{Connection, DriverManager}

import cats.implicits._
import com.spotify.docker.client.DefaultDockerClient
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.sync.config.MatcherPriceAssetConfig._
import com.wavesplatform.it.{DockerContainerLauncher, MatcherSuiteBase}
import com.wavesplatform.matcher.history.DBRecords.{EventRecord, OrderRecord}
import com.wavesplatform.matcher.model.{MatcherModel, OrderValidator}
import com.wavesplatform.matcher.settings.PostgresConnection._
import com.wavesplatform.matcher.settings.{OrderHistorySettings, PostgresConnection}
import com.wavesplatform.transaction.assets.exchange.Order.PriceConstant
import com.wavesplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import io.getquill.{PostgresJdbcContext, SnakeCase}
import net.ceedubs.ficus.Ficus._

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.{Failure, Try}

class OrderHistoryTestSuite extends MatcherSuiteBase {

  val postgresImageName, postgresUser = "postgres"
  val postgresContainerName           = "pgc"
  val postgresPassword                = "docker"
  val postgresEnv                     = s"POSTGRES_PASSWORD=$postgresPassword"
  val postgresContainerPort           = "5432"

  val postgresContainerLauncher         = new DockerContainerLauncher(postgresImageName, postgresContainerName, postgresEnv, postgresContainerPort)
  val dockerClient: DefaultDockerClient = postgresContainerLauncher.dockerClient

  val batchLingerMs: Int = OrderHistorySettings.defaultBatchLingerMs

  @annotation.tailrec
  private def retry[T](attemptsCount: Int, delayInMs: Int = 0)(fn: => T): Try[T] = {
    Try { fn } match {
      case Failure(_) if attemptsCount > 1 => if (delayInMs != 0) Thread.sleep(delayInMs); retry(attemptsCount - 1)(fn)
      case Failure(ex)                     => throw ex
      case success                         => success
    }
  }

  def createTables(postgresAddress: String): Unit = {

    val driver                  = "org.postgresql.Driver"
    val url                     = s"jdbc:postgresql://$postgresAddress/$postgresImageName"
    val orderHistoryDDLFileName = "/order-history/order-history-ddl.sql"

    def executeCreateTablesStatement(sqlConnection: Connection): Try[Unit] = Try {

      val fileStream            = getClass.getResourceAsStream(orderHistoryDDLFileName)
      val createTablesDDL       = Source.fromInputStream(fileStream).getLines.toSeq.mkString
      val createTablesStatement = sqlConnection.prepareStatement(createTablesDDL)

      createTablesStatement.executeUpdate()
      createTablesStatement.close()
    }

    Try { Class.forName(driver) } *> retry(10, 2000) { DriverManager.getConnection(url, postgresUser, postgresPassword) } >>= { sqlConnection =>
      executeCreateTablesStatement(sqlConnection).map(_ => sqlConnection.close())
    }
  }

  def getPostgresConnectionCfgString(serverName: String, port: String): String =
    s"""
       |postgres {
       |  server-name = $serverName
       |  port-number = $port
       |  user = $postgresUser
       |  password = $postgresPassword
       |  data-source-class-name = "org.postgresql.ds.PGSimpleDataSource"
       |}
    """.stripMargin

  def getOrdersHistoryCfgString(batchLingerMs: Long): String =
    s"""
       |waves.matcher {
       |  ${getPostgresConnectionCfgString(postgresContainerName, postgresContainerPort)}
       |  order-history {
       |    enabled = yes
       |    orders-batch-linger-ms = $batchLingerMs
       |    orders-batch-entries = 10000
       |    events-batch-linger-ms = $batchLingerMs
       |    events-batch-entries = 10000
       |  }
       |}
    """.stripMargin

  override protected def nodeConfigs: Seq[Config] =
    super.nodeConfigs.map(
      ConfigFactory
        .parseString(getOrdersHistoryCfgString(batchLingerMs))
        .withFallback
    )

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    // connect postgres container to the nodes network
    val nodeNetworkId = dockerClient.inspectContainer(node.containerId).networkSettings().networks().asScala.head._2.networkId()
    dockerClient.connectToNetwork(postgresContainerLauncher.containerId, nodeNetworkId)

    // start postgres container and create tables
    postgresContainerLauncher.startContainer()
    createTables(s"localhost:${postgresContainerLauncher.getHostPort.explicitGet()}")

    Seq(IssueUsdTx, IssueWctTx).map(_.json()).map(node.broadcastRequest(_)).foreach(tx => node.waitForTransaction(tx.id))
  }

  override protected def afterAll(): Unit = {
    postgresContainerLauncher.stopAndRemoveContainer()
    super.afterAll()
  }

  lazy val ctx =
    new PostgresJdbcContext(
      SnakeCase,
      ConfigFactory
        .parseString(getPostgresConnectionCfgString("localhost", postgresContainerLauncher.getHostPort.explicitGet()))
        .as[PostgresConnection]("postgres")
        .getConfig
    )

  import ctx._

  def getOrdersCount: Long = ctx.run(querySchema[OrderRecord]("orders", _.id      -> "id").size)
  def getEventsCount: Long = ctx.run(querySchema[EventRecord]("events", _.orderId -> "order_id").size)

  case class EventShortenedInfo(eventType: Double, filled: Double, totalFilled: Double, status: Byte)
  case class OrderShortenedInfo(id: String, senderPublicKey: String, side: Byte, price: Double, amount: Double)

  def getOrderInfoById(orderId: String): Option[OrderShortenedInfo] =
    ctx
      .run(
        querySchema[OrderRecord](
          "orders",
          _.id              -> "id",
          _.sender          -> "sender",
          _.senderPublicKey -> "sender_public_key",
          _.amountAssetId   -> "amount_asset_id",
          _.priceAssetId    -> "price_asset_id",
          _.side            -> "side",
          _.price           -> "price",
          _.amount          -> "amount",
          _.timestamp       -> "timestamp",
          _.expiration      -> "expiration",
          _.fee             -> "fee",
          _.created         -> "created"
        ).filter(_.id == lift(orderId))
      )
      .map { r =>
        OrderShortenedInfo(r.id, r.senderPublicKey, r.side, r.price, r.amount)
      }
      .headOption

  def getEventsInfoByOrderId(orderId: String): List[EventShortenedInfo] =
    ctx
      .run(
        querySchema[EventRecord](
          "events",
          _.orderId     -> "order_id",
          _.eventType   -> "event_type",
          _.timestamp   -> "timestamp",
          _.price       -> "price",
          _.filled      -> "filled",
          _.totalFilled -> "total_filled",
          _.status      -> "status"
        ).filter(_.orderId == lift(orderId))
      )
      .sortBy(_.timestamp)
      .map { r =>
        EventShortenedInfo(
          r.eventType, // 0 - trade, 1 - cancel
          r.filled, // filled in this trade
          r.totalFilled,
          r.status // 0 - accepted, 1 - partiallyFilled, 2 - filled, 3 - cancelled
        )
      }

  val (amount, price)            = (1000L, PriceConstant)
  val denormalizedAmount: Double = MatcherModel.denormalizeAmountAndFee(amount, Decimals)
  val denormalizedPrice: Double  = MatcherModel.fromNormalized(price, Decimals, Decimals)

  "Order history should save all orders and events" in {
    val ordersCount = OrderValidator.MaxActiveOrders

    (1 to ordersCount)
      .foreach { _ =>
        node.placeOrder(alice, wctUsdPair, BUY, 1, price, matcherFee)
        node.placeOrder(bob, wctUsdPair, SELL, 1, price, matcherFee)
      }

    retry(10, batchLingerMs) {
      getOrdersCount shouldBe ordersCount * 2
      getEventsCount shouldBe ordersCount * 2
    }
  }

  "Order history should correctly save events for the big buy order" in {

    val buyOrder   = node.placeOrder(alice, wctUsdPair, BUY, 3 * amount, price, matcherFee).message.id
    val sellOrder1 = node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder1, "Filled")

    val sellOrder2 = node.placeOrder(bob, wctUsdPair, SELL, amount, price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    node.waitOrderStatus(wctUsdPair, sellOrder2, "Filled")

    node.cancelOrder(alice, wctUsdPair, buyOrder)

    retry(10, batchLingerMs) {
      getEventsInfoByOrderId(buyOrder) shouldBe
        List(
          EventShortenedInfo(0, denormalizedAmount, denormalizedAmount, 1),
          EventShortenedInfo(0, denormalizedAmount, 2 * denormalizedAmount, 1),
          EventShortenedInfo(1, 0, 2 * denormalizedAmount, 3)
        )
    }
  }

  "Order history should correctly save events for small and big orders" in {

    val smallBuyOrder = node.placeOrder(alice, wctUsdPair, BUY, amount, price, matcherFee).message.id
    val bigSellOrder  = node.placeOrder(bob, wctUsdPair, SELL, 5 * amount, price, matcherFee).message.id

    node.waitOrderStatus(wctUsdPair, smallBuyOrder, "Filled")
    node.waitOrderStatus(wctUsdPair, bigSellOrder, "PartiallyFilled")

    retry(10, batchLingerMs) {

      getOrderInfoById(smallBuyOrder) shouldBe
        Some(OrderShortenedInfo(smallBuyOrder, alice.publicKey.toString, 0, denormalizedPrice, denormalizedAmount))

      getOrderInfoById(bigSellOrder) shouldBe
        Some(OrderShortenedInfo(bigSellOrder, bob.publicKey.toString, 1, denormalizedPrice, 5 * denormalizedAmount))

      getEventsInfoByOrderId(smallBuyOrder) shouldBe List(EventShortenedInfo(0, denormalizedAmount, denormalizedAmount, 2))
      getEventsInfoByOrderId(bigSellOrder) shouldBe List(EventShortenedInfo(0, denormalizedAmount, denormalizedAmount, 1))
    }
  }
}

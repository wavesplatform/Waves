package com.wavesplatform.it

import java.util.concurrent.Executors
import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, PortBinding}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import io.netty.util.{HashedWheelTimer, Timeout, TimerTask}
import org.asynchttpclient.DefaultAsyncHttpClient
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.SECONDS
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

case class NodeInfo(
    hostRestApiPort: Int,
    hostNetworkPort: Int,
    containerNetworkPort: Int,
    ipAddress: String,
    containerId: String)

trait Docker extends AutoCloseable {
  def startNode(nodeConfig: Config = ConfigFactory.empty()): Future[Node]
  def stopNode(containerId: String)
}

object Docker extends ScorexLogging {
  private case class WaitForNode(containerId: String, node: Node, promise: Promise[Node]) extends TimerTask {
    override def run(timeout: Timeout): Unit = node.status.onComplete {
      case Success(_) => promise.complete(Success(node))
      case Failure(_) =>
        log.info(s"Re-requesting node status from container $containerId")
        timeout.timer().newTimeout(this, 1, SECONDS)
    }
  }

  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper
  private val imageId = System.getProperty("docker.imageId")
  private val http = new DefaultAsyncHttpClient()

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) = p.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: String) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt

  val DefaultConfigTemplate = ConfigFactory.parseResources("template.conf")
  val NodeConfigs = ConfigFactory.parseResources("nodes.conf")

  def apply(suiteConfig: Config = ConfigFactory.empty): Docker = new Docker {
    private val client = DefaultDockerClient.fromEnv().build()
    private val timer = new HashedWheelTimer()
    private var nodes = Map.empty[String, NodeInfo]

    timer.start()

    private def knownPeers = nodes.values.zipWithIndex.map {
      case (ni, index) => s"-Dwaves.network.known-peers.$index=${ni.ipAddress}:${ni.containerNetworkPort}"
    } mkString " "

    override def startNode(nodeConfig: Config): Future[Node] = {
      val configOverrides = s"$knownPeers ${renderProperties(asProperties(nodeConfig.withFallback(suiteConfig)))}"
      val actualConfig = nodeConfig
        .withFallback(suiteConfig)
        .withFallback(DefaultConfigTemplate)
        .withFallback(ConfigFactory.defaultApplication())
        .withFallback(ConfigFactory.defaultReference())
        .resolve()

      val restApiPort = actualConfig.getString("waves.rest-api.port")
      val networkPort = actualConfig.getString("waves.network.port")

      val portBindings = new ImmutableMap.Builder[String, java.util.List[PortBinding]]()
        .put(restApiPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
        .put(networkPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
        .build()

      val hostConfig = HostConfig.builder()
        .portBindings(portBindings)
        .build()

      val containerConfig = ContainerConfig.builder()
        .image(imageId)
        .exposedPorts(restApiPort, networkPort)
        .hostConfig(hostConfig)
        .env(s"WAVES_OPTS=$configOverrides", s"WAVES_PORT=$networkPort")
        .build()

      val containerId = client.createContainer(containerConfig).id()
      client.startContainer(containerId)
      val containerInfo = client.inspectContainer(containerId)

      val ports = containerInfo.networkSettings().ports()

      val nodeInfo = NodeInfo(
        extractHostPort(ports, restApiPort),
        extractHostPort(ports, networkPort),
        networkPort.toInt,
        containerInfo.networkSettings().ipAddress(),
        containerId)
      nodes += containerId -> nodeInfo

      implicit val ec = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

      val p = Promise[Node]
      timer.newTimeout(WaitForNode(containerId, new Node(actualConfig, nodeInfo, http), p), 0, SECONDS)
      p.future.onComplete(_ => log.info(s"Node ${nodeInfo.containerId} started up"))
      p.future
    }

    override def stopNode(containerId: String): Unit = {
      client.stopContainer(containerId, 10)
    }

    override def close(): Unit = {
      timer.stop()
      log.info("Stopping containers")
      nodes.keys.foreach(client.stopContainer(_, 10))
      nodes.keys.foreach(client.removeContainer)
      client.close()
      http.close()
    }
  }
}

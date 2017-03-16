package com.wavesplatform.it

import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, PortBinding}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import io.netty.util.{HashedWheelTimer, Timeout, TimerTask}
import org.asynchttpclient.{AsyncHttpClient, DefaultAsyncHttpClient}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration.SECONDS
import scala.concurrent.{Future, Promise}
import scala.util.Success

case class NodeInfo(
    hostRestApiPort: Int,
    hostNetworkPort: Int,
    containerNetworkPort: Int,
    ipAddress: String,
    containerId: String)

trait Docker extends AutoCloseable {
  def startNode(config: Config = ConfigFactory.empty()): Future[NodeInfo]
  def stopNode(containerId: String)
}

object Docker extends ScorexLogging {
  private case class WaitForNode(nodeInfo: NodeInfo, client: AsyncHttpClient, promise: Promise[NodeInfo]) extends TimerTask {
    override def run(timeout: Timeout): Unit =
      client.prepareGet(s"http://localhost:${nodeInfo.hostRestApiPort}/node/status")
        .execute()
        .toCompletableFuture
        .whenCompleteAsync { (r, t) =>
          if (t == null) {
            promise.complete(Success(nodeInfo))
          } else {
            log.debug(s"Re-requesting node status from container ${nodeInfo.containerId}")
            timeout.timer().newTimeout(this, 1, SECONDS)
          }
        }
    }

  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper
  private val confTemplate = ConfigFactory.parseResources("template.conf")
  private val imageId = System.getProperty("docker.imageId")
  private val http = new DefaultAsyncHttpClient()

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) = p.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: String) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt

  def apply(): Docker = new Docker {
    private val client = DefaultDockerClient.fromEnv().build()
    private val timer = new HashedWheelTimer()
    private var nodes = Map.empty[String, NodeInfo]

    timer.start()

    private def knownPeers = nodes.values.zipWithIndex.map {
      case (ni, index) => s"-Dwaves.network.known-peers.$index=${ni.ipAddress}:${ni.containerNetworkPort}"
    } mkString " "

    override def startNode(config: Config): Future[NodeInfo] = {
      val configOverrides = s"$knownPeers ${renderProperties(asProperties(config))}"
      val actualConfig = config.withFallback(confTemplate)
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
        .env(s"""WAVES_OPTS=$configOverrides""")
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

      val p = Promise[NodeInfo]
      timer.newTimeout(WaitForNode(nodeInfo, http, p), 0, SECONDS)
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

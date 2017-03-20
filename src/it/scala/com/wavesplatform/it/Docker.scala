package com.wavesplatform.it

import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, PortBinding}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import io.netty.util.HashedWheelTimer
import org.asynchttpclient.Dsl._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.duration._

case class NodeInfo(
    hostRestApiPort: Int,
    hostNetworkPort: Int,
    containerNetworkPort: Int,
    ipAddress: String,
    containerId: String)

trait Docker extends AutoCloseable {
  def startNode(nodeConfig: Config = ConfigFactory.empty()): Node
  def stopNode(containerId: String)
  def scheduleOnce(initialDelay: FiniteDuration)(f: => Any): Unit
}

object Docker extends ScorexLogging {
  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper
  private val imageId = System.getProperty("docker.imageId")
  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxRequestRetry(1))

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

    override def startNode(nodeConfig: Config): Node = {
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

      new Node(actualConfig, nodeInfo, http, timer)
    }

    override def stopNode(containerId: String): Unit = {
      client.stopContainer(containerId, 10)
    }

    override def scheduleOnce(initialDelay: FiniteDuration)(f: => Any) =
      timer.newTimeout(_ => f, initialDelay.toMillis, MILLISECONDS)

    override def close(): Unit = {
      timer.stop()
      log.info("Stopping containers")
      nodes.keys.foreach(id => client.removeContainer(id, RemoveContainerParam.forceKill()))
      client.close()
      http.close()
    }
  }
}

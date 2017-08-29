package com.wavesplatform.it

import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DefaultDockerClient
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.{ContainerConfig, HostConfig, NetworkConfig, PortBinding}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.asynchttpclient.Dsl._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.duration._

case class NodeInfo(
                     hostRestApiPort: Int,
                     hostNetworkPort: Int,
                     containerNetworkPort: Int,
                     apiIpAddress: String,
                     networkIpAddress: String,
                     containerId: String,
                     hostMatcherApiPort: Int)

class Docker(suiteConfig: Config = ConfigFactory.empty) extends AutoCloseable with ScorexLogging {

  import Docker._

  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxConnectionsPerHost(10)
    .setMaxRequestRetry(1)
    .setReadTimeout(5000)
    .setRequestTimeout(5000))

  private val client = DefaultDockerClient.fromEnv().build()
  private var nodes = Map.empty[String, Node]
  private var seedAddress = Option.empty[String]
  private val isStopped = new AtomicBoolean(false)

  sys.addShutdownHook {
    close()
  }

  private def knownPeers = seedAddress.fold("")(sa => s"-Dwaves.network.known-peers.0=$sa")

  private val networkName = "waves-" + this.##.toLong.toHexString

  private val wavesNetwork = client.createNetwork(NetworkConfig.builder().driver("bridge").name(networkName).build())

  def startNode(nodeConfig: Config): Node = {
    val configOverrides = s"$knownPeers ${renderProperties(asProperties(nodeConfig.withFallback(suiteConfig)))}"
    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(DefaultConfigTemplate)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()

    val restApiPort = actualConfig.getString("waves.rest-api.port")
    val networkPort = actualConfig.getString("waves.network.port")
    val matcherApiPort = actualConfig.getString("waves.matcher.port")

    val portBindings = new ImmutableMap.Builder[String, java.util.List[PortBinding]]()
      .put(restApiPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(networkPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(matcherApiPort, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .build()

    val hostConfig = HostConfig.builder()
      .portBindings(portBindings)
      .build()

    val containerConfig = ContainerConfig.builder()
      .image("com.wavesplatform/waves:latest")
      .exposedPorts(restApiPort, networkPort, matcherApiPort)
      .hostConfig(hostConfig)
      .env(s"WAVES_OPTS=$configOverrides", s"WAVES_PORT=$networkPort")
      .build()

    val containerId = client.createContainer(containerConfig, actualConfig.getString("waves.network.node-name") + "-" +
      this.##.toLong.toHexString).id()
    connectToNetwork(containerId)
    client.startContainer(containerId)
    val containerInfo = client.inspectContainer(containerId)

    val ports = containerInfo.networkSettings().ports()

    val nodeInfo = NodeInfo(
      extractHostPort(ports, restApiPort),
      extractHostPort(ports, networkPort),
      networkPort.toInt,
      containerInfo.networkSettings().ipAddress(),
      containerInfo.networkSettings().networks().asScala(networkName).ipAddress(),
      containerId,
      extractHostPort(ports, matcherApiPort))
    val node = new Node(actualConfig, nodeInfo, http)
    if (seedAddress.isEmpty) {
      seedAddress = Some(s"${nodeInfo.networkIpAddress}:${nodeInfo.containerNetworkPort}")
    }
    nodes += containerId -> node
    Await.result(node.lastBlock, Duration.Inf)
    node
  }

  def stopNode(containerId: String): Unit = {
    client.stopContainer(containerId, 10)
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")
      nodes.values.foreach(n => n.close())
      nodes.keys.foreach(id => client.removeContainer(id, RemoveContainerParam.forceKill()))
      client.removeNetwork(wavesNetwork.id())
      client.close()
      http.close()
    }
  }

  def disconnectFromNetwork(containerId: String): Unit =  client.disconnectFromNetwork(containerId, wavesNetwork.id())
  def disconnectFromNetwork(node: Node): Unit = disconnectFromNetwork(node.nodeInfo.containerId)

  def connectToNetwork(containerId: String): Unit = client.connectToNetwork(containerId, wavesNetwork.id())
  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId)
}

object Docker {
  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) = p.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: String) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt

  val DefaultConfigTemplate = ConfigFactory.parseResources("template.conf")
  val NodeConfigs = ConfigFactory.parseResources("nodes.conf")
}

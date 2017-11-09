package com.wavesplatform.it

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.asynchttpclient.Dsl._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.control.NonFatal

case class NodeInfo(hostRestApiPort: Int,
                    hostNetworkPort: Int,
                    containerNetworkPort: Int,
                    apiIpAddress: String,
                    networkIpAddress: String,
                    containerId: String,
                    hostMatcherApiPort: Int)

class Docker(suiteConfig: Config = ConfigFactory.empty,
             tag: String = "") extends AutoCloseable with ScorexLogging {

  import Docker._

  private val http = asyncHttpClient(config()
    .setMaxConnections(18)
    .setMaxConnectionsPerHost(3)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setPooledConnectionIdleTimeout(2000)
    .setRequestTimeout(10000))

  private val client = DefaultDockerClient.fromEnv().build()

  private val nodes = ConcurrentHashMap.newKeySet[Node]()
  private val isStopped = new AtomicBoolean(false)

  sys.addShutdownHook {
    close()
  }

  private val networkName = "waves" + this.##.toLong.toHexString

  private lazy val wavesNetworkId: String = {
    def network: Option[Network] = try {
      val networks = client.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))
      if (networks.isEmpty) None else Some(networks.get(0))
    } catch {
      case NonFatal(_) => network
    }

    def attempt(rest: Int): String = try {
      network match {
        case Some(n) =>
          log.info(s"Network ${n.name()} (id: ${n.id()}) is created for $tag")
          n.id()
        case None =>
          val r = client.createNetwork(NetworkConfig.builder().name(networkName).driver("bridge").checkDuplicate(true).build())
          Option(r.warnings()).foreach(log.warn(_))
          attempt(rest - 1)
      }
    } catch {
      case NonFatal(e) =>
        log.warn(s"Can not create a network for $tag", e)
        if (rest == 0) throw e else attempt(rest - 1)
    }

    attempt(5)
  }

  def startNodes(nodeConfigs: Seq[Config]): Seq[Node] = {
    val all = nodeConfigs.map(startNode)
    Await.result(
      for {
        _ <- Future.traverse(all)(waitNodeIsUp)
        _ <- Future.traverse(all)(connectToAll)
      } yield (),
      5.minutes
    )
    all
  }

  def startNode(nodeConfig: Config): Node = {
    val node = startNodeInternal(nodeConfig)
    Await.result(waitNodeIsUp(node).flatMap(_ => connectToAll(node)), 3.minutes)
    node
  }

  private def connectToAll(node: Node): Future[Unit] = {
    val seedAddresses = nodes.asScala.map { n => (n.nodeInfo.networkIpAddress, n.nodeInfo.containerNetworkPort) }

    Future
      .traverse(seedAddresses) { seed =>
        node.connect(seed._1, seed._2)
      }
      .map(_ => ())
  }

  private def waitNodeIsUp(node: Node): Future[Unit] = node.waitFor[Int](_.height, _ > 0, 1.second).map(_ => ())

  private def startNodeInternal(nodeConfig: Config): Node = {
    val javaOptions = Option(System.getenv("CONTAINER_JAVA_OPTS")).getOrElse("")
    val configOverrides = s"$javaOptions ${renderProperties(asProperties(nodeConfig.withFallback(suiteConfig)))}"
    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(NodeConfigs.DefaultConfigTemplate)
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

    val containerId = client.createContainer(containerConfig, s"$networkName-${actualConfig.getString("waves.network.node-name")}").id()
    client.connectToNetwork(containerId, wavesNetworkId)

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
    nodes.add(node)
    node
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")
      nodes.asScala.foreach { node =>
        node.close()
        client.stopContainer(node.nodeInfo.containerId, 0)
        client.disconnectFromNetwork(node.nodeInfo.containerId, wavesNetworkId)
        saveLog(node)
        client.removeContainer(node.nodeInfo.containerId, RemoveContainerParam.forceKill())
      }

      client.removeNetwork(wavesNetworkId)
      http.close()
      client.close()
    }
  }

  private def saveLog(node: Node): Unit = {
    val logDir = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)
    import node.nodeInfo.containerId

    val logFile = {
      val baseName = s"${this.##.toLong.toHexString}-${node.settings.networkSettings.nodeName}"
      val fileName = if (tag.isEmpty) baseName else s"$tag-$baseName"
      logDir.resolve(s"$fileName.log").toFile
    }
    log.info(s"Writing logs of $containerId to ${logFile.getAbsolutePath}")

    val fileStream = new FileOutputStream(logFile, false)
    try {
      client
        .logs(
          containerId,
          DockerClient.LogsParam.timestamps(),
          DockerClient.LogsParam.follow(),
          DockerClient.LogsParam.stdout(),
          DockerClient.LogsParam.stderr()
        )
        .attach(fileStream, fileStream)
    } finally {
      fileStream.close()
    }
  }

  def disconnectFromNetwork(node: Node): Unit = disconnectFromNetwork(node.nodeInfo.containerId)
  private def disconnectFromNetwork(containerId: String): Unit = client.disconnectFromNetwork(containerId, wavesNetworkId)

  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId)
  private def connectToNetwork(containerId: String): Unit = client.connectToNetwork(containerId, wavesNetworkId)
}

object Docker {
  private val jsonMapper = new ObjectMapper
  private val propsMapper = new JavaPropsMapper

  def apply(owner: Class[_]): Docker = new Docker(tag = owner.getSimpleName)

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) = p.asScala.map { case (k, v) => s"-D$k=$v" } mkString " "

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: String) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt
}

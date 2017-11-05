package com.wavesplatform.it

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
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
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, blocking}
import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext.Implicits.global

case class NodeInfo(hostRestApiPort: Int,
                    hostNetworkPort: Int,
                    containerNetworkPort: Int,
                    apiIpAddress: String,
                    networkIpAddress: String,
                    containerId: String,
                    hostMatcherApiPort: Int)

class Docker(suiteConfig: Config = ConfigFactory.empty, tag: String = "") extends AutoCloseable with ScorexLogging {

  import Docker._

  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxConnectionsPerHost(10)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setRequestTimeout(10000))

  private val defaultClient = newClient

  private def newClient = DefaultDockerClient.fromEnv().build()

  private val nodes = new ConcurrentHashMap[String, (DockerClient, Node)]
  private val seedAddress = new AtomicReference[String]
  private val isStopped = new AtomicBoolean(false)

  sys.addShutdownHook {
    close()
  }

  private def knownPeers = Option(seedAddress.get()).fold("")(sa => s"-Dwaves.network.known-peers.0=$sa")

  private val networkName = "waves" + this.##.toLong.toHexString

  private val wavesNetworkId: String = {
    def network: Option[Network] = try {
      val networks = defaultClient.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))
      if (networks.isEmpty) None else Some(networks.get(0))
    } catch {
      case NonFatal(_) => network
    }

    def attempt(rest: Int): String = try {
      network match {
        case Some(n) =>
          n.id()
        case None =>
          val r = defaultClient.createNetwork(NetworkConfig.builder()
            .name(networkName)
            .driver("bridge")
            .checkDuplicate(true)
            .internal(false)
            .build())
          Option(r.warnings()).foreach(log.warn(_))
          attempt(rest - 1)
      }
    } catch {
      case NonFatal(e) =>
        log.warn(s"Can not create a network", e)
        if (rest == 0) throw e else attempt(rest - 1)
    }

    attempt(5)
  }

  def startNodesSync(nodeConfigs: Seq[Config]): Seq[Node] = Await.result(
    startNode(nodeConfigs.head).flatMap { head =>
      Future.sequence(nodeConfigs.tail.map(startNode)).flatMap { tail =>
        val all = head +: tail
        log.debug("Wait nodes are up")
        Future.traverse(all)(_.waitFor[Int](_.height, _ > 0, 1.second)).map(_ => all)
      }
    },
    5.minute
  )

  def startNodeSync(nodeConfig: Config): Node = Await.result(startNode(nodeConfig), 1.minute)

  def startNode(nodeConfig: Config): Future[Node] = Future(blocking {
    val configOverrides = s"$JavaOptions $knownPeers ${renderProperties(asProperties(nodeConfig.withFallback(suiteConfig)))}"
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

    val client = newClient
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
    seedAddress.compareAndSet(null, s"${nodeInfo.networkIpAddress}:${nodeInfo.containerNetworkPort}")

    val node = new Node(actualConfig, nodeInfo, http)
    nodes.put(containerId, (client, node))
    node
  })

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")

      def closeTask(client: DockerClient, node: Node) = Future(blocking {
        node.close()
        client.stopContainer(node.nodeInfo.containerId, 0)
        saveLog(client, node)
        client.removeContainer(node.nodeInfo.containerId, RemoveContainerParam.forceKill())
        client.close()
      })

      def closeAllTask = Future
        .sequence(nodes.values().asScala.map(Function.tupled(closeTask)))
        .map { _ =>
          http.close()
          defaultClient.removeNetwork(wavesNetworkId)
          defaultClient.close()
        }

      Await.result(closeAllTask, Duration.Inf)
    }
  }

  private def saveLog(client: DockerClient, node: Node): Unit = {
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
    client
      .logs(
        containerId,
        DockerClient.LogsParam.timestamps(),
        DockerClient.LogsParam.follow(),
        DockerClient.LogsParam.stdout(),
        DockerClient.LogsParam.stderr()
      )
      .attach(fileStream, fileStream)
  }

  def disconnectFromNetwork(node: Node): Unit = disconnectFromNetwork(node.nodeInfo.containerId)
  private def disconnectFromNetwork(containerId: String): Unit = defaultClient.disconnectFromNetwork(containerId, wavesNetworkId)

  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId)
  private def connectToNetwork(containerId: String): Unit = defaultClient.connectToNetwork(containerId, wavesNetworkId)
}

object Docker {
  private val JavaOptions = Seq(
    "-Xmx1024m"
  ).mkString(" ")
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

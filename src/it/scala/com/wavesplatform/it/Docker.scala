package com.wavesplatform.it

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages._
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import org.asynchttpclient.Dsl._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

case class NodeInfo(
                     hostRestApiPort: Int,
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

  private val networkName = "waves-" + this.##.toLong.toHexString

  private def network: Option[Network] = for {
    list <- Try(defaultClient.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))).toOption
    network <- list.asScala.headOption
  } yield network

  private val wavesNetworkId: String = {
    def attempt(rest: Int): String = {
      try {
        network match {
          case Some(n) => n.id()
          case None =>
            val r = defaultClient.createNetwork(NetworkConfig.builder().name(networkName).driver("bridge").checkDuplicate(true).internal(false).build())
            Option(r.warnings()).foreach(log.warn(_))
            attempt(rest - 1)
        }
      } catch {
        case NonFatal(e) =>
          log.warn(s"Can not create a network", e)
          if (rest == 0) throw e else attempt(rest - 1)
      }
    }

    attempt(5)
  }

  def startNodes(nodeConfigs: Seq[Config])(implicit ec: ExecutionContext): Seq[Node] = {
    Await.result(
      Future.sequence(nodeConfigs.map(asyncStartNode))
        .flatMap { xs =>
          val host = xs.head.nodeInfo.networkIpAddress
          val port = xs.head.nodeInfo.containerNetworkPort

          //eedAddress.compareAndSet(null, s"${xs.head.nodeInfo.networkIpAddress}:${xs.head.nodeInfo.containerNetworkPort}")
          Future.sequence(xs.map(_.connect(host, port))).map { _ => xs }
        },
      5.minutes
    )
  }

  def startNode(nodeConfig: Config)(implicit ec: ExecutionContext): Node = Await.result(asyncStartNode(nodeConfig), Duration.Inf)

  def asyncStartNode(nodeConfig: Config)(implicit ec: ExecutionContext): Future[Node] = Future(blocking {
    val configOverrides = s"$JavaOptions $knownPeers ${renderProperties(asProperties(nodeConfig.withFallback(suiteConfig)))}"
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
    val node = new Node(actualConfig, nodeInfo, http)
    nodes.put(containerId, (client, node))
    node
  }).flatMap { node =>
    node.lastBlock.map { _ =>
      node
    }
  }

  def pair(containerId: String): Option[(DockerClient, Node)] = Option(nodes.get(containerId))

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")
      nodes.asScala.foreach {
        case (id, (c, n)) =>
          n.close()
          c.stopContainer(id, 0)
      }
      http.close()

      saveLogs()

      nodes.asScala.foreach {
        case (id, (c, n)) =>
          c.close()
      }

      nodes.keys.asScala.foreach(id => defaultClient.removeContainer(id, RemoveContainerParam.forceKill()))
      defaultClient.removeNetwork(wavesNetworkId)
      defaultClient.close()
    }
  }

  private def saveLogs(): Unit = {
    val logDir = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)
    nodes.values.asScala.foreach { case (client, node) =>
      import node.nodeInfo.containerId

      val fileName = if (tag.isEmpty) containerId else s"$tag-$containerId"
      val logFile = logDir.resolve(s"$fileName.log").toFile
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
  }

  def disconnectFromNetwork(node: Node): Unit = disconnectFromNetwork(node.nodeInfo.containerId)
  private def disconnectFromNetwork(containerId: String): Unit = defaultClient.disconnectFromNetwork(containerId, wavesNetworkId)

  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId)
  private def connectToNetwork(containerId: String): Unit = {
    println("CONNECT!")
    defaultClient.connectToNetwork(containerId, wavesNetworkId)
  }
}

object Docker {
  private val JavaOptions = Seq(
    "-Xmx500m"
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

  val DefaultConfigTemplate = ConfigFactory.parseResources("template.conf")
  val NodeConfigs = ConfigFactory.parseResources("nodes.conf")
}

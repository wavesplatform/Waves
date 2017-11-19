package com.wavesplatform.it

import java.io.FileOutputStream
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.ConcurrentHashMap
import java.util.{Collections, Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
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
import scala.util.{Failure, Random, Try}

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

  dumpContainers(client.listContainers())
  sys.addShutdownHook {
    log.debug("Shutdown hook")
    close()
  }

  private val networkPrefix = s"172.${125 + Random.nextInt(75)}.${1 + Random.nextInt(125)}"

  private lazy val wavesNetwork: Network = {
    val networkName = s"waves-${hashCode().toLong.toHexString}"

    def network: Option[Network] = try {
      val networks = client.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))
      if (networks.isEmpty) None else Some(networks.get(0))
    } catch {
      case NonFatal(_) => network
    }

    def attempt(rest: Int): Network = try {
      network match {
        case Some(n) =>
          val ipam = s"ipam: ${n.ipam().config().asScala.map { n => s"subnet=${n.subnet()}, ip range=${n.ipRange()}" }.mkString(", ")}"
          log.info(s"Network ${n.name()} (id: ${n.id()}) is created for $tag, $ipam")
          n
        case None =>
          log.debug(s"Creating network $networkName for $tag")
          val r = client.createNetwork(NetworkConfig.builder()
            .name(networkName)
            .ipam(
              Ipam.builder()
                .driver("default")
                .config(List(
                  IpamConfig.create(s"$networkPrefix.0/24", s"$networkPrefix.0/24", s"$networkPrefix.254")
                ).asJava)
                .build()
            )
            .checkDuplicate(true)
            .build())
          Option(r.warnings()).foreach(log.warn(_))
          attempt(rest - 1)
      }
    } catch {
      case NonFatal(e) =>
        log.warn(s"Can not create a network for $tag", e)
        dumpContainers(client.listContainers())
        if (rest == 0) throw e else attempt(rest - 1)
    }

    attempt(5)
  }

  def startNodes(nodeConfigs: Seq[Config]): Seq[Node] = {
    val all = nodeConfigs.map(startNodeInternal)
    Await.result(
      for {
        _ <- Future.traverse(all)(waitNodeIsUp)
        _ <- Future.traverse(all)(connectToAll)
      } yield (),
      5.minutes
    )
    all
  }

  def startNode(nodeConfig: Config, autoConnect: Boolean = true): Node = {
    val node = startNodeInternal(nodeConfig)
    Await.result(
      waitNodeIsUp(node).flatMap(_ => if (autoConnect) connectToAll(node) else Future.successful(())),
      3.minutes
    )
    node
  }

  private def connectToAll(node: Node): Future[Unit] = {
    val seedAddresses = nodes.asScala
      .filterNot(_.nodeName == node.nodeName)
      .map { n => (n.nodeInfo.networkIpAddress, n.nodeInfo.containerNetworkPort) }

    Future
      .traverse(seedAddresses) { seed =>
        node.connect(seed._1, seed._2)
      }
      .map(_ => ())
  }

  private def waitNodeIsUp(node: Node): Future[Unit] = node.waitFor[Int](_.height, _ > 0, 1.second).map(_ => ())

  private def startNodeInternal(nodeConfig: Config): Node = try {
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

    val nodeNumber = actualConfig.getString("waves.network.node-name").replace("node", "").toInt
    val ip = s"$networkPrefix.$nodeNumber"
    val containerConfig = ContainerConfig.builder()
      .image("com.wavesplatform/waves:latest")
      .exposedPorts(restApiPort, networkPort, matcherApiPort)
      .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(
        wavesNetwork.name() -> EndpointConfig.builder()
          .ipAddress(ip)
          .ipamConfig(EndpointIpamConfig.builder().ipv4Address(ip).build())
          .build()
      ).asJava))
      .hostConfig(hostConfig)
      .env(s"WAVES_OPTS=$configOverrides", s"WAVES_NET_IP=$ip", s"WAVES_PORT=$networkPort")
      .build()

    val containerId = {
      val containerName = s"${wavesNetwork.name()}-${actualConfig.getString("waves.network.node-name")}"
      dumpContainers(
        client.listContainers(DockerClient.ListContainersParam.filter("name", containerName)),
        "Containers with same name"
      )

      log.debug(s"Creating container $containerName at $ip with options: $javaOptions")
      val r = client.createContainer(containerConfig, containerName)
      Option(r.warnings().asScala).toSeq.flatten.foreach(log.warn(_))
      r.id()
    }
    // connectToNetwork(containerId)

    client.startContainer(containerId)
    val containerInfo = inspectContainer(containerId)
    val networksSettingsStr = containerInfo.networkSettings().networks().asScala
      .map { case (name, x) => s"$name -> ${x.ipAddress()}" }
      .mkString(", ")
    log.debug(
      s"""Container ${containerInfo.name()} info:
         |IP: ${containerInfo.networkSettings().ipAddress()}
         |Networks: $networksSettingsStr""".stripMargin)

    val ports = containerInfo.networkSettings().ports()

    val nodeInfo = NodeInfo(
      extractHostPort(ports, restApiPort),
      extractHostPort(ports, networkPort),
      networkPort.toInt,
      containerInfo.networkSettings().ipAddress(),
      containerInfo.networkSettings().networks().asScala(wavesNetwork.name()).ipAddress(),
      containerId,
      extractHostPort(ports, matcherApiPort))
    val node = new Node(actualConfig, nodeInfo, http)
    nodes.add(node)
    log.debug(s"Started $containerId -> ${node.settings.networkSettings.nodeName}")
    node
  } catch {
    case NonFatal(e) =>
      log.error("Can't start a container", e)
      dumpContainers(client.listContainers())
      throw e
  }

  private def inspectContainer(containerId: String): ContainerInfo = {
    val containerInfo = client.inspectContainer(containerId)
    if (containerInfo.networkSettings().networks().asScala.contains(wavesNetwork.name())) containerInfo
    else {
      log.debug(s"Container $containerId has not connected to the network ${wavesNetwork.name()} yet, retry")
      Thread.sleep(1000)
      inspectContainer(containerId)
    }
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      log.info("Stopping containers")
      nodes.asScala.foreach { node =>
        node.close()
        client.stopContainer(node.nodeInfo.containerId, 0)
        disconnectFromNetwork(node)
        saveLog(node)
        client.removeContainer(node.nodeInfo.containerId, RemoveContainerParam.forceKill())
      }

      Try(client.removeNetwork(wavesNetwork.id)) match {
        case Failure(e) =>
          // https://github.com/moby/moby/issues/17217
          log.warn(s"Can not remove network ${wavesNetwork.name()}", e)
        case _ =>
      }

      http.close()
      client.close()
    }
  }

  private def saveLog(node: Node): Unit = {
    val logDir = Option(System.getProperty("waves.it.logging.dir")).map(Paths.get(_))
      .getOrElse(Paths.get(System.getProperty("user.dir"), "target", "logs"))

    Files.createDirectories(logDir)
    import node.nodeInfo.containerId

    val logFile = logDir.resolve(s"${node.settings.networkSettings.nodeName}.log").toFile
    log.info(s"Writing logs of $containerId to ${logFile.getAbsolutePath}")

    val fileStream = new FileOutputStream(logFile, false)
    try {
      client
        .logs(
          containerId,
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

  private def disconnectFromNetwork(containerId: String): Unit = client.disconnectFromNetwork(containerId, wavesNetwork.id())

  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId)

  private def connectToNetwork(containerId: String): Unit = client.connectToNetwork(containerId, wavesNetwork.id())

  private def dumpContainers(containers: java.util.List[Container], label: String = "Containers"): Unit = {
    val x = if (containers.isEmpty) "No" else containers.asScala
      .map { x => s"\nContainer(${x.id()}, status: ${x.status()}, names: ${x.names().asScala.mkString(", ")})" }
      .mkString("\n")

    log.debug(s"$label: $x")
  }
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

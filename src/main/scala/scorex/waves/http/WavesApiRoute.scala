package scorex.waves.http

import javax.ws.rs.Path

import akka.http.scaladsl.server.{Directive, Route}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http._
import scorex.api.http.assets.TransferRequest
import scorex.transaction.TransactionFactory
import scorex.utils.Time
import scorex.wallet.Wallet

@Path("/waves")
@Api(value = "waves")
@Deprecated
case class WavesApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time)
  extends ApiRoute with BroadcastRoute {

  override lazy val route = pathPrefix("waves") {
    externalPayment ~ signPayment ~ broadcastSignedPayment ~ payment ~ createdSignedPayment
  }

  @Deprecated
  @Path("/payment")
  @ApiOperation(value = "Send payment from wallet. Deprecated: use /assets/transfer instead",
    notes = "Send payment from wallet to another wallet. Each call sends new payment. Deprecated: use /assets/transfer instead",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.assets.TransferRequest",
      defaultValue = "{\n\t\"amount\":400,\n\t\"fee\":1,\n\t\"sender\":\"senderId\",\n\t\"recipient\":\"recipientId\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def payment: Route = (path("payment") & post & withAuth) {
    json[TransferRequest] { payment =>
      doBroadcast(TransactionFactory.transferAsset(payment, wallet, time))
    }
  }

  @Deprecated
  @Path("/payment/signature")
  def signPayment: Route = reject(path("payment" / "signature"))

  @Deprecated
  @Path("/create-signed-payment")
  def createdSignedPayment: Route = reject(path("create-signed-payment"))

  @Deprecated
  @Path("/external-payment")
  def externalPayment: Route = reject(path("external-payment"))

  @Deprecated()
  @Path("/broadcast-signed-payment")
  def broadcastSignedPayment: Route = reject(path("broadcast-signed-payment"))

  private def reject(path: Directive[Unit]): Route = (path & post) {
    complete(DiscontinuedApi)
  }
}

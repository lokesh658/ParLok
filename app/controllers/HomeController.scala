package controllers

import com.stripe.Stripe
import com.stripe.model.checkout.Session
import com.stripe.model.{Price, Product => stripeProduct}
import com.stripe.param.checkout.SessionCreateParams
import com.stripe.param.{PriceCreateParams, ProductCreateParams}
import models.{Address, User, model}
import org.mongodb.scala.bson.ObjectId
import play.api.Configuration
import play.api.data.Forms._
import play.api.data._
import play.api.data.validation.Constraints._
import play.api.mvc._

import javax.inject._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util._


// case classes for forms

case class loginData(email: String, password: String)
object loginData {
  def unapply(obj: loginData): Option[(String, String)] = {
    Some((obj.email, obj.password))
  }
}

case class signUpData(firstName: String, lastName: String, email: String, password: String)
object signUpData {
  def unapply(obj:signUpData): Option[(String, String, String, String)] = {
    Some(obj.firstName, obj.lastName, obj.email, obj.password)
  }
}

case class cartData(productId: String, userId: String, quantity: Int)
object cartData {
  def unapply(obj:cartData) :Option[(String, String, Int)] = {
    Some(obj.productId, obj.userId, obj.quantity)
  }
}
case class addressData(addressLine1:String, addressLine2: String, city: String, pincode: String, state:String, country: String)
object addressData {
  def unapply(obj: addressData): Option[(String, String, String, String, String, String)] = {
    Some(obj.addressLine1, obj.addressLine2, obj.city, obj.pincode, obj.state, obj.country)
  }
}



@Singleton
class HomeController @Inject() (cc: MessagesControllerComponents)(mod: model, config: Configuration) extends MessagesAbstractController(cc) {

  // authentication middleware using Action Composition
  // checkAuthentication Action

  case class UserAction[A](action: Action[A]) extends Action[A] {
    def apply(request: Request[A]): Future[Result] = {
      val userId: String = request.session.get("userId").getOrElse((new ObjectId()).toString)
      mod.isUserExist(userId).flatMap {
        case Some(_) => action(request)
        case None => Future(Redirect(routes.HomeController.index()).flashing("error" -> "Login first"))
      }
    }

    override def parser: BodyParser[A] = action.parser

    override def executionContext: ExecutionContext = action.executionContext
  }


  val loginForm: Form[loginData] = Form(
    mapping(
      "Email" -> text(5, 20),
      "Password" -> text(4)
    )(loginData.apply)(loginData.unapply)
  )

  val signUpForm: Form[signUpData] = Form(
    mapping(
      "FirstName" -> text,
      "LastName" -> text,
      "Email" -> text,
      "Password" -> text
    )(signUpData.apply)(signUpData.unapply)
  )

  val cartForm: Form[cartData] = Form(
    mapping(
      "productId" -> text,
      "userId" -> text,
      "quantity" -> number
    )(cartData.apply)(cartData.unapply)
  )
  val addressForm: Form[addressData] = Form(
    mapping(
      "addressLine1" -> nonEmptyText,
      "addressLine2" -> nonEmptyText,
      "city" -> nonEmptyText,
      "pincode" -> nonEmptyText,
      "state" -> nonEmptyText,
      "country" -> nonEmptyText
    )(addressData.apply)(addressData.unapply)
  )

  def index: Action[AnyContent] = Action { implicit request => Ok(views.html.index(loginForm)(signUpForm)) }

  def login: Action[AnyContent] = Action.async { implicit request =>
    loginForm.bindFromRequest().fold(
      loginWithError => Future(BadRequest(views.html.index(loginWithError)(signUpForm))),
      {
        case loginData(email, password) => mod.findUserId(email, password).map {
          case Some(userId) => Redirect(routes.HomeController.shop()).withSession("userId" -> userId.toString)
          case None => Redirect(routes.HomeController.index()).flashing("error" -> "user not found")
        }
        case _ => Future(Ok("not possible login"))
      }
    ).recover {
      case ex: Exception => InternalServerError("An error ocuured" + ex.getMessage)
    }
  }

  def signUp: Action[AnyContent] = Action.async { implicit request =>
    signUpForm.bindFromRequest().fold(
      signUpWithError => {
        Future(BadRequest(views.html.index(loginForm)(signUpWithError)))
      },
      {
        case signUpData(firstName, lastName, email, password) => {
          mod.insertUser(User(new ObjectId(), firstName, lastName, email, password, None, None, List.empty[Address])).map(userId =>
            Redirect(routes.HomeController.shop()).withSession("userId" -> userId))
        }
        case _ => Future(Ok("not possible singnup"))
      }
    ).recover {
      case ex: Exception => InternalServerError("An error ocuured" + ex.getMessage)
    }
  }

  def user(): Action[AnyContent] = UserAction {
    Action.async { implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      mod.getUser(userId).map {
        case Some(user) => Ok(views.html.userProfile(userId)(user)(addressForm))
        case None => BadRequest("user Not Found")
      }.recover {
        case ex: Exception => InternalServerError("An error occurred" + ex.getMessage)
      }
    }
  }

  def shop(): Action[AnyContent] = UserAction {
    Action.async { implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      mod.getAllProducts().map { allProducts => Ok(views.html.homePage(userId)(allProducts)(cartForm)) }
        .recover {
          case ex: Exception => InternalServerError("An error ocuured" + ex.getMessage)
        }
    }
  }
  def home(): Action[AnyContent] = UserAction{
    Action.async{ implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      mod.getAllProducts().map{allProducts =>Ok(views.html.home(userId)(allProducts)(cartForm))}
        .recover{
          case ex: Exception => InternalServerError("An error ocuured" +ex.getMessage)
        }
    }
  }
  def addCart(): Action[AnyContent] = UserAction{
    Action.async { implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      cartForm.bindFromRequest().fold(
        cartFormError => mod.getAllProducts().map{allProducts =>Ok(views.html.homePage(userId)(allProducts)(cartFormError))},
        {
          case cartData(productId, userId, quantity) => mod.addItemInCart(productId, userId, quantity).map(_=>Redirect(routes.HomeController.shop()))
          case _ => Future(BadRequest("quantity is not a number"))
        }
      ).recover{
        case ex: Exception => InternalServerError("An error occured "+ ex.getMessage)
      }

    }
  }
  def getCartItems(): Action[AnyContent] = UserAction{
    Action.async{ implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      mod.getCartItems(userId).map{cartItems => Ok(views.html.cartItems(userId)(cartItems))}
        .recover{
          case ex: Exception => InternalServerError("An error ocuured" + ex.getMessage)
        }
    }
  }

  def getProduct(productId: String): Action[AnyContent] = UserAction{
    Action.async{ implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      mod.getProduct(productId).map {
        case Some(product) => Ok(views.html.product(userId)(product)(cartForm))
        case None => BadRequest("Product doesn't exists")
      }.recover{
          case ex: Exception => InternalServerError("An error occured" + ex.getMessage)
        }
    }
  }
  def addShippingAddress(): Action[AnyContent] = UserAction{
    Action.async{ implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      addressForm.bindFromRequest().fold(
        formWithError => mod.getUser(userId).map{
          case Some(user) => {
            BadRequest(views.html.userProfile(userId)(user)(formWithError))
          }
          case None => BadRequest("user not found")
        },
        {
          case addressData(addressLine1, addressLine2, city, pincode, state, country) => mod.addShippingAddress(userId)(addressLine1, addressLine2, city, pincode, state, country).map{
            _ => Redirect(routes.HomeController.user())
          }
        }
      ).recover{
        case ex: Exception =>  InternalServerError("An error ocuured" + ex.getMessage)
      }
    }
  }

  def buyProduct(): Action[AnyContent] = UserAction {
    Action.async{ implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      cartForm.bindFromRequest().fold(
        formWithError => Future(BadRequest("quantity is not a number")),
        {
          case cartData(productId, userId, quantity) =>{
            mod.getProduct(productId).flatMap{
              case Some(product) => {
                Stripe.apiKey = config.get[String]("Payment.Secret_Key")
                val params: Future[SessionCreateParams] = for{
                  prod <- {
                    val productParams: ProductCreateParams = ProductCreateParams.builder()
                      .setName(product.name)
                      .setDescription(product.description)
                      .setActive(true)
                      .build()
                    Future(stripeProduct.create(productParams))
                  }
                  price <- {
                    val priceParams: PriceCreateParams = PriceCreateParams.builder()
                      .setUnitAmount(product.price.toLong)
                      .setCurrency("usd")
                      .setProduct(prod.getId)
                      .build()
                    Future(Price.create(priceParams))
                  }
                } yield {
                  SessionCreateParams.builder()
                    .setMode(SessionCreateParams.Mode.PAYMENT)
                    .setSuccessUrl(s"http://localhost:9000/home/$userId")
                    .setCancelUrl(s"http://localhost:9000/shop/$userId")
                    .setCurrency("usd")
                    .addLineItem(
                      SessionCreateParams.LineItem.builder()
                        .setPrice(price.getId)
                        .setQuantity(quantity.toInt)
                        .build()
                    ).build()
                }
                val session: Future[Session] = params.flatMap(parameters => Future(Session.create(parameters)))
                session.map(_.getUrl).map(url => Redirect(url))
              }
              case None => Future(BadRequest("product not found"))
            }
          }
          case _ => Future(BadRequest("not possible"))
        }
      ).recover{
        case ex: Exception => InternalServerError("An error occured" +ex.getMessage)
      }
    }
  }

  def checkOut(): Action[AnyContent] = UserAction{
    Action.async{ implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      Stripe.apiKey = config.get[String]("Payment.Secret_Key")
      val paramsBuilder: SessionCreateParams.Builder = SessionCreateParams.builder()
        .setMode(SessionCreateParams.Mode.PAYMENT)
        .setSuccessUrl(s"http://localhost:9000/home/$userId")
        .setCancelUrl(s"http://localhost:9000/shop/$userId")
        .setCurrency("usd")

      val params: Future[SessionCreateParams] = mod.getCartItems(userId).flatMap{items =>
        items.foldLeft(Future(paramsBuilder)){
          case (paramsBuild,(cartId,product, quantity)) => {
            for{
              prod <- {
                val productParams: ProductCreateParams = ProductCreateParams.builder()
                  .setName(product.name)
                  .setDescription(product.description)
                  .setActive(true)
                  .build()
                Future(stripeProduct.create(productParams))
              }
              price <- {
                val priceParams: PriceCreateParams = PriceCreateParams.builder()
                  .setUnitAmount(product.price.toLong)
                  .setCurrency("usd")
                  .setProduct(prod.getId)
                  .build()
                Future(Price.create(priceParams))
              }
            } yield {
              paramsBuild.map(_.addLineItem(
                SessionCreateParams.LineItem.builder()
                  .setQuantity(quantity)
                  .setPrice(price.getId)
                  .build()
              ))
            }
          }.flatten
        }.map{_.build()}
      }
      val session: Future[Session] = params.flatMap{paramters =>Future(Session.create(paramters))}
      session.map(_.getUrl).map(url => Redirect(url)).recover{
        case ex: Exception => InternalServerError("An error occured" +ex.getMessage)
      }
    }
  }
  def removeItemFromCart(cartItemId: String): Action[AnyContent] = UserAction{
    Action.async{ implicit request =>
      val userId: String = request.session.get("userId").getOrElse("random")
      mod.removeFromCart(cartItemId,userId).map{
        _ => Redirect(routes.HomeController.getCartItems())
      }
    }
  }

  def about(): Action[AnyContent] = Action{ implicit request =>
   Ok(views.html.about())
  }
  def contact(): Action[AnyContent] = Action{ implicit request =>
    Ok(views.html.contact())
  }
  def logout(): Action[AnyContent] = Action{
    Redirect(routes.HomeController.index()).withNewSession
  }
}

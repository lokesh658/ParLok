package controllers

import play.api.mvc._

import javax.inject._
import models.{Address, Product, User, model}
import org.mongodb.scala.bson.ObjectId
import play.api.data._
import play.api.data.Forms._

import scala.util._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


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


@Singleton
class HomeController @Inject() (cc: MessagesControllerComponents)(mod: model) extends MessagesAbstractController(cc) {

  val loginForm: Form[loginData] = Form(
    mapping(
    "Email" -> text(5,8),
    "Password" ->text(4)
    )(loginData.apply)(loginData.unapply)
  )

  val signUpForm: Form[signUpData] = Form(
    mapping(
      "FirstName" -> text,
      "LastName"  -> text,
      "Email" -> text,
      "Password" -> text
    )(signUpData.apply)(signUpData.unapply)
  )

  def index: Action[AnyContent] = Action { implicit request => Ok(views.html.index(loginForm)(signUpForm))}
  def login: Action[AnyContent] = Action { implicit request =>
    loginForm.bindFromRequest().fold(
      loginWithError =>BadRequest(views.html.index(loginWithError)(signUpForm)),
      _ =>Redirect(routes.HomeController.user("32"))
    )
  }
  def signUp: Action[AnyContent] = Action.async { implicit request =>
    signUpForm.bindFromRequest().fold(
      signUpWithError =>{
        println("gettin error")
        Future(BadRequest(views.html.index(loginForm)(signUpWithError)))
      },
      form => (form match {
        case signUpData(firstName, lastName, email, password)=> {
          mod.insertUser(User(new ObjectId(),firstName, lastName, email, password, None, None)).map(userID => Redirect(routes.HomeController.user(userID)))
        }
        case _ => Future(Ok("not possible"))
      })
    ).recover{
      case ex: Exception => InternalServerError("An error ocuured" +ex.getMessage)
    }
  }
  def user(id: String): Action[AnyContent] = Action {
    Ok(s"hello user $id")
  }
}

package controllers

import play.api.mvc._
import javax.inject._
import models.{model,User,Address,Product}

@Singleton
class HomeController @Inject() (cc: ControllerComponents)(mod: model) extends AbstractController(cc) {
  def index: Action[AnyContent] = Action {
    mod.insertUser(User("John", "Doe", "john.doe@example.com", "9876543210", Address("123 Main St", "Mumbai", "400001", "Maharashtra", "India"))
    )
    Ok(views.html.index())
  }
}

package controllers

import play.api.mvc._
import javax.inject._

@Singleton
class HomeController @Inject() (cc: ControllerComponents) extends AbstractController(cc) {
  def index: Action[AnyContent] = Action {
    Ok(views.html.index())
  }
}

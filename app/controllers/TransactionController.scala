package controllers

import javax.inject._

import play.api.mvc._
import play.api.libs.json._

import model.Transaction
import services.TransactionService

class TransactionController @Inject() (service: TransactionService) extends Controller {

  def put(id: Long) = Action(BodyParsers.parse.json) { req =>
    val parseRes = req.body.validate[Transaction]
    parseRes match {
      case JsError(errs) =>
        BadRequest(
          Json.obj(
            "status" -> "error",
            "details" -> JsError.toJson(errs)))
      case JsSuccess(t, _) => service.put(id, t) match {
        case Left(err) => err.toResult
        case Right(_) =>
          Ok(Json.obj("status" -> "ok"))
      }
    }
  }

  def get(id: Long) = Action { _ =>
    service.get(id) match {
      case Left(err) => err.toResult
      case Right(t) => Ok(Json.toJson(t))
    }
  }

  def getType(tpe: String) = Action { _ =>
    Ok(Json.toJson(service.getType(tpe)))
  }

  def getSum(id: Long) = Action { _ =>
    service.getSum(id) match {
      case Left(err) => err.toResult
      case Right(sum) => Ok(Json.obj("sum" -> sum))
    }
  }
}

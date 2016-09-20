package model

import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Transaction(
  amount: Double,
  tpe: String,
  parentId: Option[Long]
)

object Transaction {

  implicit val writer: Writes[Transaction] = (
    (JsPath \ "amount").write[Double] and
    (JsPath \ "type").write[String] and
    (JsPath \ "parent_id").writeNullable[Long]
  )(unlift(Transaction.unapply))

  implicit val reader: Reads[Transaction] = (
    (JsPath \ "amount").read[Double] and
    (JsPath \ "type").read[String] and
    (JsPath \ "parent_id").readNullable[Long]
  )(Transaction.apply _)
}

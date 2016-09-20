package services

import javax.inject._

import collection.mutable.{ Map => MutableMap }

import play.api.mvc.{ Result, Results }
import play.api.libs.json.Json

import model.Transaction
import utils.TreeSum

trait TransactionService {
  import TransactionService._
  def put(id: Long, trans: Transaction): Either[Error, Unit]
  def get(id: Long): Either[Error, Transaction]
  def getType(tpe: String): Set[Long]
  def getSum(id: Long): Either[Error, Double]
}

// Disclaimer : this implementation uses the custom TreeSum data structure which
// supposes that we read transactions from the service more often than we write
// them in. Check TreeSum comments for more details.
class InMemoryTransactionService extends TransactionService {

  import TransactionService._

  private val store = MutableMap[Long, Transaction]()
  private val types = MutableMap[String, Set[Long]]()
  private val sums = TreeSum[Double]()

  /*
   * Return Left(errorMessage) if the parentId specified does not correspond to
   * any id
   * */
  def put(id: Long, trans: Transaction): Either[Error, Unit] =
    trans.parentId
      .map(id => id -> store.get(id).isEmpty) match {
      case Some((id, true)) => 
        Left(InvalidParentId(id))
      case _ => 
        sums
          .insert(id, trans.amount, trans.parentId)
          .map { _ =>
            store += (id -> trans)
            val ids = types.getOrElse(trans.tpe, Set())
            types += (trans.tpe -> (ids + id))
          } match {
            case None => Left(TransactionIdAlreadyInserted(id))
            case Some(_) => Right(())
          }
      }

  def get(id: Long): Either[Error, Transaction] = 
    store.get(id) match {
      case Some(t) => Right(t)
      case None => Left(InvalidTransactionId(id))
    }

  def getType(tpe: String): Set[Long] = types.getOrElse(tpe, Set())

  def getSum(id: Long): Either[Error, Double] =
    sums.get(id) match {
      case Some(d) => Right(d)
      case None => Left(InvalidTransactionId(id))
    }
}

object TransactionService {

  import Results._

  sealed trait Error {
    def message: String
    def toResult: Result
  }
  case class InvalidTransactionId(id: Long) extends Error {
    val message = s"${id} is not a valid transaction id"
    val toResult = NotFound(Json.obj("status" -> "error", "details" -> message))
  }
  case class InvalidParentId(parentTransId: Long) extends Error {
    val message = s"${parentTransId} is not a valid parent transaction id"
    val toResult = NotFound(Json.obj("status" -> "error", "details" -> message))
  }
  case class TransactionIdAlreadyInserted(id: Long) extends Error {
    val message = s"Transaction ${id} has already been inserted"
    val toResult = BadRequest(Json.obj("status" -> "error", "details" -> message))
  }
}

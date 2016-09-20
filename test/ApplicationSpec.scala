import scala.concurrent.Future

import org.scalatestplus.play._

import play.api.libs.json.Json
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Result
import play.api.test._
import play.api.test.Helpers._

import model.Transaction

class ApplicationSpec extends PlaySpec with OneAppPerTest {

  def putRequest(id: Long, t: Transaction) =
    FakeRequest(PUT, s"/transactionservice/transaction/$id")
      .withJsonBody(Json.toJson(t))

  def validatePut(future: Future[Result]) = {
    status(future) mustEqual OK
    contentAsJson(future) mustEqual Json.obj("status" -> "ok")
  }

  "TransactionController" should {

    "store some transactions" in  {

      val transaction1 = Transaction(10.0, "restaurant", None)
      val transaction2 = Transaction(100.0, "shopping", None)
      val transaction3 = Transaction(50.0, "holiday", Some(1L))

      val Some(result1) = route(app, putRequest(1L, transaction1))
      validatePut(result1)
      val Some(result2) = route(app, putRequest(2L, transaction2))
      validatePut(result2)
      val Some(result3) = route(app, putRequest(3L, transaction3))
      validatePut(result3)
    }

    "reject malformed json transactions" in {
      val Some(result) = route(
        app,
        FakeRequest(PUT, "/transactionservice/transaction/1")
          .withJsonBody(Json.parse("""{ "inexistantField": "foo" }""")))

      status(result) mustEqual BAD_REQUEST
    }

    "reject transactions referring to inexisting parent transaction" in {

      val transaction1 = Transaction(10.0, "restaurant", None)
      val transaction2 = Transaction(100.0, "shopping", Some(3))

      val Some(result1) = route(app, putRequest(1L, transaction1))
      validatePut(result1)

      val Some(result2) = route(app, putRequest(2L, transaction2))
      status(result2) mustEqual NOT_FOUND
      contentAsJson(result2) mustEqual Json.obj(
        "status" -> "error",
        "details" -> "3 is not a valid parent transaction id"
      )
    }

    "reject transactions already inserted" in {

      val transaction1 = Transaction(10.0, "restaurant", None)
      val transaction2 = Transaction(100.0, "shopping", None)

      val Some(result1) = route(app, putRequest(1L, transaction1))
      validatePut(result1)

      val Some(result2) = route(app, putRequest(1L, transaction2))
      status(result2) mustEqual BAD_REQUEST
      contentAsJson(result2) mustEqual Json.obj(
        "status" -> "error",
        "details" -> "Transaction 1 has already been inserted"
      )
    }

    "get previously inserted transactions" in {
      val transaction1 = Transaction(10.0, "restaurant", None)
      val transaction2 = Transaction(50.0, "holiday", Some(1L))

      val Some(resultPut1) = route(app, putRequest(1L, transaction1))
      validatePut(resultPut1)
      val Some(resultPut2) = route(app, putRequest(2L, transaction2))
      validatePut(resultPut2)

      val Some(resultGet1) = route(app, FakeRequest(GET, "/transactionservice/transaction/1"))
      status(resultGet1) mustEqual OK
      contentAsJson(resultGet1) mustEqual Json.toJson(transaction1)
      val Some(resultGet2) = route(app, FakeRequest(GET, "/transactionservice/transaction/2"))
      status(resultGet2) mustEqual OK
      contentAsJson(resultGet2) mustEqual Json.toJson(transaction2)
    }

    "not get inexisting transactions" in {
      val Some(resultGet) = route(app, FakeRequest(GET, "/transactionservice/transaction/3"))
      status(resultGet) mustEqual NOT_FOUND
      contentAsJson(resultGet) mustEqual Json.obj(
        "status" -> "error",
        "details" -> "3 is not a valid transaction id"
      )
    }

    "get transactions of one type" in {
      val transaction1 = Transaction(10.0, "restaurant", None)
      val transaction2 = Transaction(100.0, "shopping", None)
      val transaction3 = Transaction(50.0, "holiday", Some(1L))
      val transaction4 = Transaction(70.0, "restaurant", Some(1L))

      val Some(resultPut1) = route(app, putRequest(1L, transaction1))
      validatePut(resultPut1)
      val Some(resultPut2) = route(app, putRequest(2L, transaction2))
      validatePut(resultPut2)
      val Some(resultPut3) = route(app, putRequest(3L, transaction3))
      validatePut(resultPut3)
      val Some(resultPut4) = route(app, putRequest(4L, transaction4))
      validatePut(resultPut4)

      val Some(resultTypeHoliday) = route(app, FakeRequest(GET, "/transactionservice/types/holiday"))
      status(resultTypeHoliday) mustEqual OK
      contentAsJson(resultTypeHoliday) mustEqual Json.toJson(Set(3))
      val Some(resultTypeRestaurant) = route(app, FakeRequest(GET, "/transactionservice/types/restaurant"))
      status(resultTypeRestaurant) mustEqual OK
      contentAsJson(resultTypeRestaurant) mustEqual Json.toJson(Set(1, 4))
      val Some(resultTypeMovie) = route(app, FakeRequest(GET, "/transactionservice/types/movie"))
      status(resultTypeMovie) mustEqual OK
      contentAsJson(resultTypeMovie) mustEqual Json.arr()
    }

    "get sum of previously inserted transactions" in {
      val transaction1 = Transaction(10.5, "restaurant", None)
      val transaction2 = Transaction(100.0, "shopping", None)
      val transaction3 = Transaction(50.0, "holiday", Some(1L))
      val transaction4 = Transaction(70.0, "restaurant", Some(1L))

      val Some(resultPut1) = route(app, putRequest(1L, transaction1))
      validatePut(resultPut1)
      val Some(resultPut2) = route(app, putRequest(2L, transaction2))
      validatePut(resultPut2)
      val Some(resultPut3) = route(app, putRequest(3L, transaction3))
      validatePut(resultPut3)
      val Some(resultPut4) = route(app, putRequest(4L, transaction4))
      validatePut(resultPut4)

      val Some(resultSum1) = route(app, FakeRequest(GET, "/transactionservice/sum/1"))
      status(resultSum1) mustEqual OK
      contentAsJson(resultSum1) mustEqual Json.obj("sum" -> 130.5)
      val Some(resultSum2) = route(app, FakeRequest(GET, "/transactionservice/sum/2"))
      status(resultSum2) mustEqual OK
      contentAsJson(resultSum2) mustEqual Json.obj("sum" -> 100.0)
      val Some(resultSum3) = route(app, FakeRequest(GET, "/transactionservice/sum/3"))
      status(resultSum3) mustEqual OK
      contentAsJson(resultSum3) mustEqual Json.obj("sum" -> 50.0)
      val Some(resultSum4) = route(app, FakeRequest(GET, "/transactionservice/sum/4"))
      status(resultSum4) mustEqual OK
      contentAsJson(resultSum4) mustEqual Json.obj("sum" -> 70.0)
    }

    "not get sum of inexisting transactions" in {
      val Some(resultGet) = route(app, FakeRequest(GET, "/transactionservice/transaction/3"))
      status(resultGet) mustEqual NOT_FOUND
      contentAsJson(resultGet) mustEqual Json.obj(
        "status" -> "error",
        "details" -> "3 is not a valid transaction id"
      )
    }
  }
}

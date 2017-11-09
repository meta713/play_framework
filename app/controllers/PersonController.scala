package controllers

import play.api._
import play.api.mvc._
import play.api.i18n._
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.json.{JsValue, Json, Writes}
import models._
import dal._

import scala.concurrent.{ExecutionContext, Future}
import javax.inject._

import play.mvc.Http.{HeaderNames, MimeTypes}

class PersonController @Inject() (repo: PersonRepository, val messagesApi: MessagesApi)
                                 (implicit ec: ExecutionContext) extends Controller with I18nSupport{

  /**
   * The mapping for the person form.
   */
  val personForm: Form[PersonForm] = Form {
    mapping(
      "id" -> optional(longNumber),
      "name" -> nonEmptyText,
      "age" -> number.verifying(min(0), max(140))
    )(PersonForm.apply)(PersonForm.unapply)
  }

  /**
   * The index action.
   */
  def index = Action { implicit request =>
    Ok(views.html.index(personForm))
  }

  /**
   * The add person action.
   *
   * This is asynchronous, since we're invoking the asynchronous methods on PersonRepository.
   */

  def newPerson = Action { implicit request =>
    Ok(views.html.person.personForm(personForm, None))
  }

  def toFilledForm(person: Person): Form[PersonForm] = personForm.fill(PersonForm(person.id, person.name, person.age))

  def show(id: Long) = Action.async { implicit request =>
    repo.findById(id).map {
      case Some(person) => {
        request.contentType match {
          case Some(MimeTypes.JSON) => {
            Ok(Json.toJson(person))
          }
          case _ => {
            Ok(views.html.person.show(person))
          }
        }
      }
      case None => Redirect(routes.PersonController.list)
    }
  }

  def edit(id: Long) = Action.async{ implicit request =>
    repo.findById(id).map {
      case Some(person) => Ok(views.html.person.personForm(toFilledForm(person), Some(id)))
      case None => Redirect(routes.PersonController.list)
    }
  }

  implicit val PersonWrites = new Writes[Person] {
    override def writes(person: Person): JsValue = Json.obj(
      "id" -> person.id,
      "name" -> person.name,
      "age" -> person.age
    )
  }

  def list = Action.async { implicit request =>
    repo.list().map { people =>
      request.contentType match {
        case Some(MimeTypes.JSON) => {
          Ok(Json.toJson(people))
        }
        case _ => {
          Ok(views.html.person.list(people))
        }
      }
    }
  }

  def create = Action.async { implicit request =>

    request.contentType match {
      case Some(MimeTypes.JSON) => {
        val body: AnyContent = request.body
        val jsonBody: Option[JsValue] = body.asJson

        if (request.headers.keys.contains(HeaderNames.X_REQUESTED_WITH)) {
          jsonBody.map { json =>
            repo.create(Person(None, (json \ "name").as[String], (json \ "age").as[Int])).map { _ =>
              // If successful, we simply redirect to the index page.
              Ok(Json.obj("status" ->"success"))
            }
          }.getOrElse {
            Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("parse error"))))
          }
        } else {
          Future.successful(BadRequest(Json.obj("status" ->"error", "message" -> ("CSRF protection"))))
        }
      }
      case _ => {
        personForm.bindFromRequest.fold(
          errorForm => {
            Future.successful(Ok(views.html.person.personForm(errorForm, None)))
          },
          person => {
            repo.create(Person(None, person.name, person.age)).map { _ =>
              // If successful, we simply redirect to the index page.
              Redirect(routes.PersonController.list)
            }
          }
        )
      }
    }
  }



  def addPerson = Action.async { implicit request =>
    // Bind the form first, then fold the result, passing a function to handle errors, and a function to handle succes.
    personForm.bindFromRequest.fold(
      // The error function. We return the index page with the error form, which will render the errors.
      // We also wrap the result in a successful future, since this action is synchronous, but we're required to return
      // a future because the person creation function returns a future.
      errorForm => {
        Future.successful(Ok(views.html.index(errorForm)))
      },
      // There were no errors in the from, so create the person.
      person => {
        repo.create(Person(None, person.name, person.age)).map { _ =>
          // If successful, we simply redirect to the index page.
          Redirect(routes.PersonController.index)
        }
      }
    )
  }

  /**
   * A REST endpoint that gets all the people as JSON.
   */
  def getPersons = Action.async { implicit request =>
  	repo.list().map { people =>
      Ok(Json.toJson(people))
    }
  }
}

/**
 * The create person form.
 *
 * Generally for forms, you should define separate objects to your models, since forms very often need to present data
 * in a different way to your models.  In this case, it doesn't make sense to have an id parameter in the form, since
 * that is generated once it's created.
 */
//case class CreatePersonForm(name: String, age: Int)
case class PersonForm(id: Option[Long], name: String, age: Int)
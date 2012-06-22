package com.stackmob.newman.serialization.request

import scalaz._
import Scalaz._
import com.stackmob.newman.serialization.common.SerializationBase
import com.stackmob.newman.request._
import com.stackmob.newman.request.HttpRequest.Headers
import com.stackmob.newman.request.HttpRequestWithBody.RawBody
import net.liftweb.json._
import net.liftweb.json.scalaz.JsonScalaz._
import java.net.URL
import com.stackmob.newman.HttpClient

/**
 * Created by IntelliJ IDEA.
 *
 * com.stackmob.newman.serialization.request
 *
 * User: aaron
 * Date: 5/11/12
 * Time: 1:38 PM
 */

class HttpRequestSerialization(client: HttpClient) extends SerializationBase[HttpRequest] {
  import com.stackmob.newman.serialization.common.URLSerialization.{writer => URLWriter, reader => URLReader}
  import com.stackmob.newman.serialization.request.HttpRequestTypeSerialization.{writer => HttpRequestTypeWriter, reader => HttpRequestTypeReader}
  import com.stackmob.newman.serialization.common.HeadersSerialization.{writer => HeadersWriter, reader => HeadersReader}
  import com.stackmob.newman.serialization.common.RawBodySerialization.{writer => RawBodyWriter, reader => RawBodyReader}

  protected val RequestTypeKey = "type"
  protected val URLKey = "url"
  protected val HeadersKey = "headers"
  protected val BodyKey = "body"

  implicit override val writer = new JSONW[HttpRequest] {

    override def write(req: HttpRequest) = {
      val baseFields: List[JField] = JField(RequestTypeKey, toJSON(req.requestType)(HttpRequestTypeWriter)) ::
        JField(URLKey, toJSON(req.url)(URLWriter)) ::
        JField(HeadersKey, toJSON(req.headers)(HeadersWriter)) ::
        Nil

      val finalFields: List[JField] = req match {
        case r: HttpRequestWithBody => baseFields :+ JField(BodyKey, toJSON(r.body)(RawBodyWriter))
        case _ => baseFields
      }

      JObject(finalFields)
    }
  }

  implicit override val reader = new JSONR[HttpRequest] {
    import com.stackmob.newman.request.HttpRequestType._
    override def read(json: JValue) = {
      val typeField = field[HttpRequestType](RequestTypeKey)(json)(HttpRequestTypeReader)
      typeField.flatMap { reqType: HttpRequestType =>
        val urlField = field[URL](URLKey)(json)(URLReader)
        val headersField = field[Headers](HeadersKey)(json)(HeadersReader)
        val bodyField = field[RawBody](BodyKey)(json)(RawBodyReader)
        val baseApplicative = urlField |@| headersField
        val withBodyApplicative = baseApplicative |@| bodyField
        val res: ValidationNEL[Error, HttpRequest] = reqType match {
          case GET => baseApplicative(client.get(_, _))
          case POST => withBodyApplicative(client.post(_, _, _))
          case PUT => withBodyApplicative(client.put(_, _, _))
          case DELETE => baseApplicative(client.delete(_, _))
          case HEAD => baseApplicative(client.head(_, _))
          case _ => UncategorizedError("request type",
            "unsupported request type %s".format(reqType.stringVal),
            List()).failNel
        }
        res
      }
    }
  }


}
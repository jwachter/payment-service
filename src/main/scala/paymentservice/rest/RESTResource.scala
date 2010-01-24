package paymentservice.rest

import _root_.net.liftweb.http._
import _root_.net.liftweb.common._

/*
 * Trait for simplifying the implementation of RESTful Resources
*/
trait RESTResource {
	// urls matching this resource
	val dispatch : LiftRules.DispatchPF
  
	// Define the handle function for the different request types
	val get : (Req, String) => Box[LiftResponse] = (r, c) => Full(MethodNotAllowedResponse())
    val put : (Req, String) => Box[LiftResponse] = (r, c) => Full(MethodNotAllowedResponse())
    val post : (Req, String) => Box[LiftResponse] = (r, c) => Full(MethodNotAllowedResponse())
    val delete : (Req, String) => Box[LiftResponse] = (r, c) => Full(MethodNotAllowedResponse())

    // The content types supported by this Resource
	val supportedContentTypes:List[String]
 
	private final def determineContentType(req:Req):Box[String]={
	  req match {
		// GetRequest: Context overrules Accept Header (if present)
	    case r@Req(_,contentType,GetRequest) => decide(contentType, "Accept")
	    case r@Req(_,contentType,DeleteRequest) => decide(contentType, "Accept")
	    case r@Req(_,contentType,_) => decide(contentType, "Content-Type")
	    case _ => Empty  
	  }   
	}
 
	private final def decide(contentType:String, header:String):Box[String]={
	  contentType match {
	      case "json" => Full("json")
	      case "xml" => Full("xml")
	      case _ => S.getRequestHeader(header) match {
	        case Full("application/json") => Full("json")
	        case Full("application/x-json") => Full("json")
	        case Full("text/json") => Full("json")
	        case Full("application/xml") => Full("xml")
	        case Full("text/xml") => Full("xml")
	        case _ => Empty
	      }
	    }
	}
 
	final def process(req:Req) : Box[LiftResponse] = {
		try {
			val requestType = req.requestType
			val contentType = determineContentType(req)
			if(!supportedContentTypes.contains(contentType.openOr("undefined")) || contentType.isEmpty)
				Full(UnsupportedMediaTypeResponse())
			else
				// Dispatch dependent on request type
				requestType match {
				  case GetRequest => get(req, contentType.open_!)
				  case PostRequest => post(req, contentType.open_!)
				  case PutRequest => put(req, contentType.open_!)
				  case DeleteRequest => delete(req, contentType.open_!)
				  case _ => Full(MethodNotAllowedResponse())
				}
		} catch {
			case e:NoSuchElementException => Full(MethodNotAllowedResponse())
			case e:Exception => Full(InternalServerErrorResponse())
		}
	}
}

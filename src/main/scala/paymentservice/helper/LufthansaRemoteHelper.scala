package paymentservice.helper

import _root_.scala.xml._
import _root_.net.liftweb.common._
import _root_.org.apache.http._
import _root_.org.apache.http.client._
import _root_.org.apache.http.client.methods._
import _root_.org.apache.http.impl.client._

import specification._

object LufthansaRemoteHelper {
	def retrieveTicketValue(id:String):Box[Int]={
	  val client : DefaultHttpClient = new DefaultHttpClient()
   
	// Retrieve Ticket information as XML
	val get : HttpGet= new HttpGet("http://localhost:8080/api/ticket/"+id+"/payment.xml");
 
	val handler : ResponseHandler[String] = new BasicResponseHandler
 
	 val response : HttpResponse = client.execute(get);
  
	  val contents = handler.handleResponse(response)
   
	  val xml = XML.loadString(contents)
   
	  client.getConnectionManager.shutdown;
   
	  Full(xml.text.toInt)
	}
}

/**
 * Copyright 2010 Johannes Wachter, Marcus Körner, Johannes Potschies, Jeffrey Groneberg, Sergej Jakimcuk
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package paymentservice.helper

// Import Scala classes.
import _root_.scala.xml._

// Import Lift modules.
import _root_.net.liftweb.common._

// Import Apache HTTP Client.
import _root_.org.apache.http._
import _root_.org.apache.http.client._
import _root_.org.apache.http.entity._
import _root_.org.apache.http.client.methods._
import _root_.org.apache.http.impl.client._

// Import specification classes.
import specification._

//
// Helper for communicating with the airline webservice without user interaction.
//
object LufthansaRemoteHelper {
	//
	// Retrieve the value of a specific Ticket that should be booked.
	//
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
 
	//
	// Notify the Airline that payment succeeded.
	//
	def notifyPayee(id:String, pres: PaymentResult) : Boolean = {
			val client : DefaultHttpClient = new DefaultHttpClient()
	
			// Retrieve Ticket information as XML
			val put : HttpPut= new HttpPut("http://localhost:8080/api/ticket/"+id+"/payment.xml");

			put.setEntity(new StringEntity(pres.toXML.toString))
			
			val response : HttpResponse = client.execute(put);
   

			val statusLine = response.getStatusLine
			
			client.getConnectionManager.shutdown;
			
			if(statusLine.getStatusCode == 200){
			  true
			} else {
			  false
			}
	}
}

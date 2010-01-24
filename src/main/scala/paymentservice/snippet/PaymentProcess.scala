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
package paymentservice.snippet

// Import needed Scala classes.
import _root_.scala.xml._

// Import needed Lift modules.
import _root_.net.liftweb.http._
import _root_.net.liftweb.common._

// Import Joda Time.
import _root_.org.joda.time._
import _root_.org.joda.time.format._

// Import application classes.
import paymentservice.helper._

//
// Simple Billable implementation that is sufficient for our 'dummy' payment handling here.
//
class SimpleTicket(override val id:String, override val price:Int, override val payee:String) extends specification.Billable

//
// Snippet that implements and manages the payment process.
//
class PaymentProcess{
	//
	// Hold an instance of our payment service
	// 
	private val europay = new specification.EuroPay 
  
	//
	// Holds the ID of the Ticket that should be billed.
	//
	object TicketIDHolder extends SessionVar[Box[String]](Empty)
 
	//
	// Holds the price of the Ticket that should be billed.
	//
	object PriceHolder extends SessionVar[Box[Int]](Empty)
 
	//
	// Holds the SimpleTicket as billing information for the service.
	//
	object TicketHolder extends SessionVar[Box[SimpleTicket]](Empty)
 
	//
	// Step 1: Show what has been sent from the payee.
	//
	def overview(xhtml:NodeSeq):NodeSeq = {
	  	val id = S.param("item") match {
	  	  case Full(ident) => ident
	  	  case _ => (-1).toString 
	  	}
	  	val payee = S.param("payee") match {
	  	case Full(ident) => ident
	  	case _ => ""
	  	}
    
	  	val valueContainer = LufthansaRemoteHelper.retrieveTicketValue(id)
    
	  	val value = valueContainer match {
	  	  case Full(v) if v > 0 => v
	  	  case _ => 0 
	  	}
    
	  	TicketHolder.set(Full(new SimpleTicket(id, value, payee)))
    
	  	TicketIDHolder.set(Full(id))
	  	PriceHolder.set(Full(value))
    
	  	// Goto Step 2 > Choose Billing Method
	  	S.redirectTo("/billing.html")
	}
 
	//
	// Additional Step 2 methods
	//
	def billing(xhtml:NodeSeq):NodeSeq={
	  <p><strong>{TicketHolder.is.open_!.payee}</strong> wants you to pay <strong>{TicketHolder.is.open_!.price}</strong> for item <strong>{TicketHolder.is.open_!.id}</strong></p>
	}
 
	//
	// Step 3: validate the billing methods data.
	//
	def validate(xhtml:NodeSeq):NodeSeq = {
	  val f = DateTimeFormat.forPattern("yyyy/MM/dd")
	  
	  // Check each parameter and if set an error message and redirect to entering the data again.
	  val cardType = S.param("type") match {
	    case Full(n) if !n.isEmpty => n
	    case _=> S.error("Invalid card type. Please enter again.");S.redirectTo("/billing.html") 
	  }
	  val number = S.param("number") match {
	  case Full(n) if !n.isEmpty && n.length == 6 => n
	  case _=> S.error("Invalid number. Please enter again.");S.redirectTo("/billing.html") 
	  }
	  val holder = S.param("holder") match {
	  case Full(n) if !n.isEmpty => n
	  case _=> S.error("No holder information. Please enter again.");S.redirectTo("/billing.html") 
	  }
	  val expire = S.param("expire") match {
	  case Full(n) if !n.isEmpty => try{f.parseDateTime(n).toDate}catch{case e:Exception => S.error("No holder information. Please enter again.");S.redirectTo("/billing.html")}
	  case _=> S.error("No holder information. Please enter again.");S.redirectTo("/billing.html") 
	  }
	  val securityCode = S.param("securityCode") match {
		  case Full(n) if !n.isEmpty => n
		  case _=> if(cardType == "mastercard" || cardType == "visa"){ S.error("No security code. Please enter again.");S.redirectTo("/billing.html")}else{""} 
	  }
	  val issuer = S.param("issuer") match {
		  case Full(n) if !n.isEmpty => n
		  case _=> S.error("No issuer information. Please enter again.");S.redirectTo("/billing.html") 
	  }
	  val pin = S.param("pin") match {
		  case Full(n) if !n.isEmpty => n
		  case _=> if(cardType == "ec"){S.error("No pin code. Please enter again.");S.redirectTo("/billing.html")}else{""} 
	  }
	  
	  // Iinit the credit cards
	  val res : specification.PaymentResult = cardType match {
	    case "mastercard" =>  val c = new specification.MasterCard(number, holder, expire, securityCode, issuer);europay.pay(TicketHolder.is.open_!, c) 
	    case "visa" => val c = new specification.Visa(number, holder, expire, securityCode, issuer);europay.pay(TicketHolder.is.open_!, c)
	    case "ec" => val c = new specification.ElectronicCash(number, holder, expire, pin);europay.pay(TicketHolder.is.open_!, c)
	  }
   
	  var success = false
	  if(res.success){
		  // Notify airline about successful payment
	   	  success = LufthansaRemoteHelper.notifyPayee(TicketIDHolder.is.open_!, res)
	  }
   
	  if(success){
		S.redirectTo("/complete.html") 
	  } else {
	    S.error("Couldn't complete payment because an error occured when notifying the airline.");S.redirectTo("/billing.html") 
	  }
	}

	//
	// Step 4: Show confirmation and returnlink to payee.
	//
	def complete(xhtml:NodeSeq):NodeSeq = {
		val linkBack = "http://localhost:8080/ticket.html?id="+TicketIDHolder.is.open_!
		val result = <p class="success">Payment completed! <a href={linkBack}>Return to your Ticket!</a></p>
  
		// Clear the caches.
		TicketIDHolder.set(Empty)
		PriceHolder.set(Empty)
		TicketHolder.set(Empty)
		
		result
	}
}

package paymentservice.snippet

import _root_.scala.xml._

import _root_.net.liftweb.http._
import _root_.net.liftweb.common._

import _root_.org.joda.time._
import _root_.org.joda.time.format._

import paymentservice.helper._

class SimpleTicket(override val id:String, override val price:Int, override val payee:String) extends specification.Billable

class PaymentProcess{
	private val europay = new specification.EuroPay 
  
	object TicketIDHolder extends SessionVar[Box[String]](Empty)
	object PriceHolder extends SessionVar[Box[Int]](Empty)
	object TicketHolder extends SessionVar[Box[SimpleTicket]](Empty)
	object PaymentSuccessHolder extends SessionVar[specification.PaymentResult](null)
  
	// Step 1: Show what has been sent from the payee.
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
 
	// Step 3
	def validate(xhtml:NodeSeq):NodeSeq = {
	  val f = DateTimeFormat.forPattern("yyyy/MM/dd")
	  
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
   
	  if(res.success){
		S.redirectTo("/complete.html") 
	  } else {
	    S.error("Couldn't complete payment because an error occured.");S.redirectTo("/billing.html") 
	  }
	}

	// Step 4: Show success/failure and returnlink to payee.
	def complete(xhtml:NodeSeq):NodeSeq = {
		val linkBack = "http://localhost:8080/ticket.html?id="+TicketIDHolder.is.open_!
		<p>Payment completed! <a href={linkBack}>Return to your Ticket!</a></p>
	}
}

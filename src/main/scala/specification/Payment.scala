package specification {
  
import _root_.java.util.Date
import _root_.org.joda.time._

trait Billable {
  val id: String    // globally unique identifier
  val price: Int    // to make it simple, you can assume the total price = price of the itinerary x number of travelers
  val payee: String // which airline to be paid?
}


class PaymentResult(val success:Boolean, val message:String) {

  def toXML = 
    <paymentResult>
      <success>{ success }</success>
      <message>{ message }</message>
    </paymentResult>
}

abstract class PaymentGateway {
  
  val validPayees = List("lufthansa")
  
  val url: String   // user must visit this url to complete payments

  def pay(item: Billable, visaCard: Visa): PaymentResult = {
    val check = if(item.price > 0 && validPayees.contains(item.payee)){
      if(visaCard.expireDate.after(new DateTime().plusDays(1).toDate)){
        true
      } else {
        false
      }
    } else {
      false
    }

    // Nothing special to do .............

    new PaymentResult(check, if(check){"Payed "+item.price+" for "+item.id}else{"You're transcation cannot be completed"})
  }

  def pay(item: Billable, masterCard: MasterCard): PaymentResult = {
    val check = if(item.price > 0 && validPayees.contains(item.payee)){
      if(masterCard.expireDate.after(new DateTime().plusDays(1).toDate)){
        true
      } else {
        false
      }
    } else {
      false
    }

    // Nothing special to do .............

    new PaymentResult(check, if(check){"Payed "+item.price+" for "+item.id}else{"You're transcation cannot be completed"})
  }
}

class EuroPay  extends PaymentGateway {
  val url = "http://localhost:9090"
  
  // accept Visa and Master, and ... 
  def pay(item: Billable, electronicCash: ElectronicCash): PaymentResult = {
    val check = if(item.price > 0 && validPayees.contains(item.payee)){
      if(electronicCash.expireDate.after(new DateTime().plusDays(1).toDate)){
        true
      } else {
        false
      }
    } else {
      false
    }

    // Nothing special to do .............

    new PaymentResult(check, if(check){"Payed "+item.price+" for "+item.id}else{"You're transcation cannot be completed"})
  }
}
}
package paymentservice

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot extends Bootable{
  def boot {
    LiftRules.early append { _ setCharacterEncoding "UTF-8" }
    
    // where to search snippet
    LiftRules.addToPackages("paymentservice")
  }
}


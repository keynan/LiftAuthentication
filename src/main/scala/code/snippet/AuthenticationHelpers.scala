package ca.dualityStudios.liftAuthentication

import scala.xml.{NodeSeq, Text}
import ca.dualityStudios.liftAuthentication._

trait AuthenticationHelpers {
	def MetaUser: MetaAuthenticationBehavior[_]
	
	def dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
		case "isLoggedIn" => isLoggedIn
		case "isNotLoggedIn" => isNotLoggedIn
	}
	
	def isLoggedIn(in: NodeSeq): NodeSeq = {
		if(MetaUser.isLoggedIn_?()){
			in
		} else {
			Text("")
		}
	}
	
	def isNotLoggedIn(in: NodeSeq): NodeSeq = {
		if(! MetaUser.isLoggedIn_?()){
			in
		} else {
			Text("")
		}
	}
	
}
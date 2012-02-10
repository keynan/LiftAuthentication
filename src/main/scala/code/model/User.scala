package ca.dualityStudios.liftAuthentication.model

import ca.dualityStudios.liftAuthentication._
import net.liftweb.mapper._

object User 
	extends User
	with MetaAuthenticationBehavior[User] 
	with MetaLocalCredentials[User]
	with UserManagement[User] {
	
	override def fieldOrder = List(id, email, name, password)

}


class User 
	extends AuthenticationBehavior[User]
	with LocalCredentials[User]
	with IdPK {
	
	def getSingleton = User
	
	override def username = name
	object name extends MappedString(this, 64)
	
}

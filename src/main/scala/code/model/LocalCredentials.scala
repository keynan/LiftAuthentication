package ca.dualityStudios.liftAuthentication

import net.liftweb.mapper._

trait MetaLocalCredentials[OT <: LocalCredentials[OT]] 
	extends MetaAuthenticationBehavior[OT] 
	with LocalCredentials[OT] {
	
	self: OT =>
	
	override def fieldOrder = List(primaryKeyField, email, password)

	
}

trait LocalCredentials[OT <: LocalCredentials[OT]] 
	extends AuthenticationBehavior[OT] {

	self: OT => 
	
	def username : MappedField[String, OT] = email
	
	object email extends MappedEmail(this, 128)
	object password extends MappedPassword(this)
}

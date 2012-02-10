package ca.dualityStudios.liftAuthentication

import net.liftweb.mapper._

trait MetaAuthenticationBehavior[OwnerType <: AuthenticationBehavior[OwnerType]] 
	extends AuthenticationBehavior[OwnerType] 
	with LongKeyedMetaMapper[OwnerType] {
	
	self: OwnerType =>
	
}

trait AuthenticationBehavior[OwnerType <: AuthenticationBehavior[OwnerType]]
	extends LongKeyedMapper[OwnerType] 
	with BaseMapper {
	
	self: OwnerType =>
	
}
package ca.dualityStudios.liftAuthentication

import net.liftweb.mapper._
import net.liftweb.common.{Empty, Full, Box, Loggable}
import net.liftweb.http.{S, SessionVar, RequestVar}

trait MetaAuthenticationBehavior[OwnerType <: AuthenticationBehavior[OwnerType]] 
	extends AuthenticationBehavior[OwnerType] 
	with LongKeyedMetaMapper[OwnerType] 
	with Loggable {
	
	self: OwnerType =>
	
	object curUserPK extends SessionVar[Box[Long]](Empty)
	object curUser extends RequestVar[Box[OwnerType]](Empty)
	
	def logInUser(user: OwnerType){
		logger.info("logging in user with key " + uesr.primaryKey.openOr(0L) )
		curUserPK(user.primaryKey)
	}
	
	def logUserIn = logInUser _
	
	def homePage = "/"
	def logUserOut() = {
		curUserPK.remove()
		curUser.remove()
		S.redirectTo(homePage)
	}
	
	def currentUser() : Box[OwnerType] = {
		val ret = curUser.is match {
			// if the user is cached, good
			case Full(user) => Full(user)
			case _ => {
				curUserPK.is match {
					case Full(pk) => {
						logger.info("looking for user " + pk.toString)
						find(pk) match {
							// cache the user from look up
							case Full(user) => {
								curUser(Full(user))
								curUser.is
							}
							// could not find the user!!
							case _ => {
								curUserPK.remove()
								Empty
							}
						}
					}
					// user has not logged in
					case _ => Empty
				}
			}
		}
		logger.info("Fetched " + ret.toString())
		ret
	}
	
	def isLoggedIn_?() = {
		! curUserPK.is.isEmpty
	}
	
	
}

trait AuthenticationBehavior[OwnerType <: AuthenticationBehavior[OwnerType]]
	extends LongKeyedMapper[OwnerType] 
	with BaseMapper {
	
	self: OwnerType =>
	
	def primaryKey: Box[Long]
	object email extends MappedEmail(this, 128)
	
}
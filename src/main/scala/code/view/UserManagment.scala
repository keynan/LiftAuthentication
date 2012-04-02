package ca.dualityStudios.liftAuthentication

import net.liftweb.mapper._

trait UserManagement[OT <: KeyedMapper[Long, OT]] 
{
	self: OT with KeyedMetaMapper[Long, OT] =>
}
package ca.dualityStudios.liftAuthentication.model

import ca.dualityStudios.liftAuthentication._
import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.mapper._
import net.liftweb.proto._
import scala.xml._
import net.liftweb.http.{RequestVar, SHtml}
import net.liftweb.util._
import Helpers._

object User 
	extends User
	//with MetaAuthenticationBehavior[User] 
	with MetaLocalCredentials[User]
{
	
	override def fieldOrder = List(id, email, name, password)
	
	object nameVar extends RequestVar[String]("")

	override def updateInstance(obj: User) {
		super.updateInstance(obj)
		obj.name(nameVar.is)
	}
	
	override def initVarsFrom(obj: User) {
		super.initVarsFrom(obj)
		nameVar(obj.name.get)
	}
	override def selector(onSubmit: () => Unit) = {
		"name=name" #> SHtml.textElem(nameVar) &
		super.selector(onSubmit)
	}
	
	override def _changePropertiesTemplate = 
		<form method="post">
			<table class="lift:user.properties">
				<tr>
					<td>User Name:</td>
					<td><input type="text" name="name" /></td>
				</tr>
				<tr>
					<td>Email:</td>
					<td><input type="text" name="email" /></td>
				</tr>
				<tr>
					<td></td>
					<td><input type="submit" value="Submit" /></td>
				</tr>
			</table>
		</form>
	
	override def _signUpTemplate = 
		<form method="post">
			<table class="lift:user.signup">
				<tr>
					<td>User Name:</td>
					<td><input type="text" name="name" /></td>
				</tr>
				<tr>
					<td>Email:</td>
					<td><input type="text" name="email" /></td>
				</tr>
				<tr>
					<td>Password:</td>
					<td><input type="password" name="password" /></td>
				</tr>
				<tr>
					<td>Confirm Password:</td>
					<td><input type="password" name="confirm_password" /></td>
				</tr>
				<tr>
					<td></td>
					<td><input type="submit" value="Submit" /></td>
				</tr>
			</table>
		</form>
}


class User 
	extends AuthenticationBehavior[User]
	with LocalCredentials[User]
	with IdPK {
	
	def getSingleton = User
	
	def primaryKey = 
		if(primaryKeyField.get == 0) 
			Empty
		else
			Full(primaryKeyField.get)
	
	override def username = name
	object name extends MappedString(this, 64)
	
}

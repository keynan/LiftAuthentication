package ca.dualityStudios.liftAuthentication

import scala.xml.{NodeSeq, Text}
import net.liftweb.common.{Box, Full, Empty, Loggable}
import net.liftweb.mapper._
import net.liftweb.sitemap.{Menu, Loc}
import net.liftweb.sitemap.Menu.{Menuable}
import net.liftweb.sitemap.Loc.{DispatchLocSnippets}

import net.liftweb.http.{S, SHtml, RequestVar}
import net.liftweb.util._
import Helpers._

trait MetaLocalCredentials[OT <: LocalCredentials[OT]] 
	extends MetaAuthenticationBehavior[OT] 
	with LocalCredentials[OT]
	with Loggable {
	
	self: OT =>

	def skipEmailValidation = true
	
	//////////////// MENUs /////////////////////////////////////////////

	import net.liftweb.sitemap.Menu._
	
	def applyPath(menu: Menuable with WithSlash, pathing: List[String]): Menuable with WithSlash = {
		def enPath(x: String, y: Menuable with WithSlash): Menuable with WithSlash = y / x
		pathing.foldRight(menu)(enPath)
	}
	/*{
		pathing match {
			case Nil => menu
			case x::xs => applyPath((menu / x), xs)
		}
	}*/

	val prefix = "user_managment"
	val loginPath = "login" :: Nil
	val loginPathString = "Login"
	def loginMenu: Box[Menuable] = {
		val menu = applyPath(Menu(loginPathString, "Login") / prefix,  loginPath) >>
			snippetDispatch >>
			Loc.Template(loginTemplate) >>
			testUserIsLoggedOut_?
		Full(menu)
	}

/*	
		Full(Menu(
			Loc(
				"Login", 
				loginPath, 
				loginPathString, 
				List(snippetDispatch, Loc.Template(loginTemplate), testUserIsLoggedOut_?)
			)
		))
*/
	val logoutPath = "logout" :: Nil
	val logoutPathString = "Logout"
	def logoutMenu: Box[Menuable] = {
		Full(
			applyPath(Menu(logoutPathString, "Logout") / prefix, logoutPath) >> 
				Loc.Template(logUserOut) >>
				testUserIsLoggedIn_?
		)
	}
	
	
	lazy val testUserIsLoggedIn_? = Loc.If(() => isLoggedIn_?, () => S.redirectTo(homePage, () => S.notice(S.??("user.not.logged.in"))))
	lazy val testUserIsLoggedOut_? = Loc.If(() => !isLoggedIn_?, () => S.redirectTo(logoutMenu.toString))
	
	val signUpPath = "signUp" :: Nil
	val signUpPathString = "Sign Up"
	def signUpMenu: Box[Menuable] = {
		Full(
			applyPath(Menu(signUpPathString, "Sign Up") / prefix, signUpPath) >> 
				snippetDispatch >>
				Loc.Template(signUpTemplate) >>
				testUserIsLoggedOut_?
		)
	}

	val changePropertiesPath = "update" :: Nil
	val changePropertiesString = "Update Properties"
	def changePropertiesMenu: Box[Menuable] = {
		Full(
			applyPath(Menu(changePropertiesString, "Edit User Properties") / prefix, changePropertiesPath) >>
			snippetDispatch >>
			Loc.Template(changePropertiesTemplate) >>
			testUserIsLoggedIn_?
		)
	}
	
	val changePasswordPath = "change" :: "password" :: Nil
	val changePasswordString = "Change Password"
	def changePasswordMenu: Box[Menuable] = Full(
		applyPath(Menu(changePasswordString, "Edit User Password") / prefix, changePasswordPath) >>
		snippetDispatch >>
		Loc.Template(changePasswordTemplate) >>
		testUserIsLoggedIn_?
	)
	
	///////////// Behavour ///////////////////////////////////////////////

	object usernameVar extends RequestVar[String]("")
	object passwordVar extends RequestVar[String]("")
	object oldPasswordVar extends RequestVar[String]("")
	object confirmPasswordVar extends RequestVar[String]("")
	object emailVar extends RequestVar[String]("")
	
	def updateInstance(obj: OT) {
		obj.email(emailVar.is)
	}
	
	def updatePassword(obj: OT) {
		obj.password(passwordVar.is)
		//TODO How to handle confirm?
		/////////////////////////
	}
	
	def initVarsFrom(obj: OT) {
		logger.info("initializing emailVar")
		emailVar(obj.email.get)
		logger.info("emailVar == " + emailVar.is)
	}
	
	def selector(onSubmit: () => Unit) = 
		"name=username" #> SHtml.textElem(usernameVar) &
		"name=password" #> SHtml.passwordElem(passwordVar) &
		"name=old_password" #> SHtml.passwordElem(oldPasswordVar) &
		"name=email" #> SHtml.email(emailVar) &
		"name=confirm_password" #> SHtml.passwordElem(confirmPasswordVar) &
		"type=submit" #> SHtml.submit("Submit", onSubmit)
	
	lazy val snippetDispatch = new Loc.DispatchLocSnippets  {
    def handleFail() = {
			S.redirectTo(loginMenu.toString)
		}
		
		val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "user.login" => selector(onLoginSubmit)
			case "user.signup" => selector(onSignUpSubmit)
			
			case "user.properties" => {
				currentUser match {
					case Full(user) => {
						initVarsFrom(user)
						selector(onChangePropertiesSubmit)
					}
					case _ => handleFail()
				}
			}
			
			case "user.change_password" => {
				currentUser match {
					case Full(user) => selector(onChangePasswordSubmit)
					case _ => handleFail()
				}
			}
    }
	}

	
	def onLoginSubmit() {
		logger.info("logging in: search by user name: " + usernameVar.is)
		find(By(username, usernameVar.is)) match { 
			case Full(user) if(user.password.match_?(passwordVar.is)) => {
				logger.info("logging in: user " + user.id + " found")
				logInUser(user)
				S.redirectTo(homePage, () => S.notice("Successful Login."))
			}
			case _ => {
				logger.info("logging in: failed. available users")
				User.findAll().foreach{ user => logger.info(user.username) }
				S.notice("Login Failed.")
			}
		}
		usernameVar.remove()
		passwordVar.remove()
	}
	
	def onSignUpSubmit() = {
		val user = create
		updateInstance(user)
		updatePassword(user)
		user.validate match {
			case Nil => {
				user.save()
				S.redirectTo(homePage, () => S.notice("Sign Up Successful. Please check your email to confirm your account."))
			}
			case errors => S.error(errors)
		}
	}
	
	def onChangePropertiesSubmit() = {
		val user = currentUser.open_!
		updateInstance(user)
		user.validate match {
			case Nil => {
				user.save()
				S.redirectTo(homePage, () => S.notice("User Profile Updated Successfully."))
			}
			case errors => S.error(errors)
		}
	}
	
	def onChangePasswordSubmit() = {
		val user = currentUser.open_!
		if(user.password.match_?(oldPasswordVar.is)) {
			updatePassword(user)
			user.validate match {
				case Nil => {
					user.save()
					S.redirectTo(homePage, () => S.notice("User Password Updated Successfully."))
				}
				case errors => S.error(errors)
			}
		} else {
			S.error("incorrect password")
		}
	}
	
	
	
	/////////////// Templates ////////////////////////////////////////
	def screenWrap(body: NodeSeq): NodeSeq = {
		val surrounds = "lift:surround?with=default&amp;at=content"
		<div id="main" class={surrounds} >
			{body}
		</div>
	}
	
	
	
	def loginTemplate() = screenWrap(_loginTemplate)
	def _loginTemplate = 
		<form method="post">
			<table class="lift:user.login">
				<tr>
					<td>Name:</td>
					<td><input type="text" name="username" /></td>
				</tr>
				<tr>
					<td>Password:</td>
					<td><input type="password" name="password" /></td>
				</tr>
				<tr>
					<td></td>
					<td><input type="submit" value="Submit" /></td>
				</tr>
			</table>
		</form>
	
	def changePasswordTemplate() = screenWrap(_changePasswordTemplate)
	def _changePasswordTemplate = 
		<form method="post">
			<table class="lift:user.change_password">
				<tr>
					<td>Old Password:</td>
					<td><input type="password" name="old_password" /></td>
				</tr>
				<tr>
					<td>New Password:</td>
					<td><input type="password" name="password" /></td>
				</tr>
				<tr>
					<td>Confirm New Password:</td>
					<td><input type="password" name="confirm_password" /></td>
				</tr>
				<tr>
					<td></td>
					<td><input type="submit" value="Submit" /></td>
				</tr>
			</table>
		</form>
	
	def changePropertiesTemplate() = screenWrap(_changePropertiesTemplate)
	def _changePropertiesTemplate = 
		<form method="post">
			<table class="lift:user.properties">
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
	
	def signUpTemplate() = screenWrap(_signUpTemplate)
	def _signUpTemplate = 
		<form method="post">
			<table class="lift:user.signup">
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

trait LocalCredentials[OT <: LocalCredentials[OT]] 
	extends AuthenticationBehavior[OT] {

	self: OT => 
	
	def username : MappedField[String, OT] = email
	object password extends MappedPassword(this)
}

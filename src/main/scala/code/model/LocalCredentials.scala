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

import ca.dualityStudios.liftAuthentication.helpers.{TabularForms, FormField}


trait MetaLocalCredentials[OT <: LocalCredentials[OT]] 
	extends MetaAuthenticationBehavior[OT] 
	with LocalCredentials[OT]
	with TabularForms
{
	
	self: OT =>

	def skipEmailValidation = true
	
	//////////////// MENUs /////////////////////////////////////////////

	import net.liftweb.sitemap.Menu._
	
	def applyPath(menu: Menuable with WithSlash, pathing: List[String]): Menuable with WithSlash = {
		def enPath(x: String, y: Menuable with WithSlash): Menuable with WithSlash = y / x
		pathing.foldRight(menu)(enPath)
	}
	
	lazy val testUserIsLoggedIn_? = Loc.If(() => isLoggedIn_?, () => S.redirectTo(homePage, () => S.notice(S.??("user.not.logged.in"))))
	lazy val testUserIsLoggedOut_? = Loc.If(() => !isLoggedIn_?, () => S.redirectTo(logoutMenu.toString))
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

	val logoutPath = "logout" :: Nil
	val logoutPathString = "Logout"
	def logoutMenu: Box[Menuable] = {
		Full(
			applyPath(Menu(logoutPathString, "Logout") / prefix, logoutPath) >> 
				Loc.Template(logUserOut) >>
				testUserIsLoggedIn_?
		)
	}
	
	
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
	def changePasswordMenu: Box[Menuable] = {Full(
		applyPath(Menu(changePasswordString, "Edit User Password") / prefix, changePasswordPath) >>
		snippetDispatch >>
		Loc.Template(changePasswordTemplate) >>
		testUserIsLoggedIn_?
	)}
	
	///////////// Behavour ///////////////////////////////////////////////

	object usernameVar extends RequestVar[String]("")
	object passwordVar extends RequestVar[String]("")
	object oldPasswordVar extends RequestVar[String]("")
	object confirmPasswordVar extends RequestVar[String]("")
	object emailVar extends RequestVar[String]("")
	
	val usernameField = FormField("Name:", () => SHtml.textElem(usernameVar))
	val passwordField = FormField("Password:", () => SHtml.passwordElem(passwordVar))
	val emailField = FormField("Email:", () => SHtml.email(emailVar))
	val confirmPasswordField = FormField("Confirm New Password:", () => SHtml.passwordElem(confirmPasswordVar))
	val oldPasswordField = FormField("Old Password:", () => SHtml.passwordElem(oldPasswordVar))
	val newPasswordField = FormField("New Password:", () => SHtml.passwordElem(passwordVar))
	
	def updateInstance(obj: OT) {
		obj.email(emailVar.is)
	}
	
	def updatePassword(obj: OT) {
		obj.password(passwordVar.is)
		//TODO How to handle confirm?
		/////////////////////////
	}
	
	def initVarsFrom(obj: OT) {
		logger.debug("============")
		logger.debug(obj.email.get)
		emailVar(obj.email.get)
		logger.debug(emailVar.get)
	}
	
	
	lazy val snippetDispatch = new Loc.DispatchLocSnippets  {
    def handleFail() = {
			S.redirectTo(loginMenu.toString)
		}
		
		val dispatch: PartialFunction[String, NodeSeq => NodeSeq] = {
      case "user.login" => fieldRender(List(usernameField, passwordField,
				FormField("", () => SHtml.submit("Submit", onLoginSubmit))
			))
			case "user.signup" => fieldRender(signUpFields)
			
			case "user.properties" => {
				currentUser match {
					case Full(user) => {
						initVarsFrom(user)
						fieldRender(changePropertiesFields)
					}
					case _ => handleFail()
				}
			}
			
			case "user.change_password" => {
				currentUser match {
					case Full(user) => fieldRender(List(
						oldPasswordField, newPasswordField, confirmPasswordField,
						FormField("", () => SHtml.submit("Submit", onChangePasswordSubmit))
					))
					case _ => handleFail()
				}
			}
			
    }
	}

	
	def onLoginSubmit() {
		find(By(username, usernameVar.is)) match { 
			case Full(user) if(user.password.match_?(passwordVar.is)) => {
				logInUser(user)
				S.redirectTo(homePage, () => S.notice("Successful Login."))
			}
			case _ => S.notice("Login Failed.")
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
	
	def loginTemplate() = {
		screenWrap(form("post", "user.login"))
	}
	
	def changePasswordTemplate() = {
		screenWrap(form("post", "user.change_password"))
	}
	
	
	def changePropertiesFields = List( emailField, 
		FormField("", () => SHtml.submit("Submit", onChangePropertiesSubmit))
	)
	
	def changePropertiesTemplate() = {
		screenWrap(form("post", "user.properties"))
	}
	
	def signUpFields = List(emailField, passwordField, confirmPasswordField,
		FormField("", () => SHtml.submit("Submit", onSignUpSubmit))
	)
	def signUpTemplate() = {
		screenWrap(form("post", "user.signup"))
	}
}

trait LocalCredentials[OT <: LocalCredentials[OT]] 
	extends AuthenticationBehavior[OT] {

	self: OT => 
	
	def username : MappedField[String, OT] = email
	object password extends MappedPassword(this)
}

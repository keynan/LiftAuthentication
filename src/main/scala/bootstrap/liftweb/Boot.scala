package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import ca.dualityStudios.liftAuthentication.model._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
		if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
				Props.get("db.url") openOr "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
				Props.get("db.user"), Props.get("db.password")
			)
			LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)
			DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
		
    // where to search snippet
    LiftRules.addToPackages("code")
		LiftRules.addToPackages("ca.dualityStudios.liftAuthentication")
		
		Schemifier.schemify(true, Schemifier.infoF(_), User, CrudModel)
		
    // Build SiteMap
    var entries = List(
			Menu.i("Home") / "index", // the simple way to declare a menu
			User.loginMenu.open_!,
			User.logoutMenu.open_!,
			User.signUpMenu.open_!,
			User.changePropertiesMenu.open_!,
			User.changePasswordMenu.open_!,
			Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content"))
		)
    entries = entries ::: CrudModel.menus.map(x => x.toMenu)
		LiftRules.setSiteMap(SiteMap(entries:_*))
		
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts
    LiftRules.ajaxStart = Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    LiftRules.ajaxEnd = Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

		LiftRules.htmlProperties.default.set((r: Req) =>new Html5Properties(r.userAgent))
  }
}

package ca.dualityStudios.liftAuthentication.helpers

import net.liftweb.util._
import scala.xml.{NodeSeq, Elem}

trait TabularForms {
	
	import BindHelpers._

	def form(method: String, snippet: String, fields: List[FormField]) = 
		<form method={method}><table class={"lift:" + snippet}> 
			{fields.flatMap(applyTemplate(fieldTemplate))} 
		</table></form>
	
	def applyTemplate(template: NodeSeq)(field: FormField) = {
		(
			".lbl *" #> field.label &
			"#inp" #> field.input()
		).apply(template)
	}
	
	def fieldTemplate = {
		<tr>
			<td class="lbl"></td>
			<td><input id="inp" /></td>
		</tr>
	}
}

case class FormField(val label: String, val input:() => Elem)
